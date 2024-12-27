/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Davide Bettio <davide@uninstall.it>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include <assert.h>
#include <libgen.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

// TODO: Remove this when the code is refactored
// #ifdef WITH_ZLIB
#include <zlib.h>
// #endif

#include "avmpack.h"
#include "iff.c"
#include "mapped_file.h"

#define TRY(expr)         \
    do {                  \
        if (!(expr)) {    \
            goto cleanup; \
        }                 \
    } while (0)

#define LITT_UNCOMPRESSED_SIZE_OFFSET 8
#define LITT_HEADER_SIZE 12

#define END_OF_FILE 0
#define BEAM_START_FLAG 1
#define BEAM_CODE_FLAG 2
#define BUF_SIZE 1024

typedef struct FileData
{
    uint8_t *data;
    size_t size;
} FileData;

static void pad_and_align(FILE *f);
static void *uncompress_literals(const uint8_t *litT, int size, size_t *uncompressedSize);
static bool add_module_header(FILE *f, const char *module_name, uint32_t flags);
static bool pack_beam_file(FILE *pack, const uint8_t *data, size_t size, const char *filename, int is_entrypoint, bool include_lines);

static void free_file_data(FileData *data);
static bool safe_read_file(const char *filename, FileData *data);
static bool safe_fread(void *buffer, size_t size, FILE *file);
static bool safe_fwrite(const void *buffer, size_t size, FILE *file);
static bool has_iff_header(uint8_t *data, size_t size);

static int do_pack(char *output_avm_file, char **input_files, size_t files_n, int is_archive, bool include_lines);
static int do_list(const char *avm_path);

#define program_error(...)                    \
    do {                                      \
        internal_error(argv[0], __VA_ARGS__); \
        usage(argv[0]);                       \
    } while (false)
#define packbeam_error(...)                      \
    do {                                         \
        internal_error("PackBeam", __VA_ARGS__); \
        usage("PackBeam");                       \
    } while (false)
#define packbeam_internal_error(...) internal_error("PackBeam", __VA_ARGS__)

static void internal_error(const char *program, const char *format, ...)
{
    va_list format_args;
    va_start(format_args, format);

    fprintf(stderr, "%s: ", program);
    vfprintf(stderr, format, format_args);
    fprintf(stderr, "\n");

    va_end(format_args);
}

static void usage(const char *program)
{
    fprintf(stderr, "\nUsage: %s [-h] [-l] <avm-file> [<options>]\n", program);
    fprintf(stderr, "    -h                                                Print this help menu.\n");
    fprintf(stderr, "    -i                                                Include file and line information.\n");
    fprintf(stderr, "    -l <input-avm-file>                               List the contents of an AVM file.\n");
    fprintf(stderr, "    [-a] <output-avm-file> <input-beam-or-avm-file>+  Create an AVM file (archive if -a specified).\n");
}

int main(int argc, char **argv)
{
    bool list_avm = false;
    bool is_archive = false;
    bool include_lines = false;

    int opt;
    while ((opt = getopt(argc, argv, "hail")) != -1) {
        switch (opt) {
            case 'h':
                usage(argv[0]);
                return EXIT_SUCCESS;
            case 'a':
                is_archive = true;
                break;
            case 'i':
                include_lines = true;
                break;
            case 'l':
                list_avm = true;
                break;
            case '?': {
                program_error("Unknown option: %c", optopt);
                return EXIT_FAILURE;
            }
        }
    }

    int new_argc = argc - optind;
    char **new_argv = argv + optind;

    if (list_avm) {
        if (new_argc < 1) {
            program_error("Listing needs an AVM file.");
            return EXIT_FAILURE;
        }
        const char *avm_file = new_argv[0];
        return do_list(avm_file);
    }

    if (new_argc < 2) {
        program_error("Pack needs output AVM file and at least one AVM or BEAM input file(s).");
        return EXIT_FAILURE;
    }
    char *output_avm_file = new_argv[0];
    char **input_files = &new_argv[1];
    size_t n = new_argc - 1;
    return do_pack(output_avm_file, input_files, n, is_archive, include_lines);
}

static void *print_section(void *accum, const void *section_ptr, uint32_t section_size, const void *beam_ptr, uint32_t flags, const char *section_name)
{
    UNUSED(section_ptr);
    UNUSED(section_size);
    UNUSED(beam_ptr);
    printf("%s %s\n", section_name, flags & BEAM_START_FLAG ? "*" : "");
    return accum;
}

static int do_list(const char *avm_path)
{
    MappedFile *mapped_file = NULL;
    // TODO: mapped_file_open_beam prints unnecessary warnings
    mapped_file = mapped_file_open_beam(avm_path);

    if (mapped_file == NULL) {
        packbeam_error("Cannot open AVM file %s", avm_path);
        goto cleanup;
    }
    if (!avmpack_is_valid(mapped_file->mapped, mapped_file->size)) {
        packbeam_error("%s is not an AVM file.", avm_path);
        goto cleanup;
    }

    avmpack_fold(NULL, mapped_file->mapped, print_section);
    return EXIT_SUCCESS;

cleanup:
    if (mapped_file != NULL) {
        mapped_file_close(mapped_file);
    }
    return EXIT_FAILURE;
}

static void *pack_beam_fun(void *accum, const void *section_ptr, uint32_t section_size, const void *beam_ptr, uint32_t flags, const char *section_name)
{
    UNUSED(beam_ptr);
    UNUSED(flags);
    UNUSED(section_name);
    if (accum == NULL) {
        return NULL;
    }

    FILE *pack = (FILE *) accum;
    size_t r = fwrite(section_ptr, sizeof(unsigned char), section_size, pack);
    if (r != section_size) {
        return NULL;
    }
    return accum;
}

static bool validate_pack_files(char *output_file, char **input_files, size_t files_n)
{
    FileData file_data = (FileData){ .data = NULL, .size = 0 };

    TRY(safe_read_file(output_file, &file_data));
    if (!avmpack_is_valid(file_data.data, file_data.size)) {
        packbeam_error("Invalid AVM file '%s'.", output_file);
        goto cleanup;
    }
    free_file_data(&file_data);

    for (size_t i = 0; i < files_n; ++i) {
        const char *filename = input_files[i];
        TRY(safe_read_file(filename, &file_data));

        bool is_valid_avm = avmpack_is_valid(file_data.data, file_data.size);
        bool is_valid_beam = has_iff_header(file_data.data, file_data.size);
        if (!(is_valid_avm || is_valid_beam)) {
            packbeam_error("Invalid AVM or BEAM file '%s'.", filename);
            goto cleanup;
        }

        free_file_data(&file_data);
    }

    return true;

cleanup:
    free_file_data(&file_data);
    return false;
}

static int do_pack(char *output_avm_file, char **input_files, size_t files_n, int is_archive, bool include_lines)
{
    FILE *pack = NULL;
    FileData file_data = (FileData){ .data = NULL, .size = 0 };

    TRY(validate_pack_files(output_avm_file, input_files, files_n));

    // TODO: having any error corrupts output file
    pack = fopen(output_avm_file, "w");
    if (pack == NULL) {
        packbeam_internal_error("Cannot open output file for writing %s.", output_avm_file);
        goto cleanup;
    }

    const unsigned char pack_header[24] = {
        0x23, 0x21, 0x2f, 0x75,
        0x73, 0x72, 0x2f, 0x62,
        0x69, 0x6e, 0x2f, 0x65,
        0x6e, 0x76, 0x20, 0x41,
        0x74, 0x6f, 0x6d, 0x56,
        0x4d, 0x0a, 0x00, 0x00
    };
    TRY(safe_fwrite(pack_header, 24, pack));

    for (size_t i = 0; i < files_n; ++i) {
        char *path = input_files[i];
        TRY(safe_read_file(path, &file_data));

        // TODO: redundant, we have this information in validate_pack_files
        bool is_avm = avmpack_is_valid(file_data.data, file_data.size);

        if (is_avm) {
            TRY(avmpack_fold(pack, file_data.data, pack_beam_fun) != NULL);
        } else {
            bool is_first = i == 0;
            bool is_entrypoint = !is_archive && is_first;
            char *filename = basename(path);
            TRY(pack_beam_file(pack, file_data.data, file_data.size, filename, is_entrypoint, include_lines));
        }
        free_file_data(&file_data);
    }

    TRY(add_module_header(pack, "end", END_OF_FILE));
    fclose(pack);

    return EXIT_SUCCESS;

cleanup:
    if (pack != NULL) {
        fclose(pack);
    }
    free_file_data(&file_data);
    return EXIT_FAILURE;
}

static bool pack_beam_file(FILE *pack, const uint8_t *data, size_t size, const char *section_name, int is_entrypoint, bool include_lines)
{
    void *deflated = NULL;

    long zero_pos = ftell(pack);
    if (zero_pos == -1) {
        goto cleanup;
    }

    if (is_entrypoint) {
        TRY(add_module_header(pack, section_name, BEAM_CODE_FLAG | BEAM_START_FLAG));
    } else {
        TRY(add_module_header(pack, section_name, BEAM_CODE_FLAG));
    }

    long written_beam_header_pos = ftell(pack);
    if (written_beam_header_pos == -1) {
        goto cleanup;
    }
    const unsigned char beam_header[12] = {
        0x46, 0x4f, 0x52, 0x31,
        0x00, 0x00, 0x00, 0x00,
        0x42, 0x45, 0x41, 0x4d
    };
    TRY(safe_fwrite(beam_header, 12, pack));

    unsigned long offsets[MAX_OFFS];
    unsigned long sizes[MAX_SIZES];
    scan_iff(data, size, offsets, sizes);

    if (offsets[AT8U]) {
        TRY(safe_fwrite(data + offsets[AT8U], sizes[AT8U] + IFF_SECTION_HEADER_SIZE, pack));
        pad_and_align(pack);
    }
    if (offsets[CODE]) {
        TRY(safe_fwrite(data + offsets[CODE], sizes[CODE] + IFF_SECTION_HEADER_SIZE, pack));
        pad_and_align(pack);
    }
    if (offsets[EXPT]) {
        TRY(safe_fwrite(data + offsets[EXPT], sizes[EXPT] + IFF_SECTION_HEADER_SIZE, pack));
        pad_and_align(pack);
    }
    if (offsets[LOCT]) {
        TRY(safe_fwrite(data + offsets[LOCT], sizes[LOCT] + IFF_SECTION_HEADER_SIZE, pack));
        pad_and_align(pack);
    }
    if (offsets[IMPT]) {
        TRY(safe_fwrite(data + offsets[IMPT], sizes[IMPT] + IFF_SECTION_HEADER_SIZE, pack));
        pad_and_align(pack);
    }
    if (offsets[LITU]) {
        TRY(safe_fwrite(data + offsets[LITU], sizes[LITU] + IFF_SECTION_HEADER_SIZE, pack));
        pad_and_align(pack);
    }
    if (offsets[FUNT]) {
        TRY(safe_fwrite(data + offsets[FUNT], sizes[FUNT] + IFF_SECTION_HEADER_SIZE, pack));
        pad_and_align(pack);
    }
    if (offsets[STRT]) {
        TRY(safe_fwrite(data + offsets[STRT], sizes[STRT] + IFF_SECTION_HEADER_SIZE, pack));
        pad_and_align(pack);
    }
    if (offsets[LINT] && include_lines) {
        TRY(safe_fwrite(data + offsets[LINT], sizes[LINT] + IFF_SECTION_HEADER_SIZE, pack));
        pad_and_align(pack);
    }

    if (offsets[LITT]) {
        size_t u_size;
        deflated = uncompress_literals(data + offsets[LITT], sizes[LITT], &u_size);
        TRY(safe_fwrite("LitU", 4, pack));
        uint32_t size_field = ENDIAN_SWAP_32(u_size);
        TRY(safe_fwrite(&size_field, sizeof(size_field), pack));
        TRY(safe_fwrite(deflated, u_size, pack));
        free(deflated);
    }

    pad_and_align(pack);

    long end_of_module_pos = ftell(pack);
    if (end_of_module_pos == -1) {
        goto cleanup;
    }

    size_t rsize = end_of_module_pos - zero_pos;
    uint32_t size_field = ENDIAN_SWAP_32(rsize);
    TRY(fseek(pack, zero_pos, SEEK_SET) != -1);
    TRY(safe_fwrite(&size_field, sizeof(uint32_t), pack));
    TRY(fseek(pack, end_of_module_pos, SEEK_SET) != -1);

    int beam_written_size = end_of_module_pos - written_beam_header_pos;
    uint32_t beam_written_size_field = ENDIAN_SWAP_32(beam_written_size);
    TRY(fseek(pack, written_beam_header_pos + 4, SEEK_SET) != -1);
    TRY(safe_fwrite(&beam_written_size_field, sizeof(uint32_t), pack));
    TRY(fseek(pack, end_of_module_pos, SEEK_SET) != -1);

    return true;

cleanup:
    free(deflated);

    return false;
}

static void *uncompress_literals(const uint8_t *litT, int size, size_t *uncompressedSize)
{
    unsigned int required_buf_size = READ_32_ALIGNED(litT + LITT_UNCOMPRESSED_SIZE_OFFSET);

    uint8_t *outBuf = malloc(required_buf_size);
    if (!outBuf) {
        packbeam_internal_error("Cannot allocate temporary buffer of size %u.", required_buf_size);
        AVM_ABORT();
    }

    z_stream infstream;
    infstream.zalloc = Z_NULL;
    infstream.zfree = Z_NULL;
    infstream.opaque = Z_NULL;
    infstream.avail_in = (uInt) (size - IFF_SECTION_HEADER_SIZE);
    infstream.next_in = (Bytef *) (litT + LITT_HEADER_SIZE);
    infstream.avail_out = (uInt) required_buf_size;
    infstream.next_out = (Bytef *) outBuf;

    int ret = inflateInit(&infstream);
    if (ret != Z_OK) {
        packbeam_internal_error("Failed inflateInit.");
        AVM_ABORT();
    }
    ret = inflate(&infstream, Z_NO_FLUSH);
    if (ret != Z_OK) {
        packbeam_internal_error("Failed inflate.");
        AVM_ABORT();
    }
    inflateEnd(&infstream);

    *uncompressedSize = required_buf_size;
    return outBuf;
}

static void pad_and_align(FILE *f)
{
    while ((ftell(f) % 4) != 0) {
        fputc(0, f);
    }
}

static bool add_module_header(FILE *f, const char *module_name, uint32_t flags)
{
    uint32_t size_field = 0;
    uint32_t flags_field = ENDIAN_SWAP_32(flags);
    uint32_t reserved = 0;

    TRY(safe_fwrite(&size_field, sizeof(uint32_t), f));
    TRY(safe_fwrite(&flags_field, sizeof(uint32_t), f));
    TRY(safe_fwrite(&reserved, sizeof(uint32_t), f));
    TRY(safe_fwrite(module_name, strlen(module_name) + 1, f));
    pad_and_align(f);

    return true;

cleanup:
    return false;
}

static bool has_iff_header(uint8_t *data, size_t size)
{
    return size >= 4 && iff_is_valid_beam(data);
}

static void free_file_data(FileData *data)
{
    if (data->data != NULL) {
        free(data->data);
    }
    data->data = NULL;
    data->size = 0;
}

static bool safe_fread(void *buffer, size_t size, FILE *file)
{
    size_t r = fread(buffer, sizeof(uint8_t), size, file);
    if (r != size) {
        packbeam_internal_error("Unable to read, wanted to read %zu bytes, read %zu bytes.", size, r);
        return false;
    }
    return true;
}

static bool safe_fwrite(const void *buffer, size_t size, FILE *file)
{
    size_t r = fwrite(buffer, 1, size, file);
    if (r != size) {
        packbeam_internal_error("Unable to write, wanted to write %zu bytes, wrote %zu bytes.", size, r);
        return false;
    }
    return true;
}

static bool safe_read_file(const char *filename, FileData *file_data)
{
    FILE *file = NULL;
    uint8_t *data = NULL;

    *file_data = (FileData){ .data = NULL, .size = 0 };

    file = fopen(filename, "r");
    if (file == NULL) {
        packbeam_internal_error("Cannot open file '%s'.", filename);
        goto cleanup;
    }

    TRY(fseek(file, 0, SEEK_END) == 0);
    long size = ftell(file);
    if (size == -1L) {
        goto cleanup;
    }
    TRY(fseek(file, 0, SEEK_SET) == 0);

    data = malloc(size);
    if (data == NULL) {
        goto cleanup;
    }

    TRY(safe_fread(data, size, file));
    TRY(fclose(file) == 0);

    *file_data = (FileData){
        .data = data,
        .size = size
    };
    return true;

cleanup:
    if (file != NULL) {
        fclose(file);
    }
    free(data);
    return false;
}