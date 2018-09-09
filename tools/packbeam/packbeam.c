/***************************************************************************
 *   Copyright 2018 by Davide Bettio <davide@uninstall.it>                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#include <assert.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef WITH_ZLIB
#include <zlib.h>
#endif

#include "../../src/libAtomVM/iff.c"

#define LITT_UNCOMPRESSED_SIZE_OFFSET 8
#define LITT_HEADER_SIZE 12

#define END_OF_FILE 0
#define BEAM_START_FLAG 1
#define BEAM_CODE_FLAG 2

static void pad_and_align(FILE *f);
static void *uncompress_literals(const uint8_t *litT, int size, size_t *uncompressedSize);
static void add_module_header(FILE *f, const char *module_name, uint32_t flags);

int main(int argc, char **argv)
{
    FILE *pack = fopen(argv[1], "w");
    if (!pack) {
        perror("Cannot open output file for writing");
        return EXIT_FAILURE;
    }

    const unsigned char pack_header[24] =
    {
        0x23, 0x21, 0x2f, 0x75,
        0x73, 0x72, 0x2f, 0x62,
        0x69, 0x6e, 0x2f, 0x65,
        0x6e, 0x76, 0x20, 0x41,
        0x74, 0x6f, 0x6d, 0x56,
        0x4d, 0x0a, 0x00, 0x00
    };
    assert(fwrite(pack_header, sizeof(unsigned char), 24, pack) == 24);

    for (int i = 2; i < argc; i++) {
        FILE *beam = fopen(argv[i], "r");
        if (!beam) {
            perror("Cannot open beam file: ");
            return EXIT_FAILURE;
        }

        fseek(beam, 0, SEEK_END);
        size_t beam_size = ftell(beam);
        fseek(beam, 0, SEEK_SET);
        size_t zero_pos = ftell(pack);

        char *complete_name = strdup(argv[i]);
        char *filename = basename(complete_name);
        if (i == 2) {
            add_module_header(pack, filename, BEAM_CODE_FLAG | BEAM_START_FLAG);
        } else {
            add_module_header(pack, filename, BEAM_CODE_FLAG);
        }
        free(complete_name);

        int written_beam_header_pos = ftell(pack);
        const unsigned char beam_header[12] =
        {
            0x46, 0x4f, 0x52, 0x31,
            0x00, 0x00, 0x00, 0x00,
            0x42, 0x45, 0x41, 0x4d
        };
        assert(fwrite(beam_header, 1, 12, pack) == 12);

        uint8_t *data = malloc(beam_size);
        assert(fread(data, sizeof(uint8_t), beam_size, beam) == beam_size);

        unsigned long offsets[MAX_OFFS];
        unsigned long sizes[MAX_SIZES];
        scan_iff(data, beam_size, offsets, sizes);

        if (offsets[AT8U]) {
            assert(fwrite(data + offsets[AT8U], sizeof(uint8_t), sizes[AT8U] + IFF_SECTION_HEADER_SIZE, pack) == sizes[AT8U] + IFF_SECTION_HEADER_SIZE);
            pad_and_align(pack);
        }
        if (offsets[CODE]) {
            fwrite(data + offsets[CODE], sizeof(uint8_t), sizes[CODE] + IFF_SECTION_HEADER_SIZE, pack);
            pad_and_align(pack);
        }
        if (offsets[EXPT]) {
            fwrite(data + offsets[EXPT], sizeof(uint8_t), sizes[EXPT] + IFF_SECTION_HEADER_SIZE, pack);
            pad_and_align(pack);
        }
        if (offsets[LOCT]) {
            fwrite(data + offsets[LOCT], sizeof(uint8_t), sizes[LOCT] + IFF_SECTION_HEADER_SIZE, pack);
            pad_and_align(pack);
        }
        if (offsets[IMPT]) {
            fwrite(data + offsets[IMPT], sizeof(uint8_t), sizes[IMPT] + IFF_SECTION_HEADER_SIZE, pack);
            pad_and_align(pack);
        }

        if (offsets[LITT]) {
            size_t u_size;
            void *deflated = uncompress_literals(data + offsets[LITT], sizes[LITT], &u_size);
            assert(fwrite("LitU", sizeof(uint8_t), 4, pack) == 4);
            uint32_t size_field = ENDIAN_SWAP_32(u_size);
            assert(fwrite(&size_field, sizeof(size_field), 1, pack) == 1);
            assert(fwrite(deflated, sizeof(uint8_t), u_size, pack) == u_size);
            free(deflated);
        }

        pad_and_align(pack);

        size_t end_of_module_pos = ftell(pack);

        size_t size = end_of_module_pos - zero_pos;
        uint32_t size_field = ENDIAN_SWAP_32(size);
        fseek(pack, zero_pos, SEEK_SET);
        assert(fwrite(&size_field, sizeof(uint32_t), 1, pack) == 1);
        fseek(pack, end_of_module_pos, SEEK_SET);

        int beam_written_size = end_of_module_pos - written_beam_header_pos;
        uint32_t beam_written_size_field = ENDIAN_SWAP_32(beam_written_size);
        fseek(pack, written_beam_header_pos + 4, SEEK_SET);
        assert(fwrite(&beam_written_size_field , sizeof(uint32_t), 1, pack) == 1);
        fseek(pack, end_of_module_pos, SEEK_SET);
    }

    add_module_header(pack, "end", END_OF_FILE);

    return EXIT_SUCCESS;
}

static void *uncompress_literals(const uint8_t *litT, int size, size_t *uncompressedSize)
{
    unsigned int required_buf_size = READ_32_ALIGNED(litT + LITT_UNCOMPRESSED_SIZE_OFFSET);

    uint8_t *outBuf = malloc(required_buf_size);
    if (!outBuf) {
        fprintf(stderr, "Cannot allocate temporary buffer (size = %u)", required_buf_size);
        abort();
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
        fprintf(stderr, "Failed inflateInit\n");
        abort();
    }
    ret = inflate(&infstream, Z_NO_FLUSH);
    if (ret != Z_OK) {
        fprintf(stderr, "Failed inflate\n");
        abort();
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

static void add_module_header(FILE *f, const char *module_name, uint32_t flags)
{
    uint32_t size_field = 0;
    uint32_t flags_field = ENDIAN_SWAP_32(flags);
    uint32_t reserved = 0;

    assert(fwrite(&size_field, sizeof(uint32_t), 1, f) == 1);
    assert(fwrite(&flags_field, sizeof(uint32_t), 1, f) == 1);
    assert(fwrite(&reserved, sizeof(uint32_t), 1, f) == 1);
    assert(fwrite(module_name, sizeof(char), strlen(module_name) + 1, f) == strlen(module_name) + 1);
    pad_and_align(f);
}
