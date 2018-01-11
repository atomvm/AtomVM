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
#include <stdio.h>
#include <stdlib.h>

#ifdef WITH_ZLIB
#include <zlib.h>
#endif

#include "../../src/libAtomVM/iff.c"

#define LITT_UNCOMPRESSED_SIZE_OFFSET 8
#define LITT_HEADER_SIZE 12

static void pad_and_align(FILE *f);
static void *uncompress_literals(const uint8_t *litT, int size, size_t *uncompressedSize);

int main(int argc, char **argv)
{
    FILE *pack = fopen(argv[1], "w");
    if (!pack) {
        perror("Cannot open output file for writing");
        return EXIT_FAILURE;
    }

    unsigned char beam_header[12] =
    {
        0x46, 0x4f, 0x52, 0x31,
        0x00, 0x00, 0x00, 0x00,
        0x42, 0x45, 0x41, 0x4d
    };
    assert(fwrite(beam_header, 1, 12, pack) == 12);

    for (int i = 2; i < argc; i++) {
        FILE *beam = fopen(argv[i], "r");
        if (!beam) {
            perror("Cannot open beam file: ");
            return EXIT_FAILURE;
        }

        fseek(beam, 0, SEEK_END);
        size_t size = ftell(beam);
        fseek(beam, 0, SEEK_SET);

        uint8_t *data = malloc(size);
        assert(fread(data, sizeof(uint8_t), size, beam) == size);

        unsigned long offsets[MAX_OFFS];
        unsigned long sizes[MAX_SIZES];
        scan_iff(data, size, offsets, sizes);

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
    }

    fseek(pack, 0, SEEK_END);
    size_t size = ftell(pack);
    uint32_t size_field = ENDIAN_SWAP_32(size);
    fseek(pack, 4, SEEK_SET);
    assert(fwrite(&size_field, sizeof(uint32_t), 1, pack) == 1);

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
