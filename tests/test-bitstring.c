/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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
#include <stdlib.h>
#include <string.h>

#include "bitstring.h"
#include "context.h"
#include "globalcontext.h"
#include "memory.h"
#include "term.h"
#include "utils.h"

// A copy must touch exactly the destination bytes that hold the copied bits,
// and read exactly the source bytes that hold them. Both buffers are bracketed
// with guard bytes so an off-by-one byte is caught here rather than by a
// sanitizer on an unrelated allocation.
#define GUARD 0x5A

struct guarded
{
    uint8_t before[4];
    uint8_t data[16];
    uint8_t after[4];
};

static void guarded_init(struct guarded *g, uint8_t fill)
{
    memset(g->before, GUARD, sizeof(g->before));
    memset(g->data, fill, sizeof(g->data));
    memset(g->after, GUARD, sizeof(g->after));
}

static void guarded_check(const struct guarded *g)
{
    for (size_t i = 0; i < sizeof(g->before); i++) {
        assert(g->before[i] == GUARD);
    }
    for (size_t i = 0; i < sizeof(g->after); i++) {
        assert(g->after[i] == GUARD);
    }
}

// Reference implementation: copy bits_count bits from the start of src to
// bits_offset in dst, one bit at a time, touching nothing else.
static void reference_copy_bits(
    uint8_t *dst, size_t bits_offset, const uint8_t *src, size_t bits_count)
{
    for (size_t i = 0; i < bits_count; i++) {
        size_t dst_bit = bits_offset + i;
        uint8_t mask = (uint8_t) (1U << (7 - (dst_bit % 8)));
        if (src[i / 8] & (uint8_t) (1U << (7 - (i % 8)))) {
            dst[dst_bit / 8] |= mask;
        } else {
            dst[dst_bit / 8] &= (uint8_t) ~mask;
        }
    }
}

static void check_copy_bits(size_t bits_offset, size_t bits_count, uint8_t src_fill)
{
    struct guarded src;
    guarded_init(&src, src_fill);

    struct guarded got;
    struct guarded expected;
    // Two different fills, so a byte the copy must not touch differs from the
    // byte it would be overwritten with.
    guarded_init(&got, 0xC3);
    guarded_init(&expected, 0xC3);

    reference_copy_bits(expected.data, bits_offset, src.data, bits_count);
    bitstring_copy_bits(got.data, bits_offset, src.data, bits_count);

    // The bytes past the last copied bit must be untouched, and no guard byte
    // on either side of either buffer may have moved.
    assert(memcmp(got.data, expected.data, sizeof(got.data)) == 0);
    guarded_check(&got);
    guarded_check(&src);
}

void test_copy_bits_boundaries(void)
{
    // Exact byte boundaries at an unaligned destination: the last copied bit
    // completes the destination byte, so nothing past it may be read or written.
    check_copy_bits(1, 7, 0xFF);
    check_copy_bits(1, 7, 0x00);
    // The source is consumed to its last bit and must not be read past it.
    check_copy_bits(1, 8, 0xFF);
    check_copy_bits(1, 8, 0x00);
    // A zero-bit copy touches neither buffer.
    check_copy_bits(0, 0, 0xFF);
    check_copy_bits(1, 0, 0xFF);
    check_copy_bits(7, 0, 0xFF);

    // Sweep offsets and counts, aligned and unaligned, including whole bytes.
    for (size_t offset = 0; offset < 16; offset++) {
        for (size_t count = 0; count <= 64; count++) {
            check_copy_bits(offset, count, 0xFF);
            check_copy_bits(offset, count, 0x00);
            check_copy_bits(offset, count, 0xA5);
        }
    }
}

// Copying into a destination sized to exactly the bits it holds, as a refc
// binary is: a byte of slack would hide a one-byte overrun.
void test_copy_bits_exact_allocation(void)
{
    for (size_t bits = 1; bits <= 24; bits++) {
        size_t bytes = (bits + 7) / 8;
        uint8_t *dst = calloc(bytes, 1);
        assert(dst != NULL);
        uint8_t src[4] = { 0xFF, 0xFF, 0xFF, 0xFF };
        // starting at bit 1 leaves the copy ending mid-byte or exactly on a
        // byte boundary depending on the count
        if (1 + bits <= bytes * 8) {
            bitstring_copy_bits(dst, 1, src, bits);
        }
        free(dst);
    }
}

void test_print_bitstring(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    // Reserve every term this function builds in one go: memory_ensure_free may
    // collect, and the terms below are plain C locals rather than roots, so a
    // second reservation would leave the earlier ones dangling.
    size_t heap_size = 3 * TERM_BOXED_SUB_BINARY_SIZE + 3 * term_binary_heap_size(1)
        + 2 * term_binary_heap_size(2);
    assert(memory_ensure_free(ctx, heap_size) == MEMORY_GC_OK);

    // <<1:1>>: no complete byte, a single trailing bit
    term bin1 = term_create_empty_binary(1, &ctx->heap, glb);
    ((uint8_t *) term_binary_data(bin1))[0] = 0x80;
    term bits1 = term_alloc_sub_binary_bits(bin1, 0, 0, 1, &ctx->heap);
    char buf[64];
    int len = term_snprint(buf, sizeof(buf), bits1, glb);
    assert(len > 0);
    assert(strcmp(buf, "<<1:1>>") == 0);

    // <<255, 5:7>>: a complete byte followed by a partial one
    term bin2 = term_create_empty_binary(2, &ctx->heap, glb);
    ((uint8_t *) term_binary_data(bin2))[0] = 0xFF;
    ((uint8_t *) term_binary_data(bin2))[1] = (uint8_t) (5 << 1);
    term bits2 = term_alloc_sub_binary_bits(bin2, 0, 1, 7, &ctx->heap);
    len = term_snprint(buf, sizeof(buf), bits2, glb);
    assert(len > 0);
    assert(strcmp(buf, "<<255,5:7>>") == 0);

    // a printable byte followed by trailing bits: OTP prints <<97,1:3>>, not
    // <<"a",1:3>>, so trailing bits defeat the printable-string form entirely
    term bin3 = term_create_empty_binary(2, &ctx->heap, glb);
    ((uint8_t *) term_binary_data(bin3))[0] = 'a';
    ((uint8_t *) term_binary_data(bin3))[1] = (uint8_t) (1 << 5);
    term bits3 = term_alloc_sub_binary_bits(bin3, 0, 1, 3, &ctx->heap);
    len = term_snprint(buf, sizeof(buf), bits3, glb);
    assert(len > 0);
    assert(strcmp(buf, "<<97,1:3>>") == 0);

    // a byte-aligned bitstring still prints as a plain binary, and a printable
    // one still prints as a quoted string
    term bin4 = term_create_empty_binary(1, &ctx->heap, glb);
    ((uint8_t *) term_binary_data(bin4))[0] = 0;
    len = term_snprint(buf, sizeof(buf), bin4, glb);
    assert(len > 0);
    assert(strcmp(buf, "<<0>>") == 0);

    term bin5 = term_create_empty_binary(1, &ctx->heap, glb);
    ((uint8_t *) term_binary_data(bin5))[0] = 42;
    len = term_snprint(buf, sizeof(buf), bin5, glb);
    assert(len > 0);
    assert(strcmp(buf, "<<\"*\">>") == 0);

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

int main(int argc, char **argv)
{
    UNUSED(argc);
    UNUSED(argv);

    test_copy_bits_boundaries();
    test_copy_bits_exact_allocation();
    test_print_bitstring();

    return EXIT_SUCCESS;
}
