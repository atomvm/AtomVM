/* Copyright 2026 Peter M. <petermm@gmail.com>
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */
#undef NDEBUG
#include <assert.h>
#include <setjmp.h>
#include <stdlib.h>
#include <string.h>

#include "app_heap.h"

static jmp_buf boot_return;
static int boot_status;
static _Noreturn void boot_exit(int status)
{
    boot_status = status;
    longjmp(boot_return, 1);
}

// Exercise the actual RTEMS fallback branch. Only board I/O and exit are stubbed.
#define exit boot_exit
#include "../../src/main.c"
#undef exit

static unsigned module_loads;
static bool fail_during_imports;

Module *__real_module_new_from_iff_binary(GlobalContext *global, const void *binary, unsigned long size);
Module *__wrap_module_new_from_iff_binary(GlobalContext *global, const void *binary, unsigned long size)
{
    bool replacement = module_loads++ == 0;
    if (replacement && fail_during_imports) {
        // Fail after one import descriptor has already been allocated.
        app_heap_fail_import = 2;
    }
    Module *mod = __real_module_new_from_iff_binary(global, binary, size);
    app_heap_fail_import = 0;
    assert(replacement ? mod == NULL : mod != NULL);
    return mod;
}

int rtems_atomvm_network_init(void)
{
    return 0;
}

bool storage_init(void)
{
    return true;
}

static uint32_t read_be32(const uint8_t *p)
{
    return ((uint32_t) p[0] << 24) | ((uint32_t) p[1] << 16) | ((uint32_t) p[2] << 8) | p[3];
}

bool storage_load_app(struct RtemsApp *app)
{
    uint8_t *copy = malloc(embedded_avm_size);
    assert(copy);
    memcpy(copy, embedded_avm, embedded_avm_size);
    assert(rtems_app_validate(copy, embedded_avm_size, app));
    if (!fail_during_imports) {
        // A bounded, structurally valid pack whose label allocation exceeds the
        // heap budget, after atoms and unresolved imports have been allocated.
        uint8_t *beam = (uint8_t *) app->startup_beam;
        size_t offset = 12;
        while (memcmp(beam + offset, "Code", 4) != 0) {
            offset += 8 + ((read_be32(beam + offset + 4) + 3) & ~(size_t) 3);
            assert(offset + 24 < app->startup_beam_size);
        }
        memset(beam + offset + 20, 0xff, 4); // Code.labels
        assert(rtems_app_validate(copy, embedded_avm_size, app));
    }
    return true;
}

int main(void)
{
    for (unsigned attempt = 0; attempt < 4; attempt++) {
        fail_during_imports = (attempt % 2) != 0;
        module_loads = 0;
        app_heap_denied = 0;
        assert(app_heap_used == 0);
        if (setjmp(boot_return) == 0) {
            Init(0);
            abort();
        }
        assert(boot_status == 0);
        assert(module_loads == 2);
        assert(app_heap_denied == 1);
        // Failed module, global tables, replacement buffer and successful
        // fallback must all release their allocations before the next boot.
        assert(app_heap_used == 0);
    }
    puts("RTEMS startup fallback: late and partial-import failures passed with a 256 KiB VM heap");
    return 0;
}
