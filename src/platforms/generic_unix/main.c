/*
 * This file is part of AtomVM.
 *
 * Copyright 2017-2023 Davide Bettio <davide@uninstall.it>
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

#include <libgen.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef __APPLE__
#include <mach-o/getsect.h>
#include <mach-o/ldsyms.h>
#elif defined(__linux__)
#include <errno.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#endif

#include "atom.h"
#include "avm_version.h"
#include "avmpack.h"
#include "bif.h"
#include "context.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "iff.h"
#include "mapped_file.h"
#include "module.h"
#include "sys.h"
#include "term.h"
#include "utils.h"

#ifdef __linux__
// On Linux (using ELF), the embedded avm binary is added as a section
// and the following symbols are patched.
uint64_t __atomvm_avm_offset __attribute__((section(".atomvm_avm_info")));
uint64_t __atomvm_avm_length __attribute__((section(".atomvm_avm_info")));
#endif

/**
 * @brief Destructor for embedded AVM pack data
 * @details Embedded AVM data is part of the executable and should not be freed
 */
static void embedded_avm_pack_destructor(struct AVMPackData *obj, GlobalContext *global)
{
    UNUSED(global);
    // Embedded data is part of the executable, so we only free the pack structure itself
    free(obj);
}

static const struct AVMPackInfo embedded_avm_pack_info = {
    .destructor = embedded_avm_pack_destructor
};

void print_help(const char *program_name)
{
    printf(
        "\n"
        "Syntax:\n"
        "\n"
        "    %s [-h] [-v] <path-to-avm-or-beam-file>+\n"
        "\n"
        "Options:\n"
        "\n"
        "    -h         Print this help and exit.\n"
        "    -v         Print the AtomVM version and exit.\n"
        "\n"
        "Supply one or more AtomVM packbeam (.avm) files to start your application.\n"
        "\n"
        "Example:\n"
        "\n"
        "    $ %s /path/to/my/application.avm /path/to/atomvmlib.avm\n"
        "\n",
        program_name, program_name);
}

/**
 * @brief Try to extract embedded AVM data from the executable itself
 * @param data pointer to store the embedded AVM data (if found)
 * @param size pointer to store the size of the embedded AVM data
 * @return true if embedded AVM data was found, false otherwise
 */
bool get_embedded_avm(const void **data, size_t *size)
{
#ifdef __APPLE__
    // On macOS, look for the __ATOMVM,__avm_data section
    unsigned long section_size = 0;
    const void *section_data = getsectiondata(&_mh_execute_header, "__ATOMVM", "__avm_data", &section_size);

    if (section_data && section_size > 0) {
        *data = section_data;
        *size = (size_t) section_size;
        return true;
    }
#elif defined(__linux__)
    // On Linux, check if symbols were added by objcopy during escriptize
    if (__atomvm_avm_offset != 0 && __atomvm_avm_length != 0) {
        int fd = open("/proc/self/exe", O_RDONLY);
        if (fd == -1) {
            fprintf(stderr, "Cannot open /proc/self/exe (errno = %d)\n", (int) errno);
            return false;
        }
        long page_size = sysconf(_SC_PAGESIZE);

        // Round down the offset to the nearest page boundary
        off_t page_aligned_offset = (__atomvm_avm_offset / page_size) * page_size;

        // Calculate the extra bytes needed to cover the target region
        size_t extra = __atomvm_avm_offset - page_aligned_offset;
        size_t map_length = __atomvm_avm_length + extra;

        void *map = mmap(NULL, map_length, PROT_READ, MAP_PRIVATE, fd, page_aligned_offset);
        if (map == MAP_FAILED) {
            fprintf(stderr, "Failed to mmap current executable (errno = %d)\n", (int) errno);
            close(fd);
            return false;
        }
        *data = (const void *) ((const char *) map + extra);
        *size = __atomvm_avm_length;
        return true;
    }
#else
    // embedded avm not supported yet on this target (e.g. FreeBSD)
    UNUSED(data);
    UNUSED(size);
#endif

    return false;
}

int main(int argc, char **argv)
{
    // Check for embedded AVM data first
    const void *embedded_data = NULL;
    size_t embedded_size = 0;
    bool has_embedded_avm = get_embedded_avm(&embedded_data, &embedded_size);

    // Only process getopt if not in embedded mode
    // In embedded mode, all arguments (including -h, -v) are passed to the script
    if (!has_embedded_avm) {
        int c;
        while ((c = getopt(argc, argv, "hv")) != -1) {
            switch (c) {
                case 'h':
                    print_help(argv[0]);
                    return EXIT_SUCCESS;

                case 'v':
                    printf(ATOMVM_VERSION "\n");
                    return EXIT_SUCCESS;

                default:
                    break;
            }
        }

        if (argc < 2) {
            printf("Syntax Error!  Missing .beam or .avm files.\n");
            print_help(argv[0]);
            return EXIT_FAILURE;
        }
    }

    GlobalContext *glb = globalcontext_new();

    Module *startup_module = NULL;

    if (has_embedded_avm) {
        struct AVMPackData *avmpack_data = malloc(sizeof(struct AVMPackData));
        if (!avmpack_data) {
            fprintf(stderr, "Failed to allocate memory for embedded AVM pack.\n");
            globalcontext_destroy(glb);
            return EXIT_FAILURE;
        }

        avmpack_data_init(avmpack_data, &embedded_avm_pack_info);
        avmpack_data->data = embedded_data;
        // Set the name for the embedded AVM pack so it can be found by atomvm:get_start_beam/1
        term escript_atom = globalcontext_make_atom(glb, ATOM_STR("\x7", "escript"));
        avmpack_data->name_atom_id = term_to_atom_index(escript_atom);

        synclist_append(&glb->avmpack_data, &avmpack_data->avmpack_head);
    }

    // Process command-line AVM/BEAM files only if not in embedded mode
    // In embedded mode, all remaining arguments are passed to the script
    if (!has_embedded_avm) {
        for (int i = 1; i < argc; ++i) {
            const char *ext = strrchr(argv[i], '.');
            if (ext && strcmp(ext, ".avm") == 0) {
                struct AVMPackData *avmpack_data;
                if (UNLIKELY(sys_open_avm_from_file(glb, argv[i], &avmpack_data) != AVM_OPEN_OK)) {
                    fprintf(stderr, "Failed opening %s.\n", argv[i]);
                    return EXIT_FAILURE;
                }
                synclist_append(&glb->avmpack_data, &avmpack_data->avmpack_head);

                if (IS_NULL_PTR(startup_module)) {
                    const void *startup_beam = NULL;
                    const char *startup_module_name;
                    uint32_t startup_beam_size;
                    avmpack_find_section_by_flag(avmpack_data->data, BEAM_START_FLAG, BEAM_START_FLAG, &startup_beam, &startup_beam_size, &startup_module_name);

                    if (startup_beam) {
                        avmpack_data->in_use = true;
                        startup_module = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
                        if (IS_NULL_PTR(startup_module)) {
                            fprintf(stderr, "Cannot load startup module: %s\n", startup_module_name);
                            return EXIT_FAILURE;
                        }
                        globalcontext_insert_module(glb, startup_module);
                        startup_module->module_platform_data = NULL;
                    }
                }

            } else if (ext && (strcmp(ext, ".beam") == 0)) {
                MappedFile *mapped_file = mapped_file_open_beam(argv[i]);
                if (!iff_is_valid_beam(mapped_file->mapped)) {
                    fprintf(stderr, "%s has invalid beam format.\n", argv[i]);
                    return EXIT_FAILURE;
                }
                Module *mod = module_new_from_iff_binary(glb, mapped_file->mapped, mapped_file->size);
                if (IS_NULL_PTR(mod)) {
                    fprintf(stderr, "Cannot load module: %s\n", argv[i]);
                    return EXIT_FAILURE;
                }
                globalcontext_insert_module(glb, mod);
                mod->module_platform_data = NULL;
                if (IS_NULL_PTR(startup_module) && module_search_exported_function(mod, START_ATOM_INDEX, 0) != 0) {
                    startup_module = mod;
                }

            } else {
                fprintf(stderr, "%s is not an AVM or a BEAM file.\n", argv[i]);
                return EXIT_FAILURE;
            }
        }
    }

    // Pass command-line arguments to escript in embedded mode
    run_result_t result;
    if (has_embedded_avm) {
        // Don't print return value in embedded mode (pass NULL instead of stderr)
        result = globalcontext_run(glb, startup_module, NULL, argc, argv);
    } else {
        result = globalcontext_run(glb, startup_module, stderr, 0, NULL);
    }

    int status;
    if (result == RUN_SUCCESS) {
        status = EXIT_SUCCESS;
    } else {
        status = EXIT_FAILURE;
    }

    globalcontext_destroy(glb);

    return status;
}
