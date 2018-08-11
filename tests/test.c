/***************************************************************************
 *   Copyright 2017 by Davide Bettio <davide@uninstall.it>                 *
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
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "atom.h"
#include "bif.h"
#include "context.h"
#include "../platforms/Linux/mapped_file.h"
#include "module.h"
#include "iff.h"
#include "term.h"
#include "utils.h"

struct Test{
    const char *test_file;
    int32_t expected_value;
};

struct Test tests[] =
{
    {"add.beam", 17},
    {"fact.beam", 120},
    {"mutrec.beam", 6},
    {"morelabels.beam", 6},
    {"biggerintegers.beam", 550},
    {"biggerdifference.beam", 250},
    {"moreintegertests.beam", 32},
    {"send_receive.beam", 18},
    {"byte_size_test.beam", 10},
    {"tuple.beam", 6},
    {"len_test.beam", 5},
    {"count_char.beam", 2},
    {"makelist_test.beam", 532},
    {"test_echo_driver.beam", 84},
    {"test_regecho_driver.beam", 11},
    {"test_send_nif_and_echo.beam", 11},
    {"state_test.beam", 3},
    {"booleans_test.beam", 4},
    {"booleans2_test.beam", 2},
    {"rem_and_comp_test.beam", 4},
    {"lowercase.beam", 15},
    {"huge.beam", 31},
    {"patternmatchfunc.beam", 102},
    {"moda.beam", 44},
    {"state_test2.beam", 3},
    {"state_test3.beam", 3},
    {"guards1.beam", 261},
    {"guards2.beam", 36},
    {"guards3.beam", 405},
    {"guards4.beam", 16},
    {"guards5.beam", 3},
    {"prime.beam", 1999},
    {"match.beam", 5},
    {"if_test.beam", 5},
    // sleep test will be repated 10 times
    {"sleep.beam", 392},
    {"sleep.beam", 392},
    {"sleep.beam", 392},
    {"sleep.beam", 392},
    {"sleep.beam", 392},
    {"sleep.beam", 392},
    {"sleep.beam", 392},
    {"sleep.beam", 392},
    {"sleep.beam", 392},
    {"sleep.beam", 392},
    {"hello_world.beam", 10},
    {"whereis_fail.beam", 2},
    {"try_noerror.beam", 1},
    {"catch_badmatch.beam", 1},
    {"catch_nocasematch.beam", 1},
    {"catch_noifmatch.beam", 1},
    {"list_concat.beam", 2270},
    {"make_ref_test.beam", 5},
    {"is_ref_test.beam", 3},
    {"tagged_tuple_test.beam", 48},
    {"call_with_ref_test.beam", 3},
    {NULL, 0}
};

int test_modules_execution()
{
    struct Test *test = tests;

    if (chdir("erlang_tests")) {
        return EXIT_FAILURE;
    }

    do {
        printf("-- EXECUTING TEST: %s\n", test->test_file);
        MappedFile *beam_file = mapped_file_open_beam(test->test_file);
        assert(beam_file != NULL);

        GlobalContext *glb = globalcontext_new();
        glb->avmpack_data = NULL;
        glb->avmpack_platform_data = NULL;
        Module *mod = module_new_from_iff_binary(glb, beam_file->mapped, beam_file->size);
        if (IS_NULL_PTR(mod)) {
            fprintf(stderr, "Cannot load startup module: %s\n", test->test_file);
            test++;
            continue;
        }
        globalcontext_insert_module_with_filename(glb, mod, test->test_file);
        Context *ctx = context_new(glb);
        ctx->leader = 1;

        context_execute_loop(ctx, mod, "start", 0);

        int32_t value = term_to_int32(ctx->x[0]);
        if (value != test->expected_value) {
            fprintf(stderr, "\x1b[1;31mFailed test module %s, got value: %i\x1b[0m\n", test->test_file, value);
        }

        context_destroy(ctx);
        globalcontext_destroy(glb);
        module_destroy(mod);
        mapped_file_close(beam_file);

        test++;
    } while (test->test_file);

    return EXIT_SUCCESS;
}

int main(int argc, char **argv)
{
    UNUSED(argc)

    time_t seed = time(NULL);
    printf("Seed is %li\n", seed);
    srand(seed);

    chdir(dirname(argv[0]));

    return test_modules_execution();
}
