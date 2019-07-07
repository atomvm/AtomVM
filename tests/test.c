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
#include "../platforms/generic_unix/mapped_file.h"
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
    {"send_to_dead_process.beam", 20},
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
    {"try_catch_test.beam", 109},
    {"list_concat.beam", 2270},
    {"make_ref_test.beam", 130},
    {"is_ref_test.beam", 3},
    {"tagged_tuple_test.beam", 48},
    {"call_with_ref_test.beam", 3},
    {"just_receive_test.beam", 11},
    {"gen_server_like_test.beam", 3},
    {"external_proplist_test.beam", 3},
    {"compact15bitsinteger.beam", 1567888},
    {"negatives.beam", -55996},
    {"compact23bitsinteger.beam",  47769328},
    {"compact27bitsinteger.beam",  61837935},
    {"compact23bitsneginteger.beam", -47376112},
    {"negatives2.beam", -500},
    {"datetime.beam", 3},
    {"timestamp.beam", 1},
    {"is_type.beam", 255},
    {"test_bitwise.beam", -4},
    {"test_bitwise2.beam", 0},
    {"test_gt_and_le.beam", 255},
    {"test_tuple_size.beam", 6},
    {"test_element.beam", 7},
    {"test_setelement.beam", 121},
    {"test_insert_element.beam", 121},
    {"test_delete_element.beam", 421},
    {"test_tuple_to_list.beam", 300},
    {"test_make_tuple.beam", 4},
    {"test_make_list.beam", 5},
    {"test_list_gc.beam", 2},
    {"test_list_processes.beam", 3},
    {"test_tl.beam", 5},
    {"test_list_to_atom.beam", 9},
    {"test_list_to_existing_atom.beam", 9},
    {"test_binary_to_atom.beam", 9},
    {"test_binary_to_existing_atom.beam", 9},
    {"test_atom_to_list.beam", 1},
    {"test_display.beam", 0},
    {"test_integer_to_list.beam", 1},
    {"test_list_to_integer.beam", 99},
    {"test_abs.beam", 5},
    {"test_is_process_alive.beam", 121},
    {"test_is_not_type.beam", 255},
    {"test_badarith.beam", -87381},
    {"test_badarith2.beam", -87381},
    {"test_badarith3.beam", -1365},
    {"test_badarith4.beam", -1365},
    {"test_bif_badargument.beam", -5592405},
    {"test_bif_badargument2.beam", -85},
    {"test_bif_badargument3.beam", -85},
    {"test_tuple_nifs_badargs.beam", -1398101},
    {"long_atoms.beam", 4},
    {"test_concat_badarg.beam", 4},
    {"register_and_whereis_badarg.beam", 333},
    {"test_send.beam", -3},
    {"test_open_port_badargs.beam", -21},
    {"pingpong.beam", 1},
    {"prime_ext.beam", 1999},
    {"test_try_case_end.beam", 256},
    {"test_recursion_and_try_catch.beam", 3628800},
    {"test_func_info.beam", 89},
    {"test_func_info2.beam", 1},
    {"test_func_info3.beam", 120},
    {"test_process_info.beam", 0},
    {"test_min_heap_size.beam", 0},
    {"test_system_info.beam", 0},
    {"test_funs0.beam", 20},
    {"test_funs1.beam", 517},
    {"test_funs2.beam", 52},
    {"test_funs3.beam", 40},
    {"test_funs4.beam", 6},
    {"test_funs5.beam", 1000},
    {"test_funs6.beam", 16},
    {"test_funs7.beam", 9416},
    {"test_funs8.beam", 22000},
    {"test_funs9.beam", 3555},
    {"test_funs10.beam", 6817},
    {"test_funs11.beam", 817},

    {"nested_list_size0.beam", 0},
    {"nested_list_size1.beam", 2},
    {"nested_list_size2.beam", 8},
    {"nested_list_size3.beam", 68},
    {"nested_list_size4.beam", 408},

    {"simple_list_size0.beam", 2},
    {"simple_list_size1.beam", 10},

    {"tuple_size0.beam", 1},
    {"tuple_size1.beam", 2},
    {"tuple_size2.beam", 3},
    {"tuple_size3.beam", 4},
    {"tuple_size4.beam", 13},
    {"tuple_size5.beam", 7},
    {"tuple_size6.beam", 17},

    {"tuples_and_list_size0.beam", 3},
    {"tuples_and_list_size1.beam", 5},
    {"tuples_and_list_size2.beam", 10},

    {"nested_tuple_size0.beam", 12},
    {"nested_tuple_size1.beam", 44},
    {"nested_tuple_size2.beam", 76},
    {"nested_tuple_size3.beam", 80},
    {"nested_tuple_size4.beam", 80},

    {"complex_struct_size0.beam", 43},
    {"complex_struct_size1.beam", 37},
    {"complex_struct_size2.beam", 105},
    {"complex_struct_size3.beam", 23},
    {"complex_struct_size4.beam", 126},

    {"make_garbage0.beam", -19},
    {"make_garbage1.beam", -18},
    {"make_garbage2.beam", -56},
    {"make_garbage3.beam", -7},
    {"make_garbage4.beam", -438},
    {"make_garbage5.beam", 6},
    {"make_garbage6.beam", -210},
    {"make_garbage7.beam", -210},


    {"copy_terms0.beam", 0},
    {"copy_terms1.beam", 1},
    {"copy_terms2.beam", 2},
    {"copy_terms3.beam", 5},
    {"copy_terms4.beam", 2465},
    {"copy_terms5.beam", 32},
    {"copy_terms6.beam", 2},
    {"copy_terms7.beam", 10},
    {"copy_terms8.beam", 4},
    {"copy_terms9.beam", -19},
    {"copy_terms10.beam", -18},
    {"copy_terms11.beam", 36757},
    {"copy_terms12.beam", 36757},
    {"copy_terms13.beam", 37037},
    {"copy_terms14.beam", -210},
    {"copy_terms15.beam", -438},
    {"copy_terms16.beam", 6},
    {"copy_terms17.beam", 11},
    {"copy_terms18.beam", -19},

    {"test_apply.beam", 17},
    {"test_apply_last.beam", 17},
    {"test_timestamp.beam", 1},
    {"test_set_tuple_element.beam", 0},

    {"spawn_fun1.beam", 42},
    {"spawn_fun2.beam", 33},
    {"spawn_fun3.beam", 10},

    {"binary_at_test.beam", 121},
    {"binary_first_test.beam", 82},
    {"binary_last_test.beam", 110},

    {"test_integer_to_binary.beam", 2},
    {"test_list_to_binary.beam", 1},
    {"test_binary_to_list.beam", 1},
    {"test_atom_to_binary.beam", 1},

    {"test_binary_part.beam", 12},
    {"test_binary_split.beam", 16},

    #if LONG_MAX == 9223372036854775807
        {"plusone.beam", 134217728},
    #endif

    {"plusone2.beam", 1},
    {"minusone.beam", -134217729},
    {"minusone2.beam", -16},
    {"int28mul.beam", 134217728},
    {"int28mulneg.beam", -268435456},
    {"int28mulneg2.beam", 268435448},
    {"negdiv.beam", 134217728},
    {"absovf.beam", 134217728},
    {"negovf.beam", 134217728},

    {"plusone3.beam", 134217726},
    {"plusone4.beam", 134217728},
    {"bigfact.beam", 1860480},
    {"bigfact2.beam", 189907211},
    {"bigfact3.beam", 3078559},
    {"boxedabs.beam", 15},
    {"boxedneg.beam", 15},
    {"boxedmul.beam", 15},
    {"pow32.beam", 528},
    {"pow64.beam", 1794},
    {"pow32_is_integer.beam", 528},
    {"pow64_is_integer.beam", 1794},
    {"addovf32.beam", 791},
    {"subovf32.beam", 282},
    {"negovf32.beam", -1935},
    {"addovf64.beam", 2781},
    {"subovf64.beam", 282},
    {"negovf64.beam", -6865},
    {"powsquare.beam", 1572},

    //TEST CRASHES HERE: {"memlimit.beam", 0},

    {NULL, 0}
};

int test_modules_execution()
{
    struct Test *test = tests;

    if (chdir("erlang_tests")) {
        return EXIT_FAILURE;
    }

    int failed_tests = 0;

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

        avm_int_t value = term_maybe_unbox_int(ctx->x[0]);
        if (value != test->expected_value) {
            fprintf(stderr, "\x1b[1;31mFailed test module %s, got value: %li\x1b[0m\n", test->test_file, value);
            failed_tests++;
        }

        context_destroy(ctx);
        globalcontext_destroy(glb);
        module_destroy(mod);
        mapped_file_close(beam_file);

        test++;
    } while (test->test_file);

    if (failed_tests == 0) {
        fprintf(stderr, "Success.\n");
        return EXIT_SUCCESS;
    } else {
        fprintf(stderr, "Failed: %i tests.\n", failed_tests);
        return EXIT_FAILURE;
    }
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
