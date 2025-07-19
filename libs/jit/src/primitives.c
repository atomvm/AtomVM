#include <stdint.h>

typedef uintptr_t term;

typedef struct GlobalContext GlobalContext;

#define MAX_REG 16

struct HeapFragment;
typedef struct HeapFragment HeapFragment;

struct HeapFragment
{
    HeapFragment *next;
    union
    {
        term mso_list; // root fragment holds mso_list, with heap_end being in Heap
        term *heap_end; // other fragments hold their own heap_end
    };
    term storage[];
};

struct Heap
{
    HeapFragment *root;
    term *heap_start;
    term *heap_ptr;
    term *heap_end;
};

typedef struct Heap Heap;

struct Context
{
    // First fields matches ErlNifEnv structure.
    GlobalContext *global;
    Heap heap;
    term *e;
    term x[MAX_REG + 1];
    unsigned long cp;
};

typedef struct Context Context;
typedef struct Module Module;

struct JITState {
    Module *module;
    const void *continuation; // emulated pc or pointer to entry point
    int remaining_reductions;
};

struct Module
{
    int module_index;
};

typedef struct JITState JITState;

struct Primitives
{
    term (* module_get_atom_string_by_id)(Context *ctx, Module *module, int index);
    term (* raise)(Context *ctx, Module *module, int index);
    Context * (* schedule_next_cp)(Context *ctx, JITState *jit_state);
    term (* call_ext_only)(Context *ctx, Module *module, int arity, int index);
};

void move_imm_x_0(Context *ctx, Module *module, struct Primitives *p)
{
    ctx->x[0] = 0;
}

void move_imm_x1_0(Context *ctx, Module *module)
{
    ctx->x[1] = 0;
}

void move_imm_x_42(Context *ctx, Module *module)
{
    ctx->x[0] = 42;
}

void move_imm_x_n42(Context *ctx, Module *module)
{
    ctx->x[0] = -42;
}

void move_imm_x_65536(Context *ctx, Module *module)
{
    ctx->x[0] = 65536;
}

void move_imm_x_9223372036854775808(Context *ctx, Module *module)
{
    ctx->x[0] = 9000000000000000000ULL;
}

void move_imm_y(Context *ctx, Module *module)
{
    ctx->e[12] = 42;
    ctx->e[13] = 43;
}

void call_primitive(Context *ctx, Module *module, struct Primitives *p)
{
    ctx->x[0] = p->module_get_atom_string_by_id(ctx, module, 42);
}

void call_primitive_other(Context *ctx, Module *module, struct Primitives *p)
{
    ctx->x[0] = p->call_ext_only(ctx, module, 42, 43);
}

void call_primitive_last(Context *ctx, Module *module, struct Primitives *p)
{
    p->raise(ctx, module, 42);
}

void call_primitive_last_other(Context *ctx, Module *module, struct Primitives *p)
{
    p->module_get_atom_string_by_id(ctx, module, 42);
}

int multiply(Context *ctx, Module *module, struct Primitives *p, int label) {
    return 42 + (label * 5);
}

int test_and_mask(Context *ctx, Module *module, struct Primitives *p, int label) {
    if ((p->module_get_atom_string_by_id(ctx, module, 42) & 0x3) == 0x1) {
        p->raise(ctx, module, 42);
    }
    return 0;
}

Context *next(Context *ctx, JITState *jit_state, struct Primitives *p)
{
    p->module_get_atom_string_by_id(ctx, jit_state->module, 42);
    return 0;
}

term *get_ptr_x(Context *ctx, JITState *jit_state, struct Primitives *p)
{
    return &ctx->x[0];
}

term *get_ptr_y(Context *ctx, JITState *jit_state, struct Primitives *p)
{
    return ctx->e;
}

term *get_ptr_y_3(Context *ctx, JITState *jit_state, struct Primitives *p)
{
    return &ctx->e[3];
}

Context *call_with_reductions(Context *ctx, JITState *jit_state, struct Primitives *p) {
    jit_state->remaining_reductions--;
    if (jit_state->remaining_reductions) {
        return next(ctx, jit_state, p);
    } else {
        jit_state->continuation = next;
        return p->schedule_next_cp(ctx, jit_state);
    }
}

Context *call(Context *ctx, JITState *jit_state, struct Primitives *p) {
    Context *result;
there:
    ctx->cp = (jit_state->module->module_index << 24) | (&&here - &&there) << 2;
    jit_state->remaining_reductions--;
    if (jit_state->remaining_reductions) {
        result = next(ctx, jit_state, p);
    } else {
        jit_state->continuation = next;
        return p->schedule_next_cp(ctx, jit_state);
    }
here:
    p->module_get_atom_string_by_id(ctx, jit_state->module, 42);
    return result;
}

void andbit(Context *ctx, Module *module)
{
    term t = ctx->x[0];
    term *ptr = (term *) (t & ~0x3UL);
    ctx->x[2] = ptr[1];
}

void jump_table(Context *ctx, Module *module, struct Primitives *p, int label) {
    switch (label) {
        case 1:
            move_imm_x_0(ctx, module, p);
            break;
        case 2:
            move_imm_x1_0(ctx, module);
            break;
        case 3:
            move_imm_x_42(ctx, module);
            break;
        case 4:
            move_imm_x_n42(ctx, module);
            break;
        case 5:
            move_imm_x_65536(ctx, module);
            break;
        case 6:
            move_imm_x_9223372036854775808(ctx, module);
            break;
        case 7:
            move_imm_y(ctx, module);
            break;
        default:
            __builtin_unreachable();
    }
}

