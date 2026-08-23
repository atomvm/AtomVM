/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M <petermm@gmail.com>
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#ifdef CONFIG_FLASH_MAP

#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <defaultatoms.h>
#include <erl_nif.h>
#include <erl_nif_priv.h>
#include <interop.h>
#include <nifs.h>
#include <port.h>
#include <term.h>

#include <zephyr/kernel.h>
#include <zephyr/storage/flash_map.h>
#ifdef CONFIG_SOC_FAMILY_ESPRESSIF_ESP32
#include <spi_flash_mmap.h>
#endif

#include "zephyros_sys.h"

#define FLASH_MAP_MAX_READ (64 * 1024)

static const char *const id_atom = ATOM_STR("\x2", "id");
static const char *const offset_atom = ATOM_STR("\x6", "offset");
static const char *const size_atom = ATOM_STR("\x4", "size");
#ifdef CONFIG_FLASH_MAP_LABELS
static const char *const label_atom = ATOM_STR("\x5", "label");
#endif

#ifdef CONFIG_SOC_FAMILY_ESPRESSIF_ESP32
static ErlNifResourceType *flash_mmap_resource_type;
static K_MUTEX_DEFINE(flash_mmap_lock);

struct FlashMmapMapping
{
    size_t map_addr;
    size_t map_size;
    const void *mapped;
    spi_flash_mmap_handle_t handle;
    int refcount;
    struct FlashMmapMapping *next;
};

struct FlashMmapResource
{
    struct FlashMmapMapping *mapping;
};

static struct FlashMmapMapping *flash_mmap_mappings;

static struct FlashMmapMapping *find_containing_mapping(size_t map_addr, size_t map_size)
{
    for (struct FlashMmapMapping *mapping = flash_mmap_mappings; mapping != NULL; mapping = mapping->next) {
        if (mapping->map_addr <= map_addr
            && map_addr - mapping->map_addr <= mapping->map_size
            && map_size <= mapping->map_size - (map_addr - mapping->map_addr)) {
            return mapping;
        }
    }
    return NULL;
}

static void release_mapping_locked(struct FlashMmapMapping *mapping)
{
    mapping->refcount--;
    if (mapping->refcount > 0) {
        return;
    }
    if (flash_mmap_mappings == mapping) {
        flash_mmap_mappings = mapping->next;
    } else {
        for (struct FlashMmapMapping *prev = flash_mmap_mappings; prev != NULL; prev = prev->next) {
            if (prev->next == mapping) {
                prev->next = mapping->next;
                break;
            }
        }
    }
    spi_flash_munmap(mapping->handle);
    free(mapping);
}

static void flash_mmap_dtor(ErlNifEnv *env, void *obj)
{
    UNUSED(env);
    struct FlashMmapResource *resource = obj;
    struct FlashMmapMapping *mapping = resource->mapping;
    if (IS_NULL_PTR(mapping)) {
        return;
    }

    k_mutex_lock(&flash_mmap_lock, K_FOREVER);
    release_mapping_locked(mapping);
    k_mutex_unlock(&flash_mmap_lock);
}

static const ErlNifResourceTypeInit FlashMmapResourceTypeInit = {
    .members = 1,
    .dtor = flash_mmap_dtor,
};

static void flash_map_nif_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    flash_mmap_resource_type = enif_init_resource_type(&env, "flash_mmap", &FlashMmapResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}
#endif

static term flash_error_tuple(Context *ctx, const char *reason)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term error_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
    term_put_tuple_element(error_tuple, 1, globalcontext_make_atom(ctx->global, reason));
    return error_tuple;
}

#ifdef CONFIG_FLASH_MAP_LABELS
struct find_label {
    const char *label;
    uint8_t id;
    bool found;
};

static void find_label_cb(const struct flash_area *fa, void *user)
{
    struct find_label *found = user;
    if (found->found) {
        return;
    }
    const char *label = flash_area_label(fa);
    if (label != NULL && strcmp(label, found->label) == 0) {
        found->id = fa->fa_id;
        found->found = true;
    }
}
#endif

static int open_flash_area(Context *ctx, term id_term, const struct flash_area **fa)
{
    UNUSED(ctx);
    uint8_t id;

    if (term_is_integer(id_term)) {
        avm_int_t raw_id = term_to_int(id_term);
        if (raw_id < 0 || raw_id > 255) {
            return -EINVAL;
        }
        id = (uint8_t) raw_id;
#ifdef CONFIG_FLASH_MAP_LABELS
    } else if (term_is_binary(id_term) || term_is_list(id_term)) {
        int ok;
        char *label = interop_term_to_string(id_term, &ok);
        if (!ok || label == NULL) {
            return -EINVAL;
        }
        struct find_label found = { .label = label, .found = false };
        flash_area_foreach(find_label_cb, &found);
        free(label);
        if (!found.found) {
            return -ENOENT;
        }
        id = found.id;
#endif
    } else {
        return -EINVAL;
    }

    return flash_area_open(id, fa);
}

static void count_areas_cb(const struct flash_area *fa, void *user)
{
    UNUSED(fa);
    size_t *count = user;
    (*count)++;
}

struct collect_areas {
    const struct flash_area **areas;
    size_t count;
    size_t capacity;
};

static void collect_areas_cb(const struct flash_area *fa, void *user)
{
    struct collect_areas *collected = user;
    if (collected->count < collected->capacity) {
        collected->areas[collected->count++] = fa;
    }
}

static term nif_zephyr_flash_list(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    size_t count = 0;
    flash_area_foreach(count_areas_cb, &count);

#ifdef CONFIG_FLASH_MAP_LABELS
    const size_t map_entries = 4;
#else
    const size_t map_entries = 3;
#endif

    const struct flash_area **areas = NULL;
    if (count > 0) {
        areas = malloc(count * sizeof(*areas));
        if (IS_NULL_PTR(areas)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
    }

    struct collect_areas collected = { .areas = areas, .count = 0, .capacity = count };
    flash_area_foreach(collect_areas_cb, &collected);

    size_t heap_size = 0;
    for (size_t i = 0; i < collected.count; i++) {
        heap_size += CONS_SIZE + term_map_size_in_terms(map_entries);
#ifdef CONFIG_FLASH_MAP_LABELS
        const char *label = flash_area_label(collected.areas[i]);
        if (label != NULL) {
            heap_size += term_binary_heap_size(strlen(label));
        }
#endif
    }

    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        free(areas);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    GlobalContext *glb = ctx->global;
    term id_key = globalcontext_make_atom(glb, id_atom);
    term offset_key = globalcontext_make_atom(glb, offset_atom);
    term size_key = globalcontext_make_atom(glb, size_atom);
#ifdef CONFIG_FLASH_MAP_LABELS
    term label_key = globalcontext_make_atom(glb, label_atom);
#endif

    term list = term_nil();
    for (size_t i = collected.count; i > 0; i--) {
        const struct flash_area *fa = collected.areas[i - 1];
        term map = term_alloc_map(map_entries, &ctx->heap);
        term_set_map_assoc(map, 0, id_key, term_from_int(fa->fa_id));
        term_set_map_assoc(map, 1, offset_key, term_from_int((avm_int_t) fa->fa_off));
        term_set_map_assoc(map, 2, size_key, term_from_int((avm_int_t) fa->fa_size));
#ifdef CONFIG_FLASH_MAP_LABELS
        const char *label = flash_area_label(fa);
        term label_term = UNDEFINED_ATOM;
        if (label != NULL) {
            label_term = term_from_literal_binary(label, strlen(label), &ctx->heap, glb);
        }
        term_set_map_assoc(map, 3, label_key, label_term);
#endif
        list = term_list_prepend(map, list, &ctx->heap);
    }

    free(areas);
    return list;
}

static term nif_zephyr_flash_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);

    avm_int_t offset = term_to_int(argv[1]);
    avm_int_t size = term_to_int(argv[2]);
    if (offset < 0 || size < 0 || size > FLASH_MAP_MAX_READ) {
        RAISE_ERROR(BADARG_ATOM);
    }

    const struct flash_area *fa;
    int err = open_flash_area(ctx, argv[0], &fa);
    if (err == -EINVAL) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (err == -ENOENT) {
        return flash_error_tuple(ctx, ATOM_STR("\x9", "not_found"));
    }
    if (err != 0) {
        return flash_error_tuple(ctx, ATOM_STR("\x8", "io_error"));
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_binary_heap_size((size_t) size)) != MEMORY_GC_OK)) {
        flash_area_close(fa);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term binary = term_create_uninitialized_binary((size_t) size, &ctx->heap, ctx->global);
    err = flash_area_read(fa, (off_t) offset, (void *) term_binary_data(binary), (size_t) size);
    flash_area_close(fa);
    if (err != 0) {
        return flash_error_tuple(ctx, ATOM_STR("\x8", "io_error"));
    }

    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, OK_ATOM);
    term_put_tuple_element(result, 1, binary);
    return result;
}

static term nif_zephyr_flash_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_binary);

    avm_int_t offset = term_to_int(argv[1]);
    if (offset < 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    const struct flash_area *fa;
    int err = open_flash_area(ctx, argv[0], &fa);
    if (err == -EINVAL) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (err == -ENOENT) {
        return flash_error_tuple(ctx, ATOM_STR("\x9", "not_found"));
    }
    if (err != 0) {
        return flash_error_tuple(ctx, ATOM_STR("\x8", "io_error"));
    }

    size_t size = term_binary_size(argv[2]);
    err = flash_area_write(fa, (off_t) offset, term_binary_data(argv[2]), size);
    flash_area_close(fa);
    if (err != 0) {
        return flash_error_tuple(ctx, ATOM_STR("\x8", "io_error"));
    }
    return OK_ATOM;
}

static term nif_zephyr_flash_erase(Context *ctx, int argc, term argv[])
{
    VALIDATE_VALUE(argv[1], term_is_integer);
    avm_int_t offset = term_to_int(argv[1]);
    if (offset < 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    const struct flash_area *fa;
    int err = open_flash_area(ctx, argv[0], &fa);
    if (err == -EINVAL) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (err == -ENOENT) {
        return flash_error_tuple(ctx, ATOM_STR("\x9", "not_found"));
    }
    if (err != 0) {
        return flash_error_tuple(ctx, ATOM_STR("\x8", "io_error"));
    }

    avm_int_t size;
    if (argc == 3) {
        VALIDATE_VALUE(argv[2], term_is_integer);
        size = term_to_int(argv[2]);
        if (size < 0) {
            flash_area_close(fa);
            RAISE_ERROR(BADARG_ATOM);
        }
    } else {
        if ((size_t) offset > fa->fa_size) {
            flash_area_close(fa);
            return flash_error_tuple(ctx, ATOM_STR("\x8", "io_error"));
        }
        size = (avm_int_t) (fa->fa_size - (size_t) offset);
    }

    err = flash_area_erase(fa, (off_t) offset, (size_t) size);
    flash_area_close(fa);
    if (err != 0) {
        return flash_error_tuple(ctx, ATOM_STR("\x8", "io_error"));
    }
    return OK_ATOM;
}

#ifdef CONFIG_SOC_FAMILY_ESPRESSIF_ESP32
static term nif_zephyr_flash_mmap(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);

    avm_int_t offset = term_to_int(argv[1]);
    avm_int_t size = term_to_int(argv[2]);
    if (offset < 0 || size < 0) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(flash_mmap_resource_type)) {
        return flash_error_tuple(ctx, ATOM_STR("\xD", "not_supported"));
    }

    const struct flash_area *fa;
    int err = open_flash_area(ctx, argv[0], &fa);
    if (err == -EINVAL) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (err == -ENOENT) {
        return flash_error_tuple(ctx, ATOM_STR("\x9", "not_found"));
    }
    if (err != 0) {
        return flash_error_tuple(ctx, ATOM_STR("\x8", "io_error"));
    }

    if ((size_t) offset > fa->fa_size || (size_t) size > fa->fa_size - (size_t) offset) {
        flash_area_close(fa);
        return flash_error_tuple(ctx, ATOM_STR("\x8", "io_error"));
    }

    if (size == 0) {
        flash_area_close(fa);
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_binary_heap_size(0)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term binary = term_create_uninitialized_binary(0, &ctx->heap, ctx->global);
        term result = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result, 0, OK_ATOM);
        term_put_tuple_element(result, 1, binary);
        return result;
    }

    if ((size_t) fa->fa_off > SIZE_MAX - (size_t) offset) {
        flash_area_close(fa);
        return flash_error_tuple(ctx, ATOM_STR("\x8", "io_error"));
    }
    size_t phys_addr = (size_t) fa->fa_off + (size_t) offset;
    size_t page_size = SPI_FLASH_MMU_PAGE_SIZE;
    if ((size_t) size > SIZE_MAX - phys_addr || (page_size - 1) > SIZE_MAX - (phys_addr + (size_t) size)) {
        flash_area_close(fa);
        return flash_error_tuple(ctx, ATOM_STR("\x8", "io_error"));
    }
    size_t map_addr = phys_addr & ~(page_size - 1);
    size_t map_end = (phys_addr + (size_t) size + page_size - 1) & ~(page_size - 1);
    size_t map_size = map_end - map_addr;
    flash_area_close(fa);

    k_mutex_lock(&flash_mmap_lock, K_FOREVER);
    struct FlashMmapMapping *mapping = find_containing_mapping(map_addr, map_size);
    if (mapping != NULL) {
        mapping->refcount++;
    } else {
        const void *mapped = NULL;
        spi_flash_mmap_handle_t handle = 0;
        if (spi_flash_mmap(map_addr, map_size, SPI_FLASH_MMAP_DATA, &mapped, &handle) != ESP_OK
            || IS_NULL_PTR(mapped)) {
            if (mapped != NULL) {
                spi_flash_munmap(handle);
            }
            k_mutex_unlock(&flash_mmap_lock);
            return flash_error_tuple(ctx, ATOM_STR("\x8", "io_error"));
        }

        mapping = malloc(sizeof(*mapping));
        if (IS_NULL_PTR(mapping)) {
            spi_flash_munmap(handle);
            k_mutex_unlock(&flash_mmap_lock);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        mapping->map_addr = map_addr;
        mapping->map_size = map_size;
        mapping->mapped = mapped;
        mapping->handle = handle;
        mapping->refcount = 1;
        mapping->next = flash_mmap_mappings;
        flash_mmap_mappings = mapping;
    }
    const void *mapped = mapping->mapped;
    size_t map_delta = phys_addr - mapping->map_addr;
    k_mutex_unlock(&flash_mmap_lock);

    struct FlashMmapResource *resource = enif_alloc_resource(flash_mmap_resource_type, sizeof(struct FlashMmapResource));
    if (IS_NULL_PTR(resource)) {
        k_mutex_lock(&flash_mmap_lock, K_FOREVER);
        release_mapping_locked(mapping);
        k_mutex_unlock(&flash_mmap_lock);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    resource->mapping = mapping;

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_REFC_BINARY_SIZE) != MEMORY_GC_OK)) {
        enif_release_resource(resource);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    const uint8_t *data = ((const uint8_t *) mapped) + map_delta;
    term binary = term_from_resource_binary(resource, data, (size_t) size, &ctx->heap, ctx->global);
    enif_release_resource(resource);

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &binary, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, OK_ATOM);
    term_put_tuple_element(result, 1, binary);
    return result;
}
#else
static term nif_zephyr_flash_mmap(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    return flash_error_tuple(ctx, ATOM_STR("\xD", "not_supported"));
}
#endif

static const struct Nif *flash_map_nif_get_nif(const char *nifname)
{
    if (strncmp("zephyr:", nifname, 7) != 0) {
        return NULL;
    }
    const char *rest = nifname + 7;
    if (strcmp("flash_list/0", rest) == 0) {
        static const struct Nif nif = { .base.type = NIFFunctionType, .nif_ptr = nif_zephyr_flash_list };
        return &nif;
    }
    if (strcmp("flash_read/3", rest) == 0) {
        static const struct Nif nif = { .base.type = NIFFunctionType, .nif_ptr = nif_zephyr_flash_read };
        return &nif;
    }
    if (strcmp("flash_write/3", rest) == 0) {
        static const struct Nif nif = { .base.type = NIFFunctionType, .nif_ptr = nif_zephyr_flash_write };
        return &nif;
    }
    if (strcmp("flash_erase/2", rest) == 0) {
        static const struct Nif nif = { .base.type = NIFFunctionType, .nif_ptr = nif_zephyr_flash_erase };
        return &nif;
    }
    if (strcmp("flash_erase/3", rest) == 0) {
        static const struct Nif nif = { .base.type = NIFFunctionType, .nif_ptr = nif_zephyr_flash_erase };
        return &nif;
    }
    if (strcmp("flash_mmap/3", rest) == 0) {
        static const struct Nif nif = { .base.type = NIFFunctionType, .nif_ptr = nif_zephyr_flash_mmap };
        return &nif;
    }
    return NULL;
}

#ifdef CONFIG_SOC_FAMILY_ESPRESSIF_ESP32
REGISTER_NIF_COLLECTION(flash_map, flash_map_nif_init, NULL, flash_map_nif_get_nif)
#else
REGISTER_NIF_COLLECTION(flash_map, NULL, NULL, flash_map_nif_get_nif)
#endif

#endif
