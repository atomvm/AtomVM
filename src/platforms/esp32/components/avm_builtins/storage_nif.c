/*
 * This file is part of AtomVM.
 *
 * Copyright 2024 Davide Bettio <davide@uninstall.it>
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

#include <sdkconfig.h>
#ifdef CONFIG_AVM_ENABLE_STORAGE_NIFS

#include <atom.h>
#include <defaultatoms.h>
#include <erl_nif.h>
#include <erl_nif_priv.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <smp.h>
#include <term.h>

#include "esp32_sys.h"

#include <stdlib.h>

#include <driver/sdmmc_host.h>
#include <driver/sdspi_host.h>
#include <esp_vfs_fat.h>

#include <trace.h>

#include "spi_driver.h"

#define TAG "storage_nif"

#ifndef AVM_NO_SMP
#define SMP_LOCK_INIT(mounted_fs) smp_spinlock_init(&mounted_fs->lock)
#define SMP_LOCK(mounted_fs) smp_spinlock_lock(&mounted_fs->lock)
#define SMP_UNLOCK(mounted_fs) smp_spinlock_unlock(&mounted_fs->lock)
#else
#define SMP_LOCK_INIT(mounted_fs)
#define SMP_LOCK(mounted_fs)
#define SMP_UNLOCK(mounted_fs)
#endif

// TODO: allow ro option
enum mount_type
{
    Unmounted,
    FATSPIFlash,
    FATSDSPI,
    FATSDMMC
};

struct MountedFS
{
#ifndef AVM_NO_SMP
    SpinLock lock;
#endif
    char *base_path;
    enum mount_type mount_type;
    union
    {
        sdmmc_card_t *card;
        wl_handle_t wl;
    } handle;
};

static void mounted_fs_dtor(ErlNifEnv *caller_env, void *obj);

const ErlNifResourceTypeInit mounted_fs_resource_type_init = {
    .members = 1,
    .dtor = mounted_fs_dtor
};

static term make_esp_error_tuple(esp_err_t err, Context *ctx)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, ERROR_ATOM);
    term_put_tuple_element(result, 1, esp_err_to_term(ctx->global, err));
    return result;
}

static void opts_to_fatfs_mount_config(term opts_term, esp_vfs_fat_mount_config_t *mount_config)
{
    mount_config->format_if_mount_failed = true;
    mount_config->max_files = 8;
    mount_config->allocation_unit_size = 512;
    // TODO: make it configurable: disk_status_check_enable = false
}

static term nif_esp_mount(Context *ctx, int argc, term argv[])
{
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    term source_term = argv[0];
    term target_term = argv[1];
    term filesystem_type_term = argv[2];
    term opts_term = argv[3];

    int str_ok;
    char *source = interop_term_to_string(source_term, &str_ok);
    if (!str_ok) {
        RAISE_ERROR(BADARG_ATOM);
    }

    char *target = interop_term_to_string(target_term, &str_ok);
    if (!str_ok) {
        free(source);
        RAISE_ERROR(BADARG_ATOM);
    }
    if (strlen(target) > 8) {
        free(source);
        free(target);
        RAISE_ERROR(BADARG_ATOM);
    }

    term fat_term
        = globalcontext_existing_term_from_atom_string(ctx->global, ATOM_STR("\x3", "fat"));
    if (term_is_invalid_term(fat_term) || filesystem_type_term != fat_term) {
        free(source);
        free(target);
        RAISE_ERROR(BADARG_ATOM);
    }

    if (!term_is_list(opts_term) && !term_is_map(opts_term)) {
        free(source);
        free(target);
        RAISE_ERROR(BADARG_ATOM);
    }

    esp_vfs_fat_mount_config_t mount_config = {};
    opts_to_fatfs_mount_config(opts_term, &mount_config);

    esp_err_t ret = -1;
    struct MountedFS *mount = NULL;

    const char *part_by_name_prefix = "/dev/partition/by-name/";
    int part_by_name_len = strlen(part_by_name_prefix);
    if (!strncmp(part_by_name_prefix, source, part_by_name_len)) {
        mount_config.allocation_unit_size = CONFIG_WL_SECTOR_SIZE;

        mount = enif_alloc_resource(platform->mounted_fs_resource_type, sizeof(struct MountedFS));
        if (IS_NULL_PTR(mount)) {
            free(source);
            free(target);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        SMP_LOCK_INIT(mount);
        mount->base_path = target;
        target = NULL;
        mount->mount_type = FATSPIFlash;

#if ESP_IDF_VERSION_MAJOR >= 5
        ret = esp_vfs_fat_spiflash_mount_rw_wl(
            mount->base_path, source + part_by_name_len, &mount_config, &mount->handle.wl);
#else
        ret = esp_vfs_fat_spiflash_mount(
            mount->base_path, source + part_by_name_len, &mount_config, &mount->handle.wl);
#endif

// C3 doesn't support this
#ifdef SDMMC_SLOT_CONFIG_DEFAULT
    } else if (!strcmp(source, "sdmmc")) {
        mount_config.allocation_unit_size = 512;

        sdmmc_host_t host_config = SDMMC_HOST_DEFAULT();
        sdmmc_slot_config_t slot_config = SDMMC_SLOT_CONFIG_DEFAULT();

        mount = enif_alloc_resource(platform->mounted_fs_resource_type, sizeof(struct MountedFS));
        if (IS_NULL_PTR(mount)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        SMP_LOCK_INIT(mount);
        mount->base_path = target;
        target = NULL;
        mount->mount_type = FATSDMMC;

        ret = esp_vfs_fat_sdmmc_mount(
            mount->base_path, &host_config, &slot_config, &mount_config, &mount->handle.card);
#endif

    } else if (!strcmp(source, "sdspi")) {
        mount_config.allocation_unit_size = 512;

        sdmmc_host_t host_config = SDSPI_HOST_DEFAULT();
        sdspi_device_config_t spi_slot_config = SDSPI_DEVICE_CONFIG_DEFAULT();

        term spi_port = interop_kv_get_value_default(
            opts_term, ATOM_STR("\x8", "spi_host"), term_invalid_term(), ctx->global);
        spi_host_device_t host_dev;
        // spi_driver_get_peripheral already checks if spi_port is valid
        bool ok = spi_driver_get_peripheral(spi_port, &host_dev, ctx->global);
        if (!ok) {
            free(source);
            free(target);
            RAISE_ERROR(BADARG_ATOM);
        }
        spi_slot_config.host_id = host_dev;

        term cs_term = interop_kv_get_value_default(
            opts_term, ATOM_STR("\x2", "cs"), term_invalid_term(), ctx->global);
        if (UNLIKELY(!term_is_integer(cs_term))) {
            free(source);
            free(target);
            RAISE_ERROR(BADARG_ATOM);
        }
        spi_slot_config.gpio_cs = term_to_int(cs_term);

        term cd_term = interop_kv_get_value_default(
            opts_term, ATOM_STR("\x2", "cd"), UNDEFINED_ATOM, ctx->global);
        if (cd_term != UNDEFINED_ATOM) {
            if (UNLIKELY(!term_is_integer(cd_term))) {
                free(source);
                free(target);
                RAISE_ERROR(BADARG_ATOM);
            }
            spi_slot_config.gpio_cd = term_to_int(cd_term);
        }

        mount = enif_alloc_resource(platform->mounted_fs_resource_type, sizeof(struct MountedFS));
        if (IS_NULL_PTR(mount)) {
            free(source);
            free(target);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        SMP_LOCK_INIT(mount);
        mount->base_path = target;
        target = NULL;
        mount->mount_type = FATSDSPI;

        ret = esp_vfs_fat_sdspi_mount(
            mount->base_path, &host_config, &spi_slot_config, &mount_config, &mount->handle.card);
    } else {
        free(source);
        free(target);
        RAISE_ERROR(BADARG_ATOM);
    }

    free(source);

    term return_term = term_invalid_term();
    if (UNLIKELY(ret != ESP_OK)) {
        mount->mount_type = Unmounted;
        return_term = make_esp_error_tuple(ret, ctx);
    } else {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + TERM_BOXED_RESOURCE_SIZE)
                != MEMORY_GC_OK)) {
            enif_release_resource(mount);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term mount_term = enif_make_resource(erl_nif_env_from_context(ctx), mount);
        return_term = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(return_term, 0, OK_ATOM);
        term_put_tuple_element(return_term, 1, mount_term);
    }
    enif_release_resource(mount);

    return return_term;
}

static esp_err_t do_umount(struct MountedFS *mount)
{
    SMP_LOCK(mount);
    esp_err_t ret = ESP_FAIL;

    switch (mount->mount_type) {
        case Unmounted:
            ret = ESP_OK;
            break;

        case FATSPIFlash:
#if ESP_IDF_VERSION_MAJOR >= 5
            ret = esp_vfs_fat_spiflash_unmount_rw_wl(mount->base_path, mount->handle.wl);
#else
            ret = esp_vfs_fat_spiflash_unmount(mount->base_path, mount->handle.wl);
#endif
            break;

        case FATSDSPI:
        case FATSDMMC:
            ret = esp_vfs_fat_sdcard_unmount(mount->base_path, mount->handle.card);
            break;
    }

    if (ret == ESP_OK) {
        mount->mount_type = Unmounted;
    }

    SMP_UNLOCK(mount);
    return ret;
}

static term nif_esp_umount(Context *ctx, int argc, term argv[])
{
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    void *mount_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            platform->mounted_fs_resource_type, &mount_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct MountedFS *mounted_fs = (struct MountedFS *) mount_obj_ptr;

    if (UNLIKELY(mounted_fs->mount_type == Unmounted)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    esp_err_t ret = do_umount(mounted_fs);

    if (UNLIKELY(ret != ESP_OK)) {
        return make_esp_error_tuple(ret, ctx);
    }

    return OK_ATOM;
}

static void mounted_fs_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct MountedFS *mounted_fs = (struct MountedFS *) obj;
    esp_err_t ret = do_umount(mounted_fs);

    if (UNLIKELY(ret != ESP_OK)) {
        ESP_LOGW(TAG, "Failed umount for %s in resource dtor. Please use esp:umount/1.",
            mounted_fs->base_path);
    }

    free(mounted_fs->base_path);
}

void storage_nif_init(GlobalContext *global)
{
    struct ESP32PlatformData *platform = global->platform_data;

    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    platform->mounted_fs_resource_type = enif_init_resource_type(
        &env, "mounted_fs", &mounted_fs_resource_type_init, ERL_NIF_RT_CREATE, NULL);
}

static const struct Nif esp_mount_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_mount
};

static const struct Nif esp_umount_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_umount
};

const struct Nif *storage_nif_get_nif(const char *nifname)
{
    if (strcmp("esp:mount/4", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_mount_nif;
    } else if (strcmp("esp:umount/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_umount_nif;
    }

    return NULL;
}

REGISTER_NIF_COLLECTION(storage, storage_nif_init, NULL, storage_nif_get_nif)

#endif
