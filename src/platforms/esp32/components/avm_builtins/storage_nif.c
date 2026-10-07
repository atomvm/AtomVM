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
#if defined(CONFIG_AVM_ENABLE_STORAGE_NIFS) || defined(CONFIG_AVM_ENABLE_RAW_SDCARD_NIFS)

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

#include <driver/gpio.h>
#include <driver/sdmmc_host.h>
#include <driver/sdspi_host.h>
#ifdef CONFIG_AVM_ENABLE_STORAGE_NIFS
#include <esp_vfs_fat.h>
#endif
#include <esp_log.h>
#include <sdmmc_cmd.h>
#include <soc/soc_caps.h>

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

#ifdef CONFIG_AVM_ENABLE_STORAGE_NIFS
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
#endif

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

#ifdef CONFIG_AVM_ENABLE_STORAGE_NIFS
static void opts_to_fatfs_mount_config(term opts_term, esp_vfs_fat_mount_config_t *mount_config)
{
    mount_config->format_if_mount_failed = true;
    mount_config->max_files = 8;
    mount_config->allocation_unit_size = 512;
    // TODO: make it configurable: disk_status_check_enable = false
}
#endif

#ifdef SDMMC_SLOT_CONFIG_DEFAULT
#if defined(SOC_SDMMC_USE_GPIO_MATRIX) && SOC_SDMMC_USE_GPIO_MATRIX
static bool storage_nif_set_sdmmc_slot_pin_if_present(
    term opts_term, AtomString key, gpio_num_t *pin, GlobalContext *glb)
{
    term value_term = interop_kv_get_value(opts_term, key, glb);
    if (term_is_invalid_term(value_term)) {
        // Omitted pin options keep the ESP-IDF slot default.
        return true;
    }
    if (UNLIKELY(!term_is_integer(value_term))) {
        return false;
    }
    avm_int_t pin_value = term_to_int(value_term);
    if (UNLIKELY(pin_value < 0 || pin_value > GPIO_NUM_MAX)) {
        return false;
    }
    *pin = (gpio_num_t) pin_value;
    return true;
}
#else
static bool storage_nif_has_no_sdmmc_slot_pin(
    term opts_term, AtomString key, GlobalContext *glb)
{
    term value_term = interop_kv_get_value(opts_term, key, glb);
    return term_is_invalid_term(value_term);
}
#endif

static bool storage_nif_configure_sdmmc_slot(
    term opts_term, sdmmc_slot_config_t *slot_config, GlobalContext *glb)
{
#if defined(SOC_SDMMC_USE_GPIO_MATRIX) && SOC_SDMMC_USE_GPIO_MATRIX
    if (UNLIKELY(!storage_nif_set_sdmmc_slot_pin_if_present(
            opts_term, ATOM_STR("\x3", "clk"), &slot_config->clk, glb))) {
        return false;
    }

    if (UNLIKELY(!storage_nif_set_sdmmc_slot_pin_if_present(
            opts_term, ATOM_STR("\x3", "cmd"), &slot_config->cmd, glb))) {
        return false;
    }

    if (UNLIKELY(!storage_nif_set_sdmmc_slot_pin_if_present(
            opts_term, ATOM_STR("\x2", "d0"), &slot_config->d0, glb))) {
        return false;
    }

    if (UNLIKELY(!storage_nif_set_sdmmc_slot_pin_if_present(
            opts_term, ATOM_STR("\x2", "d1"), &slot_config->d1, glb))) {
        return false;
    }

    if (UNLIKELY(!storage_nif_set_sdmmc_slot_pin_if_present(
            opts_term, ATOM_STR("\x2", "d2"), &slot_config->d2, glb))) {
        return false;
    }

    if (UNLIKELY(!storage_nif_set_sdmmc_slot_pin_if_present(
            opts_term, ATOM_STR("\x2", "d3"), &slot_config->d3, glb))) {
        return false;
    }
#else
    if (UNLIKELY(!storage_nif_has_no_sdmmc_slot_pin(
            opts_term, ATOM_STR("\x3", "clk"), glb))) {
        return false;
    }

    if (UNLIKELY(!storage_nif_has_no_sdmmc_slot_pin(
            opts_term, ATOM_STR("\x3", "cmd"), glb))) {
        return false;
    }

    if (UNLIKELY(!storage_nif_has_no_sdmmc_slot_pin(
            opts_term, ATOM_STR("\x2", "d0"), glb))) {
        return false;
    }

    if (UNLIKELY(!storage_nif_has_no_sdmmc_slot_pin(
            opts_term, ATOM_STR("\x2", "d1"), glb))) {
        return false;
    }

    if (UNLIKELY(!storage_nif_has_no_sdmmc_slot_pin(
            opts_term, ATOM_STR("\x2", "d2"), glb))) {
        return false;
    }

    if (UNLIKELY(!storage_nif_has_no_sdmmc_slot_pin(
            opts_term, ATOM_STR("\x2", "d3"), glb))) {
        return false;
    }
#endif

    term width_term = interop_kv_get_value(opts_term, ATOM_STR("\x5", "width"), glb);
    if (!term_is_invalid_term(width_term)) {
        if (UNLIKELY(!term_is_integer(width_term))) {
            return false;
        }
        avm_int_t width = term_to_int(width_term);
        if (UNLIKELY(width != 1 && width != 4)) {
            return false;
        }
        slot_config->width = (uint8_t) width;
    }

    return true;
}
#endif

enum sdcard_interface
{
    SDCardSDMMC,
    SDCardSDSPI
};

struct SDCardConfig
{
    enum sdcard_interface interface;
    sdmmc_host_t host;
    union
    {
#ifdef SDMMC_SLOT_CONFIG_DEFAULT
        sdmmc_slot_config_t mmc_slot;
#endif
        sdspi_device_config_t spi_dev;
    } slot;
};

static bool sdcard_config_from_source(
    const char *source, term opts_term, struct SDCardConfig *cfg, GlobalContext *glb)
{
#ifdef SDMMC_SLOT_CONFIG_DEFAULT
    if (!strcmp(source, "sdmmc")) {
        sdmmc_host_t host_config = SDMMC_HOST_DEFAULT();
        sdmmc_slot_config_t slot_config = SDMMC_SLOT_CONFIG_DEFAULT();
        if (UNLIKELY(!storage_nif_configure_sdmmc_slot(opts_term, &slot_config, glb))) {
            return false;
        }
        cfg->interface = SDCardSDMMC;
        cfg->host = host_config;
        cfg->slot.mmc_slot = slot_config;
        return true;
    }
#endif

#ifdef CONFIG_AVM_ENABLE_SPI_PORT_DRIVER
    if (!strcmp(source, "sdspi")) {
        sdmmc_host_t host_config = SDSPI_HOST_DEFAULT();
        sdspi_device_config_t spi_slot_config = SDSPI_DEVICE_CONFIG_DEFAULT();

        term spi_port = interop_kv_get_value(opts_term, ATOM_STR("\x8", "spi_host"), glb);
        spi_host_device_t host_dev;
        // spi_driver_get_peripheral already checks if spi_port is valid
        if (!spi_driver_get_peripheral(spi_port, &host_dev, glb)) {
            return false;
        }
        spi_slot_config.host_id = host_dev;

        term cs_term = interop_kv_get_value(opts_term, ATOM_STR("\x2", "cs"), glb);
        if (UNLIKELY(!term_is_integer(cs_term))) {
            return false;
        }
        spi_slot_config.gpio_cs = term_to_int(cs_term);

        term cd_term
            = interop_kv_get_value_default(opts_term, ATOM_STR("\x2", "cd"), UNDEFINED_ATOM, glb);
        if (cd_term != UNDEFINED_ATOM) {
            if (UNLIKELY(!term_is_integer(cd_term))) {
                return false;
            }
            spi_slot_config.gpio_cd = term_to_int(cd_term);
        }

        cfg->interface = SDCardSDSPI;
        cfg->host = host_config;
        cfg->slot.spi_dev = spi_slot_config;
        return true;
    }
#endif

    return false;
}

#ifdef CONFIG_AVM_ENABLE_RAW_SDCARD_NIFS
struct SDCardBlockDevice
{
#ifndef AVM_NO_SMP
    Mutex *lock;
#endif
    bool open;
    uint32_t sector_size;
    uint32_t sector_count;
    sdmmc_card_t *card;
};

static void sdcard_dtor(ErlNifEnv *caller_env, void *obj);

const ErlNifResourceTypeInit sdcard_resource_type_init = {
    .members = 1,
    .dtor = sdcard_dtor
};
#endif

#ifdef CONFIG_AVM_ENABLE_STORAGE_NIFS
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

    } else if (!strcmp(source, "sdmmc") || !strcmp(source, "sdspi")) {
        mount_config.allocation_unit_size = 512;

        struct SDCardConfig cfg;
        if (!sdcard_config_from_source(source, opts_term, &cfg, ctx->global)) {
            free(source);
            free(target);
            RAISE_ERROR(BADARG_ATOM);
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

        if (cfg.interface == SDCardSDSPI) {
            mount->mount_type = FATSDSPI;
            ret = esp_vfs_fat_sdspi_mount(
                mount->base_path, &cfg.host, &cfg.slot.spi_dev, &mount_config, &mount->handle.card);
#ifdef SDMMC_SLOT_CONFIG_DEFAULT
        } else {
            mount->mount_type = FATSDMMC;
            ret = esp_vfs_fat_sdmmc_mount(mount->base_path, &cfg.host, &cfg.slot.mmc_slot,
                &mount_config, &mount->handle.card);
#endif
        }
    } else {
        free(source);
        free(target);
        RAISE_ERROR(BADARG_ATOM);
    }

    free(source);

    if (UNLIKELY(ret != ESP_OK)) {
        mount->mount_type = Unmounted;
        enif_release_resource(mount); // release before raise-capable make_esp_error_tuple
        return make_esp_error_tuple(ret, ctx);
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + TERM_BOXED_RESOURCE_SIZE)
            != MEMORY_GC_OK)) {
        enif_release_resource(mount);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term mount_term = term_from_resource(mount, &ctx->heap);
    term return_term = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(return_term, 0, OK_ATOM);
    term_put_tuple_element(return_term, 1, mount_term);
    enif_release_resource(mount); // decrement refcount after enif_alloc_resource

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
#endif

#ifdef CONFIG_AVM_ENABLE_RAW_SDCARD_NIFS
// TODO: switch to the new ESP-IDF blockdev API once the API issues are fixed and it is
// proven working: https://github.com/espressif/esp-idf/issues/18875
static esp_err_t sdcard_blockdev_read_sector(
    struct SDCardBlockDevice *dev, void *dst, size_t sector)
{
    return sdmmc_read_sectors(dev->card, dst, sector, 1);
}

static esp_err_t sdcard_blockdev_write_sector(
    struct SDCardBlockDevice *dev, const void *src, size_t sector)
{
    return sdmmc_write_sectors(dev->card, src, sector, 1);
}

// Mirrors call_host_deinit() in ESP-IDF's vfs_fat_sdmmc.c: per-device/per-slot
// teardown where the host provides it (IDF >= 5.3), forceful host deinit otherwise.
static esp_err_t sdcard_host_deinit(const sdmmc_host_t *host)
{
    if (host->flags & SDMMC_HOST_FLAG_DEINIT_ARG) {
        return host->deinit_p(host->slot);
    }
    return host->deinit();
}

static esp_err_t sdcard_blockdev_close(struct SDCardBlockDevice *dev)
{
    esp_err_t ret = sdcard_host_deinit(&dev->card->host);

    free(dev->card);
    dev->card = NULL;

    return ret;
}

static esp_err_t sdcard_blockdev_init(struct SDCardConfig *cfg, struct SDCardBlockDevice *dev)
{
    esp_err_t err;

    dev->card = malloc(sizeof(sdmmc_card_t));
    if (IS_NULL_PTR(dev->card)) {
        return ESP_ERR_NO_MEM;
    }

    if (cfg->interface == SDCardSDSPI) {
        err = sdspi_host_init();
        if (UNLIKELY(err != ESP_OK)) {
            free(dev->card);
            dev->card = NULL;
            return err;
        }
        sdspi_dev_handle_t spi_handle;
        err = sdspi_host_init_device(&cfg->slot.spi_dev, &spi_handle);
        if (UNLIKELY(err != ESP_OK)) {
            // No device was added and sdspi_host_init() is stateless: nothing to
            // undo here, and sdspi_host_deinit() would destroy unrelated SDSPI
            // slots, including any FAT-mounted card.
            free(dev->card);
            dev->card = NULL;
            return err;
        }
        cfg->host.slot = spi_handle;
        err = sdmmc_card_init(&cfg->host, dev->card);
        if (UNLIKELY(err != ESP_OK)) {
            sdcard_host_deinit(&cfg->host);
            free(dev->card);
            dev->card = NULL;
            return err;
        }
#ifdef SDMMC_SLOT_CONFIG_DEFAULT
    } else {
        err = (*cfg->host.init)();
        if (UNLIKELY(err != ESP_OK)) {
            free(dev->card);
            dev->card = NULL;
            return err;
        }
        err = sdmmc_host_init_slot(cfg->host.slot, &cfg->slot.mmc_slot);
        if (UNLIKELY(err != ESP_OK)) {
            if (!(cfg->host.flags & SDMMC_HOST_FLAG_DEINIT_ARG)) {
                // Pre-5.3 IDF: the exclusive sdmmc_host_init() succeeded, so this
                // is the only host user and the host must be released. With
                // per-slot refcounting (IDF >= 5.3) no slot reference was taken:
                // a deinit here could tear down another slot's user, and the
                // tolerant host init recovers on the next open.
                cfg->host.deinit();
            }
            free(dev->card);
            dev->card = NULL;
            return err;
        }
        err = sdmmc_card_init(&cfg->host, dev->card);
        if (UNLIKELY(err != ESP_OK)) {
            sdcard_host_deinit(&cfg->host);
            free(dev->card);
            dev->card = NULL;
            return err;
        }
#else
    } else {
        free(dev->card);
        dev->card = NULL;
        return ESP_ERR_NOT_SUPPORTED;
#endif
    }

    dev->sector_size = dev->card->csd.sector_size;
    dev->sector_count = dev->card->csd.capacity;

    return ESP_OK;
}

static term nif_esp_sdcard_open(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    int str_ok;
    char *source = interop_term_to_string(argv[0], &str_ok);
    if (!str_ok) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term opts_term = argv[1];
    if (!term_is_list(opts_term) && !term_is_map(opts_term)) {
        free(source);
        RAISE_ERROR(BADARG_ATOM);
    }

    struct SDCardConfig cfg;
    bool ok = sdcard_config_from_source(source, opts_term, &cfg, glb);
    free(source);
    if (!ok) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct SDCardBlockDevice *dev
        = enif_alloc_resource(platform->sdcard_resource_type, sizeof(struct SDCardBlockDevice));
    if (IS_NULL_PTR(dev)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    dev->open = false;
    dev->card = NULL;
    dev->sector_size = 0;
    dev->sector_count = 0;
#ifndef AVM_NO_SMP
    dev->lock = smp_mutex_create();
    if (IS_NULL_PTR(dev->lock)) {
        enif_release_resource(dev);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
#endif

    esp_err_t err = sdcard_blockdev_init(&cfg, dev);

    term return_term;
    if (UNLIKELY(err != ESP_OK)) {
        return_term = make_esp_error_tuple(err, ctx);
    } else {
        dev->open = true;
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + TERM_BOXED_RESOURCE_SIZE)
                != MEMORY_GC_OK)) {
            enif_release_resource(dev);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term dev_term = term_from_resource(dev, &ctx->heap);
        return_term = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(return_term, 0, OK_ATOM);
        term_put_tuple_element(return_term, 1, dev_term);
    }
    enif_release_resource(dev);

    return return_term;
}

static term nif_esp_sdcard_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    void *dev_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            platform->sdcard_resource_type, &dev_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SDCardBlockDevice *dev = (struct SDCardBlockDevice *) dev_obj_ptr;

    if (UNLIKELY(!term_is_uint32(argv[1]))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint32_t sector = term_to_uint32(argv[1]);

    SMP_MUTEX_LOCK(dev->lock);
    if (UNLIKELY(!dev->open)) {
        SMP_MUTEX_UNLOCK(dev->lock);
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(sector >= dev->sector_count)) {
        SMP_MUTEX_UNLOCK(dev->lock);
        RAISE_ERROR(BADARG_ATOM);
    }
    size_t sector_size = dev->sector_size;

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(sector_size) + TUPLE_SIZE(2))
            != MEMORY_GC_OK)) {
        SMP_MUTEX_UNLOCK(dev->lock);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term binary = term_create_uninitialized_binary(sector_size, &ctx->heap, ctx->global);
    // term_create_uninitialized_binary returns an invalid term if the refc binary
    // allocation fails; memory_ensure_free only covers the boxed header words.
    if (UNLIKELY(term_is_invalid_term(binary))) {
        SMP_MUTEX_UNLOCK(dev->lock);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    // sdmmc_read_sectors bounces non-DMA-capable / unaligned destinations (e.g. a
    // binary in PSRAM) internally, so the binary is passed directly; it may fail
    // with ESP_ERR_NO_MEM under DMA heap pressure.
    esp_err_t err = sdcard_blockdev_read_sector(
        dev, (void *) term_binary_data(binary), (size_t) sector);
    SMP_MUTEX_UNLOCK(dev->lock);

    if (UNLIKELY(err != ESP_OK)) {
        return make_esp_error_tuple(err, ctx);
    }

    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, OK_ATOM);
    term_put_tuple_element(result, 1, binary);

    return result;
}

static term nif_esp_sdcard_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    void *dev_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            platform->sdcard_resource_type, &dev_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SDCardBlockDevice *dev = (struct SDCardBlockDevice *) dev_obj_ptr;

    if (UNLIKELY(!term_is_uint32(argv[1]))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint32_t sector = term_to_uint32(argv[1]);

    VALIDATE_VALUE(argv[2], term_is_binary);
    term data = argv[2];

    SMP_MUTEX_LOCK(dev->lock);
    if (UNLIKELY(!dev->open)) {
        SMP_MUTEX_UNLOCK(dev->lock);
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(term_binary_size(data) != dev->sector_size)) {
        SMP_MUTEX_UNLOCK(dev->lock);
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(sector >= dev->sector_count)) {
        SMP_MUTEX_UNLOCK(dev->lock);
        RAISE_ERROR(BADARG_ATOM);
    }

    esp_err_t err = sdcard_blockdev_write_sector(dev, term_binary_data(data), (size_t) sector);
    SMP_MUTEX_UNLOCK(dev->lock);

    if (UNLIKELY(err != ESP_OK)) {
        return make_esp_error_tuple(err, ctx);
    }

    return OK_ATOM;
}

static term nif_esp_sdcard_info(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    void *dev_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            platform->sdcard_resource_type, &dev_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SDCardBlockDevice *dev = (struct SDCardBlockDevice *) dev_obj_ptr;

    SMP_MUTEX_LOCK(dev->lock);
    if (UNLIKELY(!dev->open)) {
        SMP_MUTEX_UNLOCK(dev->lock);
        RAISE_ERROR(BADARG_ATOM);
    }
    uint32_t sector_size = dev->sector_size;
    uint32_t sector_count = dev->sector_count;
    SMP_MUTEX_UNLOCK(dev->lock);

    size_t needed = TUPLE_SIZE(2) + term_map_size_in_terms(2)
        + term_boxed_integer_size(sector_size) + term_boxed_integer_size(sector_count);
    if (UNLIKELY(memory_ensure_free(ctx, needed) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term sector_count_value = term_make_maybe_boxed_int64(sector_count, &ctx->heap);
    term sector_size_value = term_make_maybe_boxed_int64(sector_size, &ctx->heap);

    term info = term_alloc_map(2, &ctx->heap);
    term_set_map_assoc(
        info, 0, globalcontext_make_atom(glb, ATOM_STR("\xC", "sector_count")), sector_count_value);
    term_set_map_assoc(
        info, 1, globalcontext_make_atom(glb, ATOM_STR("\xB", "sector_size")), sector_size_value);

    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, OK_ATOM);
    term_put_tuple_element(result, 1, info);

    return result;
}

static term nif_esp_sdcard_close(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    void *dev_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            platform->sdcard_resource_type, &dev_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SDCardBlockDevice *dev = (struct SDCardBlockDevice *) dev_obj_ptr;

    SMP_MUTEX_LOCK(dev->lock);
    if (UNLIKELY(!dev->open)) {
        SMP_MUTEX_UNLOCK(dev->lock);
        RAISE_ERROR(BADARG_ATOM);
    }
    esp_err_t err = sdcard_blockdev_close(dev);
    dev->open = false;
    SMP_MUTEX_UNLOCK(dev->lock);

    if (UNLIKELY(err != ESP_OK)) {
        return make_esp_error_tuple(err, ctx);
    }

    return OK_ATOM;
}

static void sdcard_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct SDCardBlockDevice *dev = (struct SDCardBlockDevice *) obj;
    if (dev->open) {
        esp_err_t err = sdcard_blockdev_close(dev);
        dev->open = false;
        if (UNLIKELY(err != ESP_OK)) {
            ESP_LOGW(TAG,
                "Failed to close SD card block device in resource dtor. "
                "Please use esp:sdcard_close/1.");
        }
    }

#ifndef AVM_NO_SMP
    if (dev->lock) {
        smp_mutex_destroy(dev->lock);
        dev->lock = NULL;
    }
#endif
}
#endif

void storage_nif_init(GlobalContext *global)
{
    struct ESP32PlatformData *platform = global->platform_data;

    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
#ifdef CONFIG_AVM_ENABLE_STORAGE_NIFS
    platform->mounted_fs_resource_type = enif_init_resource_type(
        &env, "mounted_fs", &mounted_fs_resource_type_init, ERL_NIF_RT_CREATE, NULL);
#endif
#ifdef CONFIG_AVM_ENABLE_RAW_SDCARD_NIFS
    platform->sdcard_resource_type = enif_init_resource_type(
        &env, "sdcard", &sdcard_resource_type_init, ERL_NIF_RT_CREATE, NULL);
#endif
}

#ifdef CONFIG_AVM_ENABLE_STORAGE_NIFS
static const struct Nif esp_mount_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_mount
};

static const struct Nif esp_umount_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_umount
};
#endif

#ifdef CONFIG_AVM_ENABLE_RAW_SDCARD_NIFS
static const struct Nif esp_sdcard_open_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_sdcard_open
};

static const struct Nif esp_sdcard_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_sdcard_read
};

static const struct Nif esp_sdcard_write_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_sdcard_write
};

static const struct Nif esp_sdcard_info_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_sdcard_info
};

static const struct Nif esp_sdcard_close_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_sdcard_close
};
#endif

const struct Nif *storage_nif_get_nif(const char *nifname)
{
#ifdef CONFIG_AVM_ENABLE_STORAGE_NIFS
    if (strcmp("esp:mount/4", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_mount_nif;
    }
    if (strcmp("esp:umount/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_umount_nif;
    }
#endif
#ifdef CONFIG_AVM_ENABLE_RAW_SDCARD_NIFS
    if (strcmp("esp:sdcard_open/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_sdcard_open_nif;
    }
    if (strcmp("esp:sdcard_read/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_sdcard_read_nif;
    }
    if (strcmp("esp:sdcard_write/3", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_sdcard_write_nif;
    }
    if (strcmp("esp:sdcard_info/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_sdcard_info_nif;
    }
    if (strcmp("esp:sdcard_close/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_sdcard_close_nif;
    }
#endif

    return NULL;
}

REGISTER_NIF_COLLECTION(storage, storage_nif_init, NULL, storage_nif_get_nif)

#endif
