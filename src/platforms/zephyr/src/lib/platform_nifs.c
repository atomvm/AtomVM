#include <defaultatoms.h>
#include <nifs.h>
#include <platform_nifs.h>
#include <term.h>
#include <interop.h>
#include <erl_nif.h>
#include <erl_nif_priv.h>

#ifdef CONFIG_FAT_FILESYSTEM_ELM
#include <zephyr/fs/fs.h>
#ifndef FS_FATFS_WINDOW_ALIGNMENT
#ifdef CONFIG_FS_FATFS_WINDOW_ALIGNMENT
#define FS_FATFS_WINDOW_ALIGNMENT CONFIG_FS_FATFS_WINDOW_ALIGNMENT
#else
#define FS_FATFS_WINDOW_ALIGNMENT 4
#endif
#endif
#include <ff.h>
#endif

// #define ENABLE_TRACE
#include <trace.h>

#include "zephyros_sys.h"

#ifdef CONFIG_FAT_FILESYSTEM_ELM
struct ZephyrMountedFS {
    struct fs_mount_t mount;
    FATFS fatfs;
    char *mnt_point;
    char *storage_dev;
};

static ErlNifResourceType *zephyr_mounted_fs_resource_type = NULL;

static void zephyr_mounted_fs_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct ZephyrMountedFS *mount_res = (struct ZephyrMountedFS *) obj;
    if (mount_res->mnt_point) {
        fs_unmount(&mount_res->mount);
        free(mount_res->mnt_point);
        mount_res->mnt_point = NULL;
    }
    if (mount_res->storage_dev) {
        free(mount_res->storage_dev);
        mount_res->storage_dev = NULL;
    }
}

static term nif_zephyr_mount(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term source_term = argv[0];
    term target_term = argv[1];
    term fstype_term = argv[2];
    term opts_term = argv[3];

    UNUSED(opts_term);

    if (UNLIKELY(!globalcontext_is_term_equal_to_atom_string(ctx->global, fstype_term, ATOM_STR("\x3", "fat")))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int source_ok;
    char *source = interop_term_to_string(source_term, &source_ok);
    if (!source_ok) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int target_ok;
    char *target = interop_term_to_string(target_term, &target_ok);
    if (!target_ok) {
        free(source);
        RAISE_ERROR(BADARG_ATOM);
    }

    if (zephyr_mounted_fs_resource_type == NULL) {
        ErlNifEnv env;
        erl_nif_env_partial_init_from_globalcontext(&env, ctx->global);
        ErlNifResourceTypeInit init = {
            .dtor = zephyr_mounted_fs_dtor
        };
        zephyr_mounted_fs_resource_type = enif_init_resource_type(&env, "zephyr_mounted_fs", &init, ERL_NIF_RT_CREATE, NULL);
    }

    struct ZephyrMountedFS *mount_res = enif_alloc_resource(zephyr_mounted_fs_resource_type, sizeof(struct ZephyrMountedFS));
    if (mount_res == NULL) {
        free(source);
        free(target);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    memset(mount_res, 0, sizeof(struct ZephyrMountedFS));

    mount_res->mnt_point = target;
    mount_res->storage_dev = source;

    mount_res->mount.type = FS_FATFS;
    mount_res->mount.mnt_point = mount_res->mnt_point;
    mount_res->mount.fs_data = &mount_res->fatfs;
    mount_res->mount.storage_dev = mount_res->storage_dev;

    int rc = fs_mount(&mount_res->mount);
    if (rc != 0) {
        enif_release_resource(mount_res);
        term error_atom = globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "error"));
        term err_reason = globalcontext_make_atom(ctx->global, ATOM_STR("\xc", "mount_failed"));
        term err_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(err_tuple, 0, error_atom);
        term_put_tuple_element(err_tuple, 1, err_reason);
        return err_tuple;
    }

    term ok_atom = globalcontext_make_atom(ctx->global, ATOM_STR("\x2", "ok"));
    term res_term = term_from_resource(mount_res, &ctx->heap);
    enif_release_resource(mount_res);

    term ret_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret_tuple, 0, ok_atom);
    term_put_tuple_element(ret_tuple, 1, res_term);
    return ret_tuple;
}

static term nif_zephyr_umount(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term mount_term = argv[0];

    if (zephyr_mounted_fs_resource_type == NULL) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct ZephyrMountedFS *mount_res;
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, ctx->global);
    if (!enif_get_resource(&env, mount_term, zephyr_mounted_fs_resource_type, (void **) &mount_res)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (mount_res->mnt_point == NULL) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int rc = fs_unmount(&mount_res->mount);
    if (rc != 0) {
        term error_atom = globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "error"));
        term err_reason = globalcontext_make_atom(ctx->global, ATOM_STR("\xe", "unmount_failed"));
        term err_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(err_tuple, 0, error_atom);
        term_put_tuple_element(err_tuple, 1, err_reason);
        return err_tuple;
    }

    free(mount_res->mnt_point);
    mount_res->mnt_point = NULL;
    free(mount_res->storage_dev);
    mount_res->storage_dev = NULL;

    return globalcontext_make_atom(ctx->global, ATOM_STR("\x2", "ok"));
}

static term nif_zephyr_mkfs(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term source_term = argv[0];
    term fstype_term = argv[1];

    if (UNLIKELY(!globalcontext_is_term_equal_to_atom_string(ctx->global, fstype_term, ATOM_STR("\x3", "fat")))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int source_ok;
    char *source = interop_term_to_string(source_term, &source_ok);
    if (!source_ok) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int rc = fs_mkfs(FS_FATFS, (uintptr_t) source, NULL, 0);
    free(source);

    if (rc != 0) {
        term error_atom = globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "error"));
        term err_reason = globalcontext_make_atom(ctx->global, ATOM_STR("\xb", "mkfs_failed"));
        term err_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(err_tuple, 0, error_atom);
        term_put_tuple_element(err_tuple, 1, err_reason);
        return err_tuple;
    }

    return globalcontext_make_atom(ctx->global, ATOM_STR("\x2", "ok"));
}
#endif

static term nif_atomvm_platform(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    return globalcontext_make_atom(ctx->global, ATOM_STR("\x6", "zephyr"));
}

static const struct Nif atomvm_platform_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_platform
};

const struct Nif *platform_nifs_get_nif(const char *nifname)
{
    if (strcmp("atomvm:platform/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &atomvm_platform_nif;
    }
#ifdef CONFIG_FAT_FILESYSTEM_ELM
    if (strcmp("zephyr:mount/4", nifname) == 0) {
        static const struct Nif zephyr_mount_nif = {
            .base.type = NIFFunctionType,
            .nif_ptr = nif_zephyr_mount
        };
        return &zephyr_mount_nif;
    }
    if (strcmp("zephyr:umount/1", nifname) == 0) {
        static const struct Nif zephyr_umount_nif = {
            .base.type = NIFFunctionType,
            .nif_ptr = nif_zephyr_umount
        };
        return &zephyr_umount_nif;
    }
    if (strcmp("zephyr:mkfs/2", nifname) == 0) {
        static const struct Nif zephyr_mkfs_nif = {
            .base.type = NIFFunctionType,
            .nif_ptr = nif_zephyr_mkfs
        };
        return &zephyr_mkfs_nif;
    }
#endif
    const struct Nif *nif = nif_collection_resolve_nif(nifname);
    if (nif) {
        return nif;
    }
    return NULL;
}
