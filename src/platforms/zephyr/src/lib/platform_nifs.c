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

#ifdef CONFIG_NET_SOCKETPAIR
#include <sys/socket.h>
#include <unistd.h>
#include <fcntl.h>

struct PosixFd
{
    int fd;
    int32_t selecting_process_id;
    ErlNifMonitor selecting_process_monitor;
};
#endif

#ifdef CONFIG_FAT_FILESYSTEM_ELM
struct ZephyrMountedFS {
    struct fs_mount_t mount;
    FATFS fatfs;
    char *mnt_point;
    char *storage_dev;
};

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

    struct ZephyrPlatformData *platform = ctx->global->platform_data;
    if (platform->zephyr_mounted_fs_resource_type == NULL) {
        ErlNifEnv env;
        erl_nif_env_partial_init_from_globalcontext(&env, ctx->global);
        ErlNifResourceTypeInit init = {
            .members = 1,
            .dtor = zephyr_mounted_fs_dtor
        };
        platform->zephyr_mounted_fs_resource_type = enif_init_resource_type(&env, "zephyr_mounted_fs", &init, ERL_NIF_RT_CREATE, NULL);
    }

    struct ZephyrMountedFS *mount_res = enif_alloc_resource(platform->zephyr_mounted_fs_resource_type, sizeof(struct ZephyrMountedFS));
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

    struct ZephyrPlatformData *platform = ctx->global->platform_data;
    if (platform->zephyr_mounted_fs_resource_type == NULL) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct ZephyrMountedFS *mount_res;
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, ctx->global);
    if (!enif_get_resource(&env, mount_term, platform->zephyr_mounted_fs_resource_type, (void **) &mount_res)) {
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

#ifdef CONFIG_NET_SOCKETPAIR
static term nif_zephyr_socketpair(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    if (UNLIKELY(memory_ensure_free(ctx, 2 * TERM_BOXED_REFERENCE_RESOURCE_SIZE + 2 * TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    int fds[2];
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, fds) < 0) {
        term error_atom = globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "error"));
        term err_reason = globalcontext_make_atom(ctx->global, ATOM_STR("\x11", "socketpair_failed"));
        term err_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(err_tuple, 0, error_atom);
        term_put_tuple_element(err_tuple, 1, err_reason);
        return err_tuple;
    }

    int flags0 = fcntl(fds[0], F_GETFL, 0);
    fcntl(fds[0], F_SETFL, flags0 | O_NONBLOCK);
    int flags1 = fcntl(fds[1], F_GETFL, 0);
    fcntl(fds[1], F_SETFL, flags1 | O_NONBLOCK);

    struct PosixFd *fd_obj0 = enif_alloc_resource(ctx->global->posix_fd_resource_type, sizeof(struct PosixFd));
    struct PosixFd *fd_obj1 = enif_alloc_resource(ctx->global->posix_fd_resource_type, sizeof(struct PosixFd));

    if (fd_obj0 == NULL || fd_obj1 == NULL) {
        close(fds[0]);
        close(fds[1]);
        if (fd_obj0) enif_release_resource(fd_obj0);
        if (fd_obj1) enif_release_resource(fd_obj1);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    fd_obj0->fd = fds[0];
    fd_obj0->selecting_process_id = INVALID_PROCESS_ID;
    fd_obj1->fd = fds[1];
    fd_obj1->selecting_process_id = INVALID_PROCESS_ID;

    term ok_atom = globalcontext_make_atom(ctx->global, ATOM_STR("\x2", "ok"));
    term res_term0 = term_from_resource(fd_obj0, &ctx->heap);
    term res_term1 = term_from_resource(fd_obj1, &ctx->heap);

    enif_release_resource(fd_obj0);
    enif_release_resource(fd_obj1);

    term pair_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(pair_tuple, 0, res_term0);
    term_put_tuple_element(pair_tuple, 1, res_term1);

    term ret_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret_tuple, 0, ok_atom);
    term_put_tuple_element(ret_tuple, 1, pair_tuple);
    return ret_tuple;
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
#ifdef CONFIG_NET_SOCKETPAIR
    if (strcmp("zephyr:socketpair/0", nifname) == 0) {
        static const struct Nif zephyr_socketpair_nif = {
            .base.type = NIFFunctionType,
            .nif_ptr = nif_zephyr_socketpair
        };
        return &zephyr_socketpair_nif;
    }
#endif
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

#ifdef CONFIG_PSA_CRYPTO
#include <otp_crypto.h>
REGISTER_NIF_COLLECTION(otp_crypto, NULL, NULL, otp_crypto_nif_get_nif)
#endif

#ifdef CONFIG_AVM_ENABLE_CRYPTO
#include <otp_ssl.h>
REGISTER_NIF_COLLECTION(ssl, otp_ssl_init, NULL, otp_ssl_nif_get_nif)
#endif
