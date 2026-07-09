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

#ifdef CONFIG_PM
#include <zephyr/pm/pm.h>

static enum pm_state term_to_pm_state(term t, GlobalContext *glb, bool *ok)
{
    *ok = true;
    if (globalcontext_is_term_equal_to_atom_string(glb, t, ATOM_STR("\x6", "active"))) {
        return PM_STATE_ACTIVE;
    } else if (globalcontext_is_term_equal_to_atom_string(glb, t, ATOM_STR("\xc", "runtime_idle"))) {
        return PM_STATE_RUNTIME_IDLE;
    } else if (globalcontext_is_term_equal_to_atom_string(glb, t, ATOM_STR("\xf", "suspend_to_idle"))) {
        return PM_STATE_SUSPEND_TO_IDLE;
    } else if (globalcontext_is_term_equal_to_atom_string(glb, t, ATOM_STR("\x7", "standby"))) {
        return PM_STATE_STANDBY;
    } else if (globalcontext_is_term_equal_to_atom_string(glb, t, ATOM_STR("\xe", "suspend_to_ram"))) {
        return PM_STATE_SUSPEND_TO_RAM;
    } else if (globalcontext_is_term_equal_to_atom_string(glb, t, ATOM_STR("\xf", "suspend_to_disk"))) {
        return PM_STATE_SUSPEND_TO_DISK;
    } else if (globalcontext_is_term_equal_to_atom_string(glb, t, ATOM_STR("\x8", "soft_off"))) {
        return PM_STATE_SOFT_OFF;
    }
    *ok = false;
    return PM_STATE_ACTIVE;
}

static term pm_state_to_term(enum pm_state state, GlobalContext *glb)
{
    switch (state) {
        case PM_STATE_ACTIVE:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "active"));
        case PM_STATE_RUNTIME_IDLE:
            return globalcontext_make_atom(glb, ATOM_STR("\xc", "runtime_idle"));
        case PM_STATE_SUSPEND_TO_IDLE:
            return globalcontext_make_atom(glb, ATOM_STR("\xf", "suspend_to_idle"));
        case PM_STATE_STANDBY:
            return globalcontext_make_atom(glb, ATOM_STR("\x7", "standby"));
        case PM_STATE_SUSPEND_TO_RAM:
            return globalcontext_make_atom(glb, ATOM_STR("\xe", "suspend_to_ram"));
        case PM_STATE_SUSPEND_TO_DISK:
            return globalcontext_make_atom(glb, ATOM_STR("\xf", "suspend_to_disk"));
        case PM_STATE_SOFT_OFF:
            return globalcontext_make_atom(glb, ATOM_STR("\x8", "soft_off"));
        default:
            return globalcontext_make_atom(glb, ATOM_STR("\x7", "unknown"));
    }
}

static term nif_zephyr_pm_state_force(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    if (!term_is_integer(argv[0])) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int cpu = term_to_int(argv[0]);

    term state_term = argv[1];
    enum pm_state state = PM_STATE_ACTIVE;
    uint8_t substate_id = 0;
    uint32_t min_residency_us = 0;
    uint32_t exit_latency_us = 0;

    bool ok;
    if (term_is_atom(state_term)) {
        state = term_to_pm_state(state_term, ctx->global, &ok);
        if (!ok) {
            RAISE_ERROR(BADARG_ATOM);
        }
    } else if (term_is_tuple(state_term)) {
        int tuple_size = term_get_tuple_arity(state_term);
        if (tuple_size < 2 || tuple_size > 4) {
            RAISE_ERROR(BADARG_ATOM);
        }
        term el0 = term_get_tuple_element(state_term, 0);
        if (!term_is_atom(el0)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        state = term_to_pm_state(el0, ctx->global, &ok);
        if (!ok) {
            RAISE_ERROR(BADARG_ATOM);
        }

        term el1 = term_get_tuple_element(state_term, 1);
        if (!term_is_integer(el1)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        substate_id = term_to_int(el1);

        if (tuple_size >= 3) {
            term el2 = term_get_tuple_element(state_term, 2);
            if (!term_is_integer(el2)) {
                RAISE_ERROR(BADARG_ATOM);
            }
            min_residency_us = term_to_int(el2);
        }
        if (tuple_size == 4) {
            term el3 = term_get_tuple_element(state_term, 3);
            if (!term_is_integer(el3)) {
                RAISE_ERROR(BADARG_ATOM);
            }
            exit_latency_us = term_to_int(el3);
        }
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct pm_state_info info = {
        .state = state,
        .substate_id = substate_id,
        .min_residency_us = min_residency_us,
        .exit_latency_us = exit_latency_us,
    };

    bool forced = pm_state_force(cpu, &info);
    return forced ? TRUE_ATOM : FALSE_ATOM;
}

static term nif_zephyr_pm_state_next_get(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    if (!term_is_integer(argv[0])) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int cpu = term_to_int(argv[0]);

    const struct pm_state_info *info = pm_state_next_get(cpu);
    if (info == NULL) {
        return UNDEFINED_ATOM;
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(4)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term state_atom = pm_state_to_term(info->state, ctx->global);
    term substate_term = term_from_int(info->substate_id);
    term min_residency_term = term_from_int(info->min_residency_us);
    term exit_latency_term = term_from_int(info->exit_latency_us);

    term result_tuple = term_alloc_tuple(4, &ctx->heap);
    term_put_tuple_element(result_tuple, 0, state_atom);
    term_put_tuple_element(result_tuple, 1, substate_term);
    term_put_tuple_element(result_tuple, 2, min_residency_term);
    term_put_tuple_element(result_tuple, 3, exit_latency_term);

    return result_tuple;
}
#endif

#ifdef CONFIG_TASK_WDT
#include <zephyr/task_wdt/task_wdt.h>

static bool platform_task_wdt_initialized = true;
static bool platform_task_wdt_inited_in_kernel = false;
static uint32_t platform_task_wdt_default_timeout_ms = 5000;

static void ensure_task_wdt_kernel_inited(void)
{
    if (UNLIKELY(!platform_task_wdt_inited_in_kernel)) {
        task_wdt_init(NULL);
        platform_task_wdt_inited_in_kernel = true;
    }
}

struct zephyr_task_wdt_user_handle {
    int channel_id;
    bool active;
};

static term parse_task_wdt_config(Context *ctx, uint32_t *timeout_ms, term argv[])
{
    VALIDATE_VALUE(argv[0], term_is_tuple);
    size_t tuple_size = term_get_tuple_arity(argv[0]);
    if (tuple_size != 3) {
        RAISE_ERROR(BADARG_ATOM);
    }
    term timeout_ms_term = term_get_tuple_element(argv[0], 0);
    VALIDATE_VALUE(timeout_ms_term, term_is_integer);
    avm_int_t timeout = term_to_int(timeout_ms_term);
    if (timeout <= 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term core_mask_term = term_get_tuple_element(argv[0], 1);
    VALIDATE_VALUE(core_mask_term, term_is_integer);
    avm_int_t core_mask = term_to_int(core_mask_term);
    if (core_mask < 0 || core_mask > (1 << 2)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term trigger_panic_term = term_get_tuple_element(argv[0], 2);
    if (trigger_panic_term != TRUE_ATOM && trigger_panic_term != FALSE_ATOM) {
        RAISE_ERROR(BADARG_ATOM);
    }

    *timeout_ms = (uint32_t)timeout;
    return OK_ATOM;
}

static term nif_zephyr_task_wdt_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    uint32_t timeout_ms;
    if (term_is_invalid_term(parse_task_wdt_config(ctx, &timeout_ms, argv))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (platform_task_wdt_initialized) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term result_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(result_tuple, 1, globalcontext_make_atom(ctx->global, ATOM_STR("\xf", "already_started")));
        return result_tuple;
    }

    ensure_task_wdt_kernel_inited();

    platform_task_wdt_initialized = true;
    platform_task_wdt_default_timeout_ms = timeout_ms;

    return OK_ATOM;
}

static term nif_zephyr_task_wdt_reconfigure(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    uint32_t timeout_ms;
    if (term_is_invalid_term(parse_task_wdt_config(ctx, &timeout_ms, argv))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (!platform_task_wdt_initialized) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term result_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(result_tuple, 1, globalcontext_make_atom(ctx->global, ATOM_STR("\x6", "noproc")));
        return result_tuple;
    }

    platform_task_wdt_default_timeout_ms = timeout_ms;
    return OK_ATOM;
}

static term nif_zephyr_task_wdt_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    if (!platform_task_wdt_initialized) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term result_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(result_tuple, 1, globalcontext_make_atom(ctx->global, ATOM_STR("\x6", "noproc")));
        return result_tuple;
    }

    ensure_task_wdt_kernel_inited();
    task_wdt_suspend();
    for (int id = 0; id < CONFIG_TASK_WDT_CHANNELS; id++) {
        task_wdt_delete(id);
    }

    platform_task_wdt_initialized = false;
    return OK_ATOM;
}

static term nif_zephyr_task_wdt_add_user(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    if (!term_is_binary(argv[0])) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (!platform_task_wdt_initialized) {
        RAISE_ERROR(BADARG_ATOM);
    }

    ensure_task_wdt_kernel_inited();
    int channel_id = task_wdt_add(platform_task_wdt_default_timeout_ms, NULL, NULL);
    if (channel_id < 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_binary_heap_size(sizeof(struct zephyr_task_wdt_user_handle))) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term binary = term_create_empty_binary(sizeof(struct zephyr_task_wdt_user_handle), &ctx->heap, ctx->global);
    struct zephyr_task_wdt_user_handle *handle = (struct zephyr_task_wdt_user_handle *) term_binary_data(binary);
    handle->channel_id = channel_id;
    handle->active = true;

    term result_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result_tuple, 0, OK_ATOM);
    term_put_tuple_element(result_tuple, 1, binary);

    return result_tuple;
}

static term nif_zephyr_task_wdt_reset_user(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    if (!term_is_binary(argv[0])) {
        RAISE_ERROR(BADARG_ATOM);
    }
    size_t binary_size = term_binary_size(argv[0]);
    if (binary_size != sizeof(struct zephyr_task_wdt_user_handle)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct zephyr_task_wdt_user_handle *handle = (struct zephyr_task_wdt_user_handle *) term_binary_data(argv[0]);
    if (!handle->active) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int result = task_wdt_feed(handle->channel_id);
    if (result != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return OK_ATOM;
}

static term nif_zephyr_task_wdt_delete_user(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    if (!term_is_binary(argv[0])) {
        RAISE_ERROR(BADARG_ATOM);
    }
    size_t binary_size = term_binary_size(argv[0]);
    if (binary_size != sizeof(struct zephyr_task_wdt_user_handle)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct zephyr_task_wdt_user_handle *handle = (struct zephyr_task_wdt_user_handle *) term_binary_data(argv[0]);
    if (!handle->active) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int result = task_wdt_delete(handle->channel_id);
    if (result != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    handle->active = false;
    return OK_ATOM;
}
#endif

const struct Nif *platform_nifs_get_nif(const char *nifname)
{
    if (strcmp("atomvm:platform/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &atomvm_platform_nif;
    }
#ifdef CONFIG_PM
    if (strcmp("zephyr:pm_state_force/2", nifname) == 0) {
        static const struct Nif zephyr_pm_state_force_nif = {
            .base.type = NIFFunctionType,
            .nif_ptr = nif_zephyr_pm_state_force
        };
        return &zephyr_pm_state_force_nif;
    }
    if (strcmp("zephyr:pm_state_next_get/1", nifname) == 0) {
        static const struct Nif zephyr_pm_state_next_get_nif = {
            .base.type = NIFFunctionType,
            .nif_ptr = nif_zephyr_pm_state_next_get
        };
        return &zephyr_pm_state_next_get_nif;
    }
#endif
#ifdef CONFIG_TASK_WDT
    if (strcmp("zephyr:task_wdt_init/1", nifname) == 0) {
        static const struct Nif zephyr_task_wdt_init_nif = {
            .base.type = NIFFunctionType,
            .nif_ptr = nif_zephyr_task_wdt_init
        };
        return &zephyr_task_wdt_init_nif;
    }
    if (strcmp("zephyr:task_wdt_reconfigure/1", nifname) == 0) {
        static const struct Nif zephyr_task_wdt_reconfigure_nif = {
            .base.type = NIFFunctionType,
            .nif_ptr = nif_zephyr_task_wdt_reconfigure
        };
        return &zephyr_task_wdt_reconfigure_nif;
    }
    if (strcmp("zephyr:task_wdt_deinit/0", nifname) == 0) {
        static const struct Nif zephyr_task_wdt_deinit_nif = {
            .base.type = NIFFunctionType,
            .nif_ptr = nif_zephyr_task_wdt_deinit
        };
        return &zephyr_task_wdt_deinit_nif;
    }
    if (strcmp("zephyr:task_wdt_add_user/1", nifname) == 0) {
        static const struct Nif zephyr_task_wdt_add_user_nif = {
            .base.type = NIFFunctionType,
            .nif_ptr = nif_zephyr_task_wdt_add_user
        };
        return &zephyr_task_wdt_add_user_nif;
    }
    if (strcmp("zephyr:task_wdt_reset_user/1", nifname) == 0) {
        static const struct Nif zephyr_task_wdt_reset_user_nif = {
            .base.type = NIFFunctionType,
            .nif_ptr = nif_zephyr_task_wdt_reset_user
        };
        return &zephyr_task_wdt_reset_user_nif;
    }
    if (strcmp("zephyr:task_wdt_delete_user/1", nifname) == 0) {
        static const struct Nif zephyr_task_wdt_delete_user_nif = {
            .base.type = NIFFunctionType,
            .nif_ptr = nif_zephyr_task_wdt_delete_user
        };
        return &zephyr_task_wdt_delete_user_nif;
    }
#endif
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

#if defined(CONFIG_AVM_ENABLE_CRYPTO) && defined(CONFIG_NET_SOCKETS)
#include <otp_ssl.h>
REGISTER_NIF_COLLECTION(ssl, otp_ssl_init, NULL, otp_ssl_nif_get_nif)
#endif
