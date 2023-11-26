/*
 * This file is part of AtomVM.
 *
 * Copyright 2017-2022 Davide Bettio <davide@uninstall.it>
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

#include "sys.h"
#include "esp32_sys.h"

#include "avmpack.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "otp_socket.h"
#include "scheduler.h"
#include "utils.h"

// #define ENABLE_TRACE
#include "trace.h"

#include "esp_heap_caps.h"
#include "esp_idf_version.h"
#include "esp_pthread.h"
#include "esp_system.h"
#include "esp_timer.h"
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include <esp_log.h>
#include <esp_partition.h>
#include <limits.h>
#include <pthread.h>
#include <stdint.h>
#include <sys/socket.h>

#if ESP_IDF_VERSION_MAJOR >= 5
#include "esp_chip_info.h"
#endif

#include <esp_vfs_eventfd.h>

#ifdef HAVE_SOC_CPU_CORES_NUM
#include "soc/soc_caps.h"
#endif

#if defined(MBEDTLS_VERSION_NUMBER) && (MBEDTLS_VERSION_NUMBER >= 0x03000000)
#include <mbedtls/build_info.h>
#else
#include <mbedtls/config.h>
#endif

// Platform uses listeners
#include "listeners.h"

#define EVENT_QUEUE_LEN 16

#define TAG "sys"

#ifndef AVM_NO_SMP
#define SMP_MUTEX_LOCK(mtx) smp_mutex_lock(mtx)
#define SMP_MUTEX_UNLOCK(mtx) smp_mutex_unlock(mtx)
#else
#define SMP_MUTEX_LOCK(mtx)
#define SMP_MUTEX_UNLOCK(mtx)
#endif

static Context *port_driver_create_port(const char *port_name, GlobalContext *global, term opts);

static void *select_thread_loop(void *);
static void select_thread_signal(struct ESP32PlatformData *platform);

// clang-format off
static const char *const esp_free_heap_size_atom = "\x14" "esp32_free_heap_size";
static const char *const esp_largest_free_block_atom = "\x18" "esp32_largest_free_block";
static const char *const esp_get_minimum_free_size_atom = "\x17" "esp32_minimum_free_size";
static const char *const esp_chip_info_atom = "\xF" "esp32_chip_info";
static const char *const esp_idf_version_atom = "\xF" "esp_idf_version";
#if ESP_IDF_VERSION >= ESP_IDF_VERSION_VAL(4, 3, 2)
static const char *const esp32_atom = "\x5" "esp32";
static const char *const esp32_s2_atom = "\x8" "esp32_s2";
static const char *const esp32_s3_atom = "\x8" "esp32_s3";
static const char *const esp32_c3_atom = "\x8" "esp32_c3";
#endif
static const char *const emb_flash_atom = "\x9" "emb_flash";
static const char *const bgn_atom = "\x3" "bgn";
static const char *const ble_atom = "\x3" "ble";
static const char *const bt_atom = "\x2" "bt";
static const char *const cores_atom = "\x5" "cores";
static const char *const features_atom = "\x8" "features";
static const char *const model_atom = "\x5" "model";
static const char *const revision_atom = "\x8" "revision";
// clang-format on

struct PortDriverDefListItem *port_driver_list;
struct NifCollectionDefListItem *nif_collection_list;

QueueHandle_t event_queue = NULL;
QueueSetHandle_t event_set = NULL;

void esp32_sys_queue_init()
{
    event_set = xQueueCreateSet(EVENT_QUEUE_LEN * 4);
    event_queue = xQueueCreate(EVENT_QUEUE_LEN, sizeof(void *));
    xQueueAddToSet(event_queue, event_set);
}

static inline void sys_clock_gettime(struct timespec *t)
{
    TickType_t ticks = xTaskGetTickCount();
    t->tv_sec = (ticks * portTICK_PERIOD_MS) / 1000;
    t->tv_nsec = ((ticks * portTICK_PERIOD_MS) % 1000) * 1000000;
}

static void receive_events(GlobalContext *glb, TickType_t wait_ticks)
{
    void *sender = NULL;
    QueueSetMemberHandle_t event_source;
    while ((event_source = xQueueSelectFromSet(event_set, wait_ticks))) {
        // Listener used shared event_queue.
        if (event_source == event_queue) {
            if (UNLIKELY(xQueueReceive(event_queue, &sender, 0) == pdFALSE)) {
                continue;
            }
        } else {
            sender = event_source;
        }

#ifndef AVM_NO_SMP
        if (sender == CAST_FUNC_TO_VOID_PTR(sys_signal)) {
            // We've been signaled
            return;
        }
#endif

        struct ListHead *listeners = synclist_wrlock(&glb->listeners);
        if (!process_listener_handler(glb, sender, listeners, NULL, NULL)) {
            TRACE("sys: handler not found for: %p\n", (void *) sender);
        }

        synclist_unlock(&glb->listeners);
    }
}

void sys_poll_events(GlobalContext *glb, int timeout_ms)
{
    TickType_t wait_ticks;
    if (timeout_ms < 0) {
        wait_ticks = portMAX_DELAY;
    } else {
        wait_ticks = timeout_ms / portTICK_PERIOD_MS;
    }
    receive_events(glb, wait_ticks);
}

#ifndef AVM_NO_SMP
void sys_signal(GlobalContext *glb)
{
    void *queue_item = CAST_FUNC_TO_VOID_PTR(sys_signal);
    xQueueSendToBack(event_queue, &queue_item, 0);
}
#endif

void sys_time(struct timespec *t)
{
    struct timeval tv;
    if (UNLIKELY(gettimeofday(&tv, NULL))) {
        fprintf(stderr, "Failed gettimeofday.\n");
        AVM_ABORT();
    }

    t->tv_sec = tv.tv_sec;
    t->tv_nsec = (uint_least64_t) tv.tv_usec * 1000;
}

void sys_monotonic_time(struct timespec *t)
{
    uint64_t us_since_boot = (uint64_t) esp_timer_get_time();

    t->tv_sec = us_since_boot / 1000000;
    t->tv_nsec = (us_since_boot % 1000000) * 1000;
}

uint64_t sys_monotonic_time_u64()
{
    return esp_timer_get_time();
}

uint64_t sys_monotonic_time_ms_to_u64(uint64_t ms)
{
    return ms * 1000;
}

uint64_t sys_monotonic_time_u64_to_ms(uint64_t t)
{
    return t / 1000;
}

void sys_init_platform(GlobalContext *glb)
{
    struct ESP32PlatformData *platform = malloc(sizeof(struct ESP32PlatformData));
    glb->platform_data = platform;
    platform->select_thread_exit = false;
    platform->select_events_poll_count = -1;
    esp_vfs_eventfd_config_t eventfd_config = ESP_VFS_EVENTD_CONFIG_DEFAULT();
    esp_err_t err = esp_vfs_eventfd_register(&eventfd_config);
    if (err == ESP_OK) {
        platform->eventfd_registered = true;
    } else {
        if (UNLIKELY(err != ESP_ERR_INVALID_STATE)) {
            // Function can return fatal ESP_ERR_NO_MEM
            fprintf(stderr, "Cannot register eventfd, unexpected error = %d\n", err);
            AVM_ABORT();
        }
        platform->eventfd_registered = false;
    }
    int signal_fd = eventfd(0, 0);
    if (UNLIKELY(signal_fd < 0)) {
        fprintf(stderr, "Cannot create signal_fd\n");
        AVM_ABORT();
    }
    platform->signal_fd = signal_fd;
    if (UNLIKELY(pthread_create(&platform->select_thread, NULL, select_thread_loop, glb))) {
        AVM_ABORT();
    }
#ifndef AVM_NO_SMP
    // Use the ESP-IDF API to change the default thread attributes
    // We use the current main thread priority.
    // If the current thread is pinned to a core, we will pin other threads to
    // other cores, supposing it's pinned to core 0.
    esp_pthread_cfg_t esp_pthread_cfg = esp_pthread_get_default_config();
    esp_pthread_cfg.prio = uxTaskPriorityGet(NULL);
    BaseType_t affinity = xTaskGetAffinity(NULL);
    if (affinity == -1) {
        esp_pthread_cfg.pin_to_core = tskNO_AFFINITY;
    } else {
        if (UNLIKELY(affinity != 0)) {
            fprintf(stderr, "Unexpected affinity for main task.\n");
            AVM_ABORT();
        }
        esp_pthread_cfg.pin_to_core = 1;
    }
    if (UNLIKELY(esp_pthread_set_cfg(&esp_pthread_cfg))) {
        AVM_ABORT();
    }
#endif

    platform->entropy_is_initialized = false;
    platform->random_is_initialized = false;
}

void sys_free_platform(GlobalContext *glb)
{
    struct ESP32PlatformData *platform = glb->platform_data;
    platform->select_thread_exit = true;
    select_thread_signal(platform);
    pthread_join(platform->select_thread, NULL);
    close(platform->signal_fd);
    if (platform->eventfd_registered) {
        if (UNLIKELY(esp_vfs_eventfd_unregister() != ESP_OK)) {
            fprintf(stderr, "Cannot unregister eventfd\n");
            AVM_ABORT();
        }
    }

    if (platform->random_is_initialized) {
        mbedtls_ctr_drbg_free(&platform->random_ctx);
    }

    if (platform->entropy_is_initialized) {
        mbedtls_entropy_free(&platform->entropy_ctx);
    }

    free(platform);
}

const void *esp32_sys_mmap_partition(const char *partition_name, spi_flash_mmap_handle_t *handle, int *size)
{
    const esp_partition_t *partition = esp_partition_find_first(ESP_PARTITION_TYPE_DATA,
        ESP_PARTITION_SUBTYPE_ANY, partition_name);
    if (!partition) {
        ESP_LOGW(TAG, "AVM partition not found for %s", partition_name);
        *size = 0;
        return NULL;
    } else {
        *size = partition->size;
    }

    const void *mapped_memory;
    if (esp_partition_mmap(partition, 0, partition->size, SPI_FLASH_MMAP_DATA, &mapped_memory,
            handle)
        != ESP_OK) {
        ESP_LOGE(TAG, "Failed to map BEAM partition for %s", partition_name);
        return NULL;
    }
    ESP_LOGI(TAG, "Loaded BEAM partition %s at address 0x%"PRIx32" (size=%"PRIu32" bytes)",
        partition_name, partition->address, partition->size);

    return mapped_memory;
}

struct ESP32PartAVMPack
{
    struct AVMPackData base;
    spi_flash_mmap_handle_t part_handle;
};

static void esp32_part_avm_pack_destructor(struct AVMPackData *obj, GlobalContext *global);

static const struct AVMPackInfo esp32_part_avm_pack_info = {
    .destructor = esp32_part_avm_pack_destructor
};

static void esp32_part_avm_pack_destructor(struct AVMPackData *obj, GlobalContext *global)
{
    UNUSED(global);

    struct ESP32PartAVMPack *part_avm = CONTAINER_OF(obj, struct ESP32PartAVMPack, base);
    spi_flash_munmap(part_avm->part_handle);
    free(obj);
}

enum OpenAVMResult sys_open_avm_from_file(GlobalContext *global, const char *path, struct AVMPackData **data)
{
    TRACE("sys_open_avm_from_file: Going to open: %s\n", path);

    const char *const parts_by_name = "/dev/partition/by-name/";
    int parts_by_name_len = strlen(parts_by_name);

    struct AVMPackData *avmpack_data = NULL;
    if (!strncmp(path, parts_by_name, parts_by_name_len)) {
        spi_flash_mmap_handle_t part_handle;
        int size;
        const char *part_name = path + parts_by_name_len;
        const void *part_data = esp32_sys_mmap_partition(part_name, &part_handle, &size);
        if (IS_NULL_PTR(part_data)) {
            return AVM_OPEN_CANNOT_OPEN;
        }
        if (UNLIKELY(!avmpack_is_valid(part_data, size))) {
            return AVM_OPEN_INVALID;
        }

        struct ESP32PartAVMPack *part_avm = malloc(sizeof(struct ESP32PartAVMPack));
        if (IS_NULL_PTR(part_avm)) {
            return AVM_OPEN_FAILED_ALLOC;
        }
        avmpack_data_init(&part_avm->base, &esp32_part_avm_pack_info);
        part_avm->base.data = part_data;
        part_avm->part_handle = part_handle;
        avmpack_data = &part_avm->base;

    } else {
        struct stat file_stats;
        if (UNLIKELY(stat(path, &file_stats) < 0)) {
            return AVM_OPEN_CANNOT_OPEN;
        }
        // check int max
        int size = file_stats.st_size;

        void *file_data = malloc(size);
        if (IS_NULL_PTR(file_data)) {
            return AVM_OPEN_FAILED_ALLOC;
        }

        FILE *avm_file = fopen(path, "r");
        if (UNLIKELY(!avm_file)) {
            free(file_data);
            return AVM_OPEN_CANNOT_OPEN;
        }

        int bytes_read = fread(file_data, 1, size, avm_file);
        fclose(avm_file);

        if (UNLIKELY(bytes_read != size)) {
            free(file_data);
            return AVM_OPEN_CANNOT_READ;
        }

        if (UNLIKELY(!avmpack_is_valid(file_data, size))) {
            free(file_data);
            return AVM_OPEN_INVALID;
        }

        struct InMemoryAVMPack *in_memory_avm = malloc(sizeof(struct InMemoryAVMPack));
        if (IS_NULL_PTR(avmpack_data)) {
            free(file_data);
            return AVM_OPEN_FAILED_ALLOC;
        }
        avmpack_data_init(&in_memory_avm->base, &in_memory_avm_pack_info);
        in_memory_avm->base.data = file_data;
        avmpack_data = &in_memory_avm->base;
    }

    *data = avmpack_data;
    return AVM_OPEN_OK;
}

Module *sys_load_module_from_file(GlobalContext *global, const char *path)
{
    // TODO
    return NULL;
}

Module *sys_load_module(GlobalContext *global, const char *module_name)
{
    const void *beam_module = NULL;
    uint32_t beam_module_size = 0;

    struct ListHead *item;
    struct ListHead *avmpack_data = synclist_rdlock(&global->avmpack_data);
    LIST_FOR_EACH (item, avmpack_data) {
        struct AVMPackData *avmpack_data = GET_LIST_ENTRY(item, struct AVMPackData, avmpack_head);
        avmpack_data->in_use = true;
        if (avmpack_find_section_by_name(avmpack_data->data, module_name, &beam_module, &beam_module_size)) {
            break;
        }
    }
    synclist_unlock(&global->avmpack_data);

    if (IS_NULL_PTR(beam_module)) {
        fprintf(stderr, "Failed to open module: %s\n", module_name);
        return NULL;
    }

    Module *new_module = module_new_from_iff_binary(global, beam_module, beam_module_size);
    new_module->module_platform_data = NULL;

    return new_module;
}

// This function allows to use AtomVM as a component on ESP32 and customize it
__attribute__((weak)) Context *sys_create_port_fallback(const char *driver_name, GlobalContext *global, term opts)
{
    UNUSED(global);
    UNUSED(opts);

    fprintf(stderr, "Failed to load port \"%s\".  Ensure the port is configured properly in the build.\n", driver_name);

    return NULL;
}

Context *sys_create_port(GlobalContext *glb, const char *port_name, term opts)
{
    Context *new_ctx = port_driver_create_port(port_name, glb, opts);
    if (IS_NULL_PTR(new_ctx)) {
        new_ctx = sys_create_port_fallback(port_name, glb, opts);
    }
    return new_ctx;
}

static term get_model(Context *ctx, esp_chip_model_t model)
{
#if ESP_IDF_VERSION >= ESP_IDF_VERSION_VAL(4, 3, 2)
    switch (model) {
        case CHIP_ESP32:
            return globalcontext_make_atom(ctx->global, esp32_atom);
        case CHIP_ESP32S2:
            return globalcontext_make_atom(ctx->global, esp32_s2_atom);
        case CHIP_ESP32S3:
            return globalcontext_make_atom(ctx->global, esp32_s3_atom);
        case CHIP_ESP32C3:
            return globalcontext_make_atom(ctx->global, esp32_c3_atom);
        default:
            return UNDEFINED_ATOM;
    }
#else
    return UNDEFINED_ATOM;
#endif
}

static term get_features(Context *ctx, uint32_t features)
{
    term ret = term_nil();
    GlobalContext *glb = ctx->global;

    if (features & CHIP_FEATURE_EMB_FLASH) {
        ret = term_list_prepend(globalcontext_make_atom(glb, emb_flash_atom), ret, &ctx->heap);
    }
    if (features & CHIP_FEATURE_WIFI_BGN) {
        ret = term_list_prepend(globalcontext_make_atom(glb, bgn_atom), ret, &ctx->heap);
    }
    if (features & CHIP_FEATURE_BLE) {
        ret = term_list_prepend(globalcontext_make_atom(glb, ble_atom), ret, &ctx->heap);
    }
    if (features & CHIP_FEATURE_BT) {
        ret = term_list_prepend(globalcontext_make_atom(glb, bt_atom), ret, &ctx->heap);
    }

    return ret;
}

term sys_get_info(Context *ctx, term key)
{
    GlobalContext *glb = ctx->global;
    if (key == globalcontext_make_atom(glb, esp_free_heap_size_atom)) {
        return term_from_int32(esp_get_free_heap_size());
    }
    if (key == globalcontext_make_atom(glb, esp_largest_free_block_atom)) {
        return term_from_int32(heap_caps_get_largest_free_block(MALLOC_CAP_DEFAULT));
    }
    if (key == globalcontext_make_atom(glb, esp_get_minimum_free_size_atom)) {
        return term_from_int32(esp_get_minimum_free_heap_size());
    }
    if (key == globalcontext_make_atom(glb, esp_chip_info_atom)) {
        esp_chip_info_t info;
        esp_chip_info(&info);
        if (memory_ensure_free(ctx, term_map_size_in_terms(4) + 4 * 2) != MEMORY_GC_OK) {
            return OUT_OF_MEMORY_ATOM;
        }
        term ret = term_alloc_map(4, &ctx->heap);
        term_set_map_assoc(ret, 0, globalcontext_make_atom(glb, cores_atom), term_from_int32(info.cores));
        term_set_map_assoc(ret, 1, globalcontext_make_atom(glb, features_atom), get_features(ctx, info.features));
        term_set_map_assoc(ret, 2, globalcontext_make_atom(glb, model_atom), get_model(ctx, info.model));
        term_set_map_assoc(ret, 3, globalcontext_make_atom(glb, revision_atom), term_from_int32(info.revision));
        return ret;
    }
    if (key == globalcontext_make_atom(glb, esp_idf_version_atom)) {
        const char *str = esp_get_idf_version();
        size_t n = strlen(str);
        if (memory_ensure_free(ctx, 2 * n) != MEMORY_GC_OK) {
            return OUT_OF_MEMORY_ATOM;
        }
        return term_from_string((const uint8_t *) str, n, &ctx->heap);
    }
    return UNDEFINED_ATOM;
}

void port_driver_init_all(GlobalContext *global)
{
    for (struct PortDriverDefListItem *item = port_driver_list; item != NULL; item = item->next) {
        if (item->def->port_driver_init_cb) {
            item->def->port_driver_init_cb(global);
        }
    }
}

void port_driver_destroy_all(GlobalContext *global)
{
    for (struct PortDriverDefListItem *item = port_driver_list; item != NULL; item = item->next) {
        if (item->def->port_driver_destroy_cb) {
            item->def->port_driver_destroy_cb(global);
        }
    }
}

static Context *port_driver_create_port(const char *port_name, GlobalContext *global, term opts)
{
    for (struct PortDriverDefListItem *item = port_driver_list; item != NULL; item = item->next) {
        if (strcmp(port_name, item->def->port_driver_name) == 0) {
            return item->def->port_driver_create_port_cb(global, opts);
        }
    }

    return NULL;
}

void nif_collection_init_all(GlobalContext *global)
{
    for (struct NifCollectionDefListItem *item = nif_collection_list; item != NULL; item = item->next) {
        if (item->def->nif_collection_init_cb) {
            item->def->nif_collection_init_cb(global);
        }
    }
}

void nif_collection_destroy_all(GlobalContext *global)
{
    for (struct NifCollectionDefListItem *item = nif_collection_list; item != NULL; item = item->next) {
        if (item->def->nif_collection_destroy_cb) {
            item->def->nif_collection_destroy_cb(global);
        }
    }
}

const struct Nif *nif_collection_resolve_nif(const char *name)
{
    for (struct NifCollectionDefListItem *item = nif_collection_list; item != NULL; item = item->next) {
        const struct Nif *res = item->def->nif_collection_resolve_nif_cb(name);
        if (res) {
            return res;
        }
    }

    return NULL;
}

void event_listener_add_to_polling_set(struct EventListener *listener, GlobalContext *global)
{
    // Listener_event is not necessarily a queue to be added to event_set as
    // drivers may use event_queue instead.
    UNUSED(global);
    UNUSED(listener);
}

void sys_register_listener(GlobalContext *global, struct EventListener *listener)
{
    struct ListHead *listeners = synclist_wrlock(&global->listeners);
#ifndef AVM_NO_SMP
    sys_signal(global);
#endif
    list_append(listeners, &listener->listeners_list_head);
    synclist_unlock(&global->listeners);
}

static void listener_event_remove_from_polling_set(listener_event_t listener_event, GlobalContext *global)
{
    // Listener_event is not necessarily a queue to be added to event_set as
    // drivers may use event_queue instead.
    UNUSED(global);
    UNUSED(listener_event);
}

void sys_unregister_listener(GlobalContext *global, struct EventListener *listener)
{
    struct ListHead *dummy = synclist_wrlock(&global->listeners);
    UNUSED(dummy);
#ifndef AVM_NO_SMP
    sys_signal(global);
#endif
    list_remove(&listener->listeners_list_head);
    synclist_unlock(&global->listeners);
}

void sys_register_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    UNUSED(event);
    UNUSED(is_write);

    struct ESP32PlatformData *platform = global->platform_data;
    platform->select_events_poll_count = -1;
    select_thread_signal(platform);
}

void sys_unregister_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    UNUSED(event);
    UNUSED(is_write);

    struct ESP32PlatformData *platform = global->platform_data;
    platform->select_events_poll_count = -1;
    select_thread_signal(platform);
}

static void *select_thread_loop(void *arg)
{
    GlobalContext *glb = arg;
    struct ESP32PlatformData *platform = glb->platform_data;
    struct pollfd *fds = malloc(0);
    while (!platform->select_thread_exit) {
        int select_events_poll_count = platform->select_events_poll_count;
        int poll_count = 1;
        int fd_index;
        if (select_events_poll_count < 0) {
            // Means it is dirty and should be rebuilt.
            struct ListHead *select_events = synclist_wrlock(&glb->select_events);
            size_t select_events_new_count;
            if (select_events_poll_count < 0) {
                select_event_count_and_destroy_closed(select_events, NULL, NULL, &select_events_new_count, glb);
            } else {
                select_events_new_count = select_events_poll_count;
            }

            fds = realloc(fds, sizeof(struct pollfd) * (poll_count + select_events_new_count));

            fds[0].fd = platform->signal_fd;
            fds[0].events = POLLIN;
            fds[0].revents = 0;

            fd_index = poll_count;

            struct ListHead *item;
            LIST_FOR_EACH (item, select_events) {
                struct SelectEvent *select_event = GET_LIST_ENTRY(item, struct SelectEvent, head);
                if (select_event->read || select_event->write) {
                    fds[fd_index].fd = select_event->event;
                    fds[fd_index].events = (select_event->read ? POLLIN : 0) | (select_event->write ? POLLOUT : 0);
                    fds[fd_index].revents = 0;

                    fd_index++;
                }
            }
            synclist_unlock(&glb->select_events);

            select_events_poll_count = select_events_new_count;
            platform->select_events_poll_count = select_events_new_count;
        }

        poll_count += select_events_poll_count;

        int nb_descriptors = poll(fds, poll_count, -1);
        fd_index = 0;
        if (nb_descriptors > 0) {
            if ((fds[0].revents & fds[0].events)) {
                // We've been signaled
                uint64_t ignored;
                if (UNLIKELY(read(platform->signal_fd, &ignored, sizeof(ignored)) < 0)) {
                    fprintf(stderr, "Reading event_fd failed -- errno = %d\n", errno);
                    AVM_ABORT();
                }
                nb_descriptors--;
            }
            fd_index++;
        }

        for (int i = 0; i < select_events_poll_count && nb_descriptors > 0; i++, fd_index++) {
            if (!(fds[fd_index].revents & fds[fd_index].events)) {
                continue;
            }
            bool is_read = fds[fd_index].revents & POLLIN;
            bool is_write = fds[fd_index].revents & POLLOUT;
            fds[fd_index].revents = 0;
            nb_descriptors--;

            select_event_notify(fds[fd_index].fd, is_read, is_write, glb);
        }
    }

    free((void *) fds);

    return NULL;
}

static void select_thread_signal(struct ESP32PlatformData *platform)
{
    // Write can fail if the counter overflows
    // (very unlikely, 2^64)
    uint64_t val = 1;
    if (UNLIKELY(write(platform->signal_fd, &val, sizeof(val)) < 0)) {
        uint64_t ignored;
        if (UNLIKELY(read(platform->signal_fd, &ignored, sizeof(ignored)) < 0)) {
            fprintf(stderr, "Reading event_fd failed\n");
            AVM_ABORT();
        }
        if (UNLIKELY(write(platform->signal_fd, &val, sizeof(val)) < 0)) {
            fprintf(stderr, "Writing event_fd failed\n");
            AVM_ABORT();
        }
    }
}

bool event_listener_is_event(EventListener *listener, listener_event_t event)
{
    return listener->sender == event;
}

term esp_err_to_term(GlobalContext *glb, esp_err_t status)
{
    switch (status) {
        case ESP_FAIL:
            return globalcontext_make_atom(glb, ATOM_STR("\x8", "esp_fail"));
        case ESP_ERR_NO_MEM:
            return globalcontext_make_atom(glb, ATOM_STR("\xE", "esp_err_no_mem"));
        case ESP_ERR_INVALID_ARG:
            return globalcontext_make_atom(glb, ATOM_STR("\x13", "esp_err_invalid_arg"));
        case ESP_ERR_INVALID_STATE:
            return globalcontext_make_atom(glb, ATOM_STR("\x15", "esp_err_invalid_state"));
        case ESP_ERR_INVALID_SIZE:
            return globalcontext_make_atom(glb, ATOM_STR("\x14", "esp_err_invalid_size"));
        case ESP_ERR_NOT_FOUND:
            return globalcontext_make_atom(glb, ATOM_STR("\x11", "esp_err_not_found"));
        case ESP_ERR_NOT_SUPPORTED:
            return globalcontext_make_atom(glb, ATOM_STR("\x15", "esp_err_not_supported"));
        case ESP_ERR_TIMEOUT:
            return globalcontext_make_atom(glb, ATOM_STR("\xF", "esp_err_timeout"));
        case ESP_ERR_INVALID_RESPONSE:
            return globalcontext_make_atom(glb, ATOM_STR("\x18", "esp_err_invalid_response"));
        case ESP_ERR_INVALID_CRC:
            return globalcontext_make_atom(glb, ATOM_STR("\x13", "esp_err_invalid_crc"));
        case ESP_ERR_INVALID_VERSION:
            return globalcontext_make_atom(glb, ATOM_STR("\x17", "esp_err_invalid_version"));
        case ESP_ERR_INVALID_MAC:
            return globalcontext_make_atom(glb, ATOM_STR("\x13", "esp_err_invalid_mac"));
        default:
            return term_from_int(status);
    }
}

int sys_mbedtls_entropy_func(void *entropy, unsigned char *buf, size_t size)
{
#ifndef MBEDTLS_THREADING_C
    struct ESP32PlatformData *platform
        = CONTAINER_OF(entropy, struct ESP32PlatformData, entropy_ctx);
    SMP_MUTEX_LOCK(platform->entropy_mutex);
    int result = mbedtls_entropy_func(entropy, buf, size);
    SMP_MUTEX_UNLOCK(platform->entropy_mutex);

    return result;
#else
    return mbedtls_entropy_func(entropy, buf, size);
#endif
}

mbedtls_entropy_context *sys_mbedtls_get_entropy_context_lock(GlobalContext *global)
{
    struct ESP32PlatformData *platform = global->platform_data;

    SMP_MUTEX_LOCK(platform->entropy_mutex);

    if (!platform->entropy_is_initialized) {
        // mbedtls_entropy_init will gather entropy for us using esp_fill_random()
        // however it will be pseudorandom when wifi/bluetooth/RF is not enabled.
        //
        // TODO: when radio is not active bootloader_random_enable() must be enabled for gathering
        // entropy. However, "enable function is not safe to use if any other subsystem is
        // accessing the RF subsystem or the ADC at the same time".
        mbedtls_entropy_init(&platform->entropy_ctx);
        platform->entropy_is_initialized = true;
    }

    return &platform->entropy_ctx;
}

void sys_mbedtls_entropy_context_unlock(GlobalContext *global)
{
    struct ESP32PlatformData *platform = global->platform_data;
    SMP_MUTEX_UNLOCK(platform->entropy_mutex);
}

mbedtls_ctr_drbg_context *sys_mbedtls_get_ctr_drbg_context_lock(GlobalContext *global)
{
    struct ESP32PlatformData *platform = global->platform_data;

    SMP_MUTEX_LOCK(platform->random_mutex);

    if (!platform->random_is_initialized) {
        mbedtls_ctr_drbg_init(&platform->random_ctx);

        mbedtls_entropy_context *entropy_ctx = sys_mbedtls_get_entropy_context_lock(global);
        // Safe to unlock it now, sys_mbedtls_entropy_func will lock it again later
        sys_mbedtls_entropy_context_unlock(global);

        const char *seed = "AtomVM ESP32 Mbed-TLS initial seed.";
        int seed_len = strlen(seed);
        int seed_err = mbedtls_ctr_drbg_seed(&platform->random_ctx, sys_mbedtls_entropy_func,
            entropy_ctx, (const unsigned char *) seed, seed_len);
        if (UNLIKELY(seed_err != 0)) {
            abort();
        }
        platform->random_is_initialized = true;
    }

    return &platform->random_ctx;
}

void sys_mbedtls_ctr_drbg_context_unlock(GlobalContext *global)
{
    struct ESP32PlatformData *platform = global->platform_data;
    SMP_MUTEX_UNLOCK(platform->random_mutex);
}
