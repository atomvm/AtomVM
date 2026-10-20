/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. <petermm@gmail.com>
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

/*
 * ESP32-C3 BLE controller bring-up.
 *
 * The controller is the Espressif binary libbtdm_app_flash.a, the same blob
 * ESP-IDF uses with CONFIG_BT_CTRL_RUN_IN_FLASH_ONLY. It expects an operating
 * system adapter whose layout is fixed by OSI_VERSION; the fields below are
 * ESP-IDF's esp32c3/bt.c order. Sleep, MAC/BB power-down and BLE logging are
 * off, so their hooks are no-ops and do not need the power-management code.
 *
 * HCI goes through the controller's VHCI transport. The smoke task below sends
 * the standard GAP advertising sequence so a phone can see the board before a
 * host stack exists.
 */

#include "rtems_bt.h"

#include <rtems.h>
#include <rtems/bspIo.h>
#include <rtems/counter.h>
#include <rtems/rtems/intr.h>
#include <rtems/rtems/message.h>
#include <rtems/rtems/sem.h>
#include <rtems/rtems/semimpl.h>
#include <rtems/rtems/tasks.h>
#include <rtems/score/coresemimpl.h>
#include <rtems/score/threaddispatch.h>

#include <bsp/irq-generic.h>
#include <bsp/irq.h>

#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>

#ifdef RTEMS_HAS_WIFI
#include <esp_mac.h>
#include <esp_phy_init.h>
#endif

/* Controller entry points, from libbtdm_app_flash.a. */
extern int btdm_osi_funcs_register(void *osi_funcs);
extern int btdm_controller_init(void *config);
extern int btdm_controller_enable(uint8_t mode);
extern void btdm_controller_disable(void);
extern void btdm_controller_rom_data_init(void);
extern void btdm_controller_deinit(void);
extern void ble_min_conn_interval_enable(uint16_t interval);
extern bool btdm_lpclk_select_src(uint32_t source);
extern bool btdm_lpclk_set_div(uint32_t divider);
extern void sdk_config_extend_set_pll_track(bool enable);
extern bool API_vhci_host_check_send_available(void);
extern void API_vhci_host_send_packet(uint8_t *data, uint16_t len);
extern int API_vhci_host_register_callback(const void *callback);

/* Software coexistence, from libcoexist.a. */
extern int coex_enable(void);
extern int coex_init(void);
extern void coex_update_lpclk_interval(void);
extern void coex_disable(void);
extern int coex_schm_register_callback(int type, int (*cb)(int));
extern void coex_schm_status_bit_set(uint32_t type, uint32_t status);
extern void coex_schm_status_bit_clear(uint32_t type, uint32_t status);
extern uint32_t coex_schm_interval_get(void);
extern uint8_t coex_schm_curr_period_get(void);
extern void *coex_schm_curr_phase_get(void);
extern void coex_pti_v2(void);
extern void rtems_esp_bt_power_domain_on(void);
extern void rtems_esp_bt_periph_enable(void);
extern int rtems_esp_modem_prepare(void);
extern void rtems_esp_wifi_phy_track(void);

#define BT_OSI_MAGIC_VALUE 0xFADEBEADu
#define BT_OSI_VERSION 0x0001000Bu
#define BT_CONFIG_MAGIC_VALUE 0x5A5AA5A5u
#define BT_CONFIG_VERSION 0x02509280u
#define BT_HW_TARGET_CODE_C3_ECO0 0x01010000u
#define BT_OSI_FUNCS_TIME_BLOCKING 0xFFFFFFFFu
#define BT_MODE_BLE 1u
#define BT_SLEEP_NONE 0u
#define BT_SLEEP_CLOCK_NONE 0u
#define BT_VHCI 1u
#define BT_COEX_SCHM_CALLBACK_TYPE_BT 1
#define BT_PRIORITY_MAX 25
#define BT_EVENT_RING 4
#define BT_EVENT_MAX 260
#define BT_LPCYCLE_FRAC 19u
#define BT_ADV_INTERVAL_MS 1280
#define BT_ADV_EVENT_TIMEOUT_US 5000000
#define BT_ADV_COUNTER_OFFSET 27u

typedef void (*bt_intr_handler_t)(void *);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
typedef struct
{
    uint32_t magic;
    uint32_t version;
    uint16_t controller_task_stack_size;
    uint8_t controller_task_prio;
    uint8_t controller_task_run_cpu;
    uint8_t bluetooth_mode;
    uint8_t ble_max_act;
    uint8_t sleep_mode;
    uint8_t sleep_clock;
    uint8_t ble_st_acl_tx_buf_nb;
    uint8_t ble_hw_cca_check;
    uint16_t ble_adv_dup_filt_max;
    bool coex_param_en;
    uint8_t ce_len_type;
    bool coex_use_hooks;
    uint8_t hci_tl_type;
    void *hci_tl_funcs;
    uint8_t txant_dft;
    uint8_t rxant_dft;
    uint8_t txpwr_dft;
    uint32_t cfg_mask;
    uint8_t scan_duplicate_mode;
    uint8_t scan_duplicate_type;
    uint16_t normal_adv_size;
    uint16_t mesh_adv_size;
    uint8_t coex_phy_coded_tx_rx_time_limit;
    uint32_t hw_target_code;
    uint8_t slave_ce_len_min;
    uint8_t hw_recorrect_en;
    uint8_t cca_thresh;
    uint16_t scan_backoff_upperlimitmax;
    uint16_t dup_list_refresh_period;
    bool ble_50_feat_supp;
    uint8_t ble_cca_mode;
    uint8_t ble_data_lenth_zero_aux;
    uint8_t ble_chan_ass_en;
    uint8_t ble_ping_en;
    uint8_t ble_llcp_disc_flag;
    bool run_in_flash;
    bool dtm_en;
    bool enc_en;
    bool qa_test;
    bool connect_en;
    bool scan_en;
    bool ble_aa_check;
    bool adv_en;
} bt_controller_config_t;

typedef struct
{
    uint32_t magic;
    uint32_t version;
    int (*interrupt_alloc)(int cpu_id, int source, bt_intr_handler_t handler, void *arg, void **ret_handle);
    int (*interrupt_free)(void *handle);
    void (*interrupt_handler_set_rsv)(int interrupt_no, bt_intr_handler_t fn, void *arg);
    void (*global_intr_disable)(void);
    void (*global_intr_restore)(void);
    void (*task_yield)(void);
    void (*task_yield_from_isr)(void);
    void *(*semphr_create)(uint32_t max, uint32_t init);
    void (*semphr_delete)(void *semphr);
    int (*semphr_take_from_isr)(void *semphr, void *hptw);
    int (*semphr_give_from_isr)(void *semphr, void *hptw);
    int (*semphr_take)(void *semphr, uint32_t block_time_ms);
    int (*semphr_give)(void *semphr);
    void *(*mutex_create)(void);
    void (*mutex_delete)(void *mutex);
    int (*mutex_lock)(void *mutex);
    int (*mutex_unlock)(void *mutex);
    void *(*queue_create)(uint32_t queue_len, uint32_t item_size);
    void (*queue_delete)(void *queue);
    int (*queue_send)(void *queue, void *item, uint32_t block_time_ms);
    int (*queue_send_from_isr)(void *queue, void *item, void *hptw);
    int (*queue_recv)(void *queue, void *item, uint32_t block_time_ms);
    int (*queue_recv_from_isr)(void *queue, void *item, void *hptw);
    int (*task_create)(void (*task_func)(void *), const char *name, uint32_t stack_depth, void *param, uint32_t prio, void *task_handle, uint32_t core_id);
    void (*task_delete)(void *task_handle);
    bool (*is_in_isr)(void);
    int (*cause_sw_intr_to_core)(int core_id, int intr_no);
    void *(*malloc_fn)(size_t size);
    void *(*malloc_internal)(size_t size);
    void (*free_fn)(void *p);
    int (*read_efuse_mac)(uint8_t mac[6]);
    void (*srand_fn)(unsigned int seed);
    int (*rand_fn)(void);
    uint32_t (*btdm_lpcycles_2_hus)(uint32_t cycles, uint32_t *error_corr);
    uint32_t (*btdm_hus_2_lpcycles)(uint32_t hus);
    bool (*btdm_sleep_check_duration)(int32_t *slot_cnt);
    void (*btdm_sleep_enter_phase1)(uint32_t lpcycles);
    void (*btdm_sleep_enter_phase2)(void);
    void (*btdm_sleep_exit_phase1)(void);
    void (*btdm_sleep_exit_phase2)(void);
    void (*btdm_sleep_exit_phase3)(void);
    void (*coex_wifi_sleep_set)(bool sleep);
    int (*coex_core_ble_conn_dyn_prio_get)(bool *low, bool *high);
    int (*coex_schm_register_btdm_callback)(int (*callback)(int));
    void (*coex_schm_status_bit_set)(uint32_t type, uint32_t status);
    void (*coex_schm_status_bit_clear)(uint32_t type, uint32_t status);
    uint32_t (*coex_schm_interval_get)(void);
    uint8_t (*coex_schm_curr_period_get)(void);
    void *(*coex_schm_curr_phase_get)(void);
    int (*interrupt_enable)(void *handle);
    int (*interrupt_disable)(void *handle);
    void (*esp_hw_power_down)(void);
    void (*esp_hw_power_up)(void);
    void (*ets_backup_dma_copy)(uint32_t reg, uint32_t mem_addr, uint32_t num, bool to_rem);
    void *(*malloc_retention)(size_t size);
    void (*ets_delay_us)(uint32_t us);
    void (*btdm_rom_table_ready)(void);
    bool (*coex_bt_wakeup_request)(void);
    void (*coex_bt_wakeup_request_end)(void);
    int64_t (*get_time_us)(void);
    void (*assert_fn)(void);
} bt_osi_funcs_t;
#pragma GCC diagnostic pop

typedef struct
{
    void (*notify_host_send_available)(void);
    int (*notify_host_recv)(uint8_t *data, uint16_t len);
} bt_vhci_callback_t;

typedef struct
{
    rtems_id id;
    void *spaces;
    size_t item_size;
} bt_queue_t;

typedef struct
{
    rtems_id id;
    uint32_t max;
} bt_semaphore_t;

typedef struct
{
    void (*func)(void *);
    void *arg;
} bt_task_t;

static bt_controller_config_t bt_config = {
    .magic = BT_CONFIG_MAGIC_VALUE,
    .version = BT_CONFIG_VERSION,
    .controller_task_stack_size = 4096,
    .controller_task_prio = 23,
    .controller_task_run_cpu = 0,
    .bluetooth_mode = BT_MODE_BLE,
    /* Extended HCI commands count individual events; PDUs remain legacy. */
    .ble_max_act = 1,
    .sleep_mode = BT_SLEEP_NONE,
    .sleep_clock = BT_SLEEP_CLOCK_NONE,
    .ble_st_acl_tx_buf_nb = 0,
    .ble_hw_cca_check = 0,
    .ble_adv_dup_filt_max = 30,
    .coex_param_en = false,
    .ce_len_type = 0,
    .coex_use_hooks = false,
    .hci_tl_type = BT_VHCI,
    .hci_tl_funcs = NULL,
    .txant_dft = 0,
    .rxant_dft = 0,
    .txpwr_dft = 11,
    .cfg_mask = 1,
    .scan_duplicate_mode = 0,
    .scan_duplicate_type = 0,
    /* IDF's configurable scan cache, reduced for the C3's shared heap. */
    .normal_adv_size = 20,
    .mesh_adv_size = 0,
    .coex_phy_coded_tx_rx_time_limit = 0,
    .hw_target_code = BT_HW_TARGET_CODE_C3_ECO0,
    .slave_ce_len_min = 5,
    .hw_recorrect_en = 0,
    .cca_thresh = 75,
    .scan_backoff_upperlimitmax = 0,
    .dup_list_refresh_period = 0,
    .ble_50_feat_supp = true,
    .ble_cca_mode = 0,
    .ble_data_lenth_zero_aux = 0,
    .ble_chan_ass_en = 1,
    .ble_ping_en = 1,
    .ble_llcp_disc_flag = 0,
    .run_in_flash = true,
    .dtm_en = false,
    .enc_en = false,
    .qa_test = false,
    .connect_en = false,
    .scan_en = false,
    .ble_aa_check = false,
    .adv_en = true,
};

static _Atomic(rtems_bt_state_t) bt_state = RTEMS_BT_STOPPED;
static atomic_bool bt_alloc_failed;
static rtems_id bt_event_sem = RTEMS_INVALID_ID;
static rtems_interrupt_level bt_int_level;
static Per_CPU_Control *bt_int_cpu;
static int bt_int_nest;
static uint32_t bt_lpcycle_us;

static struct
{
    uint16_t len;
    uint8_t data[BT_EVENT_MAX];
} bt_events[BT_EVENT_RING];
static volatile unsigned bt_event_head;
static volatile unsigned bt_event_tail;

_Static_assert(sizeof(bt_controller_config_t) == 72, "BT controller config ABI");
_Static_assert(sizeof(bt_osi_funcs_t) == 252, "BT OS adapter ABI");

/* === memory, time and randomness ======================================= */

static void *bt_malloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL) {
        atomic_store(&bt_alloc_failed, true);
        printk("bt: allocation failed (%u bytes)\n", (unsigned) size);
    }
    return p;
}

static void bt_free(void *p)
{
    free(p);
}

static void bt_delay_us(uint32_t us)
{
    /* The RTEMS nanosecond conversion itself takes uint32_t. Chunk the
     * delay so neither conversion nor the hardware counter can overflow. */
    while (us != 0) {
        uint32_t chunk = us > 1000000u ? 1000000u : us;
        rtems_counter_ticks start = rtems_counter_read();
        rtems_counter_ticks want = rtems_counter_nanoseconds_to_ticks(chunk * 1000u);
        while (rtems_counter_read() - start < want) {
            /* wait */
        }
        us -= chunk;
    }
}

static int64_t bt_get_time_us(void)
{
    return (int64_t) (rtems_clock_get_uptime_nanoseconds() / 1000u);
}

static void bt_srand(unsigned int seed)
{
    (void) seed;
}

static int bt_rand(void)
{
    extern uint32_t esp_random(void);
    return (int) esp_random();
}

static int bt_read_mac(uint8_t mac[6])
{
    return esp_read_mac(mac, ESP_MAC_BT);
}

/* === interrupt locks =================================================== */

static void bt_global_intr_disable(void)
{
    rtems_interrupt_level level;

    rtems_interrupt_local_disable(level);
    if (bt_int_nest++ == 0) {
        bt_int_level = level;
        /* Like FreeRTOS critical sections, defer task switches until exit.
         * Queue/semaphore wakeups must not dispatch with interrupts masked. */
        bt_int_cpu = _Thread_Dispatch_disable();
    }
}

static void bt_global_intr_restore(void)
{
    if (--bt_int_nest == 0) {
        rtems_interrupt_local_enable(bt_int_level);
        _Thread_Dispatch_enable(bt_int_cpu);
    }
}

static bool bt_is_in_isr(void)
{
    return rtems_interrupt_is_in_progress();
}

/* === semaphores and mutexes ============================================ */

static void *bt_semphr_create(uint32_t max, uint32_t init)
{
    if (max == 0 || init > max) {
        return NULL;
    }
    bt_semaphore_t *sem = malloc(sizeof(*sem));
    if (sem == NULL) {
        return NULL;
    }
    sem->max = max;
    if (rtems_semaphore_create(
            rtems_build_name('B', 'T', 'S', 'M'), init,
            RTEMS_COUNTING_SEMAPHORE | RTEMS_PRIORITY, 0, &sem->id)
        != RTEMS_SUCCESSFUL) {
        free(sem);
        return NULL;
    }
    return sem;
}

static void bt_semphr_delete(void *semphr)
{
    bt_semaphore_t *sem = semphr;
    if (sem != NULL) {
        rtems_semaphore_delete(sem->id);
        free(sem);
    }
}

static int bt_semphr_take(void *semphr, uint32_t block_time_ms)
{
    rtems_option option;
    rtems_interval timeout;

    if (semphr == NULL) {
        return 0;
    }
    if (block_time_ms == BT_OSI_FUNCS_TIME_BLOCKING) {
        option = RTEMS_WAIT;
        timeout = RTEMS_NO_TIMEOUT;
    } else if (block_time_ms == 0) {
        option = RTEMS_NO_WAIT;
        timeout = 0;
    } else {
        option = RTEMS_WAIT;
        timeout = RTEMS_MILLISECONDS_TO_TICKS(block_time_ms);
    }

    return rtems_semaphore_obtain(
               ((bt_semaphore_t *) semphr)->id, option, timeout)
            == RTEMS_SUCCESSFUL
        ? 1
        : 0;
}

static int bt_semphr_give(void *semphr)
{
    bt_semaphore_t *sem = semphr;
    if (sem == NULL) {
        return 0;
    }
    /* Use the same RTEMS surrender path as rtems_semaphore_release(), with
     * the caller's ceiling instead of UINT32_MAX. This also handles waiters
     * atomically and is safe from an ISR. */
    Thread_queue_Context context;
    Semaphore_Control *native = _Semaphore_Get(sem->id, &context);
    if (native == NULL) {
        return 0;
    }
    return _CORE_semaphore_Surrender(
               &native->Core_control.Semaphore,
               _Semaphore_Get_operations(_Semaphore_Get_flags(native)),
               sem->max, &context)
        == STATUS_SUCCESSFUL;
}

static int bt_semphr_take_from_isr(void *semphr, void *hptw)
{
    if (hptw != NULL) {
        *(int *) hptw = 0;
    }

    return bt_semphr_take(semphr, 0);
}

static int bt_semphr_give_from_isr(void *semphr, void *hptw)
{
    if (hptw != NULL) {
        *(int *) hptw = 0;
    }

    return bt_semphr_give(semphr);
}

static void *bt_mutex_create(void)
{
    rtems_id id = RTEMS_INVALID_ID;

    if (rtems_semaphore_create(
            rtems_build_name('B', 'T', 'M', 'X'),
            1,
            RTEMS_BINARY_SEMAPHORE | RTEMS_PRIORITY | RTEMS_INHERIT_PRIORITY,
            0,
            &id)
        != RTEMS_SUCCESSFUL) {
        return NULL;
    }

    return (void *) (uintptr_t) id;
}

static void bt_mutex_delete(void *mutex)
{
    if (mutex != NULL) {
        rtems_semaphore_delete((rtems_id) (uintptr_t) mutex);
    }
}

static int bt_mutex_lock(void *mutex)
{
    return rtems_semaphore_obtain((rtems_id) (uintptr_t) mutex,
               RTEMS_WAIT, RTEMS_NO_TIMEOUT)
        == RTEMS_SUCCESSFUL;
}

static int bt_mutex_unlock(void *mutex)
{
    return rtems_semaphore_release((rtems_id) (uintptr_t) mutex) == RTEMS_SUCCESSFUL;
}

/* === queues ============================================================ */

static void *bt_queue_create(uint32_t queue_len, uint32_t item_size)
{
    bt_queue_t *q = malloc(sizeof(*q));

    if (q == NULL) {
        return NULL;
    }

    q->spaces = bt_semphr_create(queue_len, queue_len);
    if (q->spaces == NULL) {
        free(q);
        return NULL;
    }
    q->item_size = item_size;
    if (rtems_message_queue_create(
            rtems_build_name('B', 'T', 'Q', 'U'),
            queue_len,
            item_size,
            RTEMS_PRIORITY | RTEMS_LOCAL,
            &q->id)
        != RTEMS_SUCCESSFUL) {
        bt_semphr_delete(q->spaces);
        free(q);

        return NULL;
    }

    return q;
}

static void bt_queue_delete(void *queue)
{
    bt_queue_t *q = queue;

    if (q != NULL) {
        rtems_message_queue_delete(q->id);
        bt_semphr_delete(q->spaces);
        free(q);
    }
}

static int bt_queue_send(void *queue, void *item, uint32_t block_time_ms)
{
    bt_queue_t *q = queue;
    if (q == NULL || !bt_semphr_take(q->spaces, block_time_ms)) {
        return 0;
    }
    if (rtems_message_queue_send(q->id, item, q->item_size) != RTEMS_SUCCESSFUL) {
        bt_semphr_give(q->spaces);
        return 0;
    }
    return 1;
}

static int bt_queue_send_from_isr(void *queue, void *item, void *hptw)
{
    if (hptw != NULL) {
        *(int *) hptw = 0;
    }

    return bt_queue_send(queue, item, 0);
}

static int bt_queue_recv(void *queue, void *item, uint32_t block_time_ms)
{
    bt_queue_t *q = queue;
    size_t received = 0;
    rtems_option option;
    rtems_interval timeout;
    rtems_status_code sc;

    if (q == NULL) {
        return 0;
    }
    if (block_time_ms == BT_OSI_FUNCS_TIME_BLOCKING) {
        option = RTEMS_WAIT;
        timeout = RTEMS_NO_TIMEOUT;
    } else if (block_time_ms == 0) {
        option = RTEMS_NO_WAIT;
        timeout = 0;
    } else {
        option = RTEMS_WAIT;
        timeout = RTEMS_MILLISECONDS_TO_TICKS(block_time_ms);
    }

    sc = rtems_message_queue_receive(q->id, item, &received, option, timeout);

    if (sc == RTEMS_SUCCESSFUL) {
        bt_semphr_give(q->spaces);
        return 1;
    }
    return 0;
}

static int bt_queue_recv_from_isr(void *queue, void *item, void *hptw)
{
    if (hptw != NULL) {
        *(int *) hptw = 0;
    }

    return bt_queue_recv(queue, item, 0);
}

/* === tasks ============================================================= */

static void bt_task_entry(rtems_task_argument arg)
{
    bt_task_t *task = (bt_task_t *) (uintptr_t) arg;

    task->func(task->arg);
    free(task);
    rtems_task_delete(RTEMS_SELF);
}

static int bt_task_create(
    void (*task_func)(void *),
    const char *name,
    uint32_t stack_depth,
    void *param,
    uint32_t prio,
    void *task_handle,
    uint32_t core_id)
{
    rtems_id id = RTEMS_INVALID_ID;
    bt_task_t *task = malloc(sizeof(*task));
    rtems_task_priority rtems_prio = prio >= BT_PRIORITY_MAX ? 1 : BT_PRIORITY_MAX - prio;

    (void) name;
    (void) core_id;

    if (task == NULL) {
        return 0;
    }
    task->func = task_func;
    task->arg = param;

    if (rtems_task_create(
            rtems_build_name('B', 'T', 'T', 'K'),
            rtems_prio,
            stack_depth, /* ESP-IDF FreeRTOS uses bytes, not vanilla words. */
            RTEMS_DEFAULT_MODES,
            RTEMS_FLOATING_POINT | RTEMS_LOCAL,
            &id)
        != RTEMS_SUCCESSFUL) {
        printk("bt: cannot create controller task\n");
        free(task);

        return 0;
    }

    if (task_handle != NULL) {
        *(rtems_id *) task_handle = id;
    }

    if (rtems_task_start(id, bt_task_entry, (rtems_task_argument) task)
        != RTEMS_SUCCESSFUL) {
        rtems_task_delete(id);
        free(task);

        return 0;
    }

    return 1;
}

static void bt_task_delete(void *task_handle)
{
    if (task_handle != NULL) {
        rtems_task_delete((rtems_id) (uintptr_t) task_handle);
    }
}

static void bt_task_yield(void)
{
    rtems_task_wake_after(RTEMS_YIELD_PROCESSOR);
}

static void bt_task_yield_from_isr(void)
{
    /* RTEMS dispatches at the end of the outermost interrupt. */
}

/* === controller interrupts ============================================= */

static bt_intr_handler_t bt_intr_handlers[BSP_INTERRUPT_VECTOR_COUNT];
static void *bt_intr_args[BSP_INTERRUPT_VECTOR_COUNT];

static int bt_interrupt_install(
    int source,
    bt_intr_handler_t handler,
    void *arg)
{
    rtems_status_code sc;

    if (source < 0 || source >= BSP_INTERRUPT_VECTOR_COUNT) {
        return -1;
    }

    sc = rtems_interrupt_handler_install(
        (rtems_vector_number) source,
        "bt",
        RTEMS_INTERRUPT_SHARED,
        handler,
        arg);
    if (sc != RTEMS_SUCCESSFUL) {
        printk("bt: cannot install interrupt source %d (%i)\n", source, (int) sc);

        return -1;
    }

    bt_intr_handlers[source] = handler;
    bt_intr_args[source] = arg;
    /* ESP-IDF uses hardware level 3; RTEMS priorities are zero-based. */
    (void) bsp_interrupt_set_priority((rtems_vector_number) source, 2);
    (void) rtems_interrupt_vector_enable((rtems_vector_number) source);

    return 0;
}

static int bt_interrupt_alloc(
    int cpu_id,
    int source,
    bt_intr_handler_t handler,
    void *arg,
    void **ret_handle)
{
    (void) cpu_id;

    if (ret_handle == NULL || bt_interrupt_install(source, handler, arg) != 0) {
        return -1;
    }

    *ret_handle = (void *) (uintptr_t) source;

    return 0;
}

static int bt_interrupt_free(void *handle)
{
    int source = (int) (uintptr_t) handle;

    if (source < 0 || source >= BSP_INTERRUPT_VECTOR_COUNT) {
        return -1;
    }

    (void) rtems_interrupt_vector_disable((rtems_vector_number) source);
    if (bt_intr_handlers[source] != NULL) {
        rtems_interrupt_handler_remove(
            (rtems_vector_number) source,
            bt_intr_handlers[source],
            bt_intr_args[source]);
        bt_intr_handlers[source] = NULL;
        bt_intr_args[source] = NULL;
    }

    return 0;
}

static int bt_interrupt_enable(void *handle)
{
    return rtems_interrupt_vector_enable(
               (rtems_vector_number) (uintptr_t) handle)
            == RTEMS_SUCCESSFUL
        ? 0
        : -1;
}

static int bt_interrupt_disable(void *handle)
{
    return rtems_interrupt_vector_disable(
               (rtems_vector_number) (uintptr_t) handle)
            == RTEMS_SUCCESSFUL
        ? 0
        : -1;
}

/* === sleep and coexistence hooks ======================================= */

static uint32_t bt_lpcycles_2_hus(uint32_t cycles, uint32_t *error_corr)
{
    uint64_t res = (uint64_t) bt_lpcycle_us * cycles * 2;

    if (error_corr != NULL) {
        res += *error_corr;
    }
    uint32_t hus = (uint32_t) (res >> BT_LPCYCLE_FRAC);
    if (error_corr != NULL) {
        *error_corr = (uint32_t) (res - ((uint64_t) hus << BT_LPCYCLE_FRAC));
    }
    return hus;
}

static uint32_t bt_hus_2_lpcycles(uint32_t hus)
{
    return (uint32_t) ((((uint64_t) hus << BT_LPCYCLE_FRAC) / bt_lpcycle_us) >> 1);
}

static bool bt_sleep_check_duration(int32_t *slot_cnt)
{
    if (*slot_cnt < 24) {
        return false;
    }
    *slot_cnt -= 8;
    return true;
}

static void bt_sleep_noop_u32(uint32_t value)
{
    (void) value;
}

static void bt_sleep_noop(void)
{
}

static void bt_coex_wifi_sleep_set(bool sleep)
{
    (void) sleep;
}

static int bt_coex_schm_register_btdm_callback(int (*callback)(int))
{
    return coex_schm_register_callback(
        BT_COEX_SCHM_CALLBACK_TYPE_BT, callback);
}

static uint8_t bt_coex_schm_curr_period_get(void)
{
    return coex_schm_curr_period_get();
}

static void bt_hw_power_noop(void)
{
}

static void bt_backup_dma_copy(uint32_t reg, uint32_t mem_addr, uint32_t num, bool to_rem)
{
    (void) reg;
    (void) mem_addr;
    (void) num;
    (void) to_rem;
}

static void bt_rom_table_ready(void)
{
    /* Match IDF's default minimum connection interval feature. The
     * remaining ROM function table is unused by the flash-only controller. */
    ble_min_conn_interval_enable(3);
}

static bool bt_coex_bt_wakeup_request(void)
{
    /* IDF async_wakeup_request(): no wakeup was requested with sleep off. */
    return false;
}

static void bt_coex_bt_wakeup_request_end(void)
{
}

static void bt_assert(void)
{
    /* IDF's callback dumps diagnostics and returns to the controller. */
    printk("bt: controller assert callback\n");
}

static bt_osi_funcs_t bt_osi_funcs = {
    .magic = BT_OSI_MAGIC_VALUE,
    .version = BT_OSI_VERSION,
    .interrupt_alloc = bt_interrupt_alloc,
    .interrupt_free = bt_interrupt_free,
    .interrupt_handler_set_rsv = NULL,
    .global_intr_disable = bt_global_intr_disable,
    .global_intr_restore = bt_global_intr_restore,
    .task_yield = bt_task_yield,
    .task_yield_from_isr = bt_task_yield_from_isr,
    .semphr_create = bt_semphr_create,
    .semphr_delete = bt_semphr_delete,
    .semphr_take_from_isr = bt_semphr_take_from_isr,
    .semphr_give_from_isr = bt_semphr_give_from_isr,
    .semphr_take = bt_semphr_take,
    .semphr_give = bt_semphr_give,
    .mutex_create = bt_mutex_create,
    .mutex_delete = bt_mutex_delete,
    .mutex_lock = bt_mutex_lock,
    .mutex_unlock = bt_mutex_unlock,
    .queue_create = bt_queue_create,
    .queue_delete = bt_queue_delete,
    .queue_send = bt_queue_send,
    .queue_send_from_isr = bt_queue_send_from_isr,
    .queue_recv = bt_queue_recv,
    .queue_recv_from_isr = bt_queue_recv_from_isr,
    .task_create = bt_task_create,
    .task_delete = bt_task_delete,
    .is_in_isr = bt_is_in_isr,
    .cause_sw_intr_to_core = NULL,
    .malloc_fn = bt_malloc,
    .malloc_internal = bt_malloc,
    .free_fn = bt_free,
    .read_efuse_mac = bt_read_mac,
    .srand_fn = bt_srand,
    .rand_fn = bt_rand,
    .btdm_lpcycles_2_hus = bt_lpcycles_2_hus,
    .btdm_hus_2_lpcycles = bt_hus_2_lpcycles,
    .btdm_sleep_check_duration = bt_sleep_check_duration,
    .btdm_sleep_enter_phase1 = bt_sleep_noop_u32,
    .btdm_sleep_enter_phase2 = bt_sleep_noop,
    .btdm_sleep_exit_phase1 = NULL,
    .btdm_sleep_exit_phase2 = NULL,
    .btdm_sleep_exit_phase3 = bt_sleep_noop,
    .coex_wifi_sleep_set = bt_coex_wifi_sleep_set,
    .coex_core_ble_conn_dyn_prio_get = NULL,
    .coex_schm_register_btdm_callback = bt_coex_schm_register_btdm_callback,
    .coex_schm_status_bit_set = coex_schm_status_bit_set,
    .coex_schm_status_bit_clear = coex_schm_status_bit_clear,
    .coex_schm_interval_get = coex_schm_interval_get,
    .coex_schm_curr_period_get = bt_coex_schm_curr_period_get,
    .coex_schm_curr_phase_get = coex_schm_curr_phase_get,
    .interrupt_enable = bt_interrupt_enable,
    .interrupt_disable = bt_interrupt_disable,
    .esp_hw_power_down = bt_hw_power_noop,
    .esp_hw_power_up = bt_hw_power_noop,
    .ets_backup_dma_copy = bt_backup_dma_copy,
    .malloc_retention = bt_malloc,
    .ets_delay_us = bt_delay_us,
    .btdm_rom_table_ready = bt_rom_table_ready,
    .coex_bt_wakeup_request = bt_coex_bt_wakeup_request,
    .coex_bt_wakeup_request_end = bt_coex_bt_wakeup_request_end,
    .get_time_us = bt_get_time_us,
    .assert_fn = bt_assert,
};

/* === HCI transport ===================================================== */

static void bt_event_push(const uint8_t *data, uint16_t len)
{
    rtems_interrupt_level level;
    unsigned head;
    unsigned next;
    bool queued = false;

    if (len > BT_EVENT_MAX) {
        return;
    }

    rtems_interrupt_local_disable(level);
    head = bt_event_head;
    next = (head + 1) % BT_EVENT_RING;
    if (next != bt_event_tail) {
        memcpy(bt_events[head].data, data, len);
        bt_events[head].len = len;
        bt_event_head = next;
        queued = true;
    }
    rtems_interrupt_local_enable(level);

    if (queued && bt_event_sem != RTEMS_INVALID_ID) {
        rtems_semaphore_release(bt_event_sem);
    }
}

static void bt_vhci_ready(void)
{
}

static int bt_vhci_recv(uint8_t *data, uint16_t len)
{
    bt_event_push(data, len);

    return 0;
}

static const bt_vhci_callback_t bt_vhci_cb = {
    .notify_host_send_available = bt_vhci_ready,
    .notify_host_recv = bt_vhci_recv,
};

static bool bt_send(const uint8_t *data, size_t len)
{
    int tries = 100;

    while (tries-- > 0 && !API_vhci_host_check_send_available()) {
        rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(10));
    }
    if (!API_vhci_host_check_send_available()) {
        return false;
    }

    API_vhci_host_send_packet((uint8_t *) data, (uint16_t) len);

    return true;
}

/* One controller-limited event per counter, consumed by the HCI task. */
static int bt_adv_complete;

static int bt_drain_events(uint16_t opcode)
{
    int status = -1;
    while (bt_event_tail != bt_event_head) {
        unsigned tail = bt_event_tail;

        const uint8_t *data = bt_events[tail].data;
        if (bt_events[tail].len >= 7 && data[0] == 0x04
            && data[1] == 0x0e && data[4] == (opcode & 0xff)
            && data[5] == (opcode >> 8)) {
            status = data[6];
        }
        if (bt_events[tail].len >= 4 && data[0] == 0x04
            && data[1] == 0x3e && data[3] == 0x12) {
            /* LE Advertising Set Terminated: limit reached, handle 0,
             * exactly one completed advertising event. */
            bt_adv_complete = bt_events[tail].len == 9 && data[2] == 6
                    && data[4] == 0x43 && data[5] == 0 && data[8] == 1
                ? 1
                : -1;
        }
        printk("bt: event");
        for (uint16_t i = 0; i < bt_events[tail].len; i++) {
            printk(" %02x", bt_events[tail].data[i]);
        }
        printk("\n");
        bt_event_tail = (tail + 1) % BT_EVENT_RING;
    }
    return status;
}

/* Serialize commands using the controller's Command Complete response. */
static bool bt_command(const uint8_t *command, size_t len)
{
    uint16_t opcode = command[1] | ((uint16_t) command[2] << 8);
    if (!bt_send(command, len)) {
        printk("bt: HCI command %04x send timed out\n", opcode);
        return false;
    }
    for (unsigned i = 0; i < 200; i++) {
        int status = bt_drain_events(opcode);
        if (status >= 0) {
            if (status != 0) {
                printk("bt: HCI command %04x failed: %02x\n", opcode, status);
            }
            return status == 0;
        }
        rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(10));
    }
    printk("bt: HCI command %04x response timed out\n", opcode);
    return false;
}

static const uint8_t bt_hci_reset[] = { 0x01, 0x03, 0x0c, 0x00 };

/* Enable LE Meta events and the Advertising Set Terminated subevent. */
static const uint8_t bt_hci_event_mask[] = {
    0x01, 0x01, 0x0c, 0x08, 0, 0, 0, 0, 0, 0, 0, 0x20
};
static const uint8_t bt_hci_le_event_mask[] = {
    0x01, 0x01, 0x20, 0x08, 0, 0, 0x02, 0, 0, 0, 0, 0
};

/* LE Set Extended Advertising Parameters, with legacy ADV_NONCONN_IND PDUs. */
static const uint8_t bt_hci_adv_params[] = {
    0x01, 0x36, 0x20, 0x19,
    0x00, /* advertising handle */
    0x10, 0x00, /* legacy, non-connectable, non-scannable */
    0x00, 0x08, 0x00, /* minimum interval 1.28 s */
    0x00, 0x08, 0x00, /* maximum interval 1.28 s */
    0x07, /* all three advertising channels */
    0x00, /* public address */
    0x00, /* public peer address type */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, /* no filtering */
    0x7f, /* no transmit-power preference */
    0x01, /* primary PHY: LE 1M */
    0x00, /* secondary maximum skip */
    0x01, /* secondary PHY: LE 1M (unused for legacy PDUs) */
    0x00, /* SID */
    0x00, /* no scan-request notifications */
};

/* LE Set Extended Advertising Data: handle, complete data, no fragmentation. */
static uint8_t bt_hci_adv_data[] = {
    0x01, 0x37, 0x20, 0x1b,
    0x00, 0x03, 0x01, 0x17,
    0x02, 0x01, 0x06, /* flags: LE general discoverable, BR/EDR not supported */
    0x0b, 0x09, 'A', 't', 'o', 'm', 'V', 'M', '-', 'B', 'L', 'E',
    0x07, 0xff, 0xff, 0xff, /* manufacturer data, test company ID 0xffff */
    0x00, 0x00, 0x00, 0x00, /* little-endian advertising-event counter */
};

/* LE Set Extended Advertising Enable: handle 0, no duration, one event only. */
static const uint8_t bt_hci_adv_enable[] = {
    0x01, 0x39, 0x20, 0x06, 0x01, 0x01, 0x00, 0x00, 0x00, 0x01
};

_Static_assert(sizeof(bt_hci_adv_params) == 29, "HCI header plus 25 advertising parameters");
_Static_assert(sizeof(bt_hci_adv_data) == 31, "HCI header plus 27 advertising data parameters");
_Static_assert(BT_ADV_COUNTER_OFFSET + sizeof(uint32_t) == sizeof(bt_hci_adv_data), "Counter fills manufacturer element");

static bool bt_controller_initialize(void);

static void bt_smoke_task(rtems_task_argument arg)
{
    (void) arg;

    if (!bt_controller_initialize()) {
        atomic_store(&bt_state, RTEMS_BT_FAILED);
        rtems_task_exit();
    }
    rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(2000));

    if (!bt_command(bt_hci_reset, sizeof(bt_hci_reset))
        || !bt_command(bt_hci_event_mask, sizeof(bt_hci_event_mask))
        || !bt_command(bt_hci_le_event_mask, sizeof(bt_hci_le_event_mask))
        || !bt_command(bt_hci_adv_params, sizeof(bt_hci_adv_params))
        || !bt_command(bt_hci_adv_data, sizeof(bt_hci_adv_data))
        || !bt_command(bt_hci_adv_enable, sizeof(bt_hci_adv_enable))) {
        printk("bt: advertising setup failed\n");
        atomic_store(&bt_state, RTEMS_BT_FAILED);
        rtems_task_exit();
    }

    printk("bt: advertising as AtomVM-BLE\n");
    atomic_store(&bt_state, RTEMS_BT_STARTED);
    uint32_t counter = 0;
    int64_t deadline = bt_get_time_us() + BT_ADV_EVENT_TIMEOUT_US;

    for (;;) {
        if (bt_event_sem != RTEMS_INVALID_ID) {
            (void) rtems_semaphore_obtain(
                bt_event_sem, RTEMS_WAIT, RTEMS_MILLISECONDS_TO_TICKS(1000));
        } else {
            rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(1000));
        }
        (void) bt_drain_events(0);
        rtems_esp_wifi_phy_track();
        if (bt_adv_complete < 0 || (!bt_adv_complete && bt_get_time_us() >= deadline)) {
            printk("bt: advertising event failed or timed out\n");
            atomic_store(&bt_state, RTEMS_BT_FAILED);
            rtems_task_exit();
        }
        if (!bt_adv_complete) {
            continue;
        }
        bt_adv_complete = 0;
        /* A fresh enable may transmit immediately. Pace the single-event
         * bursts while guaranteeing that different events use new data. */
        rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(BT_ADV_INTERVAL_MS));
        counter++;
        for (unsigned i = 0; i < sizeof(counter); i++) {
            bt_hci_adv_data[BT_ADV_COUNTER_OFFSET + i] = (uint8_t) (counter >> (8 * i));
        }
        if (!bt_command(bt_hci_adv_data, sizeof(bt_hci_adv_data))
            || !bt_command(bt_hci_adv_enable, sizeof(bt_hci_adv_enable))) {
            printk("bt: advertising counter update failed\n");
            atomic_store(&bt_state, RTEMS_BT_FAILED);
            rtems_task_exit();
        }
        printk("bt: advertising counter %u\n", (unsigned) counter);
        deadline = bt_get_time_us() + BT_ADV_EVENT_TIMEOUT_US;
    }
}

static bool bt_controller_initialize(void)
{
    rtems_status_code sc;

    /* IDF powers the BT domain before preparing the shared Wi-Fi/BT domain. */
    rtems_esp_bt_power_domain_on();
    if (rtems_esp_modem_prepare() != 0) {
        printk("bt: shared modem initialization failed\n");
        return false;
    }

    btdm_controller_rom_data_init();

    if (btdm_osi_funcs_register(&bt_osi_funcs) != 0) {
        printk("bt: OS adapter rejected\n");

        return false;
    }

    if (coex_init() != 0) {
        printk("bt: coexistence init failed\n");
        return false;
    }

    /* Match IDF's complete peripheral reset, including the low-power block,
     * before programming its clock. */
    rtems_esp_bt_periph_enable();

    /* btdm_low_power_mode_init(): even with sleep disabled, select main
     * XTAL (source 0), divide the C3's 40 MHz crystal to 1 MHz, and notify
     * coexistence. Keep the conversion in IDF's Q19 microsecond format. */
    if (!btdm_lpclk_select_src(0) || !btdm_lpclk_set_div(40)) {
        printk("bt: XTAL low-power clock configuration failed\n");
        return false;
    }
    bt_lpcycle_us = 1u << BT_LPCYCLE_FRAC;
    coex_update_lpclk_interval();

    printk("bt: controller init\n");
    atomic_store(&bt_alloc_failed, false);
    if (btdm_controller_init(&bt_config) != 0 || atomic_load(&bt_alloc_failed)) {
        printk("bt: controller init failed\n");
        return false;
    }

#ifdef RTEMS_HAS_WIFI
    printk("bt: PHY enable\n");
    esp_phy_enable(PHY_MODEM_BT);
#endif

    printk("bt: coexistence enable\n");
    if (coex_enable() != 0) {
        printk("bt: coexistence enable failed\n");
    }

    printk("bt: controller enable\n");
    sdk_config_extend_set_pll_track(false);
    if (btdm_controller_enable(BT_MODE_BLE) != 0) {
        printk("bt: controller enable failed\n");
        return false;
    }

    coex_pti_v2();

    if (API_vhci_host_register_callback(&bt_vhci_cb) != 0) {
        printk("bt: VHCI registration failed\n");
        return false;
    }

    sc = rtems_semaphore_create(
        rtems_build_name('B', 'T', 'E', 'V'),
        0,
        RTEMS_COUNTING_SEMAPHORE | RTEMS_PRIORITY,
        0,
        &bt_event_sem);
    if (sc != RTEMS_SUCCESSFUL) {
        bt_event_sem = RTEMS_INVALID_ID;
    }

    return true;
}

bool rtems_bt_start_async(void)
{
    rtems_bt_state_t expected = RTEMS_BT_STOPPED;
    if (!atomic_compare_exchange_strong(&bt_state, &expected, RTEMS_BT_STARTING)) {
        return expected != RTEMS_BT_FAILED;
    }

    /* Reserve the HCI/initialization stack before controller allocations. */
    rtems_id task;
    rtems_status_code sc = rtems_task_create(
        rtems_build_name('B', 'T', 'H', 'C'), 10, 4096,
        RTEMS_DEFAULT_MODES, RTEMS_FLOATING_POINT | RTEMS_LOCAL, &task);
    if (sc == RTEMS_SUCCESSFUL) {
        sc = rtems_task_start(task, bt_smoke_task, 0);
        if (sc != RTEMS_SUCCESSFUL) {
            rtems_task_delete(task);
        }
    }
    if (sc != RTEMS_SUCCESSFUL) {
        printk("bt: cannot start HCI task (%u)\n", (unsigned) sc);
        atomic_store(&bt_state, RTEMS_BT_FAILED);
        return false;
    }
    return true;
}

rtems_bt_state_t rtems_bt_get_state(void)
{
    return atomic_load(&bt_state);
}

bool rtems_bt_is_started(void)
{
    return rtems_bt_get_state() == RTEMS_BT_STARTED;
}
