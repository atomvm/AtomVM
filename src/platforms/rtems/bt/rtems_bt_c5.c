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
 * ESP32-C5 BLE controller glue.
 *
 * The C5 controller is a NimBLE-derived blob (libble_app.a) that expects the
 * host to supply the NPL callbacks, a handful of OS services (tasks,
 * interrupts, memory) and the PHY/baseband/coexistence bring-up.  This file
 * is the RTEMS equivalent of components/bt/controller/esp32c5/bt.c.
 */

#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <mbedtls/ecdh.h>
#include <mbedtls/ecp.h>
#include <rtems.h>
#include <rtems/rtems/intr.h>

#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

#include <bsp.h>
#include <bsp/irq-generic.h>
#include <esp_err.h>
#include <esp_intr_alloc.h>
#include <esp_mac.h>
#include <esp_phy_init.h>
#include <rtems/bspIo.h>
#include <sdkconfig.h>

#include <nimble/nimble_npl.h>

#include "rtems_bt.h"

#define RTEMS_BT_C5_TAG "rtems-bt-c5"

#define EXT_FUNC_VERSION 0x20250825
#define EXT_FUNC_MAGIC_VALUE 0xA5A5A5A5
#define OSI_COEX_VERSION 0x00010006
#define OSI_COEX_MAGIC_VALUE 0xFADEBEAD

/*
 * ESP32-C5 controller config, copied field-for-field from the vendor
 * esp_bt.h (CONFIG_VERSION/CONFIG_MAGIC below).  The vendor header cannot be
 * built out of tree: it includes the NimBLE host NPL headers and reaches a
 * hal/efuse_hal.h that uses #include_next, which a flat BSP include directory
 * cannot satisfy.  The layout and the constant leave no room to drift -- the
 * bytes go straight into the blob -- so this mirrors bt.c on the C3 side.
 *
 * Values follow ESP-IDF's controller-only C5 build, with smaller buffer pools
 * and one connection to leave heap for AtomVM and Wi-Fi. Crystal and CPU
 * frequencies match the board; rtc_freq is set during rtems_bt_c5_init().
 */
#define BT_CONFIG_VERSION 0x20260123u
#define BT_CONFIG_MAGIC_VALUE 0x5A5AA5A5u
#define BT_MODE_BLE 1u
/* ESP-IDF's priority range; RTEMS priorities increase in the other direction. */
#define BT_PRIORITY_MAX 25

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
typedef struct
{
    uint32_t config_version;
    uint16_t ble_ll_resolv_list_size;
    uint16_t ble_hci_evt_hi_buf_count;
    uint16_t ble_hci_evt_lo_buf_count;
    uint8_t ble_ll_sync_list_cnt;
    uint8_t ble_ll_sync_cnt;
    uint16_t ble_ll_rsp_dup_list_count;
    uint16_t ble_ll_adv_dup_list_count;
    uint8_t ble_ll_tx_pwr_dbm;
    uint64_t rtc_freq;
    uint16_t ble_ll_sca;
    uint8_t ble_ll_scan_phy_number;
    uint16_t ble_ll_conn_def_auth_pyld_tmo;
    uint8_t ble_ll_jitter_usecs;
    uint16_t ble_ll_sched_max_adv_pdu_usecs;
    uint16_t ble_ll_sched_direct_adv_max_usecs;
    uint16_t ble_ll_sched_adv_max_usecs;
    uint16_t ble_scan_rsp_data_max_len;
    uint8_t ble_ll_cfg_num_hci_cmd_pkts;
    uint32_t ble_ll_ctrl_proc_timeout_ms;
    uint16_t nimble_max_connections;
    uint8_t ble_whitelist_size;
    uint16_t ble_acl_buf_size;
    uint16_t ble_acl_buf_count;
    uint16_t ble_hci_evt_buf_size;
    uint16_t ble_multi_adv_instances;
    uint16_t ble_ext_adv_max_size;
    uint16_t controller_task_stack_size;
    uint8_t controller_task_prio;
    uint8_t controller_run_cpu;
    uint8_t enable_qa_test;
    uint8_t enable_bqb_test;
    uint8_t enable_tx_cca;
    uint8_t cca_rssi_thresh;
    uint8_t sleep_en;
    uint8_t coex_phy_coded_tx_rx_time_limit;
    uint8_t dis_scan_backoff;
    uint8_t ble_scan_classify_filter_enable;
    uint8_t cca_drop_mode;
    int8_t cca_low_tx_pwr;
    uint8_t main_xtal_freq;
    uint8_t cpu_freq_mhz;
    uint8_t ignore_wl_for_direct_adv;
    uint8_t enable_pcl;
    uint8_t csa2_select;
    uint8_t enable_csr;
    uint8_t ble_aa_check;
    uint8_t ble_llcp_disc_flag;
    uint16_t scan_backoff_upperlimitmax;
    uint8_t ble_chan_ass_en;
    uint8_t ble_data_lenth_zero_aux;
    uint8_t vhci_enabled;
    uint8_t ptr_check_enabled;
    uint8_t ble_adv_tx_options;
    uint8_t skip_unnecessary_checks_en;
    uint8_t fast_conn_data_tx_en;
    int8_t ch39_txpwr;
    uint8_t adv_rsv_cnt;
    uint8_t conn_rsv_cnt;
    uint8_t priority_level_cfg;
    uint8_t slv_fst_rx_lat_en;
    uint8_t dl_itvl_phy_sync_en;
    uint8_t scan_allow_adi_filter;
    uint8_t enhanced_mem_resv;
    uint8_t rxbuf_reserved;
    uint32_t config_magic;
} bt_controller_config_t;
#pragma GCC diagnostic pop

struct osi_coex_funcs_t
{
    uint32_t _magic;
    uint32_t _version;
    void (*_coex_wifi_sleep_set)(bool sleep);
    int (*_coex_core_ble_conn_dyn_prio_get)(bool *low, bool *high);
    void (*_coex_schm_status_bit_set)(uint32_t type, uint32_t status);
    void (*_coex_schm_status_bit_clear)(uint32_t type, uint32_t status);
};

struct ext_funcs_t
{
    uint32_t ext_version;
    int (*_esp_intr_alloc)(int source, int flags, intr_handler_t handler, void *arg, void **ret_handle);
    int (*_esp_intr_free)(void **ret_handle);
    void *(*_malloc)(size_t size);
    void (*_free)(void *p);
    int (*_task_create)(void *task_func, const char *name, uint32_t stack_depth, void *param,
        uint32_t prio, void *task_handle, uint32_t core_id);
    void (*_task_delete)(void *task_handle);
    void (*_osi_assert)(const uint32_t ln, const char *fn, uint32_t param1, uint32_t param2);
    uint32_t (*_os_random)(void);
    int (*_ecc_gen_key_pair)(uint8_t *public, uint8_t *priv);
    int (*_ecc_gen_dh_key)(const uint8_t *remote_pub_key_x, const uint8_t *remote_pub_key_y,
        const uint8_t *local_priv_key, uint8_t *dhkey);
    uint32_t magic;
};

extern void esp_unregister_ext_funcs(void);
extern void esp_unregister_npl_funcs(void);
extern void esp_ble_unregister_bb_funcs(void);
extern int esp_register_ext_funcs(struct ext_funcs_t *);
extern int esp_register_npl_funcs(struct npl_funcs_t *);
extern int ble_osi_coex_funcs_register(struct osi_coex_funcs_t *);
extern int r_ble_controller_init(bt_controller_config_t *cfg);
extern int r_ble_controller_enable(uint8_t mode);
extern int r_ble_controller_disable(void);
extern int r_ble_controller_deinit(void);
extern int r_esp_ble_ll_set_public_addr(const uint8_t *addr);
extern int esp_ble_register_bb_funcs(void);
extern void bt_bb_v2_init_cmplx(int print_version);
extern int coex_init(void);
extern int coex_enable(void);
extern void coex_disable(void);
extern void coex_schm_status_bit_set(uint32_t type, uint32_t status);
extern void coex_schm_status_bit_clear(uint32_t type, uint32_t status);
extern uint32_t rtc_clk_xtal_freq_get(void);

/*
 * Controller stack init/enable, from libble_app.a.  The vendor ble.c only
 * sequences these six (DTM, PAWR, error-sim and adv-fast-tx are all off in the
 * controller-only build), so the sequence is repeated here rather than built.
 */
extern int base_stack_initEnv(void);
extern int adv_stack_initEnv(void);
extern int extAdv_stack_initEnv(void);
extern int scan_stack_initEnv(void);
extern int sync_stack_initEnv(void);
extern int conn_stack_initEnv(void);
extern void base_stack_deinitEnv(void);
extern void adv_stack_deinitEnv(void);
extern void extAdv_stack_deinitEnv(void);
extern void scan_stack_deinitEnv(void);
extern void sync_stack_deinitEnv(void);
extern void conn_stack_deinitEnv(void);
extern int base_stack_enable(void);
extern int adv_stack_enable(void);
extern int extAdv_stack_enable(void);
extern int scan_stack_enable(void);
extern int sync_stack_enable(void);
extern int conn_stack_enable(void);
extern void base_stack_disable(void);
extern void adv_stack_disable(void);
extern void extAdv_stack_disable(void);
extern void scan_stack_disable(void);
extern void sync_stack_disable(void);
extern void conn_stack_disable(void);
extern void mmgmt_enableRxbufOptFeature(void);
extern void r_filter_duplicate_mode_enable(uint32_t mode);
extern void r_filter_duplicate_mode_disable(uint32_t mode);
extern void r_filter_duplicate_set_ring_list_max_num(uint32_t count);
extern void r_scan_duplicate_cache_refresh_set_time(uint32_t seconds);

/* Buffer pools, from libble_app.a.  The vendor os_msys_init() is a wrapper over
 * this with ESP-IDF's MSYS pool sizes; called directly so os_msys_init.c (and
 * its config macros) need not be built. */
extern int r_esp_ble_msys_init(
    uint16_t msys_size1, uint16_t msys_size2, uint16_t msys_cnt1, uint16_t msys_cnt2, uint8_t from_heap);
extern void r_esp_ble_msys_deinit(void);

#define BT_MSYS_1_BLOCK_SIZE 256
#define BT_MSYS_1_BLOCK_COUNT 4
#define BT_MSYS_2_BLOCK_SIZE 320
#define BT_MSYS_2_BLOCK_COUNT 8
#define BT_MSYS_FROM_HEAP 1

/* Track the attempted stages too: a failing vendor initializer may have
 * allocated part of its environment. Its deinitializer handles that case. */
static unsigned bt_stack_initialized;
static unsigned bt_stack_enabled;
static int (*const bt_stack_init[])(void) = {
    base_stack_initEnv, adv_stack_initEnv, extAdv_stack_initEnv,
    scan_stack_initEnv, sync_stack_initEnv, conn_stack_initEnv
};
static void (*const bt_stack_deinit[])(void) = {
    base_stack_deinitEnv, adv_stack_deinitEnv, extAdv_stack_deinitEnv,
    scan_stack_deinitEnv, sync_stack_deinitEnv, conn_stack_deinitEnv
};
static int (*const bt_stack_enable[])(void) = {
    base_stack_enable, adv_stack_enable, extAdv_stack_enable,
    scan_stack_enable, sync_stack_enable, conn_stack_enable
};
static void (*const bt_stack_disable[])(void) = {
    base_stack_disable, adv_stack_disable, extAdv_stack_disable,
    scan_stack_disable, sync_stack_disable, conn_stack_disable
};

static int ble_stack_initEnv(void)
{
    for (unsigned i = 0; i < 6; i++) {
        bt_stack_initialized = i + 1;
        int rc = bt_stack_init[i]();
        if (rc != 0) {
            return rc;
        }
    }
    return 0;
}

static int ble_stack_enable(void)
{
    for (unsigned i = 0; i < 6; i++) {
        bt_stack_enabled = i + 1;
        int rc = bt_stack_enable[i]();
        if (rc != 0) {
            return rc;
        }
    }
    mmgmt_enableRxbufOptFeature();
    return 0;
}

static void ble_stack_disable(void)
{
    while (bt_stack_enabled != 0) {
        bt_stack_disable[--bt_stack_enabled]();
    }
}

static void ble_stack_deinitEnv(void)
{
    while (bt_stack_initialized != 0) {
        bt_stack_deinit[--bt_stack_initialized]();
    }
}

static void bt_scan_duplicate_config(void)
{
    /* IDF defaults: device address + PDU type, mesh exception, 20 entries,
     * and no periodic cache refresh. */
    r_filter_duplicate_mode_disable(UINT32_MAX);
    r_filter_duplicate_mode_enable((1u << 0) | (1u << 2) | (1u << 4));
    r_filter_duplicate_set_ring_list_max_num(20);
    r_scan_duplicate_cache_refresh_set_time(0);
}

extern int rtems_esp_modem_prepare(void);
extern void rtems_esp_bt_periph_enable(void);
extern void rtems_esp_bt_periph_disable(void);

struct npl_funcs_t *rtems_bt_npl_funcs(void);

/* Controller HCI transport. Local declarations avoid pulling the NimBLE host
 * os_mbuf header (and its dependencies) just for these prototypes. */
#define ESP_HCI_INTERNAL_BUF_CMD (3)
#define HCI_H4_CMD 0x01
#define HCI_H4_EVT 0x04
struct os_mbuf;
typedef int esp_hci_internal_rx_cmd_fn(uint8_t *cmd, void *arg);
typedef int esp_hci_internal_rx_acl_fn(struct os_mbuf *om, void *arg);
extern uint8_t *r_ble_hci_trans_buf_alloc(int type);
extern void r_ble_hci_trans_buf_free(uint8_t *buf);
extern int r_ble_hci_trans_hs_cmd_tx(uint8_t *cmd);
extern void r_ble_hci_trans_cfg_hs(esp_hci_internal_rx_cmd_fn *evt_cb, void *evt_arg,
    esp_hci_internal_rx_acl_fn *acl_cb, void *acl_arg);
extern void r_os_mbuf_free_chain(struct os_mbuf *om);

static int rtems_bt_c5_event_cb(uint8_t *evt, void *arg);
static int bt_acl_cb(struct os_mbuf *om, void *arg);

static rtems_id bt_event_sem;
static bool bt_c5_inited;
static bool bt_bb_initialized;
static atomic_bool bt_c5_enabled;

/* === OS services ======================================================= */

typedef struct
{
    void (*func)(void *);
    void *arg;
} bt_task_t;

static void bt_task_entry(rtems_task_argument arg)
{
    bt_task_t *task = (bt_task_t *) (uintptr_t) arg;

    bt_task_t entry = *task;
    free(task);
    entry.func(entry.arg);
    rtems_task_delete(RTEMS_SELF);
}

static int bt_task_create(
    void *task_func,
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
    task->func = (void (*)(void *)) task_func;
    task->arg = param;

    if (rtems_task_create(
            rtems_build_name('B', 'T', 'C', '5'),
            rtems_prio,
            stack_depth, /* ESP-IDF passes bytes. */
            RTEMS_DEFAULT_MODES,
            RTEMS_FLOATING_POINT | RTEMS_LOCAL,
            &id)
        != RTEMS_SUCCESSFUL) {
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
    rtems_task_delete(task_handle == NULL ? RTEMS_SELF : (rtems_id) (uintptr_t) task_handle);
}

typedef void (*bt_intr_handler_t)(void *);

static bt_intr_handler_t bt_intr_handlers[BSP_INTERRUPT_VECTOR_COUNT];
static void *bt_intr_args[BSP_INTERRUPT_VECTOR_COUNT];

static int bt_interrupt_install(int source, bt_intr_handler_t handler, void *arg)
{
    rtems_status_code sc;

    if (source < 0 || source >= BSP_INTERRUPT_VECTOR_COUNT || handler == NULL
        || bt_intr_handlers[source] != NULL) {
        return -1;
    }

    sc = bsp_interrupt_set_priority((rtems_vector_number) source, 2);
    if (sc != RTEMS_SUCCESSFUL) {
        return -1;
    }
    if (rtems_interrupt_handler_install(
            (rtems_vector_number) source,
            "bt",
            RTEMS_INTERRUPT_SHARED,
            handler,
            arg)
        != RTEMS_SUCCESSFUL) {
        return -1;
    }

    sc = rtems_interrupt_vector_enable((rtems_vector_number) source);
    if (sc != RTEMS_SUCCESSFUL) {
        rtems_interrupt_handler_remove((rtems_vector_number) source,
            handler, arg);
        printk("bt: cannot enable interrupt source %d (%i)\n", source, (int) sc);
        return -1;
    }
    bt_intr_handlers[source] = handler;
    bt_intr_args[source] = arg;

    return 0;
}

static int bt_intr_alloc(int source, int flags, intr_handler_t handler, void *arg, void **ret_handle)
{
    (void) flags;

    if (ret_handle == NULL || bt_interrupt_install(source, handler, arg) != 0) {
        return -1;
    }

    *ret_handle = (void *) (uintptr_t) source;

    return 0;
}

static int bt_intr_free(void **ret_handle)
{
    int source;

    if (ret_handle == NULL || *ret_handle == NULL) {
        return -1;
    }
    source = (int) (uintptr_t) *ret_handle;
    if (source < 0 || source >= BSP_INTERRUPT_VECTOR_COUNT) {
        return -1;
    }

    if (rtems_interrupt_vector_disable((rtems_vector_number) source) != RTEMS_SUCCESSFUL) {
        return -1;
    }
    if (bt_intr_handlers[source] != NULL) {
        if (rtems_interrupt_handler_remove(
                (rtems_vector_number) source, bt_intr_handlers[source], bt_intr_args[source])
            != RTEMS_SUCCESSFUL) {
            return -1;
        }
        bt_intr_handlers[source] = NULL;
        bt_intr_args[source] = NULL;
    }

    *ret_handle = NULL;
    return 0;
}

static void bt_osi_assert(const uint32_t ln, const char *fn, uint32_t param1, uint32_t param2)
{
    printk("%s: BLE assert: line %u in %s, param 0x%x 0x%x\n",
        RTEMS_BT_C5_TAG, (unsigned) ln, fn, (unsigned) param1, (unsigned) param2);
    rtems_fatal_error_occurred(RTEMS_FATAL_SOURCE_APPLICATION);
}

extern int rtems_esp_bt_random(void *buf, size_t len);

static uint32_t bt_os_random(void)
{
    uint32_t value = 0;
    if (rtems_esp_bt_random(&value, sizeof(value)) != 0) {
        bt_osi_assert(__LINE__, __func__, 0, 0);
    }
    return value;
}

static int bt_ecc_random(void *context, unsigned char *buf, size_t len)
{
    (void) context;
    return rtems_esp_bt_random(buf, len);
}

/* Use the SDK's software ECP primitives with the radio's hardware RNG.
 * Its standalone PSA RNG has no entropy source before AtomVM initializes.
 * Each Bluetooth P-256 coordinate and scalar is little endian. */
static int bt_ecc_gen_key_pair(uint8_t *public_key, uint8_t *priv)
{
    if (public_key == NULL || priv == NULL) {
        return -1;
    }
    mbedtls_ecp_group group;
    mbedtls_ecp_point pub;
    mbedtls_mpi secret;
    mbedtls_ecp_group_init(&group);
    mbedtls_ecp_point_init(&pub);
    mbedtls_mpi_init(&secret);
    int rc = mbedtls_ecp_group_load(&group, MBEDTLS_ECP_DP_SECP256R1);
    if (rc == 0) {
        rc = mbedtls_ecp_gen_keypair(&group, &secret, &pub, bt_ecc_random, NULL);
    }
    if (rc == 0) {
        rc = mbedtls_mpi_write_binary_le(&pub.MBEDTLS_PRIVATE(X), public_key, 32);
    }
    if (rc == 0) {
        rc = mbedtls_mpi_write_binary_le(&pub.MBEDTLS_PRIVATE(Y), public_key + 32, 32);
    }
    if (rc == 0) {
        rc = mbedtls_mpi_write_binary_le(&secret, priv, 32);
    }
    mbedtls_mpi_free(&secret);
    mbedtls_ecp_point_free(&pub);
    mbedtls_ecp_group_free(&group);
    return rc;
}

static int bt_ecc_gen_dh_key(
    const uint8_t *remote_pub_key_x,
    const uint8_t *remote_pub_key_y,
    const uint8_t *local_priv_key,
    uint8_t *dhkey)
{
    if (remote_pub_key_x == NULL || remote_pub_key_y == NULL
        || local_priv_key == NULL || dhkey == NULL) {
        return -1;
    }
    mbedtls_ecp_group group;
    mbedtls_ecp_point pub;
    mbedtls_mpi secret, dh;
    mbedtls_ecp_group_init(&group);
    mbedtls_ecp_point_init(&pub);
    mbedtls_mpi_init(&secret);
    mbedtls_mpi_init(&dh);
    int rc = mbedtls_ecp_group_load(&group, MBEDTLS_ECP_DP_SECP256R1);
    if (rc == 0) {
        rc = mbedtls_mpi_read_binary_le(&pub.MBEDTLS_PRIVATE(X), remote_pub_key_x, 32);
    }
    if (rc == 0) {
        rc = mbedtls_mpi_read_binary_le(&pub.MBEDTLS_PRIVATE(Y), remote_pub_key_y, 32);
    }
    if (rc == 0) {
        rc = mbedtls_mpi_lset(&pub.MBEDTLS_PRIVATE(Z), 1);
    }
    if (rc == 0) {
        rc = mbedtls_mpi_read_binary_le(&secret, local_priv_key, 32);
    }
    if (rc == 0) {
        rc = mbedtls_ecp_check_pubkey(&group, &pub);
    }
    if (rc == 0) {
        rc = mbedtls_ecp_check_privkey(&group, &secret);
    }
    if (rc == 0) {
        rc = mbedtls_ecdh_compute_shared(&group, &dh, &pub, &secret, bt_ecc_random, NULL);
    }
    if (rc == 0) {
        rc = mbedtls_mpi_write_binary_le(&dh, dhkey, 32);
    }
    /* MbedTLS clears the private scalar/shared secret when freeing MPIs. */
    mbedtls_mpi_free(&dh);
    mbedtls_mpi_free(&secret);
    mbedtls_ecp_point_free(&pub);
    mbedtls_ecp_group_free(&group);
    return rc;
}

static struct ext_funcs_t bt_ext_funcs = {
    .ext_version = EXT_FUNC_VERSION,
    ._esp_intr_alloc = bt_intr_alloc,
    ._esp_intr_free = bt_intr_free,
    ._malloc = malloc,
    ._free = free,
    ._task_create = bt_task_create,
    ._task_delete = bt_task_delete,
    ._osi_assert = bt_osi_assert,
    ._os_random = bt_os_random,
    ._ecc_gen_key_pair = bt_ecc_gen_key_pair,
    ._ecc_gen_dh_key = bt_ecc_gen_dh_key,
    .magic = EXT_FUNC_MAGIC_VALUE,
};

static const struct osi_coex_funcs_t bt_coex_funcs = {
    ._magic = OSI_COEX_MAGIC_VALUE,
    ._version = OSI_COEX_VERSION,
    ._coex_wifi_sleep_set = NULL,
    ._coex_core_ble_conn_dyn_prio_get = NULL,
    ._coex_schm_status_bit_set = coex_schm_status_bit_set,
    ._coex_schm_status_bit_clear = coex_schm_status_bit_clear,
};

/* === controller ======================================================== */

static void bt_set_public_addr(void)
{
    uint8_t mac[6];

    if (esp_read_mac(mac, ESP_MAC_BT) != ESP_OK) {
        return;
    }

    uint8_t tmp = mac[0];
    mac[0] = mac[5];
    mac[5] = tmp;
    tmp = mac[1];
    mac[1] = mac[4];
    mac[4] = tmp;
    tmp = mac[2];
    mac[2] = mac[3];
    mac[3] = tmp;

    (void) r_esp_ble_ll_set_public_addr(mac);
}

int rtems_bt_c5_init(bt_controller_config_t *cfg)
{
    esp_err_t err;
    int rc;

    if (bt_c5_inited) {
        return 0;
    }
    if (cfg == NULL) {
        return -1;
    }

    bool npl_registered = false;
    bool periph_enabled = false;
    bool bb_registered = false;
    bool controller_initialized = false;
    bool msys_attempted = false;

    rc = esp_register_ext_funcs(&bt_ext_funcs);
    if (rc != 0) {
        return rc;
    }
    rc = esp_register_npl_funcs(rtems_bt_npl_funcs());
    if (rc != 0) {
        goto fail;
    }
    npl_registered = true;
    rtems_esp_bt_periph_enable();
    periph_enabled = true;
    err = rtems_esp_modem_prepare();
    if (err != ESP_OK) {
        rc = -1;
        goto fail;
    }
    rc = ble_osi_coex_funcs_register((struct osi_coex_funcs_t *) &bt_coex_funcs);
    if (rc != 0) {
        goto fail;
    }
    rc = coex_init();
    if (rc != 0) {
        goto fail;
    }
    rc = esp_ble_register_bb_funcs();
    if (rc != 0) {
        goto fail;
    }
    bb_registered = true;
    cfg->rtc_freq = 100000;
    rc = r_ble_controller_init(cfg);
    if (rc != 0) {
        goto fail;
    }
    controller_initialized = true;
    rc = ble_stack_initEnv();
    if (rc != 0) {
        goto fail;
    }
    bt_scan_duplicate_config();
    msys_attempted = true;
    rc = r_esp_ble_msys_init(BT_MSYS_1_BLOCK_SIZE, BT_MSYS_2_BLOCK_SIZE,
        BT_MSYS_1_BLOCK_COUNT, BT_MSYS_2_BLOCK_COUNT, BT_MSYS_FROM_HEAP);
    if (rc != 0) {
        goto fail;
    }
    bt_set_public_addr();
    r_ble_hci_trans_cfg_hs(rtems_bt_c5_event_cb, NULL, bt_acl_cb, NULL);
    bt_c5_inited = true;
    return 0;

fail:
    printk("%s: controller initialization failed (%d)\n", RTEMS_BT_C5_TAG, rc);
    if (msys_attempted) {
        r_esp_ble_msys_deinit();
    }
    ble_stack_deinitEnv();
    if (controller_initialized) {
        (void) r_ble_controller_deinit();
    }
    if (bb_registered) {
        esp_ble_unregister_bb_funcs();
    }
    if (periph_enabled) {
        rtems_esp_bt_periph_disable();
    }
    if (npl_registered) {
        esp_unregister_npl_funcs();
    }
    esp_unregister_ext_funcs();
    return rc;
}

int rtems_bt_c5_enable(void)
{
    if (!bt_c5_inited) {
        return -1;
    }
    if (bt_c5_enabled) {
        return 0;
    }
    esp_phy_enable(PHY_MODEM_BT);
    if (!bt_bb_initialized) {
        bt_bb_v2_init_cmplx(1);
        bt_bb_initialized = true;
    }
    int rc = coex_enable();
    if (rc == 0) {
        rc = ble_stack_enable();
    }
    if (rc == 0) {
        rc = r_ble_controller_enable(BT_MODE_BLE);
    }
    if (rc != 0) {
        ble_stack_disable();
        coex_disable();
        esp_phy_disable(PHY_MODEM_BT);
        return rc;
    }
    bt_c5_enabled = true;
    return 0;
}

int rtems_bt_c5_disable(void)
{
    if (!bt_c5_enabled) {
        return 0;
    }
    int rc = r_ble_controller_disable();
    if (rc != 0) {
        return rc;
    }
    ble_stack_disable();
    coex_disable();
    esp_phy_disable(PHY_MODEM_BT);
    bt_c5_enabled = false;
    return 0;
}

int rtems_bt_c5_deinit(void)
{
    if (!bt_c5_inited) {
        return 0;
    }
    int rc = rtems_bt_c5_disable();
    if (rc != 0) {
        return rc;
    }
    r_ble_hci_trans_cfg_hs(NULL, NULL, NULL, NULL);
    if (bt_event_sem != RTEMS_INVALID_ID) {
        rtems_semaphore_delete(bt_event_sem);
        bt_event_sem = RTEMS_INVALID_ID;
    }
    r_esp_ble_msys_deinit();
    ble_stack_deinitEnv();
    rc = r_ble_controller_deinit();
    esp_ble_unregister_bb_funcs();
    rtems_esp_bt_periph_disable();
    esp_unregister_npl_funcs();
    esp_unregister_ext_funcs();
    bt_c5_inited = false;
    return rc;
}

/* === HCI =============================================================== */

/*
 * The controller command buffer starts at the opcode; the H4 packet indicator
 * is not part of it (see IDF's VHCI driver, which copies data + 1).  Callers
 * pass a complete H4 packet, so strip the indicator here.
 */
static int rtems_bt_c5_hci_cmd(const uint8_t *cmd, size_t len)
{
    uint8_t *buf;

    if (cmd == NULL || len < 2) {
        return -1;
    }

    buf = r_ble_hci_trans_buf_alloc(ESP_HCI_INTERNAL_BUF_CMD);
    if (buf == NULL) {
        return -1;
    }

    memcpy(buf, cmd + 1, len - 1);

    return r_ble_hci_trans_hs_cmd_tx(buf);
}

/* === minimal HCI advertising smoke test ================================ */

#define BT_EVENT_RING 4
#define BT_EVENT_MAX 260
#define BT_ADV_INTERVAL_MS 1280
#define BT_ADV_EVENT_TIMEOUT_US 5000000
#define BT_ADV_COUNTER_OFFSET 27u

static struct
{
    uint8_t data[BT_EVENT_MAX];
    uint16_t len;
} bt_events[BT_EVENT_RING];
static volatile unsigned bt_event_head;
static volatile unsigned bt_event_tail;
static atomic_int bt_state = RTEMS_BT_STOPPED;

static void bt_event_push(const uint8_t *data, uint16_t len)
{
    rtems_interrupt_level level;
    unsigned head;
    unsigned next;
    bool queued = false;

    if (len > BT_EVENT_MAX - 1) {
        return;
    }

    rtems_interrupt_local_disable(level);
    head = bt_event_head;
    next = (head + 1) % BT_EVENT_RING;
    if (next != bt_event_tail) {
        /* The controller supplies a raw event; the smoke-test parser
         * uses H4 framing, like the C3 transport. */
        bt_events[head].data[0] = HCI_H4_EVT;
        memcpy(bt_events[head].data + 1, data, len);
        bt_events[head].len = len + 1;
        bt_event_head = next;
        queued = true;
    }
    rtems_interrupt_local_enable(level);

    if (queued && bt_event_sem != RTEMS_INVALID_ID) {
        rtems_semaphore_release(bt_event_sem);
    }
}

static int rtems_bt_c5_event_cb(uint8_t *evt, void *arg)
{
    (void) arg;
    if (evt != NULL) {
        bt_event_push(evt, evt[1] + 2);
        r_ble_hci_trans_buf_free(evt);
    }
    return 0;
}

static int bt_acl_cb(struct os_mbuf *om, void *arg)
{
    (void) arg;
    if (om != NULL) {
        r_os_mbuf_free_chain(om);
    }
    return -1;
}

static bool bt_send(const uint8_t *data, size_t len)
{
    return rtems_bt_c5_hci_cmd(data, len) == 0;
}

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
            bt_adv_complete = bt_events[tail].len == 9 && data[2] == 6
                    && data[4] == 0x43 && data[5] == 0 && data[8] == 1
                ? 1
                : -1;
        }
        printk("bt: event");
        for (uint16_t i = 0; i < bt_events[tail].len; i++) {
            printk(" %02x", data[i]);
        }
        printk("\n");
        bt_event_tail = (tail + 1) % BT_EVENT_RING;
    }

    return status;
}

static bool bt_command(const uint8_t *command, size_t len)
{
    uint16_t opcode = command[1] | ((uint16_t) command[2] << 8);

    if (!bt_send(command, len)) {
        printk("bt: HCI command %04x send failed\n", opcode);

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

static const uint8_t bt_hci_event_mask[] = {
    0x01, 0x01, 0x0c, 0x08, 0, 0, 0, 0, 0, 0, 0, 0x20
};
static const uint8_t bt_hci_le_event_mask[] = {
    0x01, 0x01, 0x20, 0x08, 0, 0, 0x02, 0, 0, 0, 0, 0
};

static const uint8_t bt_hci_adv_params[] = {
    0x01,
    0x36,
    0x20,
    0x19,
    0x00,
    0x10,
    0x00,
    0x00,
    0x08,
    0x00,
    0x00,
    0x08,
    0x00,
    0x07,
    0x00,
    0x00,
    0x00,
    0x00,
    0x00,
    0x00,
    0x00,
    0x00,
    0x00,
    0x7f,
    0x01,
    0x00,
    0x01,
    0x00,
    0x00,
};

static uint8_t bt_hci_adv_data[] = {
    0x01,
    0x37,
    0x20,
    0x1b,
    0x00,
    0x03,
    0x01,
    0x17,
    0x02,
    0x01,
    0x06,
    0x0b,
    0x09,
    'A',
    't',
    'o',
    'm',
    'V',
    'M',
    '-',
    'B',
    'L',
    'E',
    0x07,
    0xff,
    0xff,
    0xff,
    0x00,
    0x00,
    0x00,
    0x00,
};

static const uint8_t bt_hci_adv_enable[] = {
    0x01, 0x39, 0x20, 0x06, 0x01, 0x01, 0x00, 0x00, 0x00, 0x01
};

_Static_assert(sizeof(bt_hci_adv_params) == 29, "HCI header plus 25 advertising parameters");
_Static_assert(sizeof(bt_hci_adv_data) == 31, "HCI header plus 27 advertising data parameters");
_Static_assert(BT_ADV_COUNTER_OFFSET + sizeof(uint32_t) == sizeof(bt_hci_adv_data), "Counter fills manufacturer element");

static int64_t bt_get_time_us(void)
{
    return (int64_t) (rtems_clock_get_uptime_nanoseconds() / 1000u);
}

static bool bt_controller_initialize(void)
{
    /* Keep the controller's packet sizes, but budget fewer queued packets
     * and one connection for C5's shared 384 KiB SRAM. rtc_freq is set in
     * rtems_bt_c5_init() after selecting the main XTAL. */
    bt_controller_config_t config = {
        .config_version = BT_CONFIG_VERSION,
        .ble_ll_resolv_list_size = 4,
        .ble_hci_evt_hi_buf_count = 8,
        .ble_hci_evt_lo_buf_count = 4,
        .ble_ll_sync_list_cnt = 5,
        .ble_ll_sync_cnt = 1,
        .ble_ll_rsp_dup_list_count = 20,
        .ble_ll_adv_dup_list_count = 20,
        .ble_ll_tx_pwr_dbm = 9,
        .rtc_freq = 32768,
        .ble_ll_sca = 60,
        .ble_ll_scan_phy_number = 2,
        .ble_ll_conn_def_auth_pyld_tmo = 3000,
        .ble_ll_jitter_usecs = 16,
        .ble_ll_sched_max_adv_pdu_usecs = 376,
        .ble_ll_sched_direct_adv_max_usecs = 502,
        .ble_ll_sched_adv_max_usecs = 852,
        .ble_scan_rsp_data_max_len = 1650,
        .ble_ll_cfg_num_hci_cmd_pkts = 1,
        .ble_ll_ctrl_proc_timeout_ms = 40000,
        .nimble_max_connections = 1,
        .ble_whitelist_size = 12,
        .ble_acl_buf_size = 517,
        .ble_acl_buf_count = 4,
        .ble_hci_evt_buf_size = 257,
        .ble_multi_adv_instances = 1,
        .ble_ext_adv_max_size = 1650,
        .controller_task_stack_size = 4096,
        .controller_task_prio = 23,
        .controller_run_cpu = 0,
        .cca_rssi_thresh = 206,
        .ble_scan_classify_filter_enable = 1,
        .main_xtal_freq = rtc_clk_xtal_freq_get(),
        /* The C5 BSP selects PLL_F160M, independently of IDF defaults. */
        .cpu_freq_mhz = 160,
        .csa2_select = 1,
        .scan_backoff_upperlimitmax = 32,
        .ble_data_lenth_zero_aux = 1,
        .vhci_enabled = 1,
        .fast_conn_data_tx_en = 1,
        .ch39_txpwr = 9,
        .adv_rsv_cnt = 1,
        .conn_rsv_cnt = 2,
        .priority_level_cfg = 20,
        .config_magic = BT_CONFIG_MAGIC_VALUE,
    };
    rtems_status_code sc;

    if (rtems_bt_c5_init(&config) != 0) {
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
        (void) rtems_bt_c5_deinit();
        return false;
    }
    if (rtems_bt_c5_enable() != 0) {
        (void) rtems_bt_c5_deinit();
        return false;
    }
    return true;
}

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
        if (bt_adv_complete < 0 || (!bt_adv_complete && bt_get_time_us() >= deadline)) {
            printk("bt: advertising event failed or timed out\n");
            atomic_store(&bt_state, RTEMS_BT_FAILED);
            rtems_task_exit();
        }
        if (!bt_adv_complete) {
            continue;
        }
        bt_adv_complete = 0;
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

static bool bt_task_start(rtems_task_entry entry)
{
    rtems_bt_state_t expected = RTEMS_BT_STOPPED;

    if (!atomic_compare_exchange_strong(&bt_state, &expected, RTEMS_BT_STARTING)) {
        return expected != RTEMS_BT_FAILED;
    }

    rtems_id task;
    rtems_status_code sc = rtems_task_create(
        rtems_build_name('B', 'T', 'H', 'C'), 10, 4096,
        RTEMS_DEFAULT_MODES, RTEMS_FLOATING_POINT | RTEMS_LOCAL, &task);

    if (sc == RTEMS_SUCCESSFUL) {
        sc = rtems_task_start(task, entry, 0);
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

bool rtems_bt_start_async(void)
{
    return bt_task_start(bt_smoke_task);
}

rtems_bt_state_t rtems_bt_get_state(void)
{
    return atomic_load(&bt_state);
}

bool rtems_bt_is_started(void)
{
    return rtems_bt_get_state() == RTEMS_BT_STARTED;
}
