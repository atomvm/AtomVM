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

#include "sntp.h"
#include "rtc.h"

#include <stdatomic.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#if defined(RTEMS_HAS_LIBBSD) || defined(RTEMS_HAS_LWIP)
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <rtems.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#define NTP_PORT_DEFAULT 123
#define NTP_PACKET_SIZE 48
#define NTP_TIMESTAMP_DELTA 2208988800ULL
#define SNTP_TASK_PRIORITY 120
#define SNTP_TASK_STACK_SIZE 2048

static rtems_id sntp_task_id = RTEMS_ID_NONE;
static atomic_bool sntp_running = false;
static char *sntp_hostname = NULL;
static int sntp_port = NTP_PORT_DEFAULT;
static rtems_sntp_sync_cb sntp_cb = NULL;
static void *sntp_cb_arg = NULL;

int rtems_sntp_sync_now(const char *hostname, int port, int timeout_ms, struct timeval *tv_out)
{
    if (hostname == NULL || hostname[0] == '\0') {
        hostname = "pool.ntp.org";
    }
    if (port <= 0) {
        port = NTP_PORT_DEFAULT;
    }
    if (timeout_ms <= 0) {
        timeout_ms = 3000;
    }

    char port_str[16];
    snprintf(port_str, sizeof(port_str), "%d", port);

    struct addrinfo hints;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_DGRAM;
    hints.ai_protocol = IPPROTO_UDP;

    struct addrinfo *res = NULL;
    int err = getaddrinfo(hostname, port_str, &hints, &res);
    if (err != 0 || res == NULL) {
        return -1;
    }

    int sock = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
    if (sock < 0) {
        freeaddrinfo(res);
        return -1;
    }

    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    if (res->ai_addrlen >= sizeof(server_addr)) {
        memcpy(&server_addr, res->ai_addr, sizeof(server_addr));
    } else {
        memcpy(&server_addr, res->ai_addr, res->ai_addrlen);
    }
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(port);
    freeaddrinfo(res);

    uint8_t req[NTP_PACKET_SIZE];
    memset(req, 0, sizeof(req));
    req[0] = 0x23; // LI = 0, VN = 4, Mode = 3 (Client)

    ssize_t sent = -1;
    for (int send_try = 0; send_try < 5; send_try++) {
        sent = sendto(sock, req, sizeof(req), 0, (struct sockaddr *) &server_addr, sizeof(server_addr));
        if (sent == sizeof(req)) {
            break;
        }
        rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(100));
    }

    if (sent != sizeof(req)) {
        close(sock);
        return -1;
    }

    fd_set readfds;
    FD_ZERO(&readfds);
    FD_SET(sock, &readfds);

    struct timeval timeout;
    timeout.tv_sec = timeout_ms / 1000;
    timeout.tv_usec = (timeout_ms % 1000) * 1000;

    int sel_ret = select(sock + 1, &readfds, NULL, NULL, &timeout);
    if (sel_ret <= 0) {
        close(sock);
        return -1;
    }

    uint8_t resp[NTP_PACKET_SIZE];
    ssize_t recvd = recvfrom(sock, resp, sizeof(resp), 0, NULL, NULL);
    close(sock);

    if (recvd < NTP_PACKET_SIZE) {
        return -1;
    }

    uint8_t mode = resp[0] & 0x07;
    if (mode != 4 && mode != 5) {
        return -1;
    }

    uint8_t stratum = resp[1];
    if (stratum == 0) {
        return -1;
    }

    uint32_t sec_be, frac_be;
    memcpy(&sec_be, &resp[40], 4);
    memcpy(&frac_be, &resp[44], 4);
    uint32_t ntp_sec = ntohl(sec_be);
    uint32_t ntp_frac = ntohl(frac_be);

    if (ntp_sec <= NTP_TIMESTAMP_DELTA) {
        return -1;
    }

    time_t unix_sec = (time_t) (ntp_sec - NTP_TIMESTAMP_DELTA);
    uint32_t unix_usec = (uint32_t) (((uint64_t) ntp_frac * 1000000ULL) >> 32);

    struct timeval tv;
    tv.tv_sec = unix_sec;
    tv.tv_usec = unix_usec;

    struct timespec ts;
    ts.tv_sec = unix_sec;
    ts.tv_nsec = (long) unix_usec * 1000L;
    clock_settime(CLOCK_REALTIME, &ts);

#if defined(RTEMS_BOARD_ESP32C3)
    esp32c3_rtc_timekeeper_record(&tv);
#endif

    if (tv_out != NULL) {
        *tv_out = tv;
    }
    return 0;
}

static rtems_task sntp_task_func(rtems_task_argument arg)
{
    (void) arg;
    int max_retries = 5;

    for (int retry = 0; retry < max_retries && atomic_load_explicit(&sntp_running, memory_order_acquire); retry++) {
        struct timeval tv;
        if (rtems_sntp_sync_now(sntp_hostname, sntp_port, 3000, &tv) == 0) {
            if (sntp_cb != NULL && atomic_load_explicit(&sntp_running, memory_order_acquire)) {
                sntp_cb(&tv, sntp_cb_arg);
            }
            break;
        }
        rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(1000));
    }

    atomic_store_explicit(&sntp_running, false, memory_order_release);
    sntp_task_id = RTEMS_ID_NONE;
    rtems_task_exit();
}

int rtems_sntp_start(const char *hostname, int port, rtems_sntp_sync_cb cb, void *arg)
{
    rtems_sntp_stop();

    if (hostname != NULL && hostname[0] != '\0') {
        sntp_hostname = strdup(hostname);
    } else {
        sntp_hostname = strdup("pool.ntp.org");
    }
    sntp_port = (port > 0) ? port : NTP_PORT_DEFAULT;
    sntp_cb = cb;
    sntp_cb_arg = arg;

    atomic_store_explicit(&sntp_running, true, memory_order_release);

    rtems_status_code sc = rtems_task_create(
        rtems_build_name('S', 'N', 'T', 'P'),
        SNTP_TASK_PRIORITY,
        SNTP_TASK_STACK_SIZE,
        RTEMS_DEFAULT_MODES,
        RTEMS_DEFAULT_ATTRIBUTES,
        &sntp_task_id
    );

    if (sc != RTEMS_SUCCESSFUL) {
        atomic_store_explicit(&sntp_running, false, memory_order_release);
        free(sntp_hostname);
        sntp_hostname = NULL;
        return -1;
    }

    sc = rtems_task_start(sntp_task_id, sntp_task_func, 0);
    if (sc != RTEMS_SUCCESSFUL) {
        rtems_task_delete(sntp_task_id);
        sntp_task_id = RTEMS_ID_NONE;
        atomic_store_explicit(&sntp_running, false, memory_order_release);
        free(sntp_hostname);
        sntp_hostname = NULL;
        return -1;
    }

    return 0;
}

void rtems_sntp_stop(void)
{
    atomic_store_explicit(&sntp_running, false, memory_order_release);
    if (sntp_task_id != RTEMS_ID_NONE) {
        rtems_task_delete(sntp_task_id);
        sntp_task_id = RTEMS_ID_NONE;
    }
    free(sntp_hostname);
    sntp_hostname = NULL;
    sntp_cb = NULL;
    sntp_cb_arg = NULL;
}

#else

int rtems_sntp_sync_now(const char *hostname, int port, int timeout_ms, struct timeval *tv_out)
{
    (void) hostname;
    (void) port;
    (void) timeout_ms;
    (void) tv_out;
    return -1;
}

int rtems_sntp_start(const char *hostname, int port, rtems_sntp_sync_cb cb, void *arg)
{
    (void) hostname;
    (void) port;
    (void) cb;
    (void) arg;
    return -1;
}

void rtems_sntp_stop(void)
{
}

#endif
