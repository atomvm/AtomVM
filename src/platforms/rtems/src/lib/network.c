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

#include "network.h"

#include <stdio.h>
#ifdef RTEMS_HAS_LIBBSD
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <rtems.h>
#include <rtems/bsd/bsd.h>
#include <rtems/dhcpcd.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

#ifdef RTEMS_HAS_LIBBSD
static const char dhcpcd_conf[] = "hostname atomvm\n"
                                  "duid\n"
                                  "option domain_name_servers, domain_name, domain_search, host_name\n"
                                  "option classless_static_routes\n"
                                  "option interface_mtu\n"
                                  "require dhcp_server_identifier\n"
                                  "waitip 4\n"
                                  "interface ffec0\n";

static int write_all(int fd, const char *buf, size_t len)
{
    while (len > 0) {
        ssize_t written = write(fd, buf, len);
        if (written < 0) {
            if (errno == EINTR) {
                continue;
            }
            return -1;
        }
        if (written == 0) {
            return -1;
        }
        buf += written;
        len -= (size_t) written;
    }
    return 0;
}

static int write_dhcpcd_conf(void)
{
    if (mkdir("/etc", 0755) != 0 && errno != EEXIST) {
        return -1;
    }
    int fd = open("/etc/dhcpcd.conf", O_CREAT | O_WRONLY | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    if (fd < 0) {
        return -1;
    }
    int write_rv = write_all(fd, dhcpcd_conf, sizeof(dhcpcd_conf) - 1);
    int close_rv = close(fd);
    if (write_rv != 0 || close_rv != 0) {
        return -1;
    }
    return 0;
}

static const char *dhcpcd_env_value(char *const *env, const char *name)
{
    size_t name_len = strlen(name);
    for (; *env != NULL; env++) {
        if (strncmp(*env, name, name_len) == 0 && (*env)[name_len] == '=') {
            return *env + name_len + 1;
        }
    }
    return NULL;
}

static int write_resolv_conf(const char *servers)
{
    while (isspace((unsigned char) *servers)) {
        servers++;
    }
    if (*servers == '\0') {
        return -1;
    }

    int fd = open("/etc/resolv.conf", O_CREAT | O_WRONLY | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    if (fd < 0) {
        return -1;
    }

    int rv = 0;
    while (*servers != '\0') {
        const char *server = servers;
        while (*servers != '\0' && !isspace((unsigned char) *servers)) {
            servers++;
        }
        if (write_all(fd, "nameserver ", sizeof("nameserver ") - 1) != 0
            || write_all(fd, server, (size_t) (servers - server)) != 0
            || write_all(fd, "\n", 1) != 0) {
            rv = -1;
            break;
        }
        while (isspace((unsigned char) *servers)) {
            servers++;
        }
    }

    if (close(fd) != 0) {
        rv = -1;
    }
    return rv;
}

static void dhcpcd_hook_handler(rtems_dhcpcd_hook *hook, char *const *env)
{
    (void) hook;

    const char *servers = dhcpcd_env_value(env, "new_domain_name_servers");
    if (servers != NULL && write_resolv_conf(servers) != 0) {
        fprintf(stderr, "failed to write DHCP resolver configuration\n");
    }
}

static rtems_dhcpcd_hook dhcpcd_hook = {
    .name = "atomvm-resolv-conf",
    .handler = dhcpcd_hook_handler
};
#endif

int rtems_atomvm_network_init(void)
{
#ifdef RTEMS_HAS_LIBBSD
    rtems_status_code sc = rtems_bsd_initialize();
    if (sc != RTEMS_SUCCESSFUL) {
        fprintf(stderr, "rtems_bsd_initialize failed: %s\n", rtems_status_text(sc));
        return -1;
    }

    if (rtems_bsd_ifconfig_lo0() != 0) {
        fprintf(stderr, "rtems_bsd_ifconfig_lo0 failed\n");
        return -1;
    }

    if (write_dhcpcd_conf() != 0) {
        fprintf(stderr, "failed to write /etc/dhcpcd.conf\n");
        return -1;
    }

    rtems_dhcpcd_add_hook(&dhcpcd_hook);
    sc = rtems_dhcpcd_start(NULL);
    if (sc != RTEMS_SUCCESSFUL) {
        fprintf(stderr, "rtems_dhcpcd_start failed: %s\n", rtems_status_text(sc));
        return -1;
    }

    return 0;
#else
    return 0;
#endif
}
