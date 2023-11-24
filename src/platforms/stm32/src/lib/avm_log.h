/* This file is part of AtomVM.
 *
 * Copyright 2023 Winford (Uncle Grumpy) <winford@object.stream>
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

#ifndef _AVM_LOG_H_
#define _AVM_LOG_H_

#include <stdint.h>
#include <stdio.h>

#include <sys.h>

enum AVMLogLevel
{
    LOG_NONE = 0,
    LOG_ERROR,
    LOG_WARN,
    LOG_INFO,
    LOG_DEBUG
};

#ifndef AVM_LOG_DISABLE

#ifdef CONFIG_LOG_LEVEL_MAX
#define LOG_LEVEL_MAX CONFIG_LOG_LEVEL_MAX
#else
#define LOG_LEVEL_MAX LOG_INFO
#endif

/* clang-format off */
#ifdef ENABLE_LOG_COLOR
// #define LOG_COLOR_BLACK   "\033[0;30m"
// #define LOG_COLOR_GREY    "\033[1;30m"
// #define LOG_COLOR_RED     "\033[0;31m"
// #define LOG_BOLD_RED      "\033[1;31m"
// #define LOG_COLOR_GREEN   "\033[0;32m"
// #define LOG_COLOR_ALIEN   "\033[1;32m"
// #define LOG_COLOR_ORANGE  "\033[0;33m"
// #define LOG_COLOR_YELLOW  "\033[1;33m"
// #define LOG_COLOR_BLUE    "\033[0;34m"
// #define LOG_COLOR_PURPLE  "\033[0;35m"
// #define LOG_COLOR_PINK    "\033[1;35m"
// #define LOG_COLOR_CYAN    "\033[0;36m"
#define LOG_COLOR_ERROR   "\033[1;31m"
#define LOG_COLOR_WARN    "\033[0;33m"
#define LOG_COLOR_INFO    "\033[0;32m"
#define LOG_COLOR_DEBUG   "\033[0;34m"
#define LOG_RESET_COLOR   "\033[0m"
#else /* ENABLE_LOG_COLOR OFF */
#define LOG_COLOR_ERROR
#define LOG_COLOR_WARN
#define LOG_COLOR_INFO
#define LOG_COLOR_DEBUG
#define LOG_RESET_COLOR
#endif /* ENABLE_LOG_COLOR */

#ifdef ENABLE_LOG_LINE_INFO
#define LINE_FORMAT " (%s:%i)"
#define LINE_DATA   , __FILE__, __LINE__
#else /* ENABLE_LOG_LINE_INFO OFF*/
#define LINE_FORMAT
#define LINE_DATA
#endif /* ENABLE_LOG_LINE_INFO */
/* clang-format on */

#define AVM_LOGE(tag, format, ...)                                                                                                               \
    do {                                                                                                                                         \
        uint64_t logtime = sys_monotonic_time_u64();                                                                                             \
        if (LOG_LEVEL_MAX >= LOG_ERROR)                                                                                                          \
            printf(LOG_COLOR_ERROR "ERROR [%llu] %s: " format " (%s:%i)" LOG_RESET_COLOR "\n", logtime, tag, ##__VA_ARGS__, __FILE__, __LINE__); \
    } while (0)
#define AVM_LOGW(tag, format, ...)                                                                                                    \
    do {                                                                                                                              \
        uint64_t logtime = sys_monotonic_time_u64();                                                                                  \
        if (LOG_LEVEL_MAX >= LOG_WARN)                                                                                                \
            printf(LOG_COLOR_WARN "WARN [%llu] %s: " format LINE_FORMAT LOG_RESET_COLOR "\n", logtime, tag, ##__VA_ARGS__ LINE_DATA); \
    } while (0)
#define AVM_LOGI(tag, format, ...)                                                                                                    \
    do {                                                                                                                              \
        uint64_t logtime = sys_monotonic_time_u64();                                                                                  \
        if (LOG_LEVEL_MAX >= LOG_INFO)                                                                                                \
            printf(LOG_COLOR_INFO "INFO [%llu] %s: " format LINE_FORMAT LOG_RESET_COLOR "\n", logtime, tag, ##__VA_ARGS__ LINE_DATA); \
    } while (0)
#define AVM_LOGD(tag, format, ...)                                                                                                      \
    do {                                                                                                                                \
        uint64_t logtime = sys_monotonic_time_u64();                                                                                    \
        if (LOG_LEVEL_MAX >= LOG_DEBUG)                                                                                                 \
            printf(LOG_COLOR_DEBUG "DEBUG [%llu] %s: " format LINE_FORMAT LOG_RESET_COLOR "\n", logtime, tag, ##__VA_ARGS__ LINE_DATA); \
    } while (0)
#else
#define AVM_LOGE(tag, format, ...)
#define AVM_LOGW(tag, format, ...)
#define AVM_LOGI(tag, format, ...)
#define AVM_LOGD(tag, format, ...)
#endif /* AVM_LOG_DISABLE */

#endif /* _AVM_LOG_H_ */
