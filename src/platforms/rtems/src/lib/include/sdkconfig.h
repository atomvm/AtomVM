/* SPDX-License-Identifier: BSD-2-Clause */

/*
 * Copyright (C) 2026 Samuel Price <thesamprice@gmail.com>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES ARE DISCLAIMED.
 */

/*
 * sdkconfig.h for building ESP-IDF components outside ESP-IDF.
 *
 * Every source in esp-hal-3rdparty includes this.  In ESP-IDF it is generated
 * from Kconfig by the build system; there is no Kconfig here, so it is written
 * by hand and grows one entry at a time as a source is added and asks for
 * something.
 *
 * Written rather than generated on purpose.  A generated file would carry
 * several thousand CONFIG_ symbols, and the ones that matter would be
 * indistinguishable from the ones that do not.  Here every entry is present
 * because a specific source needed it, so the file doubles as the record of
 * what has actually been built.
 *
 * Anything absent is absent because nothing has asked yet.  A source that
 * needs a new one fails at compile time with the name in the error, which is
 * the behaviour wanted: a silently-zero CONFIG_ is how a feature gets built
 * out without anyone deciding to.
 */

#ifndef RTEMS_ESP_SDKCONFIG_H
#define RTEMS_ESP_SDKCONFIG_H

/* The target.  Selects the register layouts and the per-chip branches. */
#define CONFIG_IDF_TARGET_ESP32C3 1

/*
 * The same fact as a string.  esp_supplicant/src/esp_wps.c does
 *
 *     const char *wps_model_number = CONFIG_IDF_TARGET;
 *
 * at file scope, so without it that file does not compile and
 * wps_get_wps_sm_cb() -- which esp_wpa_main.c calls unconditionally, WPS
 * enabled or not -- is undefined at link.  ESP-IDF generates both forms from
 * the one Kconfig choice; here they are two lines that must agree.
 */
#define CONFIG_IDF_TARGET "esp32c3"

/*
 * Fine timing measurement.
 *
 * On for a reason that is not a preference: the blobs reference the 24
 * est_PHY_*_FTM_COMP_* variables unconditionally, and with FTM off
 * ftm_load_calibration.c defines none of them and the link fails.  So the
 * choice is FTM on, or 24 hand-written stubs whose values would be wrong.
 */
#define CONFIG_ESP_WIFI_FTM_ENABLE 1

/* The C3 is 2.4 GHz only. */
#define CONFIG_SOC_WIFI_SUPPORT_5G 0

/*
 * Maximum transmit power, in dBm, asked for by esp_phy/esp32c3/phy_init_data.c
 * -- which clamps it to 0x50 in quarter-dBm, so 20 dBm is also the ceiling the
 * data allows.  20 is ESP-IDF's own Kconfig default.
 *
 * This is a regulatory limit as well as a hardware one, and the number that
 * applies depends on where the radio is operated and on the antenna.  It is
 * here because the PHY data will not compile without it, not because 20 has
 * been checked against any particular jurisdiction; the country setting that
 * governs channels goes in separately through esp_wifi_set_country_code().
 */
#define CONFIG_ESP_PHY_MAX_TX_POWER 20

/*
 * Log level, asked for by log/include/esp_log_level.h once anything includes
 * esp_log.h -- which the supplicant's headers do.
 *
 * 3 is ESP_LOG_INFO, ESP-IDF's own default.  It only sets the compile-time
 * ceiling for ESP_LOGx in ESP-IDF sources; this port routes those through
 * printk in src/rtems_esp_glue.c rather than through esp_log's own writer, so
 * raising it produces more output and not a different mechanism.
 */
#define CONFIG_LOG_DEFAULT_LEVEL 3

/*
 * The compile-time ceiling.  esp_log_level.h wants both: DEFAULT is the level
 * a tag starts at, MAXIMUM is the one above which ESP_LOGx compiles to
 * nothing at all.  Only the supplicant needs it, and only once it is built
 * with DEBUG_PRINT -- without that, wpa_printf() expands to do {} while (0)
 * and neither symbol is ever referenced, which is why this was missing for so
 * long and why its absence surfaced as 19 of 48 files failing to compile the
 * moment the supplicant was asked to say anything.
 */
#define CONFIG_LOG_MAXIMUM_LEVEL 5

/*
 * Log API version 2, which is the one whose ESP_LOG_LEVEL reduces to a single
 * esp_log() call.  Version 1 expands into a chain that wants
 * CONFIG_LOG_TIMESTAMP_SOURCE_* and LOG_FORMAT as well, none of which this
 * port has; undefined, ESP_LOG_VERSION reads as 0 and takes the version 1
 * branch.
 */
#define CONFIG_LOG_VERSION 2

/*
 * Buffer counts, all named by WIFI_INIT_CONFIG_DEFAULT() in esp_wifi.h.
 *
 * These are the branch's own Kconfig defaults, read out of
 * components/esp_wifi/Kconfig rather than remembered.  They are the largest
 * single claim the WiFi libraries make on the heap, so they are the first
 * numbers to lower if RAM gets tight.
 */
#define CONFIG_ESP_WIFI_STATIC_RX_BUFFER_NUM 10
#define CONFIG_ESP_WIFI_DYNAMIC_RX_BUFFER_NUM 32
#define CONFIG_ESP_WIFI_DYNAMIC_TX_BUFFER_NUM 32

/*
 * Dynamic TX buffers, which is the Kconfig choice's own default and also what
 * it recommends: "If PSRAM is disabled, Dynamic should be selected to improve
 * the utilization of RAM."  The C3 has no PSRAM -- SOC_SPIRAM_SUPPORTED is not
 * defined for it -- so the condition is unambiguous here.
 *
 * ESP_WIFI_DYNAMIC_TX_BUFFER is the choice symbol; TX_BUFFER_TYPE is the int
 * esp_wifi.h reads, and Kconfig derives one from the other.  Both are needed
 * because esp_wifi.h tests each.
 */
#define CONFIG_ESP_WIFI_DYNAMIC_TX_BUFFER 1
#define CONFIG_ESP_WIFI_TX_BUFFER_TYPE 1

/* Static management-frame buffers, Kconfig's default. */
#define CONFIG_ESP_WIFI_DYNAMIC_RX_MGMT_BUF 0
#define CONFIG_ESP_WIFI_RX_MGMT_BUF_NUM_DEF 5

/* ESP-NOW peers. Two is Kconfig's default; nothing here uses ESP-NOW. */
#define CONFIG_ESP_WIFI_ESPNOW_MAX_ENCRYPT_NUM 2

/*
 * === wpa_supplicant ====================================================
 *
 * The supplicant's own feature macros -- CONFIG_IEEE80211W, CONFIG_WPS,
 * CONFIG_CRYPTO_INTERNAL and the rest -- are deliberately NOT here, and an
 * earlier version of this file that put them here was wrong in a way worth
 * recording, because the build did not complain.
 *
 * A #define here reaches a supplicant source only indirectly and only late:
 * utils/includes.h includes port/include/supplicant_opt.h, which includes this
 * file.  So it arrives below the first #include and nowhere above it -- and 11
 * of the 147 sources test a feature macro *above* their first #include.
 * src/crypto/ccmp.c is the clearest:
 *
 *     #ifdef CONFIG_IEEE80211W        <- line 9
 *     #include "utils/includes.h"     <- line 11
 *
 * The #include that would define the macro is inside the #ifdef the macro
 * gates, so the file can never switch itself on.  Measured, same flags, one
 * -D apart:
 *
 *     ccmp.o without CONFIG_IEEE80211W:   860 bytes, 0 defined symbols
 *     ccmp.o with    CONFIG_IEEE80211W:  5824 bytes, 8 defined symbols
 *
 * The 860-byte one was in libwpa.a, satisfying nothing, while rsn_supp/wpa.h
 * two directories over -- which includes sdkconfig.h at the top, before any
 * test -- saw the macro set and laid out structs that assume it.  That is not
 * a feature being off, it is two halves of one library disagreeing about a
 * struct, and no diagnostic says so.
 *
 * Upstream does not have the problem because it never puts them here:
 * components/wpa_supplicant/CMakeLists.txt passes them with
 * target_compile_definitions(... PRIVATE ...), once, to every file.
 * tools/supplicant-build.sh now does the same and carries that list.
 *
 * What belongs here is the layer above: the CONFIG_ESP_WIFI_* Kconfig choices
 * that upstream's CMakeLists translates into those macros.  They are recorded
 * as comments rather than #defines because nothing in this port reads them --
 * the translation is done by hand in supplicant-build.sh -- and a #define that
 * nothing reads is the same trap again.
 *
 * The choices, and what each costs:
 *
 *   CONFIG_ESP_WIFI_ENABLE_WPA3_SAE = n   (upstream default y)
 *   CONFIG_ESP_WIFI_ENABLE_WPA3_OWE_STA = n
 *       No WPA3-Personal and no Opportunistic Wireless Encryption.  Both need
 *       elliptic-curve arithmetic; see the crypto entry below for why that is
 *       not available yet.  A WPA3-only access point will not accept this
 *       station.  A WPA3-transition AP will, over WPA2-PSK, which is what
 *       CONFIG_IEEE80211W below is for.
 *
 *   CONFIG_ESP_WIFI_ENTERPRISE_SUPPORT = n
 *       No EAP-TLS/PEAP/TTLS, so no 802.1X network.  This is also why
 *       mbedtls_config.h builds no TLS and no X.509.
 *
 *   CONFIG_ESP_WIFI_MBEDTLS_CRYPTO = n   (upstream default y)
 *       The supplicant uses its own portable C crypto -- aes-internal.c,
 *       sha1-internal.c, sha256-internal.c, crypto_internal*.c -- rather than
 *       routing through mbedtls.  This is upstream's else-branch, not a
 *       hand-made combination, and for a PSK station it is complete: CCMP,
 *       the EAPOL-Key MIC, PBKDF2 and the RFC 3394 group-key unwrap are all
 *       there.
 *
 *       It is not a preference.  esp_supplicant/src/crypto/crypto_mbedtls.c
 *       is written against ESP-IDF's mbedtls *component*, not against
 *       espressif/mbedtls: it includes "mbedtls/esp_config.h",
 *       "mbedtls/ecp.h" and "mbedtls/bignum.h", and in mbedtls 4.x the last
 *       two exist only as mbedtls/private/.  All three are supplied by
 *       components/mbedtls/port/include, and esp_config.h in turn selects the
 *       ESP32 hardware PSA drivers (psa_crypto_driver_esp_hmac_opaque_contexts.h,
 *       absent here) and redefines the MBEDTLS_* set that libmbedtls.a was
 *       compiled with -- which would give crypto_mbedtls.o a different
 *       psa_hash_operation_t than the library it calls.  Turning this on means
 *       porting components/mbedtls/port first.  See docs/step6-supplicant.md.
 *
 *       The cost is speed, not capability: PBKDF2-SHA1 runs 4096 HMAC
 *       iterations in software once per association.  Nothing is lost to the
 *       ESP32-C3's AES and SHA peripherals either, because this port does not
 *       drive them -- the mbedtls built here is software as well.
 *
 *   CONFIG_ESP_WIFI_SOFTAP_SUPPORT = n
 *   CONFIG_ESP_WIFI_11KV_SUPPORT = n, CONFIG_ESP_WIFI_RRM_SUPPORT = n,
 *   CONFIG_ESP_WIFI_WNM_SUPPORT = n, CONFIG_ESP_WIFI_MBO_SUPPORT = n
 *       No access-point mode and no 802.11k/v assisted roaming.  A station
 *       still roams, it just does not get the neighbour report and BSS
 *       transition hints, so it decides when to move on signal strength alone.
 *
 *   CONFIG_ESP_WIFI_DPP_SUPPORT = n, CONFIG_ESP_WIFI_NAN_USD_ENABLE = n,
 *   CONFIG_ESP_WIFI_PASN_SUPPORT = n, CONFIG_ESP_WIFI_WPS_SOFTAP_REGISTRAR = n
 *       Wi-Fi Easy Connect, Wi-Fi Aware, pre-association security negotiation
 *       and the WPS registrar.  All upstream defaults, all off upstream too.
 *
 * CONFIG_IEEE80211W -- protected management frames -- is on, in
 * supplicant-build.sh with the rest.  Upstream defines it unconditionally, and
 * it matters here: WPA3-transition access points require PMF and a growing
 * number of WPA2-only ones are configured to, so a station without it fails to
 * associate with networks that look ordinary.
 */

#endif /* RTEMS_ESP_SDKCONFIG_H */
