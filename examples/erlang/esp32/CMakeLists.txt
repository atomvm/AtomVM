#
# This file is part of AtomVM.
#
# Copyright 2019-2020 Fred Dushin <fred@dushin.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

project(examples_erlang_esp32)

include(BuildErlang)

pack_runnable(blink blink eavmlib estdlib)
pack_runnable(deep_sleep deep_sleep eavmlib estdlib)
pack_runnable(morse_server morse_server estdlib eavmlib)
pack_runnable(ap_sta_network ap_sta_network eavmlib estdlib)
pack_runnable(set_network_config set_network_config eavmlib estdlib)
pack_runnable(udp_server_blink udp_server_blink eavmlib estdlib)
pack_runnable(tcp_client_esp32 tcp_client_esp32 eavmlib estdlib)
pack_runnable(tcp_server_blink tcp_server_blink eavmlib estdlib)
pack_runnable(esp_random esp_random eavmlib estdlib)
pack_runnable(esp_nvs esp_nvs eavmlib)
pack_runnable(sht31 sht31 eavmlib estdlib)
pack_runnable(sx127x sx127x eavmlib estdlib)
pack_runnable(reformat_nvs reformat_nvs eavmlib)
pack_runnable(uartecho uartecho eavmlib estdlib)
pack_runnable(ledc_example ledc_example eavmlib estdlib)
pack_runnable(epmd_disterl epmd_disterl eavmlib estdlib)
