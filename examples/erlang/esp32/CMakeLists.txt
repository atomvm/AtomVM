##
## Copyright (c) 2019 Fred Dushin <fred@dushin.net>
##

project(examples_erlang_esp32)

include(BuildErlang)

pack_runnable(blink blink eavmlib estdlib)
pack_runnable(morse_server morse_server estdlib eavmlib)
pack_runnable(sta_network sta_network eavmlib estdlib)
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
