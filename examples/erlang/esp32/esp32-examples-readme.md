# ESP32 Examples

AtomVM includes examples that are specifically designed for the ESP32 and other microcontrollers.

# Flashing AtomVM Examples for ESP32

In order to run the ESP32 examples, you will need to flash the example AVM files that are created as part of the build to your device.

In the remainder of this document, we assume a script `flash.sh`, which has something like the following contents:

    #!/bin/sh
    ${IDF_PATH}/components/esptool_py/esptool/esptool.py \
        --chip esp32 \
        --port /dev/ttyUSB0 \
        --baud 115200 \
        --before default_reset --after hard_reset \
        write_flash -u --flash_mode dio --flash_freq 40m --flash_size detect 0x110000 ${1}

> Note.  Set the IDF_PATH environement variable to be the root directory of the ESP IDF tool, which you have installed as part of the AtomVM build tool chain.

> Note.  Substitute the USB serial device on your platform for `/dev/ttyUSB0`, in this script.

You can montor the console output of these examples by issuing the `monitor` target to `make`, in the `src/platforms/esp32` directory of the AtomVM source tree:

    shell$ make monitor
    MONITOR
    --- WARNING: Serial ports accessed as /dev/tty.* will hang gdb if launched.
    --- Using /dev/cu.SLAB_USBtoUART instead...
    --- idf_monitor on /dev/cu.SLAB_USBtoUART 115200 ---
    --- Quit: Ctrl+] | Menu: Ctrl+T | Help: Ctrl+T followed by Ctrl+H ---
    ets Jun  8 2016 00:22:57
    ...

## `blink`

The `blink` example will turn the blue LED on an ESP32 SoC (pin 2) on and off, once every second.

Flash the example program to your device as follows:

    shell$ flash.sh examples/erlang/esp32/blink.avm
    esptool.py v2.6-beta1
    Serial port /dev/tty.SLAB_USBtoUART
    Connecting........_____....._
    Chip is ESP32D0WDQ6 (revision 1)
    Features: WiFi, BT, Dual Core, 240MHz, VRef calibration in efuse, Coding Scheme None
    MAC: 3c:71:bf:84:d9:08
    Uploading stub...
    Running stub...
    Stub running...
    Configuring flash size...
    Auto-detected Flash size: 4MB
    Wrote 32768 bytes at 0x00110000 in 2.9 seconds (91.3 kbit/s)...
    Hash of data verified.

    Leaving...
    Hard resetting via RTS pin...

You should see the blue LED turn on and off on your ESP32 device.

## `sta_network`

The `sta_network` example will connect to your local WiFi network and obtain and IP address.  Once a connection is established, a `connected` message will be displayed.  Once an IP address is obtained, the device IP address, netmask, and gateway will be displayed on the console.

> Note.  AtomVM currently only supports station mode (STA).

> Note.  AtomVM currently only supports IPv4 addresses.

> Note.  You will need to edit the `examples/erlang/esp32/sta_network.erl` source file and set the `ssid` and `psk` parameters to match your local WiFi network, and then rebuild the example.

Flash the example program to your device as follows:

    shell$ flash.sh examples/erlang/esp32/sta_network.avm
    esptool.py v2.6-beta1
    Serial port /dev/tty.SLAB_USBtoUART
    Connecting........_____....._____....._____.....__
    Chip is ESP32D0WDQ6 (revision 1)
    Features: WiFi, BT, Dual Core, 240MHz, VRef calibration in efuse, Coding Scheme None
    MAC: 3c:71:bf:84:d9:08
    Uploading stub...
    Running stub...
    Stub running...
    Configuring flash size...
    Auto-detected Flash size: 4MB
    Wrote 49152 bytes at 0x00110000 in 4.3 seconds (91.3 kbit/s)...
    Hash of data verified.

    Leaving...
    Hard resetting via RTS pin...

You should see the following output when monitoring the ESP32 output (truncated for brevity):

    shell$ make monitor
    MONITOR
    --- WARNING: Serial ports accessed as /dev/tty.* will hang gdb if launched.
    --- Using /dev/cu.SLAB_USBtoUART instead...
    --- idf_monitor on /dev/cu.SLAB_USBtoUART 115200 ---
    --- Quit: Ctrl+] | Menu: Ctrl+T | Help: Ctrl+T followed by Ctrl+H ---
    ets Jun  8 2016 00:22:57
    ...
    Found AVM partition: size: 1048576, address: 0x110000
    Booting file mapped at: 0x3f420000, size: 1048576
    Starting: sta_network.beam...
    ---
    I (220) wifi: wifi driver task: 3ffc3b54, prio:23, stack:3584, core=0
    I (220) wifi: wifi firmware version: d5da5a5
    I (220) wifi: config NVS flash: enabled
    I (220) wifi: config nano formating: disabled
    I (230) system_api: Base MAC address is not set, read default base MAC address from BLK0 of EFUSE
    I (240) system_api: Base MAC address is not set, read default base MAC address from BLK0 of EFUSE
    I (270) wifi: Init dynamic tx buffer num: 32
    I (270) wifi: Init data frame dynamic rx buffer num: 32
    I (270) wifi: Init management frame dynamic rx buffer num: 32
    I (270) wifi: Init static rx buffer size: 1600
    I (280) wifi: Init static rx buffer num: 10
    I (280) wifi: Init dynamic rx buffer num: 32
    I (290) NETWORK: starting wifi: SSID: [myssid], password: [XXXXXXXX].
    I (360) phy: phy_version: 4000, b6198fa, Sep  3 2018, 15:11:06, 0, 0
    I (370) wifi: mode : sta (3c:71:bf:84:d9:08)
    I (370) NETWORK: SYSTEM_EVENT_STA_START received.
    I (490) wifi: n:1 0, o:1 0, ap:255 255, sta:1 0, prof:1
    I (1470) wifi: state: init -> auth (b0)
    I (1480) wifi: state: auth -> assoc (0)
    I (1490) wifi: state: assoc -> run (10)
    I (1500) wifi: connected with myssid, channel 1
    I (1500) wifi: pm start, type: 1

    I (1500) NETWORK: SYSTEM_EVENT_STA_CONNECTED received.
    I (3690) event: sta ip: 192.168.1.236, mask: 255.255.255.0, gw: 192.168.1.1
    I (3690) NETWORK: SYSTEM_EVENT_STA_GOT_IP: 192.168.1.236
    connected
    {{192,168,1,236},{255,255,255,0},{192,168,1,1}}

## `udp_server_blink`

The `udp_server_blink` example will connect to your local WiFi network and obtain and IP address.  It will then start a UDP server on port 44444.  When a UDP message is received, the blue LED on the ESP32 SoC (pin 2) will toggle on and off.

> Note.  AtomVM currently only supports station mode (STA).

> Note.  AtomVM currently only supports IPv4 addresses.

> Note.  You will need to edit the `examples/erlang/esp32/udp_server_blink.erl` source file and set the `ssid` and `psk` parameters to match your local WiFi network, and then rebuild the example.

Flash the example program to your device as follows:

    shell$ flash.sh examples/erlang/esp32/udp_server_blink.avm
    esptool.py v2.6-beta1
    Serial port /dev/tty.SLAB_USBtoUART
    Connecting........_____....._____....._____.....__
    Chip is ESP32D0WDQ6 (revision 1)
    Features: WiFi, BT, Dual Core, 240MHz, VRef calibration in efuse, Coding Scheme None
    MAC: 3c:71:bf:84:d9:08
    Uploading stub...
    Running stub...
    Stub running...
    Configuring flash size...
    Auto-detected Flash size: 4MB
    Wrote 49152 bytes at 0x00110000 in 4.3 seconds (91.3 kbit/s)...
    Hash of data verified.

    Leaving...
    Hard resetting via RTS pin...

You should see the following output when monitoring the ESP32 output (truncated for brevity):

    shell$ make monitor
    MONITOR
    --- WARNING: Serial ports accessed as /dev/tty.* will hang gdb if launched.
    --- Using /dev/cu.SLAB_USBtoUART instead...
    --- idf_monitor on /dev/cu.SLAB_USBtoUART 115200 ---
    --- Quit: Ctrl+] | Menu: Ctrl+T | Help: Ctrl+T followed by Ctrl+H ---
    ets Jun  8 2016 00:22:57
    ...
    Found AVM partition: size: 1048576, address: 0x110000
    Booting file mapped at: 0x3f420000, size: 1048576
    Starting: udp_server_blink.beam...
    ---
    I (222) wifi: wifi driver task: 3ffc3de8, prio:23, stack:3584, core=0
    I (222) wifi: wifi firmware version: d5da5a5
    I (222) wifi: config NVS flash: enabled
    I (232) wifi: config nano formating: disabled
    I (232) system_api: Base MAC address is not set, read default base MAC address from BLK0 of EFUSE
    I (242) system_api: Base MAC address is not set, read default base MAC address from BLK0 of EFUSE
    I (312) wifi: Init dynamic tx buffer num: 32
    I (312) wifi: Init data frame dynamic rx buffer num: 32
    I (312) wifi: Init management frame dynamic rx buffer num: 32
    I (312) wifi: Init static rx buffer size: 1600
    I (322) wifi: Init static rx buffer num: 10
    I (322) wifi: Init dynamic rx buffer num: 32
    I (332) NETWORK: starting wifi: SSID: [myssid], password: [XXXXXXXX].
    I (442) phy: phy_version: 4000, b6198fa, Sep  3 2018, 15:11:06, 0, 0
    I (442) wifi: mode : sta (3c:71:bf:84:d9:08)
    I (442) NETWORK: SYSTEM_EVENT_STA_START received.
    I (572) wifi: n:1 0, o:1 0, ap:255 255, sta:1 0, prof:1
    I (1552) wifi: state: init -> auth (b0)
    I (1552) wifi: state: auth -> assoc (0)
    I (1562) wifi: state: assoc -> run (10)
    I (1582) wifi: connected with myssid, channel 1
    I (1582) wifi: pm start, type: 1

    I (1582) NETWORK: SYSTEM_EVENT_STA_CONNECTED received.
    I (2212) event: sta ip: 192.168.1.236, mask: 255.255.255.0, gw: 192.168.1.1
    I (2212) NETWORK: SYSTEM_EVENT_STA_GOT_IP: 192.168.1.236
    connected
    {{192,168,1,236},{255,255,255,0},{192,168,1,1}}
    Opening socket 44444 ...
    {socket,<0.6.0>,44444}
    Waiting to receive data...

You can send UDP packets to the AtomVM instance using `netcat` (or `nc` on some platforms), in a separate terminal window:

    shell$ nc -u 192.168.1.236 44444

Every time you enter a line of text, the blue LED on the ESP32 SoC (pin 2) should toggle on and off.
