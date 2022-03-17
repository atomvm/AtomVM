<!--
 Copyright 2019 Fred Dushin <fred@dushin.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Example Programs

AtomVM includes a collection of useful examples for getting started.  This section describes what these examples do, and how to run them, for example, on an ESP32 device.

## Erlang Examples

Erlang examples may be run in the UNIX shell or on supported microcontroller devices.

### `hello_world`

This example program prints the string "Hello World" and quits.

#### Command line

The `hello_world.avm` file will get created as part of a build.  This file may be supplied as an argument to the `AtomVM` command:

    shell$ ./src/AtomVM ./examples/erlang/hello_world.avm
    Hello World
    Return value: ok

### `udp_server`

This example program listens on UDP port 44444 and will print information about the received message, including the source IP, (ephemeral) source port, and packet received, to the console.

#### Command line

The `udp_server.avm` file will get created as part of a build.  This file may be supplied as an argument to the `AtomVM` command:

    shell$ ./src/AtomVM ./examples/erlang/udp_server.avm
    Opened UDP socket on "0.0.0.0:44404".
    Waiting to receive data...

You can send UDP packets to the AtomVM instance using `netcat` (or `nc` on some platforms), in a separate terminal window:

    shell$ nc -u localhost 44404

This command will wait for you to enter a line of text, e.g.,

    testing 1 2 3

In the AtomVM termianl window, you see:

    Received UDP packet <<116,101,115,116,105,110,103,32,49,32,50,32,51,10>> from "127.0.0.1:55261"
    Waiting to receive data...

> Note.  Netcat appends a newline character at the end of the input, so the packet binary does not display as printable text.

### `udp_client`

This example program send the packet of data (":アトムＶＭ") over UDP to port 44444 on the loopback address every 5 seconds, in a loop.  The program will print a period (`.`) to the console, every time it sends a message.

This command may be used in tandem with the `udp_server` program to illustrate sending messages between AtomVM processes over UDP.

#### Command line

The `udp_client.avm` file will get created as part of a build.  This file may be supplied as an argument to the `AtomVM` command:

    shell$ ./src/AtomVM ./examples/erlang/udp_client.avm
    Opened UDP socket on "0.0.0.0:63665".
    Sent <<58,-94,-56,-32,54,45>>
    Sent <<58,-94,-56,-32,54,45>>
    Sent <<58,-94,-56,-32,54,45>>
    ...

If you are running the `udp_server` program, you should see messages like the following printed to the console:

    Received UDP packet <<58,-94,-56,-32,54,45>> from "127.0.0.1:63665"
    Waiting to receive data...
    Received UDP packet <<58,-94,-56,-32,54,45>> from "127.0.0.1:63665"
    Waiting to receive data...
    Received UDP packet <<58,-94,-56,-32,54,45>> from "127.0.0.1:63665"
    Waiting to receive data...
    ...

> Note. AtomVM does not currently treat characters outside of the printable ASCII character set as printable characters.

### `tcp_server`

This example program listens on TCP port 44404 and accept connections on that port.  Once accepted, it will wait for packets to be sent from the client.  Once received, the server will print the packet received to the console, and then echo the packet back to the calling client.

#### Command line

The `tcp_server.avm` file will get created as part of a build.  This file may be supplied as an argument to the `AtomVM` command:

    shell$ ./src/AtomVM ./examples/erlang/tcp_server.avm
    Listening on "0.0.0.0:44404".
    Waiting to accept connection...

You can send TCP packets to the AtomVM instance using `netcat` (or `nc` on some platforms), in a separate terminal window:

    shell$ nc localhost 44404

This will open a TCP connection to the `tcp_server`, and you should see the following on the console:

    Accepted connection.  local: "127.0.0.1:44404" peer: "127.0.0.1:56628"
    Waiting to receive data...
    Waiting to accept connection...

The netcat command will wait for you to enter a line of text, e.g.,

    testing 1 2 3

In the AtomVM terminal window, you see:

    Received packet [116,101,115,116,105,110,103,32,49,32,50,32,51,10] from "127.0.0.1:56628".  Echoing back...
    Waiting to receive data...

> Note.  Netcat appends a newline character at the end of the input, so the packet binary does not display as printable text.

You may enter as much data as you like, though by default, the packet size will be limited to 128 bytes.

If you stop the `netcat` command (via ^C), you should

    Connection closed.

printed to the AtomVM console.

Note that you can have multiple, concurrent TCP/IP connections to your AtomVM server.

### `tcp_client`

This example program send the packet of data (":アトムＶＭ") over TCP to port 44404 on the loopback address every 1 second, in a loop.  The program will wait for a response from the server, before proceeding.

This command may be used in tandem with the `tcp_server` program to illustrate sending messages between AtomVM processes over TCP.

> Note.  You will need to change the `Address` variable in the `tcp_client.erl` program in order to test against an AtomVM server running on a different host or device.

#### Command line

The `tcp_client.avm` file will get created as part of a build.  This file may be supplied as an argument to the `AtomVM` command:

    shell$ ./src/AtomVM ./examples/erlang/tcp_client.avm
    Connected to "127.0.0.1:44404" from "127.0.0.1:56741"
    Sent <<58,-94,-56,-32,54,45>> to "127.0.0.1:44404"
    Received [58,162,200,224,54,45] from "127.0.0.1:44404"
    Sent <<58,-94,-56,-32,54,45>> to "127.0.0.1:44404"
    Received [58,162,200,224,54,45] from "127.0.0.1:44404"
    Sent <<58,-94,-56,-32,54,45>> to "127.0.0.1:44404"
    Received [58,162,200,224,54,45] from "127.0.0.1:44404"
    ...

If you are running the `tcp
_server` program, you should see messages like the following printed to the console:

    Accepted connection.  local: "127.0.0.1:44404" peer: "127.0.0.1:56741"
    Waiting to receive data...
    Waiting to accept connection...
    Received packet [58,162,200,224,54,45] from "127.0.0.1:56741".  Echoing back...
    Waiting to receive data...
    Received packet [58,162,200,224,54,45] from "127.0.0.1:56741".  Echoing back...
    Waiting to receive data...
    Received packet [58,162,200,224,54,45] from "127.0.0.1:56741".  Echoing back...
    Waiting to receive data...
    ...

> Note. AtomVM does not currently treat characters outside of the printable ASCII character set as printable characters.

You may run multiple concurrent instances of the `tcp_client` against a single `tcp_server` instance.

## ESP32 Examples

AtomVM includes examples that are specifically designed for the ESP32 and other microcontrollers.

## Flashing AtomVM Examples for ESP32

In order to run the ESP32 examples, you will need to flash the example AVM files that are created as part of the build to your device.

In the remainder of this document, we assume the `flash.sh` script, located in the `tools/dev` directory of the AtomVM source tree.

> Note. You must set the `ESP_IDF` environment variable to the root directory of the ESP IDF SDK installation on your development machine.

You can control the serial port and baud rate via the `FLASH_SERIAL_PORT` and `FLASH_BAUD_RATE` environment variables, e.g.,

    shell$ export FLASH_SERIAL_PORT="/dev/tty.SLAB_USBtoUART"
    shell$ export FLASH_BAUD_RATE=921600

The default values for these variables, if not set, are `/dev/ttyUSB0` and `115200`, respectively.

> Note. Experiment with baud rates (e.g., 921600).  You may find you can shorten the flash-debug-flash cycle with higher rates.

You can montor the console output of these examples by issuing the `monitor` target to `make`, in the `src/platforms/esp32` directory of the AtomVM source tree:

    shell$ make monitor
    MONITOR
    --- WARNING: Serial ports accessed as /dev/tty.* will hang gdb if launched.
    --- Using /dev/cu.SLAB_USBtoUART instead...
    --- idf_monitor on /dev/cu.SLAB_USBtoUART 115200 ---
    --- Quit: Ctrl+] | Menu: Ctrl+T | Help: Ctrl+T followed by Ctrl+H ---
    ets Jun  8 2016 00:22:57
    ...

### `blink`

The `blink` example will turn the blue LED on an ESP32 SoC (pin 2) on and off, once every second.

Flash the example program to your device as follows:

    shell$ ./tools/dev/flash.sh build/examples/erlang/esp32/blink.avm
    ...
    Hard resetting via RTS pin...

You should see the blue LED turn on and off on your ESP32 device.

### `esp_random`

This demo program illustrates use of the ESP32 `random`, `restart`, and `reset_reason` functions.

The program will generate a random binary of a random size (at most 127 bytes) every 5 seconds.  If a 0-length byte sequence is generated (1:128 probability), the ESP will restart.

Flash the example program to your device as follows:

    shell$ ./tools/dev/flash.sh build/examples/erlang/esp32/esp_random.avm
    ...
    Hard resetting via RTS pin...

You should see something like the following output when monitoring the ESP32 output (truncated for brevity):

    shell$ make monitor
    ...
    Found AVM partition: size: 1048576, address: 0x110000
    Booting file mapped at: 0x3f420000, size: 1048576
    Starting: esp_random.beam...
    ---
    Reset reason: esp_rst_poweron
    Random bytes: <<71,13,221,24,8,15,...,197,120,152,205>>
    Random bytes: <<37,155,124,177,44,141,40,106,...,43,48,62,109,2,78,39,107>>
    Random bytes: <<217,210,239,183,...,78,68,253,146,212,71,17,208,219,126,240,218,34,0,152,80,20,166,194,106>>
    Random bytes: <<112,77,123,249,162,...,238,237,128,227,58,29,64,74>>
    ...
    <<"">>
    ets Jun  8 2016 00:22:57
    ...
    Found AVM partition: size: 1048576, address: 0x110000
    Booting file mapped at: 0x3f420000, size: 1048576
    Starting: esp_random.beam...
    ---
    esp_rst_sw
    Random bytes: <<155,174,204,143,232,202,136,...,118,177,77,230,10,21,72,91,92,160,198,115,249,217,206,52,102,32,230>>
    ...

### `esp_nvs`

This demo program illustrates the use of ESP32 non-volatile storage (NVS).

The program will store the number of times the device has been rebooted, along with the start time, in NVS.

Flash the example program to your device as follows:

    shell$ ./tools/dev/flash.sh build/examples/erlang/esp32/esp_nvs.avm
    ...
    Hard resetting via RTS pin...

You should see the following output when monitoring the ESP32 output (truncated for brevity):

    shell$ make monitor
    ...
    Found AVM partition: size: 1048576, address: 0x110000
    Booting file mapped at: 0x3f420000, size: 1048576
    Starting: esp_nvs.beam...
    ---
    Saving count 0 to NVS...
    Reset device to increment.
    AtomVM finished with return value = ok
    going to sleep forever..

Hit the reset button on your device, and the ESP device will reboot, and display something like the following:

    Found AVM partition: size: 1048576, address: 0x110000
    Booting file mapped at: 0x3f420000, size: 1048576
    Starting: esp_nvs.beam...
    ---
    Saving count 1 to NVS...
    Reset device to increment.
    AtomVM finished with return value = ok
    going to sleep forever..

### `reformat_nvs`

This demo program will reformat the non-volatile storage (NVS) partition.

Flash the example program to your device as follows:

    shell$ ./tools/dev/flash.sh build/examples/erlang/esp32/reformat_nvs.avm
    ...
    Hard resetting via RTS pin...

You should see the following output when monitoring the ESP32 output (truncated for brevity):

    shell$ make monitor
    ...
    Found AVM partition: size: 1048576, address: 0x110000
    Booting file mapped at: 0x3f420000, size: 1048576
    Starting: esp_nvs.beam...
    ---
    Warning: Reformatted NVS partition!
    AtomVM finished with return value = ok
    going to sleep forever..

The NVS partition on your ESP device should be reformatted.

> Note.  This program will irrevocably delete all existing key-values stored on the NVS partition.  Use with caution.

### `set_network_config`

This demo program can be used to set the WIFI credentials in NVS.  Setting WIFI credentials in NVS can greatly simplify the task of running ESP programs that require connectivity to WIFI networks.

> Note.  Credentials are stored unencrypted and in plaintext and should not be considered secure.  Future versions may use encrypted NVS storage.

Edit the `sta_network_config.erl` program and set the `Ssid` binary with your WIFI AP SSID, and `Psk` binary with the password used to access your WIFI network.  Save the file, rebuild, and flash to your device:

    shell$ make
    ...
    shell$ ./tools/dev/flash.sh build/examples/erlang/esp32/sta_network_config.avm
    ...
    Hard resetting via RTS pin...

You should see the following output when monitoring the ESP32 output (truncated for brevity):

    shell$ make monitor
    ...
    Found AVM partition: size: 1048576, address: 0x110000
    Booting file mapped at: 0x3f420000, size: 1048576
    Starting: set_network_config.beam...
    ---
    {atomvm,sta_ssid,<<"myssid">>}
    {atomvm,sta_psk,<<"xxxxxx">>}
    AtomVM finished with return value = ok
    going to sleep forever..

You may now run programs that use your WIFI network (see below) without needing to enter WIFI credentials.

### `sta_network`

The `sta_network` example will connect to your local WiFi network and obtain and IP address.  Once a connection is established, a `connected` message will be displayed.  Once an IP address is obtained, the device IP address, netmask, and gateway will be displayed on the console.

> Note.  AtomVM currently only supports station mode (STA).

> Note.  AtomVM currently only supports IPv4 addresses.

> Note.  If you have not set WIFI credentials in NVS (see above), you will need to edit the `examples/erlang/esp32/sta_network.erl` source file and set the `ssid` and `psk` parameters to match your local WiFi network, and then rebuild the example.

Flash the example program to your device as follows:

    shell$ ./tools/dev/flash.sh build/examples/erlang/esp32/sta_network.avm
    ...
    Hard resetting via RTS pin...

You should see the following output when monitoring the ESP32 output (truncated for brevity):

    shell$ make monitor
    ...
    Starting: sta_network.beam...
    ---
    I (220) wifi: wifi driver task: 3ffc3b54, prio:23, stack:3584, core=0
    I (220) wifi: wifi firmware version: d5da5a5
    I (220) wifi: config NVS flash: enabled
    I (220) wifi: config nano formatting: disabled
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

### `udp_server_blink`

The `udp_server_blink` example will connect to your local WiFi network and obtain and IP address.  It will then start a UDP server on port 44444.  When a UDP message is received, the blue LED on the ESP32 SoC (pin 2) will toggle on and off.

> Note.  AtomVM currently only supports station mode (STA).

> Note.  AtomVM currently only supports IPv4 addresses.

> Note.  You will need to edit the `examples/erlang/esp32/udp_server_blink.erl` source file and set the `ssid` and `psk` parameters to match your local WiFi network, and then rebuild the example.

Flash the example program to your device as follows:

    shell$ ./tools/dev/flash.sh build/examples/erlang/esp32/udp_server_blink.avm
    ...
    Hard resetting via RTS pin...

You should see the following output when monitoring the ESP32 output (truncated for brevity):

    shell$ make monitor
    ...
    Found AVM partition: size: 1048576, address: 0x110000
    Booting file mapped at: 0x3f420000, size: 1048576
    Starting: udp_server_blink.beam...
    ---
    I (222) wifi: wifi driver task: 3ffc3de8, prio:23, stack:3584, core=0
    I (222) wifi: wifi firmware version: d5da5a5
    I (222) wifi: config NVS flash: enabled
    I (232) wifi: config nano formatting: disabled
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
    Acquired IP address: "192.168.1.236" Netmask: "255.255.255.0" Gateway: "192.168.1.1"
    Opened UDP socket on "0.0.0.0:44404".
    Waiting to receive data...

You can send UDP packets to the AtomVM instance using `netcat` (or `nc` on some platforms), in a separate terminal window:

    shell$ nc -u 192.168.1.236 44404

Every time you enter a line of text, the blue LED on the ESP32 SoC (pin 2) should toggle on and off, and you should see output on the console, such as

    Received UDP packet <<100,115,102,115,100,10>> from "192.168.1.237:53291"
    Waiting to receive data...

### `tcp_server_blink`

The `tcp_server_blink` example will connect to your local WiFi network and obtain and IP address.  It will then start a TCP server on port 44404.  When a TCP message is received, the blue LED on the ESP32 SoC (pin 2) will toggle on and off.

> Note.  AtomVM currently only supports station mode (STA).

> Note.  AtomVM currently only supports IPv4 addresses.

> Note.  You will need to edit the `examples/erlang/esp32/tcp_server_blink.erl` source file and set the `ssid` and `psk` parameters to match your local WiFi network, and then rebuild the example.

Flash the example program to your device as follows:

    shell$ ./tools/dev/flash.sh build/examples/erlang/esp32/tcp_server_blink.avm
    ...
    Hard resetting via RTS pin...

You should see the following output when monitoring the ESP32 output (truncated for brevity):

    shell$ make monitor
    Found AVM partition: size: 1048576, address: 0x110000
    Booting file mapped at: 0x3f420000, size: 1048576
    Starting: tcp_server_blink.beam...
    ---
    start
    I (296) wifi: wifi driver task: 3ffc650c, prio:23, stack:3584, core=0
    I (296) wifi: wifi firmware version: 9415913
    I (296) wifi: config NVS flash: enabled
    I (296) wifi: config nano formatting: disabled
    I (296) system_api: Base MAC address is not set, read default base MAC address from BLK0 of EFUSE
    I (306) system_api: Base MAC address is not set, read default base MAC address from BLK0 of EFUSE
    I (346) wifi: Init dynamic tx buffer num: 32
    I (346) wifi: Init data frame dynamic rx buffer num: 32
    I (346) wifi: Init management frame dynamic rx buffer num: 32
    I (346) wifi: Init static rx buffer size: 1600
    I (356) wifi: Init static rx buffer num: 10
    I (356) wifi: Init dynamic rx buffer num: 32
    I (366) NETWORK: starting wifi: SSID: [myssid], password: [XXXXXXXX].
    I (446) phy: phy_version: 4008, c9ae59f, Jan 25 2019, 16:54:06, 0, 0
    I (446) wifi: mode : sta (3c:71:bf:84:d9:08)
    I (446) NETWORK: SYSTEM_EVENT_STA_START received.
    I (1176) wifi: n:6 0, o:1 0, ap:255 255, sta:6 0, prof:1
    I (2156) wifi: state: init -> auth (b0)
    I (2166) wifi: state: auth -> assoc (0)
    I (2166) wifi: state: assoc -> run (10)
    I (2196) wifi: connected with myssid, channel 6
    I (2196) wifi: pm start, type: 1
    I (2196) NETWORK: SYSTEM_EVENT_STA_CONNECTED received.
    I (2746) event: sta ip: 192.168.1.236, mask: 255.255.255.0, gw: 192.168.1.1
    I (2746) NETWORK: SYSTEM_EVENT_STA_GOT_IP: 192.168.1.236
    Acquired IP address: "192.168.1.236" Netmask: "255.255.255.0" Gateway: "192.168.1.1"
    Listening on "0.0.0.0:44404".
    Waiting to accept connection...

You can send TCP packets to the AtomVM instance using `netcat` (or `nc` on some platforms), in a separate terminal window, e.g.,

    shell$ nc 192.168.1.236 44404

On the ESP32 console, you should see:

    Accepted connection.  local: "192.168.1.236:44404" peer: "192.168.1.237:55275"
    Waiting to receive data...
    Waiting to accept connection...

Every time you enter a line of text, the blue LED on the ESP32 SoC (pin 2) should toggle on and off, and the data you entered should get echoed back to the `netcat` console.

On the ESP32 console, you should see:

    Received packet [115,100,102,115,100,102,10] from "192.168.1.237:55275".  Echoing back...
    Waiting to receive data...

every time a packet is sent to the server.
