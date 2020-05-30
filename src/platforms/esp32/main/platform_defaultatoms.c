/***************************************************************************
 *   Copyright 2019 by Davide Bettio <davide@uninstall.it>                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#include "platform_defaultatoms.h"

static const char *const set_level_atom = "\x9" "set_level";
static const char *const read_atom = "\x4" "read";
static const char *const input_atom = "\x5" "input";
static const char *const output_atom = "\x6" "output";
static const char *const set_direction_atom ="\xD" "set_direction";
static const char *const set_int_atom = "\x7" "set_int";
static const char *const gpio_interrupt_atom = "\xE" "gpio_interrupt";
static const char *const none_atom = "\x4" "none";
static const char *const rising_atom = "\x6" "rising";
static const char *const falling_atom = "\x7" "falling";
static const char *const both_atom = "\x4" "both";
static const char *const low_atom = "\x3" "low";
static const char *const high_atom = "\x4" "high";
static const char *const esp32_atom = "\x5" "esp32";

static const char *const proto_atom = "\x5" "proto";
static const char *const udp_atom = "\x3" "udp";
static const char *const tcp_atom = "\x3" "tcp";
static const char *const socket_atom = "\x6" "socket";
static const char *const fcntl_atom = "\x5" "fcntl";
static const char *const bind_atom = "\x4" "bind";
static const char *const getsockname_atom = "\xB" "getsockname";
static const char *const recvfrom_atom = "\x8" "recvfrom";
static const char *const sendto_atom = "\x6" "sendto";
static const char *const address_atom = "\x7" "address";
static const char *const port_atom = "\x4" "port";
static const char *const controlling_process_atom = "\x13" "controlling_process";
static const char *const binary_atom = "\x6" "binary";
static const char *const active_atom = "\x6" "active";
static const char *const buffer_atom = "\x6" "buffer";
static const char *const connect_atom = "\x7" "connect";
static const char *const send_atom = "\x4" "send";
static const char *const tcp_closed_atom = "\xA" "tcp_closed";
static const char *const recv_atom = "\x4" "recv";
static const char *const listen_atom = "\x6" "listen";
static const char *const backlog_atom = "\x7" "backlog";
static const char *const accept_atom = "\x6" "accept";
static const char *const fd_atom = "\x2" "fd";

static const char *const init_atom = "\x4" "init";
static const char *const close_atom = "\x5" "close";
static const char *const get_port_atom = "\x8" "get_port";
static const char *const sockname_atom = "\x8" "sockname";
static const char *const peername_atom = "\x8" "peername";

static const char *const sta_atom = "\x3" "sta";
static const char *const ssid_atom = "\x4" "ssid";
static const char *const psk_atom = "\x3" "psk";
static const char *const sntp_atom = "\x4" "sntp";
static const char *const sta_got_ip_atom = "\xA" "sta_got_ip";
static const char *const sta_connected_atom = "\xD" "sta_connected";
static const char *const sta_disconnected_atom = "\x10" "sta_disconnected";
static const char *const sta_dhcp_hostname_atom = "\xD" "dhcp_hostname";

//spidriver
static const char *const bus_config_atom = "\xA" "bus_config";
static const char *const miso_io_num_atom = "\xB" "miso_io_num";
static const char *const mosi_io_num_atom = "\xB" "mosi_io_num";
static const char *const sclk_io_num_atom = "\xB" "sclk_io_num";
static const char *const device_config_atom = "\xD" "device_config";
static const char *const spi_clock_hz_atom = "\xC" "spi_clock_hz";
static const char *const spi_mode_atom = "\x8" "spi_mode";
static const char *const spi_cs_io_num_atom = "\xD" "spi_cs_io_num";
static const char *const address_len_bits_atom = "\x10" "address_len_bits";
static const char *const read_at_atom = "\x7" "read_at";
static const char *const write_at_atom = "\x8" "write_at";

//i2cdriver
static const char *const begin_transmission_atom = "\x12" "begin_transmission";
static const char *const end_transmission_atom = "\x10" "end_transmission";
static const char *const write_byte_atom = "\xA" "write_byte";
static const char *const read_bytes_atom = "\xA" "read_bytes";
static const char *const scl_io_num_atom = "\xA" "scl_io_num";
static const char *const sda_io_num_atom = "\xA" "sda_io_num";
static const char *const i2c_clock_hz_atom = "\xC" "i2c_clock_hz";

//uart
static const char *const name_atom = "\x4" "name";
static const char *const speed_atom = "\x5" "speed";
static const char *const write_atom = "\x5" "write";
static const char *const data_bits_atom = "\x9" "data_bits";
static const char *const stop_bits_atom = "\x9" "stop_bits";
static const char *const flow_control_atom = "\xC" "flow_control";
static const char *const hardware_atom = "\x8" "hardware";
static const char *const software_atom = "\x8" "software";
static const char *const parity_atom = "\x6" "parity";
static const char *const even_atom = "\x4" "even";
static const char *const odd_atom = "\x3" "odd";

void platform_defaultatoms_init(GlobalContext *glb)
{
    int ok = 1;

    ok &= globalcontext_insert_atom(glb, set_level_atom) == SET_LEVEL_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, read_atom) == READ_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, input_atom) == INPUT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, output_atom) == OUTPUT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, set_direction_atom) == SET_DIRECTION_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, set_int_atom) == SET_INT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, gpio_interrupt_atom) == GPIO_INTERRUPT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, none_atom) == NONE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, rising_atom) == RISING_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, falling_atom) == FALLING_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, both_atom) == BOTH_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, low_atom) == LOW_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, high_atom) == HIGH_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, esp32_atom) == ESP32_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, proto_atom) == PROTO_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, udp_atom) == UDP_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, tcp_atom) == TCP_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, socket_atom) == SOCKET_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, fcntl_atom) == FCNTL_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, bind_atom) == BIND_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, getsockname_atom) == GETSOCKNAME_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, recvfrom_atom) == RECVFROM_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sendto_atom) == SENDTO_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, address_atom) == ADDRESS_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, port_atom) == PORT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, controlling_process_atom) == CONTROLLING_PROCESS_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, binary_atom) == BINARY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, active_atom) == ACTIVE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, buffer_atom) == BUFFER_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, connect_atom) == CONNECT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, send_atom) == SEND_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, tcp_closed_atom) == TCP_CLOSED_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, recv_atom) == RECV_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, listen_atom) == LISTEN_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, backlog_atom) == BACKLOG_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, accept_atom) == ACCEPT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, fd_atom) == FD_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, init_atom) == INIT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, close_atom) == CLOSE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, get_port_atom) == GET_PORT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sockname_atom) == SOCKNAME_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, peername_atom) == PEERNAME_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, sta_atom) == STA_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, ssid_atom) == SSID_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, psk_atom) == PSK_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sntp_atom) == SNTP_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sta_got_ip_atom) == STA_GOT_IP_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sta_connected_atom) == STA_CONNECTED_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sta_disconnected_atom) == STA_DISCONNECTED_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sta_dhcp_hostname_atom) == STA_DHCP_HOSTNAME_INDEX;

    //spidriver
    ok &= globalcontext_insert_atom(glb, bus_config_atom) == BUS_CONFIG_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, miso_io_num_atom) == MISO_IO_NUM_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, mosi_io_num_atom) == MOSI_IO_NUM_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sclk_io_num_atom) == SCLK_IO_NUM_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, device_config_atom) == DEVICE_CONFIG_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, spi_clock_hz_atom) == SPI_CLOCK_HZ_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, spi_mode_atom) == SPI_MODE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, spi_cs_io_num_atom) == SPI_CS_IO_NUM_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, address_len_bits_atom) == ADDRESS_LEN_BITS_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, read_at_atom) == READ_AT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, write_at_atom) == WRITE_AT_ATOM_INDEX;

    //i2cdriver
    ok &= globalcontext_insert_atom(glb, begin_transmission_atom) == BEGIN_TRANSMISSION_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, end_transmission_atom) == END_TRANSMISSION_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, write_byte_atom) == WRITE_BYTE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, read_bytes_atom) == READ_BYTES_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, scl_io_num_atom) == SCL_IO_NUM_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sda_io_num_atom) == SDA_IO_NUM_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, i2c_clock_hz_atom) == I2C_CLOCK_HZ_ATOM_INDEX;

    //uart
    ok &= globalcontext_insert_atom(glb, name_atom) == NAME_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, speed_atom) == SPEED_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, write_atom) == WRITE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, data_bits_atom) == DATA_BITS_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, stop_bits_atom) == STOP_BITS_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, flow_control_atom) == FLOW_CONTROL_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, hardware_atom) == HARDWARE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, software_atom) == SOFTWARE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, parity_atom) == PARITY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, even_atom) == EVEN_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, odd_atom) == ODD_ATOM_INDEX;


    if (!ok) {
        abort();
    }
}
