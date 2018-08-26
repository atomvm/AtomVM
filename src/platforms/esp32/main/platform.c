/***************************************************************************
 *   Copyright 2018 by Davide Bettio <davide@uninstall.it>                 *
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

#include "context.h"

void consume_gpio_mailbox(Context *ctx);
void consume_network_mailbox(Context *ctx);
void consume_udpdriver_mailbox(Context *ctx);

native_handler platform_open_port(const char *driver_name)
{
    if (!strcmp(driver_name, "gpio")) {
        return consume_gpio_mailbox;
    }else if (!strcmp(driver_name, "network")) {
        return consume_network_mailbox;
    }else if (!strcmp(driver_name, "udp")) {
        return consume_udpdriver_mailbox;
    } else {
        return NULL;
    }
}
