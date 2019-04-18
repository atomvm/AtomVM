/***************************************************************************
 *   Copyright 2018 by Davide Bettio <davide@uninstall.it>                 *
 *   Copyright 2018 by Fred Dushin <fred@dushin.net>                       *
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

#include "network_driver.h"
#include "port.h"

const char *connected_a  = "\xD" "sta_connected";
const char *sta_got_ip_a = "\xA" "sta_got_ip";

static term create_dummy_ip_info(Context *ctx)
{
    // {{192,168,1,236}, {255,255,255,0}, {192,168,1,1}}
    term ip = term_alloc_tuple(4, ctx);
    term_put_tuple_element(ip, 0, term_from_int32(192));
    term_put_tuple_element(ip, 1, term_from_int32(168));
    term_put_tuple_element(ip, 2, term_from_int32(1));
    term_put_tuple_element(ip, 3, term_from_int32(236));

    term netmask = term_alloc_tuple(4, ctx);
    term_put_tuple_element(netmask, 0, term_from_int32(255));
    term_put_tuple_element(netmask, 1, term_from_int32(255));
    term_put_tuple_element(netmask, 2, term_from_int32(255));
    term_put_tuple_element(netmask, 3, term_from_int32(0));

    term gateway = term_alloc_tuple(4, ctx);
    term_put_tuple_element(gateway, 0, term_from_int32(192));
    term_put_tuple_element(gateway, 1, term_from_int32(168));
    term_put_tuple_element(gateway, 2, term_from_int32(1));
    term_put_tuple_element(gateway, 3, term_from_int32(1));

    term ret = term_alloc_tuple(3, ctx);
    term_put_tuple_element(ret, 0, ip);
    term_put_tuple_element(ret, 1, netmask);
    term_put_tuple_element(ret, 2, gateway);

    return ret;
}

void network_driver_start(Context *ctx, term_ref pid, term_ref ref, term config)
{
    UNUSED(config);

    port_ensure_available(ctx, 24);

    // ok
    port_send_reply(ctx, pid, ref, OK_ATOM);

    // sta_connected
    port_send_reply(ctx, pid, ref, context_make_atom(ctx, connected_a));

    // {sta_got_ip, IpInfo}
    term sta_got_ip_tuple = term_alloc_tuple(2, ctx);
    term_put_tuple_element(sta_got_ip_tuple, 0, context_make_atom(ctx, sta_got_ip_a));
    term_put_tuple_element(sta_got_ip_tuple, 1, create_dummy_ip_info(ctx));
    port_send_reply(ctx, pid, ref, sta_got_ip_tuple);
}

term network_driver_ifconfig(Context *ctx)
{
    return port_create_error_tuple(ctx, UNDEFINED_ATOM);
}
