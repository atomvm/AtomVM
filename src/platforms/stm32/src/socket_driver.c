/***************************************************************************
 *   Copyright 2018 by Riccardo Binetti <rbino@gmx.com>                    *
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

#include "socket.h"
#include "socket_driver.h"
#include "port.h"

// TODO: everything is unimplemented

void *socket_driver_create_data()
{
    return NULL;
}

void socket_driver_delete_data(void *data)
{
}

term_ref socket_driver_do_init(CContext *cc, term params)
{
    return port_create_error_tuple(cc, "unimplemented");
}

term_ref socket_driver_do_bind(CContext *cc, term address, term port)
{
    return port_create_error_tuple(cc, "unimplemented");
}

term_ref socket_driver_do_send(CContext *cc, term dest_address, term dest_port, term buffer)
{
    return port_create_error_tuple(cc, "unimplemented");
}

void socket_driver_do_recvfrom(CContext *cc, term_ref pid, term_ref ref)
{
    port_send_reply(
        cc, pid, ref,
        port_create_error_tuple(cc, "unimplemented")
    );
}
