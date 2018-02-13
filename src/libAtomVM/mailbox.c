/***************************************************************************
 *   Copyright 2017 by Davide Bettio <davide@uninstall.it>                 *
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

#include "mailbox.h"
#include "scheduler.h"

void mailbox_send(Context *c, term t)
{
    Message *m = malloc(sizeof(Message));
    m->message = t;
    linkedlist_append(&c->mailbox, &m->mailbox_list_head);

    scheduler_make_ready(c->global, c);
}

term mailbox_receive(Context *c)
{
    Message *m = GET_LIST_ENTRY(c->mailbox, Message, mailbox_list_head);
    linkedlist_remove(&c->mailbox, &m->mailbox_list_head);

    term rt = m->message;

    free(m);

    return rt;
}

term mailbox_peek(Context *c)
{
    Message *m = GET_LIST_ENTRY(c->mailbox, Message, mailbox_list_head);

    return m->message;
}
