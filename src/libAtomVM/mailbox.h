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

/**
 * @file mailbox.h
 * @brief Mailbox management functions such as send and receive functions.
 *
 * @details Mailbox management functions should be used to send messages to a certain process or port and to receive them.
 */

#ifndef _MAILBOX_H_
#define _MAILBOX_H_

#include "linkedlist.h"
#include "term.h"
#include "context.h"

typedef struct
{
    struct ListHead mailbox_list_head;
    term message;
} Message;

/**
 * @brief Sends a message to a certain mailbox.
 *
 * @details Sends a term to a certain process or port mailbox.
 * @param c the process context.
 * @param t the term that will be sent.
 */
extern void mailbox_send(Context *c, term t);

/**
 * @brief Gets next message from a mailbox.
 *
 * @details Dequeue a term that has been previously queued on a certain process or driver mailbox.
 * @param c the process or driver context.
 * @returns next queued term.
 */
extern term mailbox_receive(Context *c);

/**
 * @brief Gets next message from a mailbox (without removing it).
 *
 * @details Peek the mailbox and retrieve a term that has been previously queued on a certain process or driver mailbox.
 * @param c the process or driver context.
 * @returns peek queued term.
 */
extern term mailbox_peek(Context *c);

#endif
