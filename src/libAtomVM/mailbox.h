/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

/**
 * @file mailbox.h
 * @brief Mailbox management functions such as send and receive functions.
 *
 * @details Mailbox management functions should be used to send messages to a certain process or port and to receive them.
 */

#ifndef _MAILBOX_H_
#define _MAILBOX_H_

#include "context.h"
#include "list.h"
#include "term.h"

typedef struct
{
    struct ListHead mailbox_list_head;
    int msg_memory_size;
    term mso_list;
    term message; // must be declared last
} Message;

/**
 * @brief Sends a message to a certain mailbox.
 *
 * @details Sends a term to a certain process or port mailbox.
 * @param c the process context.
 * @param t the term that will be sent.
 */
void mailbox_send(Context *c, term t);

/**
 * @brief Gets next message from a mailbox.
 *
 * @details Dequeue a term that has been previously queued on a certain process or driver mailbox.
 * @param c the process or driver context.
 * @returns next queued term.
 */
term mailbox_receive(Context *c);

/**
 * @brief Dequeue next message struct from mailbox.
 *
 * @details Dequeue a message that has been previously queued on a certain process or driver mailbox.
 * @param c the process or driver context.
 * @returns dequeued message, the caller must free() the message.
 */
Message *mailbox_dequeue(Context *c);

/**
 * @brief Gets next message from a mailbox (without removing it).
 *
 * @details Peek the mailbox and retrieve a term that has been previously queued on a certain process or driver mailbox.
 * @param c the process or driver context.
 * @returns peek queued term.
 */
term mailbox_peek(Context *c);

/**
 * @brief Remove next message from mailbox.
 *
 * @details Discard a term that has been previously queued on a certain process or driver mailbox.
 * @param c the process or driver context.
 */
void mailbox_remove(Context *c);

/**
 * @brief Free memory associated with a mailbox message.
 *
 * @details The supplied message will be free'd, and
 * to any references to shared memory will decrement
 * reference counts.
 * @param m the message to free.
 */
void mailbox_destroy_message(Message *m);

#endif
