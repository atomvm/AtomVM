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
 * @details Mailbox management functions should be used to send messages to a certain process or
 * port and to receive them.
 */

#ifndef _MAILBOX_H_
#define _MAILBOX_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>

#include "list.h"
#include "smp.h"
#include "term_typedef.h"

struct Context;

#ifndef TYPEDEF_CONTEXT
#define TYPEDEF_CONTEXT
typedef struct Context Context;
#endif

struct Heap;

#ifndef TYPEDEF_HEAP
#define TYPEDEF_HEAP
typedef struct Heap Heap;
#endif

typedef struct Message Message;
typedef struct MailboxMessage MailboxMessage;

enum MessageType
{
    NormalMessage,
    KillSignal,
    GCSignal,
    ProcessInfoRequestSignal,
    TrapAnswerSignal,
    TrapExceptionSignal,
};

struct MailboxMessage
{
    MailboxMessage *next;
    // Make sure MailboxMessage structure matches HeapFragment
    union
    {
        enum MessageType type;
        term *heap_fragment_end;
    };
};

struct Message
{
    MailboxMessage base;

    term message;
    term *heap_end;
    term storage[];
};

struct TermSignal
{
    MailboxMessage base;

    term signal_term;
    term *heap_end;
    term storage[];
};

struct BuiltInAtomSignal
{
    MailboxMessage base;

    term atom;
};

struct BuiltInAtomRequestSignal
{
    MailboxMessage base;

    int32_t sender_pid;
    term atom;
};

typedef struct
{
    // Outer list is inserted into by other processes and protected by a CAS
    // Owner of mailbox perform deletion of all items (reversing the list into
    // the inner list)
    MailboxMessage *ATOMIC outer_first;
    MailboxMessage *inner_first;
    MailboxMessage *inner_last;
    // Receive pointers are on inner list items.
    MailboxMessage *receive_pointer;
    MailboxMessage *receive_pointer_prev;
} Mailbox;

// TODO: a lot of this code depends on Context * and should be decoupled
// where possible.

/**
 * @brief Initialize the mailbox
 *
 * @param mbx the mailbox to initialize.
 */
void mailbox_init(Mailbox *mbox);

/**
 * @brief Compute the mailbox length, in messages.
 *
 * @details To be called from the process only.
 * @param mbox the mailbox to get the length of.
 */
size_t mailbox_len(Mailbox *mbox);

/**
 * @brief Compute the mailbox size, in bytes.
 *
 * @details To be called from the process only.
 * @param mbox the mailbox to get the size of.
 */
size_t mailbox_size(Mailbox *mbox);

/**
 * @brief Process the outer list of messages.
 *
 * @details To be called from the process only
 * @param mbox the mailbox to work with
 * @return the signal messages in received order.
 */
MailboxMessage *mailbox_process_outer_list(Mailbox *mbox);

/**
 * @brief Sends a message to a certain mailbox.
 *
 * @details Sends a term to a certain process or port mailbox. Can be called
 * from another process.
 * @param c the process context.
 * @param t the term that will be sent.
 */
void mailbox_send(Context *c, term t);

/**
 * @brief Sends a term-based signal to a certain mailbox.
 *
 * @param c the process context.
 * @param type the type of the signal
 * @param t the term added to the message
 */
void mailbox_send_term_signal(Context *c, enum MessageType type, term t);

/**
 * @brief Sends a built-in atom signal to a certain mailbox.
 *
 * @param c the process context.
 * @param type the type of the signal
 * @param atom the built-in atom
 */
void mailbox_send_built_in_atom_signal(Context *c, enum MessageType type, term atom);

/**
 * @brief Sends a built-in atom-based request signal to a certain mailbox.
 *
 * @param c the process context.
 * @param type the type of the signal
 * @param sender_pid the sender of the signal (to get the answer)
 * @param atom the built-in atom
 */
void mailbox_send_built_in_atom_request_signal(
    Context *c, enum MessageType type, int32_t sender_pid, term atom);

/**
 * @brief Sends an empty body signal to a certain mailbox.
 *
 * @param c the process context.
 * @param type the type of the signal
 */
void mailbox_send_empty_body_signal(Context *c, enum MessageType type);

/**
 * @brief Reset mailbox receive pointer.
 *
 * @details To be called from the process only.
 * @param mbox the mailbox to work with
 */
void mailbox_reset(Mailbox *mbox);

/**
 * @brief Advance pointer to next message in a receive loop.
 *
 * @details To be called from the process only.
 * @param mbox the mailbox to work with
 */
void mailbox_next(Mailbox *mbox);

/**
 * @brief Determine if there is a next item in message list.
 *
 * @details To be called from the process only.
 * @param mbox the mailbox to test
 * @returns \c true if peek would succeed.
 */
static inline bool mailbox_has_next(Mailbox *mbox)
{
    return mbox->receive_pointer != NULL;
}

/**
 * @brief Gets next message from a mailbox (without removing it).
 *
 * @details Peek the mailbox and retrieve a term that has been previously
 * queued on a certain process or driver mailbox. To be called from the process
 * only.
 * @param ctx the calling context, owning the mailbox and where the message should be copied to.
 * @param out the allocated term.
 * @returns true if a term was available
 */
bool mailbox_peek(Context *ctx, term *out);

/**
 * @brief Remove next message from mailbox.
 *
 * @details Discard a term that has been previously queued on a certain process
 * or driver mailbox. To be called from the process only. Term messages are
 * actually added as fragments to the heap and will be gone at next GC.
 * @param mbx the mailbox to remove next message from.
 */
void mailbox_remove_message(Mailbox *mbox, Heap *ctx);

/**
 * @brief Get first message from mailbox.
 *
 * @details Get the first message and sets the receive pointer to it so it can
 * be removed later. Used by ports & drivers. To be called from the process
 * only.
 * @param mbox the mailbox to get the current message from.
 * @returns first message or NULL.
 */
Message *mailbox_first(Mailbox *mbox);

/**
 * @brief Free memory associated with a mailbox.
 *
 * @details All messages in the mailbox will be freed or appended to the heap.
 * @param mbox the mailbox to free.
 * @param heap the heap to add messages to.
 */
void mailbox_destroy(Mailbox *mbox, Heap *heap);

/**
 * @brief Dispose a (processed) mailbox message. The message will be freed or
 * appended to current heap and will be destroyed on garbage collect.
 * This function is called by mailbox_remove and is only needed for signal
 * messages.
 *
 * @param m the message to free.
 * @param heap heap to append the message to.
 */
void mailbox_message_dispose(MailboxMessage *m, Heap *heap);

/**
 * @brief Output mailbox to stderr for crashdump reporting.
 *
 * @param ctx the owner of the mailbox to dump
 */
void mailbox_crashdump(Context *ctx);

#ifdef __cplusplus
}
#endif

#endif
