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
 * @file scheduler.h
 * @brief Scheduling functions.
 *
 * @details Scheduling functions are used to schedule processes.
 */

#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_

#include "context.h"
#include "globalcontext.h"
#include "linkedlist.h"

#define DEFAULT_REDUCTIONS_AMOUNT 1024

/**
 * @brief move a process to waiting queue and wait a ready one
 *
 * @details move current process to the waiting queue, and schedule the next one or sleep until an event is received.
 * @param global the global context.
 * @param c the process context.
 * @param timeout in milliseconds.
 */
Context *scheduler_wait(GlobalContext *global, Context *c, int timeout);

/**
 * @brief make sure a process is on the ready queue
 *
 * @details make a process ready again by moving it to the ready queue.
 * @param global the global context.
 * @param c the process context.
 */
void scheduler_make_ready(GlobalContext *global, Context *c);

/**
 * @brief just move a process to the wait queue
 *
 * @details make a process waiting.
 * @param global the global context.
 * @param c the process context.
 */
void scheduler_make_waiting(GlobalContext *global, Context *c);

int schudule_processes_count(GlobalContext *global);

Context *scheduler_next(GlobalContext *global, Context *c);

#endif
