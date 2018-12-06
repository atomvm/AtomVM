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

#include <sys.h>

// Monotonically increasing number of milliseconds from reset
// Overflows every 49 days
// TODO: use 64 bit (remember to take into account atomicity)
static volatile uint32_t system_millis;

// Called when systick fires
void sys_tick_handler()
{
    system_millis++;
}

// Sleep for delay milliseconds
static void msleep(uint32_t delay)
{
    // TODO: use a smarter sleep instead of busy waiting
    uint32_t wake = system_millis + delay;
    while (wake > system_millis);
}
