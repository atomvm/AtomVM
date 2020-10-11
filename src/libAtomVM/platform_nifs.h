/***************************************************************************
 *   Copyright 2019 by Fred Dushin <fred@dushin.net>                       *
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

#ifndef _PLATFORM_NIFS_H_
#define _PLATFORM_NIFS_H_

#include "exportedfunction.h"
#include "module.h"

/**
* @brief    Returns the Nif assocatiated with a nif name.
*
* @details  This function is used to locate platform-specific Nifs.
*           Each platform must include an implementation of this function,
*           even if it just returns NULL.
* @param    nifname a null-terminated module:function/arity name
* @return   the Nif structure associated with the supplied nif name, or
*           NULL, if there is no such Nif.
*/
const struct Nif *platform_nifs_get_nif(const char *nifname);

#endif
