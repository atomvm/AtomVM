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

#include "platform_nifs.h"
#include "defaultatoms.h"
#include "nifs.h"
#include "platform_defaultatoms.h"
#include "term.h"

//#define ENABLE_TRACE
#include "trace.h"

static term nif_atomvm_platform(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
    return STM32_ATOM;
}

static const struct Nif atomvm_platform_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_platform
};

const struct Nif *platform_nifs_get_nif(const char *nifname)
{
    if (strcmp("atomvm:platform/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &atomvm_platform_nif;
    }
    return NULL;
}
