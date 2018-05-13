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

#include "mapped_file.h"

#include "utils.h"

#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

MappedFile *mapped_file_open_beam(const char *file_name)
{
    MappedFile *mf = malloc(sizeof(MappedFile));
    if (IS_NULL_PTR(mf)) {
        return NULL;
    }

    mf->fd = open(file_name, O_RDONLY);
    if (mf->fd < 0) {
        fprintf(stderr, "Cannot open %s\n", file_name);
        free(mf);
        return NULL;
    }

    struct stat file_stats;
    fstat(mf->fd, &file_stats);
    mf->size = file_stats.st_size;

    mf->mapped = mmap(NULL, mf->size, PROT_READ, MAP_SHARED, mf->fd, 0);
    if (IS_NULL_PTR(mf->mapped)) {
        fprintf(stderr, "Cannot mmap %s\n", file_name);
        close(mf->fd);
        free(mf);
        return NULL;
    }

    return mf;
}

void mapped_file_close(MappedFile *mf)
{
    munmap(mf->mapped, mf->size);
    close(mf->fd);
    free(mf);
}
