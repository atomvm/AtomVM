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

#include "mapped_file.h"

#include "utils.h"

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

MappedFile *mapped_file_open_beam(const char *file_name)
{
    MappedFile *mf = malloc(sizeof(MappedFile));
    if (IS_NULL_PTR(mf)) {
        fprintf(stderr, "Unable to allocate MappedFile struct\n");
        return NULL;
    }

    mf->fd = open(file_name, O_RDONLY);
    if (UNLIKELY(mf->fd < 0)) {
        free(mf);
        fprintf(stderr, "Unable to open %s\n", file_name);
        return NULL;
    }

    struct stat file_stats;
    fstat(mf->fd, &file_stats);
    mf->size = file_stats.st_size;

    int prot;
#ifdef AVM_NO_JIT
    prot = PROT_READ;
#elif defined(__APPLE__) && defined(__arm64__)
    prot = PROT_READ;
#else
    prot = PROT_READ | PROT_EXEC;
#endif

    mf->mapped = mmap(NULL, mf->size, prot, MAP_SHARED, mf->fd, 0);
    if (UNLIKELY(mf->mapped == MAP_FAILED)) {
        fprintf(stderr, "Cannot mmap %s -- errno=%d\n", file_name, errno);
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
