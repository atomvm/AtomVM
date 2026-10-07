/* Copyright 2026 Peter M. <petermm@gmail.com>
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */
#include "app_heap.h"

#undef NDEBUG
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <exportedfunction.h>

#ifndef AVM_NO_SMP
#include <pthread.h>
static pthread_mutex_t heap_lock = PTHREAD_MUTEX_INITIALIZER;
#define LOCK() pthread_mutex_lock(&heap_lock)
#define UNLOCK() pthread_mutex_unlock(&heap_lock)
#else
#define LOCK() ((void) 0)
#define UNLOCK() ((void) 0)
#endif

void *__real_malloc(size_t size);
void *__real_realloc(void *ptr, size_t size);
void __real_free(void *ptr);

static struct Allocation
{
    void *ptr;
    size_t size;
} allocations[8192];

size_t app_heap_used;
unsigned app_heap_denied;
unsigned app_heap_fail_import;

static struct Allocation *find_allocation(void *ptr)
{
    for (size_t i = 0; i < sizeof(allocations) / sizeof(allocations[0]); i++) {
        if (allocations[i].ptr == ptr) {
            return &allocations[i];
        }
    }
    return NULL;
}

void *__wrap_malloc(size_t size)
{
    LOCK();
    if (size > APP_HEAP_LIMIT - app_heap_used
        || (app_heap_fail_import && size == sizeof(struct UnresolvedFunctionCall) && --app_heap_fail_import == 0)) {
        app_heap_denied++;
        UNLOCK();
        return NULL;
    }
    void *ptr = __real_malloc(size);
    if (ptr) {
        struct Allocation *slot = find_allocation(NULL);
        assert(slot);
        *slot = (struct Allocation){ ptr, size };
        app_heap_used += size;
    }
    UNLOCK();
    return ptr;
}

void *__wrap_calloc(size_t count, size_t size)
{
    if (size && count > SIZE_MAX / size) {
        return NULL;
    }
    void *ptr = __wrap_malloc(count * size);
    if (ptr) {
        memset(ptr, 0, count * size);
    }
    return ptr;
}

void __wrap_free(void *ptr)
{
    if (!ptr) {
        return;
    }
    LOCK();
    struct Allocation *slot = find_allocation(ptr);
    if (slot) {
        app_heap_used -= slot->size;
        *slot = (struct Allocation){ 0 };
    }
    __real_free(ptr);
    UNLOCK();
}

void *__wrap_realloc(void *ptr, size_t size)
{
    if (!ptr) {
        return __wrap_malloc(size);
    }
    if (!size) {
        __wrap_free(ptr);
        return NULL;
    }
    LOCK();
    struct Allocation *slot = find_allocation(ptr);
    assert(slot); // All reallocations in this test originate in the VM.
    if (size > APP_HEAP_LIMIT - app_heap_used + slot->size) {
        app_heap_denied++;
        UNLOCK();
        return NULL;
    }
    void *result = __real_realloc(ptr, size);
    if (result) {
        app_heap_used = app_heap_used - slot->size + size;
        *slot = (struct Allocation){ result, size };
    }
    UNLOCK();
    return result;
}

char *__wrap_strdup(const char *str)
{
    size_t size = strlen(str) + 1;
    char *copy = __wrap_malloc(size);
    if (copy) {
        memcpy(copy, str, size);
    }
    return copy;
}
