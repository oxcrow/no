#pragma once

#include "../src/base.h"

static void passTestAllocator001AA(void) {
    struct Allocator mem;
    allocatorInit(&mem, ALLOCATOR_MODE_GENERAL, 4096);

    void * a = memoryAlloc(&mem, 100, sizeof(usize));
    void * b = memoryAlloc(&mem, 100, sizeof(float));
    void * c = memoryAlloc(&mem, 100, sizeof(char));

    allocatorDrop(&mem);
}

static void passTestAllocator002AA() {
}
