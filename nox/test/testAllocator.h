#pragma once

#include <assert.h>
#include <stdio.h>

#include "../src/base.h"

static void passTestAllocator001AA(void) {
    struct Allocator mem;
    allocatorInitHeap(&mem, 4096);

    void * a = memoryAlloc(&mem, 100, sizeof(usize), 0);
    void * b = memoryAlloc(&mem, 100, sizeof(float), 0);
    void * c = memoryAlloc(&mem, 100, sizeof(char), 0);

    allocatorDrop(&mem);
}

static void passTestAllocator002AA() {
    struct Allocator mem, arena;
    allocatorInitHeap(&mem, 4096);
    allocatorInitArena(&arena, memoryAlloc(&mem, 2048, sizeof(char), 16), 2048);

    void * a = memoryAlloc(&arena, 100, sizeof(usize), 16);
    void * b = memoryAlloc(&arena, 100, sizeof(float), 4);
    void * c = memoryAlloc(&arena, 100, sizeof(char), 1);
    void * d = memoryAlloc(&arena, 100, sizeof(char), 1);
    void * e = memoryAlloc(&arena, 100, sizeof(char), 1);

    assert(arena.numAllocatedBytes == 1500);

    allocatorDrop(&arena);
    allocatorDrop(&mem);
}
