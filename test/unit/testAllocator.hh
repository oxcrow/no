#pragma once
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "../../nox/lib/core/base.hh"

static void passTestAllocatorAA(void) {
    const usize maxBytesAlloc = 4096;
    Allocator a = {
        Memory{calloc(maxBytesAlloc, sizeof(char)), maxBytesAlloc, 0},
        maxBytesAlloc,
        0,
    };
    Status s = {0};

    a.alloc(4095, 1, 0, s); stop(s);
    a.alloc(1, 1, 1, s); stop(s);

    free(a.mBase.mPtr);
}

static void passTestAllocatorAB() {
}
