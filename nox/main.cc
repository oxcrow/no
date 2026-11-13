#include <stdio.h>
#include <stdlib.h>

#include "lib/core/base.hh"

void uasge() {
    fprintf(stderr, "Usage: no <command> [options]\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "No: A language for the workers of the world.\n");
}

int main() {
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
    return 0;
}
