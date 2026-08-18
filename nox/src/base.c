#include "base.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void * xmalloc(usize n, usize s) {
    return malloc(n * s);
}

/// Align memory to bytes
static void * alignToBytes(void * memory, usize alignBytes) {
    uptr head = (uptr)memory;

    const usize remainder = head % alignBytes;
    const usize offset = (remainder != 0) ? alignBytes - remainder : 0;

    return (void *)(head + offset);
}

void allocatorInitHeap(Allocator * mem, usize maxNumAllowedBytes) {
    mem->mode = ALLOCATOR_MODE_GENERAL;

    mem->allocMethod = xmalloc;
    mem->freeMethod = free;

    mem->maxNumAllowedBytes = maxNumAllowedBytes;
    mem->numAllocatedBytes = 0;

    mem->heap.numAllocatedBuffers = 0;
    mem->heap.allocatedBuffers = calloc(4096, sizeof(void *));
}

void allocatorInitArena(Allocator * mem, void * buffer, usize maxNumAllowedBytes) {
    mem->mode = ALLOCATOR_MODE_ARENA;

    // We do not allocate anything!
    mem->allocMethod = NULL;
    mem->freeMethod = NULL;

    mem->maxNumAllowedBytes = maxNumAllowedBytes;
    mem->numAllocatedBytes = 0;

    mem->heap.numAllocatedBuffers = 0;
    mem->heap.allocatedBuffers = NULL;

    mem->arena.head = buffer;
    mem->arena.offsetBytes = 0;
}

void allocatorResetArena(Allocator * mem) {
    mem->numAllocatedBytes = 0;
    mem->arena.offsetBytes = 0;
}

void allocatorDrop(Allocator * mem) {
    // Clean all buffers that have been allocated
    // NOTE: No need to clean memory for arena!
    if (mem->mode == ALLOCATOR_MODE_GENERAL) {
        if (mem->heap.numAllocatedBuffers != 0) {
            for (usize i = 0; i < mem->heap.numAllocatedBuffers; i++) {
                if (mem->heap.allocatedBuffers[i] != NULL) {
                    free(mem->heap.allocatedBuffers[i]);
                }
            }
        }
        if (mem->heap.allocatedBuffers != NULL) {
            free(mem->heap.allocatedBuffers);
        }
    }
    if (mem->mode == ALLOCATOR_MODE_ARENA) {
        mem->maxNumAllowedBytes = 0;
        mem->numAllocatedBytes = 0;
        mem->arena.head = NULL;
        mem->arena.offsetBytes = 0;
    }
}

void * memoryAlloc(Allocator * mem, usize numElements, usize elemSize, usize alignBytes) {
    if (mem->mode == ALLOCATOR_MODE_GENERAL) {
        void * memory = mem->allocMethod(numElements, elemSize);
        if (memory == NULL) {
            dieAt("Unable to allocate memory.", XFILE, XLINE);
        }

        // Track allocated buffers
        mem->heap.allocatedBuffers[mem->heap.numAllocatedBuffers] = memory;
        mem->heap.numAllocatedBuffers++;

        // Track allocated bytes
        mem->numAllocatedBytes += numElements * elemSize;
        if (mem->numAllocatedBytes > mem->maxNumAllowedBytes) {
            dieAt("Unable to allocate more memory than allowed.", XFILE, XLINE);
        }

        return memory;
    }

    if (mem->mode == ALLOCATOR_MODE_ARENA) {
        // Align bytes to either the requested value, or 16 (default)
        const usize maxAlignBytes = alignBytes > 0 ? alignBytes : 16;
        // The last offset before allocating memory
        const usize lastOffsetBytes = mem->arena.offsetBytes;

        // Allocate memory by offseting head with the max calculated alignment
        void * newHead = alignToBytes((void *)((uptr)(mem->arena.head) + lastOffsetBytes), maxAlignBytes);

        // Difference between the old head and the new alligned head.
        // Helps use ensure if the memory was alligned or not.
        const usize diffAlignedHead = ((uptr)newHead - (uptr)((uptr)mem->arena.head + lastOffsetBytes));
        // Increment "bump" offset to "allocate" memory in the arena
        const usize newOffsetBytes = lastOffsetBytes + diffAlignedHead + (numElements * elemSize);

        // Update state
        mem->arena.offsetBytes = newOffsetBytes;
        // Track allocated bytes
        mem->numAllocatedBytes = newOffsetBytes;
        if (mem->numAllocatedBytes > mem->maxNumAllowedBytes) {
            dieAt("Unable to allocate more memory than allowed.", XFILE, XLINE);
        }

        return newHead;
    }

    never("wut?");
    return NULL;
}

void memoryFree(Allocator * mem, void ** ptrToData) {
    // Free memory only if it is alive
    if (ptrToData != NULL) {
        if (*ptrToData != NULL) {
            mem->freeMethod(*ptrToData);
        }
        *ptrToData = NULL;
    }
}

void dieAt(const char * message, const char * filePath, usize lineIndex) {
    fprintf(stderr, "%s%s%s  %s\n", ANSI_RED, RUNE_DOT_CHAR, ANSI_RESET, message);
    fprintf(stderr, "\n");

    fprintf(
        stderr, "%s%s (%s%s:%lu%s)\n",
        RUNE_BEND_CHAR, RUNE_DASH_CHAR, ANSI_ITALIC,
        strstr(filePath, "nox/"), lineIndex,
        ANSI_RESET
    );

    exit(EXIT_FAILURE);
}

usize stringLength(const char * string, usize maxLength) {
    if (string == NULL) {
        return 0;
    }

    for (usize i = 0; i < maxLength; i++) {
        const char c = string[at(i, maxLength)];
        if (c == '\0') {
            return (i + 1);
        }
    }

    return maxLength;
}

bool stringEqual(const char * string, const char * other, usize maxLength) {
    if (string == NULL || other == NULL) {
        return false;
    }

    const usize stringLen = stringLength(string, maxLength);
    const usize otherLen = stringLength(other, maxLength);
    const usize minLen = MIN(stringLen, otherLen);
    if (stringLen != otherLen) {
        return false;
    }

    for (usize i = 0; i < minLen; i++) {
        const char a = string[at(i, maxLength)];
        const char b = other[at(i, maxLength)];
        if (a != b) {
            return false;
        }
    }

    return true;
}
