#include "base.h"

#include <string.h>

void allocatorInit(Allocator * mem, enum AllocatorMode mode, usize maxNumAllowedBytes) {
    mem->mode = mode;

    mem->allocMethod = calloc;
    mem->freeMethod = free;

    mem->maxNumAllowedBytes = maxNumAllowedBytes;
    mem->numAllocatedBytes = 0;
    mem->numAllocatedBuffers = 0;

    mem->allocatedBuffers = calloc(4096, sizeof(void *));
}

void allocatorDrop(Allocator * mem) {
    // Clean all buffers that have been allocated
    if (mem->numAllocatedBuffers != 0) {
        for (usize i = 0; i < mem->numAllocatedBuffers; i++) {
            free(mem->allocatedBuffers[i]);
        }
    }
    free(mem->allocatedBuffers);
}

void * memoryAlloc(Allocator * mem, usize numElements, usize elemSize) {
    void * memory = mem->allocMethod(numElements, elemSize);
    if (memory == NULL) {
        dieAt("Unable to allocate memory.", XFILE, XLINE);
    }

    // Track allocated buffers
    if (mem->mode == ALLOCATOR_MODE_GENERAL) {
        mem->allocatedBuffers[mem->numAllocatedBuffers] = memory;
        mem->numAllocatedBuffers++;
    }

    // Track allocated bytes
    mem->numAllocatedBytes += numElements * elemSize;
    if (mem->numAllocatedBytes > mem->maxNumAllowedBytes) {
        dieAt("Unable to allocate more memory than allowed.", XFILE, XLINE);
    }

    return memory;
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

    fprintf(stderr, "%s%s (%s%s:%lu%s)\n", RUNE_BEND_CHAR, RUNE_DASH_CHAR, ANSI_ITALIC, strstr(filePath, "nox/"), lineIndex, ANSI_RESET);

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
