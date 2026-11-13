#include "base.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void dieAt(const char * message, const char * filePath, usize lineIdx) {
    fprintf(stderr, "%s<>%s %s\n", ANSI_RED, ANSI_RESET, message);
    fprintf(stderr, "%s%s((Error emitted from compiler source location: %s:%lu)%s\n", ANSI_FAINT, ANSI_ITALIC, strstr(filePath, "nox/"), lineIdx, ANSI_RESET);
    fprintf(stderr, "%s%s (Users can ignore this. It's for us compiler devs. (U_U)))%s\n", ANSI_FAINT, ANSI_ITALIC, ANSI_RESET);
    exit(EXIT_FAILURE);
}

/// Align memory to bytes
static void * alignToBytes(void * memory, usize alignBytes) {
    uptr head = (uptr)memory;

    const usize remainder = head % alignBytes;
    const usize offset = (remainder != 0) ? alignBytes - remainder : 0;

    return PTRMATH((void *)(head + offset));
}

Memory Allocator::alloc(usize numElems, usize elemSizeBytes, usize alignBytes, Status & s) {
    const usize numBytes = numElems * elemSizeBytes;
    // Align bytes to either the requested value, or 16 (default)
    const usize maxAlignBytes = alignBytes > 0 ? alignBytes : 16;
    // The last base index offset (helpful to debug)
    const usize lastBaseIdx = mBase.mLen;
    Memory m;

    // Allocate memory by offseting head with the max calculated alignment
    void * newHead = alignToBytes(PTRMATH((void *)((uptr)(mBase.mPtr) + lastBaseIdx)), maxAlignBytes);

    // Difference between the old head and the new alligned head.
    // Helps us ensure if the memory was alligned or not.
    const usize diffAlignedHead = ((uptr)newHead - (uptr)((uptr)mBase.mPtr + lastBaseIdx));
    // Increment "bump" offset to "allocate" memory in the arena
    const usize newBaseIdx = lastBaseIdx + diffAlignedHead + (numBytes);

    // Verify that we have enough memory to allocate more
    const bool notEnoughMem = newBaseIdx > mBase.mCap; duck(
        s, m, notEnoughMem, "Unable to allocate more memory than allowed."
    );

    // Update state
    mNumBytesAlloc = newBaseIdx;
    mBase.mLen = newBaseIdx;

    m.mPtr = newHead;
    m.mCap = numBytes;
    m.mLen = 0;

    return m;
}

Memory Allocator::stash() const {
    return mBase;
}

void Allocator::reset(Memory state) {
    mNumBytesAlloc = state.mLen;
    mBase = state;
}
