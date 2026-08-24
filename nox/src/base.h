#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

////////////////////////////////////////////////////////////////////////////////
// Common macros and types
////////////////////////////////////////////////////////////////////////////////

// Location macros for diagnostics
#define XFILE __FILE__
#define XLINE __LINE__

// Managing raw memory with pointers can lead to heinous bugs.
// We can not prevent them if we can not grep/search for them.
// Thus, we should use this macros to mark risky operations.
// Such as, ptrMath(buffer + N)
// It's not much, but helps.
#define ptrMath(X) X
#define ptrCast(X) X

// Branch predictor hints
#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)

// Common number types
typedef size_t usize;
typedef uintptr_t uptr;
typedef uint64_t u64;
typedef uint32_t u32;
typedef uint8_t u8;
typedef double f64;
typedef float f32;

// Common tuple types
typedef struct u32x2 {
    u32 x[2];
} u32x2;

/// Universally unique identifier for nodes in AST.
/// Useful for comparing nodes in O(1) time.
typedef struct UUID {
    u32 libraryId;
    u32 moduleId;
    u32 fileId;
    u32 selfId;
} UUID;

/// Allocation mode used
enum AllocatorMode {
    ALLOCATOR_MODE_GENERAL,
    ALLOCATOR_MODE_ARENA,
};

/// Single threaded memory allocator
/// WARNING: THIS IS NOT INTENDED TO BE THREAD SAFE!
/// To safely use this, allocate memory from master thread only.
/// We can then safely use the allocated memory from any other thread.
typedef struct Allocator {
    /// Allocation mode
    enum AllocatorMode mode;

    // Methods to allocate, resize, free memory
    void * (*allocMethod)(usize, usize);
    void (*freeMethod)(void *);

    /// Maximum number of bytes allowed to be allocated (to prevent Out of Memory)
    usize maxNumAllowedBytes;

    /// Number of bytes allocated by this allocator
    usize numAllocatedBytes;

    struct {
        /// Number of allocated buffers that must be freed manually
        usize numAllocatedBuffers;

        /// Buffers that have been allocated, and must be freed manually
        void ** allocatedBuffers;
    } heap;

    struct {
        /// Head of arena allocated memory
        void * head;

        /// Offset of arena
        usize offsetBytes;
    } arena;
} Allocator;

void allocatorInitHeap(Allocator * mem, usize maxNumAllowedBytes);
void allocatorInitArena(Allocator * mem, void * buffer, usize maxNumAllowedBytes);
void allocatorResetArena(Allocator * mem);
void allocatorDrop(Allocator * mem);
//
void * memoryAlloc(Allocator * mem, usize numElements, usize elemSize, usize alignBytes);
void memoryFree(Allocator * mem, void ** ptrToData);

// Stretchy string header
typedef struct Yarn {
    usize cap;
    usize len;
    char * buf;
} Yarn;

static inline Yarn * charToYarn(const char * string) {
    return (Yarn *)(string - sizeof(Yarn) + sizeof(char *));
}

static inline char * yarnToChar(const Yarn * yarn) {
    return yarn->buf;
}

static inline usize yarnLength(const char * string) {
    return charToYarn(string)->len;
}

static inline usize yarnCapacity(const char * string) {
    return charToYarn(string)->cap;
}

/// Status code returned from functions
typedef struct Status {
    usize code;
    const char * message;
    const char * filePath;
    usize lineIndex;
} Status;

/// Create status code for everything went well
static inline void seemsOK(Status * status) {
    status->code = 0,
    status->message = NULL;
    status->filePath = NULL;
    status->lineIndex = 0;
}

/// Create status code for errors with diagnostic message and location
static inline void seemsWrong(Status * status, usize code, const char * message, const char * filePath, usize lineIndex) {
    status->code = code;
    status->message = message;
    status->filePath = filePath;
    status->lineIndex = lineIndex;
}

/// We ducked up badly. Set the error status with message.
#define duck(STATUS, MESSAGE)           seemsWrong((STATUS), (OHNO_COMMON_ERROR), (MESSAGE), XFILE, XLINE)
#define duckWhat(STATUS, MESSAGE, CODE) seemsWrong((STATUS), (CODE), (MESSAGE), XFILE, XLINE)

/// Error codes emmitted by the solver.
/// Why prefix every status code with "Oh no? (OHNO)?"
/// Because I want a unique prefix that will not clash with anything else. Fuck you for asking.
enum SomeErrorCodes {
    OHNO_COMMON_ERROR = 1001,
    OHNO_END,
};

// Ignore a variable
#define ignore(X) (void)(X)

// Mathematical macros
#define MIN(X, Y) (((X) <= (Y)) ? (X) : (Y))
#define MAX(X, Y) (((X) >= (Y)) ? (X) : (Y))

// ANSI codes
#define ANSI_ITALIC "\033[3m"
#define ANSI_RED    "\033[31m"
#define ANSI_GREEN  "\033[32m"
#define ANSI_CYAN   "\033[36m"
#define ANSI_RESET  "\033[0m"

// UTF-8 encoded runes
// "●" is U+25CF
// "○" is U+25C7
// "╰" is U+2570
// "─" is U+2500
// "│" is U+2502
#define RUNE_TAB       "    "
#define RUNE_DOT_CHAR  "●"
#define RUNE_BEND_CHAR "╰"
#define RUNE_SIDE_CHAR "│"
#define RUNE_DASH_CHAR "─"

// Debugging macros
#define dbgString(X) printf("(%s: %s), ", #X, (X));
#define dbgInt(X)    printf("(%s: %zu), ", #X, (usize)(X));
#define dbgEnd()     printf("\n");

// Error detection and reporting macros

/// Kill process and report error at given position
void dieAt(const char * message, const char * filePath, usize lineIndex);

/// Kill process
#define die(STATUS, MESSAGE) \
    dieAt( \
        (((STATUS) != NULL) ? (((Status *)(STATUS))->message) : (MESSAGE)), \
        (((STATUS) != NULL) ? (((Status *)(STATUS))->filePath) : (XFILE)), \
        (((STATUS) != NULL) ? (((Status *)(STATUS))->lineIndex) : (XLINE)) \
    )

/// Kill process when we execute unreachable code
#define never(MESSAGE) \
    dieAt((MESSAGE), (XFILE), (XLINE))

/// Kill process when we execute code that is yet to be implemented
#define todo(MESSAGE) \
    dieAt((MESSAGE), (XFILE), (XLINE))

#define ensure(CONDITION) \
    { \
        if (unlikely(!(CONDITION))) { \
            dieAt("Unable to ensure constraint. (" #CONDITION ")", XFILE, XLINE); \
        } \
        while (0) { \
        } \
    }

/// Verify index does not access out of bounds
static inline usize indexAt(usize index, usize arrayLength, const char * filePath, usize lineIndex) {
    if (!(index < arrayLength)) {
        dieAt("Unable to access array index since it is out of bounds.", filePath, lineIndex);
    }
    return index;
}

/// Verify index does not access out of bounds
#ifdef NOX_RELEASE_MODE
#define at(INDEX, LENGTH) (INDEX)
#else
#define at(INDEX, LENGTH) indexAt((INDEX), (LENGTH), XFILE, XLINE)
#endif

/// Invoke function and return error if it fails
/// Use as, `auto x = something(); back(error, status);`
/// 1. The {} around if branch prevents it from getting attached to an else
/// 2. The while(0) eats up the trailing semicolon and prevents bugs
/// 3. The status is a pointer as that's how it'll be passed to functions.
/// 4. The [[unlikely]] ensures we don't slow down code execution in hot paths.
#define back(ERROR, STATUS) \
    { \
        if (unlikely((STATUS)->code)) { \
            return ERROR; \
        } \
    } \
    while (0) { \
    }

/// Kill process if status contains error code
#define stopWhat(STATUS, MESSAGE) \
    { \
        if (unlikely((STATUS)->code)) { \
            die((STATUS), (MESSAGE)); \
        } \
    } \
    while (0) { \
    }

/// Kill process if status contains error code
#define stop(STATUS) stopWhat((STATUS), "Stop.")

/// C does not allow us to return void; as an expression. That is a problem!
/// Thus, use this with `back` macro to return errors from void functions.
/// Such as, `foo(x, y, z, &status); back(unit, &status);`
#define unit /* nothing */

////////////////////////////////////////////////////////////////////////////////
// Utilities and algorithms
////////////////////////////////////////////////////////////////////////////////

/// Hash string to 32 bit integer
static inline u32 fnv1a(const char * str, usize strLen) {
    u32 hash = 0x811c9dc5; // FNV offset basis for 32-bit
    for (usize i = 0; i < strLen; i++) {
        hash ^= (u8)str[i]; // XOR with byte
        hash *= 0x1000193;  // Multiply by FNV prime
    }
    return hash;
}

/// Length of a null-terminated string
usize stringLength(const char * string, usize maxLength);

/// Compare if two strings are the equal in length and value
bool stringEqual(const char * string, const char * other, usize maxLength);
