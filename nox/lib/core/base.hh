#pragma once

#include <stddef.h>
#include <stdint.h>

// Ignore a variable
#define IGNORE(X) (void)(X)

// Macros to mark locations
#define SOURCE __FILE__
#define LINE   __LINE__

// Branch predictor hints
#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)

// Managing raw memory with pointers can lead to heinous bugs.
// We can not prevent them if we can not grep/search for them.
// Thus, we should use this macros to mark risky operations.
// Ex: `PTRMATH(buffer + N)` and `PTRCAST((void *)buffer)`
// It's not much, but helps.
#define PTRMATH(X) X
#define PTRCAST(X) X

// Convert to string
#define STRINGIFY(X) #X

// Common number types
typedef size_t usize;
typedef uintptr_t uptr;
typedef uint64_t u64;
typedef uint32_t u32;
typedef uint8_t u8;
typedef double f64;
typedef float f32;

/// Status code returned from functions
struct Status {
    const char * mMessage[32];
    const char * mFilePath;
    usize mLineIdx;
    usize mCode;
    usize mLen;
};

/// Container that may optionally contain value
template <typename T>
struct Option {
    T mItem;
};

// Mathematical macros
#define MIN(X, Y) (((X) <= (Y)) ? (X) : (Y))
#define MAX(X, Y) (((X) >= (Y)) ? (X) : (Y))

// ANSI codes
#define ANSI_FAINT  "\033[2m"
#define ANSI_ITALIC "\033[3m"
#define ANSI_BLACK  "\033[30m"
#define ANSI_RED    "\033[31m"
#define ANSI_GREEN  "\033[32m"
#define ANSI_YELLOW "\033[33m"
#define ANSI_BLUE   "\033[34m"
#define ANSI_PURPLE "\033[35m"
#define ANSI_CYAN   "\033[36m"
#define ANSI_WHITE  "\033[37m"
#define ANSI_RESET  "\033[0m"

///////////////////////////////////////////////////////////////////////////////
// Error handling utils
///////////////////////////////////////////////////////////////////////////////

/// Invoke function and return error if it fails.
///
/// Use as, `auto x = something(); back(status, error);`
///
/// This looks little bit ugly, but it works good enough for us.
/// 1. The {} around if branch prevents it from getting attached to an else
/// 2. The while(0) eats up the trailing semicolon and prevents bugs
/// 3. The status is a reference as that's how it'll be passed to functions.
/// 4. The `unlikely` ensures we don't slow down code execution in hot paths.
#define back(STATUS, ERROR) \
    { \
        if (unlikely((STATUS).mCode)) { \
            return (ERROR); \
        } \
    } \
    while (0) { \
    }

#define unit /* nothing */

/// Create and propagate an error upwards the call stack along with source info.
/// duck, cause when something bad happens, that's what we say.
#define duck(STATUS, ERROR, CONDITION, MESSAGE) \
    { \
        if (unlikely((CONDITION))) { \
            /* So we don't overflow message buffer */ \
            if (likely((STATUS).mLen < 32)) { \
                (STATUS).mMessage[(STATUS).mLen] = (MESSAGE); \
                (STATUS).mFilePath = SOURCE; \
                (STATUS).mLineIdx = LINE; \
                (STATUS).mCode = -1; \
                (STATUS).mLen++; \
            } \
            back((STATUS), (ERROR)); \
        } \
    }

#define stop(STATUS) \
    if ((STATUS).mCode) { \
        dieAt((STATUS).mMessage[0], (STATUS).mFilePath, (STATUS).mLineIdx); \
    }

#define ensure(STATUS, ERROR, CONDITION, MESSAGE) duck(STATUS, ERROR, !(CONDITION), MESSAGE)

/// Kill process and report error at given position
void dieAt(const char * message, const char * filePath, usize lineIdx);

/// Verify index does not access out of bounds
static inline usize indexAt(usize idx, usize arrayLen, const char * filePath, usize lineIdx) {
    if (!(idx < arrayLen)) {
        dieAt("Unable to access array index since it is out of bounds.", filePath, lineIdx);
    }
    return idx;
}

/// Verify index does not access out of bounds
#define at(INDEX, LENGTH) indexAt((INDEX), (LENGTH), SOURCE, LINE)

///////////////////////////////////////////////////////////////////////////////
// Memory handling utils
///////////////////////////////////////////////////////////////////////////////

/// Unmanaged memory that we'll mostly use to hold data.
/// The user must manage (alloc, free) it manually.
struct Memory {
    void * mPtr;
    usize mCap;
    usize mLen;
};

/// Memory allocator to manage lifetimes of our objects.
struct Allocator {
    Memory mBase;
    /// Max number of bytes that can be allocated by this allocator.
    /// This is an user specified limit that must be respected.
    usize mMaxNumBytesAlloc;
    /// Number of bytes allocated by this allocator.
    usize mNumBytesAlloc;

    /// Allocate the specified number of bytes.
    Memory alloc(usize numElems, usize elemSizeBytes, usize alignBytes, Status & s);
    /// Stash the current state of the allocator, so we may reset to it.
    Memory stash() const;
    /// Reset the allocator to a previous state.
    void reset(Memory state);
};

///////////////////////////////////////////////////////////////////////////////
// File handling utils
///////////////////////////////////////////////////////////////////////////////

const char * readFile(Allocator & mem, const char * filePath, Status & s);
