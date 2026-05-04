#pragma once
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

////////////////////////////////////////////////////////////////////////////////////////////////////
// Common macros and types
////////////////////////////////////////////////////////////////////////////////////////////////////

// Status code returned from functions
typedef size_t Status;

// Status codes
#define OK    0
#define ERROR 1

// Location macros for diagnostics
#define XFILE __FILE__
#define XLINE __LINE__

// Common number types
typedef size_t usize;
typedef uint32_t u32;
typedef uint64_t u64;

// ASCII character string
typedef struct String {
	char * ptr;
	usize len;
} String;

// Span over an array
typedef struct Span {
	void * ptr;
	usize len;
} Span;

// Ignore a variable
#define IGNORE(X) (void)(X)

////////////////////////////////////////////////////////////////////////////////////////////////////
// Error detection and reporting macros
////////////////////////////////////////////////////////////////////////////////////////////////////

/// Kill compiler process and report error at given position
#define dieAt(MESSAGE, FILE, LINE) \
	fprintf(stderr, "%s (File: \"%s\", Line: %lu)\n", (MESSAGE), (FILE), (usize)(LINE)); \
	exit(EXIT_FAILURE)

/// Kill compiler process
#define die(MESSAGE) dieAt((MESSAGE), XFILE, XLINE)

/// Verify index does not access out of bounds
static inline usize indexAt(usize index, usize arrayLength, const char * filePath, usize lineIndex) {
	if (!(index < arrayLength)) {
		dieAt("Unable to access index that is out of bounds.", filePath, lineIndex);
	}
	return index;
}

/// Verify index does not access out of bounds
#define at(INDEX, LENGTH) indexAt((INDEX), (LENGTH), XFILE, XLINE)

////////////////////////////////////////////////////////////////////////////////////////////////////
// Data structures used a lot by the compiler
////////////////////////////////////////////////////////////////////////////////////////////////////

/// Command line arguments
struct CmdLineArgs {
	/// Where the root file is stored?
	String filePath;

	/// What command to run?
	enum Command {
		COMMAND_BUILD,
		COMMAND_CLEAN,
		COMMAND_TEST,
		COMMAND_RUN,
	} command;

	/// What emitter to use?
	enum Emitter {
		EMIT_QBE,
		EMIT_ASM,
		EMIT_EXE,
	} emit;
};

Status parseCmdLineArgs(struct CmdLineArgs * args, usize argCount, char ** argVec);

////////////////////////////////////////////////////////////////////////////////////////////////////
// Utilities and algorithms
////////////////////////////////////////////////////////////////////////////////////////////////////

/// Length of a null-terminated string
usize stringLength(const char * string, usize maxLength);

/// Compare if two strings are the equal in length and value
bool stringEqual(const char * string, const char * other, usize maxLength);
