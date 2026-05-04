#pragma once
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

////////////////////////////////////////////////////////////////////////////////////////////////////
// Common macros and types
////////////////////////////////////////////////////////////////////////////////////////////////////

// Location macros for diagnostics
#define XFILE __FILE__
#define XLINE __LINE__

// Common number types
typedef size_t usize;
typedef uint32_t u32;
typedef uint64_t u64;

/// Status code returned from functions
typedef struct Status {
	usize code;
	const char * message;
	const char * filePath;
	usize lineIndex;
} Status;

/// Create status code for everything went well
static inline Status seemsOK(void) {
	return (Status){
		.code = 0,
		.message = NULL,
		.filePath = NULL,
		.lineIndex = 0};
}

/// Create status code for errors with diagnostic message and location
static inline Status seemsWrong(usize code, const char * message, const char * filePath, usize lineIndex) {
	return (Status){
		.code = code,
		.message = message,
		.filePath = filePath,
		.lineIndex = lineIndex};
}

#define OK()           seemsOK()
#define ERROR(MESSAGE) seemsWrong((OHNO_COMMON_ERROR), (MESSAGE), XFILE, XLINE)

/// Error codes emmitted by the compiler
/// Why prefix every status code with "Oh no? (OHNO)?"
/// Because I want a unique prefix that will not clash with anything else. Fuck you for asking.
enum SomeErrorCodes {
	OHNO_COMMON_ERROR = 1001,
	OHNO_END,
};

// ASCII character string
typedef struct String {
	char * ptr;
	usize len;
} String;

// Dynamic vector array
typedef struct Vector {
	void * ptr;
	usize len;
	usize cap;
} Vector;

// Ignore a variable
#define IGNORE(X) (void)(X)

// Mathematical macros
#define MIN(X, Y) (((X) <= (Y)) ? (X) : (Y))
#define MAX(X, Y) (((X) >= (Y)) ? (X) : (Y))

// ANSI codes
#define ANSI_ITALIC  "\033[3m"
#define ANSI_RED     "\033[31m"
#define ANSI_RESET   "\033[0m"
#define RUNE_RED_DOT "\033[31m●\033[0m"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Error detection and reporting macros
////////////////////////////////////////////////////////////////////////////////////////////////////

/// Kill compiler process and report error at given position
static void dieAt(const char * message, const char * filePath, usize lineIndex) {
	fprintf(stderr, "%s %s\n\n", RUNE_RED_DOT, message);

	fprintf(stderr, "%s", ANSI_ITALIC);
	fprintf(stderr, "(File: \"%s\", Line: %lu)\n", filePath, lineIndex);
	fprintf(stderr, "(Users should ignore this section)\n");
	fprintf(stderr, "%s", ANSI_RESET);

	exit(EXIT_FAILURE);
}

/// Kill compiler process
#define die(STATUS, MESSAGE) \
	dieAt( \
		(((STATUS) != NULL) ? (((Status *)(STATUS))->message) : (MESSAGE)), \
		(((STATUS) != NULL) ? (((Status *)(STATUS))->filePath) : (XFILE)), \
		(((STATUS) != NULL) ? (((Status *)(STATUS))->lineIndex) : (XLINE)))

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
		CMDLINE_COMMAND_BUILD,
		CMDLINE_COMMAND_CLEAN,
		CMDLINE_COMMAND_TEST,
		CMDLINE_COMMAND_RUN,
	} command;

	/// What emitter to use?
	enum Emitter {
		CMDLINE_EMIT_QBE,
		CMDLINE_EMIT_ASM,
		CMDLINE_EMIT_EXE,
	} emit;

	bool help;
};

/// Parse command line arguments and store them
Status cmdParseArgs(struct CmdLineArgs * args, usize argCount, char ** argVec);

////////////////////////////////////////////////////////////////////////////////////////////////////
// Utilities and algorithms
////////////////////////////////////////////////////////////////////////////////////////////////////

/// Length of a null-terminated string
usize stringLength(const char * string, usize maxLength);

/// Compare if two strings are the equal in length and value
bool stringEqual(const char * string, const char * other, usize maxLength);
