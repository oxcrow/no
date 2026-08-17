#pragma once
#include "../base.h"

#include "../ast/token.h"

typedef struct Parser {
    struct ParserState {
        /// File that we're currently parsing
        const char * filePath;
        /// Code that we're currently parsing
        const char * code;
        usize codeLen;

        /// Index to current token
        u32 tokenIndex;

        /// Index to the current character
        u32 charIndex;
        /// Index to the current line
        u32 lineIndex;

        /// Current and next token
        Token currToken, nextToken;
    } state;
} Parser;

void parseFile(Allocator * mem, const char * filePath, const char * code, Status * s);
