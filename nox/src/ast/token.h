#pragma once
#include "../base.h"

typedef enum TokenKind {
    TOKEN_UNKNOWN,
    TOKEN_WHITE,
    TOKEN_LINE,

    TOKEN_COMMENT,

    TOKEN_AS,
    TOKEN_DEF,
    TOKEN_ELSE,
    TOKEN_IF,
    TOKEN_FN,
    TOKEN_LET,
    TOKEN_MOD,
    TOKEN_RUN,

    TOKEN_SEMICOLON,
    TOKEN_COLON,
    TOKEN_COMMA,
    TOKEN_DOLLAR,
    TOKEN_HASH,
    TOKEN_DOTDOT,
    TOKEN_DOT,

    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_LBRACK,
    TOKEN_RBRACK,
    TOKEN_RARROW,

    TOKEN_EQEQ,
    TOKEN_EQ,
    TOKEN_GTEQ,
    TOKEN_GT,
    TOKEN_LTEQ,
    TOKEN_LT,

    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_STAR,
    TOKEN_SLASH,

    TOKEN_XNICK,
    TOKEN_XNAME,
    TOKEN_XSTR,
    TOKEN_XINT,

    TOKEN_NONE,

    TOKEN_END,
} TokenKind;

typedef struct Token {
    /// The kind of token identified
    enum TokenKind kind;

    /// The character span range where the token exists in code
    /// NOTE: This is a half open interval [start, end) (end is excluded).
    u32 spanRange[2];

    /// The line where token was parsed
    u32 lineIndex;

    /// Hash of special tokens
    u64 hash;
} Token;
