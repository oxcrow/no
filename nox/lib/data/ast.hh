#pragma once
#include "../core/base.hh"
#include <cassert>

enum class TokenKind : u8 {
    Whitespace = 0,
    Document,
    Comment,

    Export,
    Function,
    Let,
    Return,

    Int,

    LParen,
    RParen,
    LBrace,
    RBrace,

    XName,
    XInt,
    Unit,

    None,
};

struct Span {
    u32 mRange[2];

    Span(usize startIdx, usize endIdx)
        : mRange{(u32)startIdx, (u32)endIdx} {
    }
    u32 operator[](usize i) const {
        return mRange[i];
    }
    usize len() const {
        return mRange[1] - mRange[0];
    }
};

struct Token {
    TokenKind mKind = TokenKind::None;
    /// Span of characters that contains the token
    Span mSpan;
    /// Line of code that contains the token
    u32 mLineIdx;
    /// Extra data that may be required
    union {
        struct {
            /// Hash of the name (FNV1A)
            u32 mHash;
            /// Id of the name
            u32 mId;
        } mName;
    } mExtra;
};
