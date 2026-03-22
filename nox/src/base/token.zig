const lib = @import("../lib.zig");
const Span = lib.Span;

pub const Token = struct {
    kind: TokenKind,
    span: Span,
    line: u32,

    pub fn len(self: Self) usize {
        return self.span.len();
    }

    const Self = @This();
};

pub const TokenKind = enum {
    As,
    Break,
    Continue,
    Float,
    Function,
    Int,
    Let,
    Macro,
    Mod,
    Mut,
    New,
    Ok,
    Return,
    Struct,
    Type,
    Use,

    Exclamation,
    Semicolon,
    Ampersand,
    BackSlash,
    Question,
    ColonColon,
    Colon,
    Comma,
    Hash,
    DotDotEq,
    DotDot,
    Dot,

    LtEq,
    GtEq,
    Lt,
    Gt,
    NoEq,
    EqEq,
    Eq,
    At,

    Plus,
    Minus,
    Star,
    Slash,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBrack,
    RBrack,

    IdVal,
    IntVal,
    FloatVal,

    Document,
    Comment,
    White,
    None,
    Eof,
};
