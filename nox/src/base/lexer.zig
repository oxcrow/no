const std = @import("std");
const lib = @import("../lib.zig");

const Span = lib.Span;
const Token = lib.token.Token;
const TokenKind = lib.token.TokenKind;

pub fn tokenize(mem: std.mem.Allocator, com: lib.Compiler, text: []const u8) !lib.MultiList(Token) {
    var tokens = try lib.MultiList(Token).initCapacity(mem, text.len / 3);
    const allocated = tokens.capacity();
    errdefer tokens.deinit(mem);

    var iline: usize = 1;
    var iword: usize = 1;
    var icols: usize = 1;
    var ichar: usize = 0;
    var code = text;

    while (ichar < text.len) {
        if (code[0] == '\n') {
            iline += 1;
            iword = 1;
            icols = 1;
        }

        // Tokenize next token
        const token, const rest = try nextToken(text, code, iline, icols, ichar, com.cfg.mode);
        if (token.kind != TokenKind.None and token.kind != TokenKind.White) {
            try tokens.appendAssumeCapacity(token);
            iword += 1;
        }

        // Report lexer errors
        if (token.kind == TokenKind.None) {
            lib.crash(.LexUnknownToken, @src(), .{ text, iline, icols });
        }

        // Update state
        ichar += token.len();
        icols += token.len();
        code = rest;
    }

    // Append End of File token to the end
    const eof = Token{
        .kind = .Eof,
        .span = Span.init(@truncate(text.len), @truncate(text.len + 1)),
        .line = @truncate(iline),
    };
    try tokens.append(mem, eof);

    // Verify that memory was not reallocated while lexing.
    if (com.cfg.debug) {
        if (allocated != tokens.capacity()) {
            @panic("Memory allocated was not enough for lexer.");
        }
    }

    return tokens;
}

pub fn nextToken(text: []const u8, code: []const u8, iline: usize, icols: usize, ichar: usize, mode: lib.Mode) !struct { Token, []const u8 } {
    const span1 = Span.init(@truncate(ichar), @truncate(ichar + 1));
    const span2 = Span.init(@truncate(ichar), @truncate(ichar + 2));
    const span3 = Span.init(@truncate(ichar), @truncate(ichar + 3));
    const span6 = Span.init(@truncate(ichar), @truncate(ichar + 6));

    const a = nextChar(code, 0);
    const b = nextChar(code, 1);
    const c = nextChar(code, 2);
    const d = nextChar(code, 3);
    const e = nextChar(code, 4);
    const f = nextChar(code, 5);
    const g = nextChar(code, 6);

    var token = Token{ .kind = TokenKind.None, .span = span1, .line = @truncate(iline) };

    switch (a) {
        ' ' => {
            token.kind = TokenKind.White;
        },
        '\r' => {
            // Ensure only LF are used. Not CR or CRLF!
            lib.crash(.LexCarraigeReturnUsed, @src(), .{ text, iline, icols });
        },
        '\n' => {
            // Ensure only tabs are used for indentation!
            // NOTE: We will advance tokens to skip past all indentation (whitespace)!
            // NOTE: We NEVER tokenize more than one line at a time!
            if (mode == .Standard) {
                for (code, 0..) |x, i| {
                    if (x == '\n') {
                        token.kind = TokenKind.White;
                        if (i == 0) {
                            continue;
                        }
                        break; // Never tokenize more than one line!
                    } else if (x == '\t') {
                        lib.crash(.LexTabsUsedForIndentation, @src(), .{ text, iline, icols + (i - 1) });
                    } else if (x == ' ') {
                        token.kind = TokenKind.White;
                        continue;
                    } else {
                        token.span.end = @truncate(ichar + i); // Advance and skip tokens
                        token.kind = TokenKind.White;
                        break;
                    }
                }
            } else {
                token.kind = TokenKind.White;
            }
        },
        '\t' => {
            // Ensure only spaces are used for allignment!
            if (mode == .Standard) {
                lib.crash(.LexTabsUsedForAlignment, @src(), .{ text, iline, icols });
            }
            token.kind = TokenKind.White;
        },
        'f' => {
            if (a == 'f' and b == 'n' and isdel(c)) {
                token.kind = TokenKind.Function;
                token.span = span2;
            }
        },
        'i' => {
            if (a == 'i' and b == 'n' and c == 't' and isdel(d)) {
                token.kind = TokenKind.Int;
                token.span = span3;
            }
        },
        'l' => {
            if (a == 'l' and b == 'e' and c == 't' and isdel(d)) {
                token.kind = TokenKind.Let;
                token.span = span3;
            }
        },
        'm' => {
            if (a == 'm' and b == 'o' and c == 'd' and isdel(d)) {
                token.kind = TokenKind.Mod;
                token.span = span3;
            } else if (a == 'm' and b == 'u' and c == 't' and isdel(d)) {
                token.kind = TokenKind.Mut;
                token.span = span3;
            }
        },
        'n' => {
            if (a == 'n' and b == 'e' and c == 'w' and isdel(d)) {
                token.kind = TokenKind.New;
                token.span = span3;
            }
        },
        'o' => {
            if (a == 'o' and b == 'k' and isdel(c)) {
                token.kind = TokenKind.Ok;
                token.span = span2;
            }
        },
        'r' => {
            if (a == 'r' and b == 'e' and c == 't' and d == 'u' and e == 'r' and f == 'n' and isdel(g)) {
                token.kind = TokenKind.Return;
                token.span = span6;
            }
        },
        'u' => {
            if (a == 'u' and b == 's' and c == 'e' and isdel(d)) {
                token.kind = TokenKind.Use;
                token.span = span3;
            }
        },

        '!' => {
            token.kind = TokenKind.Exclamation;
        },
        ';' => {
            token.kind = TokenKind.Semicolon;
        },
        '&' => {
            token.kind = TokenKind.Ampersand;
        },
        '\\' => {
            token.kind = TokenKind.BackSlash;
        },
        '?' => {
            token.kind = TokenKind.Question;
        },
        ':' => {
            if (a == ':' and b == ':') {
                token.kind = TokenKind.ColonColon;
                token.span = span2;
            } else {
                token.kind = TokenKind.Colon;
            }
        },
        ',' => {
            token.kind = TokenKind.Comma;
        },
        '#' => {
            token.kind = TokenKind.Hash;
        },
        '.' => {
            if (a == '.' and b == '.' and c == '=') {
                token.kind = TokenKind.DotDotEq;
                token.span = span3;
            } else if (a == '.' and b == '.') {
                token.kind = TokenKind.DotDot;
                token.span = span2;
            } else {
                token.kind = TokenKind.Dot;
            }
        },
        '=' => {
            if (a == '=' and b == '=') {
                token.kind = TokenKind.EqEq;
                token.span = span2;
            } else {
                token.kind = TokenKind.Eq;
            }
        },
        '@' => {
            token.kind = TokenKind.At;
        },

        '+' => {
            token.kind = TokenKind.Plus;
        },
        '-' => {
            token.kind = TokenKind.Minus;
        },
        '*' => {
            token.kind = TokenKind.Star;
        },
        '/' => {
            if (a == '/' and b == '/' and c == '/') {
                const iend = std.mem.indexOfScalar(u8, code, '\n') orelse code.len;
                token.kind = TokenKind.Document;
                token.span.end = @truncate(ichar + iend);
            } else if (a == '/' and b == '/') {
                const iend = std.mem.indexOfScalar(u8, code, '\n') orelse code.len;
                token.kind = TokenKind.Comment;
                token.span.end = @truncate(ichar + iend);
            } else {
                token.kind = TokenKind.Slash;
            }
        },

        '(' => {
            token.kind = TokenKind.LParen;
        },
        ')' => {
            token.kind = TokenKind.RParen;
        },
        '{' => {
            token.kind = TokenKind.LBrace;
        },
        '}' => {
            token.kind = TokenKind.RBrace;
        },

        else => {},
    }

    // Parse identifiers and numbers
    if (token.kind == TokenKind.None) {
        if (std.ascii.isAlphabetic(a) or a == '_') {
            var iend: usize = 0;
            for (code) |x| {
                if (std.ascii.isAlphanumeric(x) or x == '_') {
                    iend += 1;
                } else {
                    break;
                }
            }
            token.kind = TokenKind.IdVal;
            token.span.end = @truncate(ichar + iend);
        } else if (std.ascii.isDigit(a)) {
            var iend: usize = 0;
            for (code) |x| {
                if (std.ascii.isDigit(x)) {
                    iend += 1;
                } else {
                    break;
                }
            }
            token.kind = TokenKind.IntVal;
            token.span.end = @truncate(ichar + iend);
        }
    }

    return .{ token, code[token.len()..] };
}

fn nextChar(code: []const u8, i: usize) u8 {
    if (i < code.len) {
        return code[i];
    } else {
        return ' ';
    }
}

fn isdel(c: u8) bool {
    return switch (c) {
        '0'...'9', 'a'...'z', 'A'...'Z' => false,
        else => true,
    };
}
