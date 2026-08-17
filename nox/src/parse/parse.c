#include "parse.h"

#include <ctype.h>
#include <string.h>

Token lexNextToken(Parser * p, Status * s);

/// What is the character at given index?
static char charAtIndex(const char * text, usize charIndex, usize codeLen) {
    if (charIndex >= codeLen) {
        return ' ';
    }
    return text[at(charIndex, codeLen)];
}

/// Is character a delimiter?
static bool isdel(char c) {
    if (isalnum(c)) {
        return false;
    } else {
        return true;
    }
}

/// Character index at the next newline.
static usize endOfLineCharIndex(const char * code, usize charIndex, usize codeLen) {
    for (usize i = charIndex; i < codeLen; i++) {
        const char c = code[at(i, codeLen)];
        if (c == '\n') {
            return i;
        }
    }
    return codeLen;
}

Token lexWord(const char * code, usize lineIndex, usize charIndex, usize codeLen) {
    const char a = charAtIndex(code, charIndex + 0, codeLen);
    const char b = charAtIndex(code, charIndex + 1, codeLen);
    const char c = charAtIndex(code, charIndex + 2, codeLen);
    const char d = charAtIndex(code, charIndex + 3, codeLen);
    const char e = charAtIndex(code, charIndex + 4, codeLen);

    const u32 span1[2] = {charIndex, charIndex + 1};
    const u32 span2[2] = {charIndex, charIndex + 2};
    const u32 span3[2] = {charIndex, charIndex + 3};
    const u32 span4[2] = {charIndex, charIndex + 4};

    Token token = {
        .kind = TOKEN_UNKNOWN,
        .spanRange = {span1[0], span1[1]},
        .lineIndex = lineIndex,
        .hash = 0,
    };

    switch (a) {
    case ' ':
    case '\t':
        token.kind = TOKEN_WHITE;
        break;
    case '\n':
        token.kind = TOKEN_LINE;
        break;
    case '\r':
        dieAt("Unable to lex Carraige Return (\\r). It is illegal.", XFILE, XLINE);
        break;
    case 'a': {
        if (a == 'a' && b == 's' && isdel(c)) {
            token.kind = TOKEN_AS;
            token.spanRange[0] = span2[0];
            token.spanRange[1] = span2[1];
        }
        break;
    }
    case 'd': {
        if (a == 'd' && b == 'e' && c == 'f' && isdel(d)) {
            token.kind = TOKEN_DEF;
            token.spanRange[0] = span3[0];
            token.spanRange[1] = span3[1];
        }
        break;
    }
    case 'e': {
        if (a == 'e' && b == 'l' && c == 's' && d == 'e' && isdel(e)) {
            token.kind = TOKEN_ELSE;
            token.spanRange[0] = span4[0];
            token.spanRange[1] = span4[1];
        }
        break;
    }
    case 'f': {
        if (a == 'f' && b == 'n' && isdel(c)) {
            token.kind = TOKEN_FN;
            token.spanRange[0] = span2[0];
            token.spanRange[1] = span2[1];
        }
        break;
    }
    case 'i': {
        if (a == 'i' && b == 'f' && isdel(c)) {
            token.kind = TOKEN_IF;
            token.spanRange[0] = span2[0];
            token.spanRange[1] = span2[1];
        }
        break;
    }
    case 'l': {
        if (a == 'l' && b == 'e' && c == 't' && isdel(d)) {
            token.kind = TOKEN_LET;
            token.spanRange[0] = span3[0];
            token.spanRange[1] = span3[1];
        }
        break;
    }
    case 'm': {
        if (a == 'm' && b == 'o' && c == 'd' && isdel(d)) {
            token.kind = TOKEN_MOD;
            token.spanRange[0] = span3[0];
            token.spanRange[1] = span3[1];
        }
        break;
    }
    case 'r': {
        if (a == 'r' && b == 'u' && c == 'n' && isdel(d)) {
            token.kind = TOKEN_RUN;
            token.spanRange[0] = span3[0];
            token.spanRange[1] = span3[1];
        }
        break;
    }
    //
    case ';': {
        token.kind = TOKEN_SEMICOLON;
        break;
    }
    case ':': {
        token.kind = TOKEN_COLON;
        break;
    }
    case ',': {
        token.kind = TOKEN_COMMA;
        break;
    }
    case '$': {
        token.kind = TOKEN_DOLLAR;
        break;
    }
    case '#': {
        token.kind = TOKEN_HASH;
        break;
    }
    case '.': {
        if (a == '.' && b == '.') {
            token.kind = TOKEN_DOTDOT;
            token.spanRange[0] = span2[0];
            token.spanRange[1] = span2[1];
        } else {
            token.kind = TOKEN_DOT;
        }
        break;
    }
    //
    case '(': {
        token.kind = TOKEN_LPAREN;
        break;
    }
    case ')': {
        token.kind = TOKEN_RPAREN;
        break;
    }
    case '{': {
        token.kind = TOKEN_LBRACE;
        break;
    }
    case '}': {
        token.kind = TOKEN_RBRACE;
        break;
    }
    case '[': {
        token.kind = TOKEN_LBRACK;
        break;
    }
    case ']': {
        token.kind = TOKEN_RBRACK;
        break;
    }
    //
    case '=': {
        if (a == '=' && b == '=') {
            token.kind = TOKEN_EQEQ;
            token.spanRange[0] = span2[0];
            token.spanRange[1] = span2[1];
        } else {
            token.kind = TOKEN_EQ;
        }
        break;
    }
    case '>': {
        if (a == '>' && b == '=') {
            token.kind = TOKEN_GTEQ;
            token.spanRange[0] = span2[0];
            token.spanRange[1] = span2[1];
        } else {
            token.kind = TOKEN_GT;
        }
        break;
    }
    case '<': {
        if (a == '<' && b == '=') {
            token.kind = TOKEN_LTEQ;
            token.spanRange[0] = span2[0];
            token.spanRange[1] = span2[1];
        } else {
            token.kind = TOKEN_LT;
        }
        break;
    }
    //
    case '+': {
        token.kind = TOKEN_PLUS;
        break;
    }
    case '-': {
        if (a == '-' && b == '>') {
            token.kind = TOKEN_RARROW;
            token.spanRange[0] = span2[0];
            token.spanRange[1] = span2[1];
        } else {
            token.kind = TOKEN_MINUS;
        }
        break;
    }
    case '*': {
        token.kind = TOKEN_STAR;
        break;
    }
    case '/': {
        if (a == '/' && b == '/') {
            token.kind = TOKEN_COMMENT;
            token.spanRange[0] = token.spanRange[0];
            token.spanRange[1] = endOfLineCharIndex(code, charIndex, codeLen);
        } else {
            token.kind = TOKEN_SLASH;
        }
        break;
    }
    //
    case '\"': {
        usize endOfStr = codeLen;
        for (usize i = charIndex + 1; i < codeLen; i++) {
            const char x = charAtIndex(code, i, codeLen);
            if (x == '\"') {
                endOfStr = i;
                break;
            }
        }
        token.kind = TOKEN_XSTR;
        token.spanRange[0] = token.spanRange[0];
        token.spanRange[1] = endOfStr;
        break;
    }
    //
    default:
        break;
    }

    if (token.kind == TOKEN_UNKNOWN) {
        // Lex identifiers
        if (isalpha(a) || a == '_') {
            // Find the end of identifer and store it
            usize endOfName = codeLen;
            for (usize i = charIndex; i < codeLen; i++) {
                const char x = charAtIndex(code, i, codeLen);
                if (!isalpha(x)) {
                    endOfName = i;
                    break;
                }
            }
            token.kind = TOKEN_XNAME;
            token.spanRange[0] = token.spanRange[0];
            token.spanRange[1] = endOfName;
        }
        // TODO: Lex integer, float, octal, hex, binary numbers
        if (isdigit(a)) {
            // Find the end of integer number and store it
            usize endOfNumber = codeLen;
            for (usize i = charIndex; i < codeLen; i++) {
                const char x = charAtIndex(code, i, codeLen);
                if (!isdigit(x)) {
                    endOfNumber = i;
                    break;
                }
            }
            token.kind = TOKEN_XINT;
            token.spanRange[0] = token.spanRange[0];
            token.spanRange[1] = endOfNumber;
        }
        // Lex nicknames
        if (a == '@') {
            // Find the end of nickname and store it
            usize endOfName = codeLen;
            for (usize i = charIndex + 1; i < codeLen; i++) {
                const char x = charAtIndex(code, i, codeLen);
                if (!isalpha(x)) {
                    endOfName = i;
                    break;
                }
            }
            token.kind = TOKEN_XNICK;
            token.spanRange[0] = token.spanRange[0];
            token.spanRange[1] = endOfName;
        }
    }

    return token;
}

TokenKind currTokenKind(const Parser * p) {
    return p->state.currToken.kind;
}

TokenKind peekTokenKind(const Parser * p) {
    return p->state.nextToken.kind;
}

static bool tokenIsWhitespace(TokenKind kind) {
    switch (kind) {
    case TOKEN_WHITE:
    case TOKEN_LINE:
    case TOKEN_COMMENT:
        return true;
    default:
        return false;
    }
}

void skipToken(Parser * p, Status * s) {
    p->state.currToken = p->state.nextToken;
    lexNextToken(p, s); back(unit, s);
}

void skipWhitespace(Parser * p, Status * s) {
    Token token = p->state.currToken;
    while (tokenIsWhitespace(token.kind)) {
        // Skip token so we get rid of the whitespace
        // Assign next token to end this loop
        token = lexNextToken(p, s);
    }
}

Token lexNextToken(Parser * p, Status * s) {
    // Ensure that we do not read out of bounds, by returning an END(Of File) token
    if (p->state.charIndex >= p->state.codeLen) {
        return (Token){.kind = TOKEN_END};
    }
    // Track new lines
    if (p->state.code[at(p->state.charIndex, p->state.codeLen)] == '\n') {
        p->state.lineIndex++;
    }

    // Lex next token and ensure that is valid before updating parser state
    const Token token = lexWord(
        p->state.code, p->state.lineIndex, p->state.charIndex, p->state.codeLen
    );
    if (token.kind == TOKEN_UNKNOWN) {
        duck(s, "Unable to parse unknown token.");
        return (Token){0};
    } else {
        // Skip all whitespaces until we find a valid token
        if (tokenIsWhitespace(token.kind)) {
            p->state.charIndex = token.spanRange[1];
            lexNextToken(p, s); back((Token){0}, s);
        } else {
            p->state.charIndex = token.spanRange[1];
            p->state.nextToken = token;
            p->state.tokenIndex++;
        }
    }

    return token;
}

Token eatToken(Parser * p, TokenKind kind, Status * s) {
    skipWhitespace(p, s); back((Token){0}, s);
    const Token token = p->state.currToken;

    if (token.kind != kind) {
        duck(s, "Unable to parse and match token.");
        return (Token){0};
    } else {
        skipToken(p, s); back((Token){0}, s);
    }

    return token;
}

u32 parseNameStep(Parser * p, Status * s) {
    eatToken(p, TOKEN_XNAME, s); back(0, s);
    return 0;
}

u32x2 parseNameList(Parser * p, TokenKind separator, TokenKind listEndToken, Status * s) {
    while (currTokenKind(p) == TOKEN_XNAME) {
        const u32 nameIndex = parseNameStep(p, s); back((u32x2){0}, s);
        if (currTokenKind(p) == listEndToken) {
            break;
        } else {
            eatToken(p, separator, s); back((u32x2){0}, s);
        }
    }
    return (u32x2){0};
}

u32 parseModStep(Allocator * mem, Parser * p, Status * s) {
    eatToken(p, TOKEN_MOD, s); back(0, s);
    eatToken(p, TOKEN_XNAME, s); back(0, s);

    switch (currTokenKind(p)) {
    case TOKEN_DOT: {
        eatToken(p, TOKEN_DOT, s); back(0, s);
        eatToken(p, TOKEN_LBRACE, s); back(0, s);
        const u32x2 nameRange = parseNameList(p, TOKEN_COMMA, TOKEN_RBRACE, s); back(0, s);
        eatToken(p, TOKEN_RBRACE, s); back(0, s);
        break;
    }
    default:
        break;
    }

    switch (currTokenKind(p)) {
    case TOKEN_AS: {
        eatToken(p, TOKEN_AS, s); back(0, s);
        const u32 aliasIndex = parseNameStep(p, s); back(0, s);
        break;
    }
    default:
        break;
    }

    eatToken(p, TOKEN_SEMICOLON, s); back(0, s);

    return 0;
}

u32x2 parseModList(Allocator * mem, Parser * p, Status * s) {
    while (currTokenKind(p) == TOKEN_MOD) {
        const u32 modIndex = parseModStep(mem, p, s); back((u32x2){0}, s);
    }
    return (u32x2){0};
}

void parseFileStep(Allocator * mem, Parser * p, Status * s) {
    parseModList(mem, p, s); back(unit, s);
}

void parseFile(Allocator * mem, const char * filePath, const char * code, Status * s) {
    Parser p = {
        .state = {
            .filePath = filePath,
            .code = code,
            .codeLen = strlen(code),
            .tokenIndex = 0,
            .charIndex = 0,
            .lineIndex = 0
        }
    };

    lexNextToken(&p, s); back(unit, s);
    skipToken(&p, s); back(unit, s);

    parseFileStep(mem, &p, s); back(unit, s);
}
