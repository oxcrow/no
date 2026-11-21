package parse

import (
	. "no/core"
	. "no/data"
)

// Ambiguities we should be aware of?
// Nothing. We do not depend on whitespace (newlines). They suck.
// We shall thus design the grammar to work without semicolons, using lookahead tokens.
// But there are somethings we can do to avoid looking ahead indefinitely forever.
//
// Case: Expr that span multiple lines.
//
//	x = x + 1
//	-1
//
// We assume that anything we can parse as expr, will be parsed as expr.
// It doesn't matter that -1 is on a separate line. It should be parsed as, `x = x + 1 - 1`
//
// Case: Expr that terminates based on grammar
//
//	x = x + 1
//	y = -1
//
// We assume that `x + 1 y` can not be a valid expr, thus `y = -1` is a seperate stmt.
//
// Case: Struct init across lines
//
//	x = Node
//	{
//	}
//
// We don't know for sure if the {} is intended to be a block of scope, or struct.
// While it is not ambiguous, we have to lookahead many tokens to parse it.
// It might be safer/faster if we add a dot '.' at the end of Node.
//
//	x = Node.{}
//
// Now we can parse it immediately as a struct initialization.
// Same rule applies to tuples, also.

type Parser struct {
	State ParserState
	Code  string
	Ast   Ast
}

type ParserState struct {
	// On which character/line are we parsing?
	CharIdx int
	LineIdx int
	// Current and next token
	Curr Token
	Next Token
}

func (p Parser) CurrKind() TokenKind {
	return p.State.Curr.Kind
}

func (p Parser) NextKind() TokenKind {
	return p.State.Next.Kind
}

func (p Parser) IsEof() bool {
	return p.State.CharIdx >= len(p.Code)
}

func ParseFile(filePath string) (Ast, string) {
	code := ReadFile(filePath)

	guessNumNodes := guessNumNodes(len(code))
	p := Parser{
		State: ParserState{
			CharIdx: 0,
			LineIdx: 0,
		},
		Code: code,
		Ast: Ast{
			Nodes: make([]Node, 0, guessNumNodes),
			Spans: make([]uint32, 0, guessNumNodes),
			File:  filePath,
		},
	}

	testLexer(p)

	return p.Ast, code
}

func LexWord(code string, charIdx int, lineIdx int) Token {
	s1 := [2]uint32{uint32(charIdx), uint32(charIdx + 1)}
	s2 := [2]uint32{uint32(charIdx), uint32(charIdx + 2)}
	s3 := [2]uint32{uint32(charIdx), uint32(charIdx + 3)}
	s4 := [2]uint32{uint32(charIdx), uint32(charIdx + 4)}
	s5 := [2]uint32{uint32(charIdx), uint32(charIdx + 5)}
	s6 := [2]uint32{uint32(charIdx), uint32(charIdx + 6)}
	s8 := [2]uint32{uint32(charIdx), uint32(charIdx + 8)}

	t := Token{LineIdx: uint32(lineIdx), Span: s1, Kind: TOKEN_NONE}

	a := charAt(code, charIdx+0)
	b := charAt(code, charIdx+1)
	c := charAt(code, charIdx+2)
	d := charAt(code, charIdx+3)
	e := charAt(code, charIdx+4)
	f := charAt(code, charIdx+5)
	g := charAt(code, charIdx+6)
	h := charAt(code, charIdx+7)
	i := charAt(code, charIdx+8)

	switch a {
	case ' ', '\t', '\n':
		t.Kind = TOKEN_WHITESPACE
	case '\r':
		Die([]string{
			Damn("Unable to tokenize Carraige Return (\\r) from code."),
			"",
			Info("Sorry, I don't allow Carraige Returns in my code."),
			Info("They're not portable to Linux/MAC. I don't feel"),
			Info("the need to infect my code with them."),
			"",
			Hint("Instead, use Line Feed (\\n), please."),
			Hint("Configure your IDE to emit \\n."),
		})

	case 'a':
		if a == 'a' && b == 'n' && c == 'd' && isdel(d) {
			t.Kind = TOKEN_AND
			t.Span = s3
		}
	case 'b':
		if a == 'b' && b == 'r' && c == 'e' && d == 'a' && e == 'k' && isdel(f) {
			t.Kind = TOKEN_BREAK
			t.Span = s5
		} else if a == 'b' && b == 'o' && c == 'o' && d == 'l' && isdel(e) {
			t.Kind = TOKEN_BOOL
			t.Span = s4
		}
	case 'c':
		if a == 'c' && b == 'o' && c == 'n' && d == 't' && e == 'i' && f == 'n' && g == 'u' && h == 'e' && isdel(i) {
			t.Kind = TOKEN_CONTINUE
			t.Span = s8
		}
	case 'd':
		if a == 'd' && b == 'e' && c == 'f' && isdel(d) {
			t.Kind = TOKEN_DEF
			t.Span = s3
		}
	case 'e':
		if a == 'e' && b == 'l' && c == 's' && d == 'e' && isdel(e) {
			t.Kind = TOKEN_ELSE
			t.Span = s4
		} else if a == 'e' && b == 'n' && c == 'u' && d == 'm' && isdel(e) {
			t.Kind = TOKEN_ENUM
			t.Span = s4
		}
	case 'f':
		if a == 'f' && b == 'a' && c == 'l' && d == 's' && e == 'e' && isdel(f) {
			t.Kind = TOKEN_XFALSE
			t.Span = s5
		} else if a == 'f' && b == 'o' && c == 'r' && isdel(d) {
			t.Kind = TOKEN_FOR
			t.Span = s3
		} else if a == 'f' && b == 'n' && isdel(c) {
			t.Kind = TOKEN_FUNC
			t.Span = s2
		}
	case 'i':
		if a == 'i' && b == 'f' && isdel(c) {
			t.Kind = TOKEN_IF
			t.Span = s2
		} else if a == 'i' && b == 'n' && isdel(c) {
			t.Kind = TOKEN_IN
			t.Span = s2
		} else if a == 'i' && b == 'n' && c == 't' && isdel(d) {
			t.Kind = TOKEN_INT
			t.Span = s3
		} else if a == 'i' && b == '6' && c == '4' && isdel(d) {
			t.Kind = TOKEN_I64
			t.Span = s3
		} else if a == 'i' && b == '3' && c == '2' && isdel(d) {
			t.Kind = TOKEN_I32
			t.Span = s3
		} else if a == 'i' && b == '1' && c == '6' && isdel(d) {
			t.Kind = TOKEN_I16
			t.Span = s3
		} else if a == 'i' && b == '8' && isdel(c) {
			t.Kind = TOKEN_I8
			t.Span = s2
		}
	case 'l':
		if a == 'l' && b == 'i' && c == 'b' && isdel(d) {
			t.Kind = TOKEN_LIB
			t.Span = s3
		}
	case 'm':
		if a == 'm' && b == 'a' && c == 'c' && d == 'r' && e == 'o' && isdel(f) {
			t.Kind = TOKEN_MACRO
			t.Span = s5
		} else if a == 'm' && b == 'a' && c == 't' && d == 'c' && e == 'h' && isdel(f) {
			t.Kind = TOKEN_MATCH
			t.Span = s5
		} else if a == 'm' && b == 'o' && c == 'd' && isdel(d) {
			t.Kind = TOKEN_MOD
			t.Span = s3
		}
	case 'o':
		if a == 'o' && b == 'r' && isdel(c) {
			t.Kind = TOKEN_OR
			t.Span = s2
		}
	case 'p':
		if a == 'p' && b == 'u' && c == 'b' && isdel(d) {
			t.Kind = TOKEN_PUB
			t.Span = s3
		}
	case 'r':
		if a == 'r' && b == 'e' && c == 't' && d == 'u' && e == 'r' && f == 'n' && isdel(g) {
			t.Kind = TOKEN_RETURN
			t.Span = s6
		} else if a == 'r' && b == 'i' && c == 's' && d == 'k' && isdel(e) {
			t.Kind = TOKEN_RISK
			t.Span = s4
		}
	case 's':
		if a == 's' && b == 'a' && c == 'f' && d == 'e' && isdel(e) {
			t.Kind = TOKEN_SAFE
			t.Span = s4
		} else if a == 's' && b == 't' && c == 'r' && d == 'u' && e == 'c' && f == 't' && isdel(g) {
			t.Kind = TOKEN_STRUCT
			t.Span = s6
		}
	case 't':
		if a == 't' && b == 'r' && c == 'u' && d == 'e' && isdel(e) {
			t.Kind = TOKEN_XTRUE
			t.Span = s4
		} else if a == 't' && b == 'h' && c == 'e' && d == 'n' && isdel(e) {
			t.Kind = TOKEN_THEN
			t.Span = s4
		} else if a == 't' && b == 'y' && c == 'p' && d == 'e' && isdel(e) {
			t.Kind = TOKEN_TYPE
			t.Span = s4
		}

	case 'u':
		if a == 'u' && b == 'n' && c == 'i' && d == 'o' && e == 'n' && isdel(f) {
			t.Kind = TOKEN_UNION
			t.Span = s5
		} else if a == 'u' && b == 's' && c == 'e' && isdel(d) {
			t.Kind = TOKEN_USE
			t.Span = s3
		} else if a == 'u' && b == '6' && c == '4' && isdel(d) {
			t.Kind = TOKEN_U64
			t.Span = s3
		} else if a == 'u' && b == '3' && c == '2' && isdel(d) {
			t.Kind = TOKEN_U32
			t.Span = s3
		} else if a == 'u' && b == '1' && c == '6' && isdel(d) {
			t.Kind = TOKEN_U16
			t.Span = s3
		} else if a == 'u' && b == '8' && isdel(c) {
			t.Kind = TOKEN_U8
			t.Span = s2
		}

	case '!':
		if a == '!' && b == '=' {
			t.Kind = TOKEN_NOTEQ
			t.Span = s2
		} else {
			t.Kind = TOKEN_EXCLAMATION
		}
	case ';':
		t.Kind = TOKEN_SEMICOLON
	case '&':
		t.Kind = TOKEN_AMPERSAND
	case '?':
		t.Kind = TOKEN_QUESTION
	case ':':
		t.Kind = TOKEN_COLON
	case ',':
		t.Kind = TOKEN_COMMA
	case '#':
		t.Kind = TOKEN_HASH
	case '+':
		if a == '+' && b == '=' {
			t.Kind = TOKEN_PLUSEQ
			t.Span = s2
		} else {
			t.Kind = TOKEN_PLUS
		}
	case '-':
		if a == '-' && b == '>' {
			t.Kind = TOKEN_RARROW
			t.Span = s2
		} else if a == '-' && b == '=' {
			t.Kind = TOKEN_MINUSEQ
			t.Span = s2
		} else {
			t.Kind = TOKEN_MINUS
		}
		t.Kind = TOKEN_MINUS
	case '*':
		if a == '*' && b == '=' {
			t.Kind = TOKEN_STAREQ
			t.Span = s2
		} else {
			t.Kind = TOKEN_STAR
		}
	case '/':
		if a == '/' && b == '/' {
			endIdx := len(code)
			for i, c := range code[charIdx:] {
				if c == '\n' {
					endIdx = charIdx + i
					break
				}
			}
			t.Kind = TOKEN_COMMENT
			t.Span[1] = uint32(endIdx)
		} else if a == '/' && b == '*' {
			endIdx := len(code)
		outCom:
			for i, c := range code[charIdx+2:] {
				d := charAt(code, (charIdx+2)+(i+1)) // charIdx+2 to skip the /*, then i+1 to skip */
				if c == '*' && d == '/' {
					endIdx = charIdx + i + 2 + 2 // +2 for /*, +2 for */
					break outCom
				} else if c == '/' && d == '*' {
					Die([]string{
						Damn("Unable to tokenize multiple nested multi line comments."),
						"",
						Info("Nested multi line comments make the lexer complex."),
						Info("Specifically, they require a state to tokenize."),
						Info("Thus, I can not compile them."),
						"",
						Hint("Please use multi line comments more judiciously."),
						Hint("If possible, only use them for fixing bugs by"),
						Hint("blocking out sections of code, for some time."),
						"",
						Hint("Prefer to use /// for documentation."),
						Hint("Prefer to use //  for comments."),
					})
				}
			}
			t.Kind = TOKEN_COMMENT
			t.Span[1] = uint32(endIdx)
		} else if a == '/' && b == '=' {
			t.Kind = TOKEN_SLASHEQ
			t.Span = s2
		} else {
			t.Kind = TOKEN_SLASH
		}
	case '=':
		if a == '=' && b == '=' {
			t.Kind = TOKEN_EQEQ
			t.Span = s2
		} else {
			t.Kind = TOKEN_EQ
		}
	case '<':
		if a == '<' && b == '=' {
			t.Kind = TOKEN_LTEQ
			t.Span = s2
		} else {
			t.Kind = TOKEN_LT
		}
	case '>':
		if a == '>' && b == '=' {
			t.Kind = TOKEN_GTEQ
			t.Span = s2
		} else {
			t.Kind = TOKEN_GT
		}
	case '.':
		if a == '.' && b == '.' {
			t.Kind = TOKEN_DOTDOT
			t.Span = s2
		} else {
			t.Kind = TOKEN_DOT
		}
	case '|':
		t.Kind = TOKEN_BAR

	case '(':
		t.Kind = TOKEN_LPAREN
	case ')':
		t.Kind = TOKEN_RPAREN
	case '{':
		t.Kind = TOKEN_LBRACE
	case '}':
		t.Kind = TOKEN_RBRACE
	case '[':
		t.Kind = TOKEN_LBRACK
	case ']':
		t.Kind = TOKEN_RBRACK

	default:
	}

	// If no token is found, maybe it's name or digit?
	if t.Kind == TOKEN_NONE {
		if isAlpha(a) || a == '_' || a == '@' {
			endIdx := len(code)
			for i, c := range code[charIdx:] {
				if a == '@' && i == 0 {
					continue // Skip the @ token
				}
				if !isAlpha(byte(c)) && !isDigit(byte(c)) && c != '_' {
					endIdx = charIdx + i
					break
				}
			}
			t.Kind = TOKEN_XNAME
			t.Span[1] = uint32(endIdx)
		} else if isDigit(a) {
			endIdx := len(code)
			for i, c := range code[charIdx:] {
				if !isDigit(byte(c)) && c != '_' {
					endIdx = charIdx + i
					break
				}
			}
			t.Kind = TOKEN_XINT
			t.Span[1] = uint32(endIdx)
		}
	}

	// Maybe its a string?
	if a == '"' {
		endIdx := len(code)
	out:
		for i, c := range code[charIdx+1:] {
			switch c {
			case '"':
				endIdx = charIdx + i + 2 // +1 for first quote, +1 for second
				break out
			case '\n':
				Die([]string{
					Damn("Unable to tokenize string that span multiple lines."),
					"",
					Info("Strings that span multiple lines, are not allowed."),
					Info("I expect a string to be on one line."),
					"",
					Hint("Fix your code, please."),
				})
			default:
			}
		}
		t.Kind = TOKEN_XSTR
		t.Span[1] = uint32(endIdx)
	}

	return t
}

func testLexer(p Parser) {
	for !p.IsEof() {
		if p.Code[p.State.CharIdx] == '\n' {
			p.State.LineIdx++
		}
		t := LexWord(p.Code, p.State.CharIdx, p.State.LineIdx)
		if !t.Kind.IsValid() {
			Die(Todo("token-is-not-valid"))
		}
		p.State.CharIdx += int(t.Span[1] - t.Span[0])
		Dbg("Line: ", t.LineIdx, ",\tKind: ", t.Kind, ",\tToken: ", p.Code[t.Span[0]:t.Span[1]])
	}
}

func charAt(code string, charIdx int) byte {
	if charIdx < len(code) {
		return code[charIdx]
	}
	return ' '
}

func isdel(c byte) bool {
	switch {
	case c >= '0' && c <= '9':
		return false
	case c >= 'a' && c <= 'z':
		return false
	case c >= 'A' && c <= 'Z':
		return false
	default:
		return true
	}
}

func isAlpha(c byte) bool {
	switch {
	case c >= 'a' && c <= 'z':
		return true
	case c >= 'A' && c <= 'Z':
		return true
	default:
		return false
	}
}

func isDigit(c byte) bool {
	switch {
	case c >= '0' && c <= '9':
		return true
	default:
		return false
	}
}

func guessNumNodes(codeLen int) int {
	switch {
	// For small files upto 2 KB, we allocate enough nodes for almost all files
	case codeLen < 2048:
		return 256
	// For medium files upto 1 MB, we slightly reduce the number of nodes
	case codeLen < 1_048_576:
		return codeLen / 8
	// For extreme files, we aggressively reduce nodes to save memory
	default:
		return codeLen / 12
	}
}
