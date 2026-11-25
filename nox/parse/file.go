package parse

import (
	. "no/core"
	. "no/data"

	"fmt"
	"runtime"
	"strings"
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

func (p Parser) CurrToken() Token {
	return p.State.Curr
}

func (p Parser) CurrKind() TokenKind {
	return p.State.Curr.Kind
}

func (p Parser) NextKind() TokenKind {
	return p.State.Next.Kind
}

func (p Parser) CurrText() string {
	return p.Code[p.State.Curr.Span[0]:p.State.Curr.Span[1]]
}

func (p Parser) CurrLineText() string {
	startIdx := p.State.CharIdx
	endIdx := len(p.Code)
	for i := p.State.CharIdx; i >= 0; i-- {
		if p.Code[i] == '\n' {
			startIdx = i + 1
			break
		}
	}
	for i, c := range p.Code[p.State.CharIdx:] {
		if c == '\n' {
			endIdx = p.State.CharIdx + i
			break
		}
	}
	return p.Code[startIdx:endIdx]
}

func (p Parser) PeekLineText(lineIdx int) string {
	startIdx := 0
	endIdx := len(p.Code)
	newLineIdx := 0
	found := false

	s := ""

	if lineIdx >= 0 {
		for i, c := range p.Code {
			if c == '\n' {
				newLineIdx++
			}
			if newLineIdx == lineIdx && startIdx == 0 {
				// We can't break out, so we only run this once
				startIdx = i + 1
			} else if newLineIdx == lineIdx+1 {
				endIdx = i
				found = true
				break
			}
		}
		if found {
			if p.State.CharIdx >= startIdx && p.State.CharIdx <= endIdx {
				// BUG: This code causes out of bounds error, when erroridx is negative
				errorIdx := startIdx + (int(p.CurrToken().Span[0]) - startIdx)
				s = p.Code[startIdx:errorIdx] +
					fmt.Sprintf("%s%s%s%s", ANSI_ITALIC, ANSI_RED, "<<<look around here>>>", ANSI_RESET) +
					p.Code[errorIdx:endIdx]
			} else {
				s = p.Code[startIdx:endIdx]
			}
		}
	}

	return s
}

func (p Parser) PeekKind(steps uint32) TokenKind {
	charIdx := int(p.State.Curr.Span[0])
	lineIdx := int(p.State.Curr.LineIdx)
	t := p.State.Curr

	for i := 0; i < int(steps+1); i++ {
		if p.IsEof() {
			return TOKEN_NONE
		}
		if p.Code[charIdx] == '\n' {
			lineIdx++
		}
		{
			n := LexWord(p.Code, charIdx, lineIdx)
			for func() bool {
				switch n.Kind {
				case TOKEN_WHITESPACE, TOKEN_COMMENT:
					return true
				default:
					return false
				}
			}() {
				n = LexWord(p.Code, int(n.Span[1]), int(n.LineIdx))
			}
			t = n
		}
		charIdx += t.Len()
	}

	return t.Kind
}

func (p Parser) IsEof() bool {
	return p.State.CharIdx >= len(p.Code)
}

func (p *Parser) LexWord() {
	if p.IsEof() {
		p.State.Curr = p.State.Next
		p.State.Next.Kind = TOKEN_NONE
		p.State.Next.Span[0] = uint32(len(p.Code))
		p.State.Next.Span[1] = uint32(len(p.Code) + 1)
		return
	}
	if p.Code[p.State.CharIdx] == '\n' {
		p.State.LineIdx++
	}

	t := LexWord(p.Code, p.State.CharIdx, p.State.LineIdx)
	if !t.Kind.IsValid() {
		Die([]string{Damn("Parsed token is not valid")})
	}

	switch t.Kind {
	case TOKEN_WHITESPACE, TOKEN_COMMENT:
		p.State.CharIdx += t.Len()
		p.LexWord()
	default:
		p.State.Curr = p.State.Next
		p.State.Next = t
		p.State.CharIdx += t.Len()
	}
}

func (p Parser) Hecc(e int) []string {
	l := []string{
		"",
		Info(ANSI_ITALIC + "Could you kindly look around the marked section of code?" + ANSI_RESET),
		"",
		Line(p.State.LineIdx+1-2, p.PeekLineText(p.State.LineIdx-2)),
		Line(p.State.LineIdx+1-1, p.PeekLineText(p.State.LineIdx-1)),
		Line(p.State.LineIdx+1+0, p.PeekLineText(p.State.LineIdx+0)),
		Line(p.State.LineIdx+1+1, p.PeekLineText(p.State.LineIdx+1)),
		Line(p.State.LineIdx+1+2, p.PeekLineText(p.State.LineIdx+2)),
	}
	s := []string{}
	h := []string{}

	if e == 0 {
		s = []string{Damn("Unable to match expected token while parsing code.")}
		h = []string{
			Hint("Most likely you wrote code with wrong grammar."),
			Hint("I don't know what hints will help fix this."),
			Hint("So, \"Words of encouragement\" ~ Rocky"),
		}
		s = append(s, l...)
		s = append(s, "")
		s = append(s, h...)
	} else {
		e, h := Buck(e)
		s = append(s, e...)
		s = append(s, l...)
		if len(h) > 0 {
			s = append(s, "")
			s = append(s, h...)
		}
	}

	return s
}

func (p *Parser) Consume(k TokenKind, e int) Token {
	t := p.State.Curr
	if p.State.Curr.Kind != k {
		_, file, line, _ := runtime.Caller(1)
		_, file, _ = strings.Cut(file, "nox")
		file = "nox" + file
		DieAt(p.Hecc(e), file, line)
	}
	p.LexWord()
	return t
}

func (p *Parser) Maybe(k TokenKind) Token {
	t := p.State.Curr
	if p.State.Curr.Kind == k {
		p.LexWord()
	}
	return t
}

func (p *Parser) Exists(k TokenKind) bool {
	if p.CurrKind() == k {
		p.Consume(k, 0)
		return true
	}
	return false
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
			File:  filePath,
		},
	}

	// Initialize the parser state by running it twice.
	p.LexWord()
	p.LexWord()

	hasMod, modNode := func() (bool, Node) {
		if p.CurrKind() == TOKEN_MOD {
			modToken := p.Consume(TOKEN_MOD, 0)
			switch p.CurrKind() {
			case TOKEN_XNAME:
				nameToken := p.Consume(TOKEN_XNAME, 0)
				Ignore(nameToken)
			case TOKEN_DOT:
				Die(Todo("parse-mod-path"))
			default:
			}
			modNode := Node{
				CharIdx: uint16(modToken.Span[0]),
				Kind:    NODE_MOD,
			}
			return true, modNode
		}
		return false, Node{}
	}()

	ParseEntityList(&p, IsEntity)

	Ignore(hasMod, modNode)
	return p.Ast, code
}

func ParseEntityList(p *Parser, e func(TokenKind) bool) Node {
	n := Node{}
	for e(p.CurrKind()) {
		hasPub := p.Exists(TOKEN_PUB)
		switch p.CurrKind() {
		case TOKEN_FUNC:
			p.Consume(TOKEN_FUNC, 0)
			p.Consume(TOKEN_XNAME, 0)
			p.Consume(TOKEN_LPAREN, 0)
			for p.CurrKind() != TOKEN_RPAREN {
				p.Consume(TOKEN_XNAME, 0)
				ParseType(p)
				if p.CurrKind() != TOKEN_RPAREN {
					p.Consume(TOKEN_COMMA, 0)
				}
			}
			p.Consume(TOKEN_RPAREN, 0)
			ParseRetType(p)
			ParseBlock(p)
		case TOKEN_STRUCT:
			p.Consume(TOKEN_STRUCT, 0)
			p.Consume(TOKEN_XNAME, 0)
			p.Consume(TOKEN_LBRACE, 0)
			// Parse member variables inside structs
			for p.CurrKind() != TOKEN_FUNC && p.CurrKind() != TOKEN_RBRACE {
				hasPub := p.Exists(TOKEN_PUB)
				p.Consume(TOKEN_XNAME, 0)
				ParseType(p)
				if p.CurrKind() != TOKEN_FUNC && p.CurrKind() != TOKEN_RBRACE {
					p.Consume(TOKEN_COMMA, 0)
				}
				Ignore(hasPub)
			}
			// Parse functions and structs inside structs
			for p.CurrKind() != TOKEN_RBRACE {
				hasPub := p.Exists(TOKEN_PUB)
				ParseEntityList(p, IsEntityStruct)
				if !IsEntityStruct(p.CurrKind()) && p.CurrKind() != TOKEN_RBRACE {
					Die(p.Hecc(EStructInStruct))
				}
				Ignore(hasPub)
			}
			p.Consume(TOKEN_RBRACE, 0)
		default:
			Die(Todo("parse-entity"))
		}
		Ignore(hasPub)
	}
	return n
}

func ParseRetType(p *Parser) Node {
	n := Node{}
	switch p.CurrKind() {
	case TOKEN_LBRACE:
		// Unit type
	default:
		n = ParseType(p)
	}
	return n
}

func ParseType(p *Parser) Node {
	n := Node{}
	switch p.CurrKind() {
	case TOKEN_XNAME:
		p.Consume(TOKEN_XNAME, 0)
		n.Kind = NODE_XNAME
	default:
		Die(Todo("parse-type"))
	}
	return n
}

func ParseBlock(p *Parser) Node {
	n := Node{}
	p.Consume(TOKEN_LBRACE, 0)
	s := ParseStmtList(p, InStmt)
	if p.CurrKind() == TOKEN_COLON {
		p.Consume(TOKEN_COLON, 0)
		ParseExpr(p, InStmt)
	}
	p.Consume(TOKEN_RBRACE, 0)
	Ignore(s)
	return n
}

func ParseStmtList(p *Parser, b BlockKind) Node {
	n := Node{}
	for IsStmtOkay(p.CurrKind(), b) {
		switch p.CurrKind() {
		case TOKEN_FOR:
			p.Consume(TOKEN_FOR, 0)
			ParseStmtList(p, InFor)
			p.Consume(TOKEN_LBRACE, 0)
			ParseStmtList(p, InStmt)
			p.Consume(TOKEN_RBRACE, 0)
			hasThen := p.Exists(TOKEN_THEN)
			if hasThen {
				p.Consume(TOKEN_LBRACE, 0)
				ParseStmtList(p, InStmt)
				p.Consume(TOKEN_RBRACE, 0)
			}
		case TOKEN_MATCH:
			Die(Todo("parse-match"))
		case TOKEN_IF:
			Die(Todo("parse-if"))
		case TOKEN_BREAK:
			Die(Todo("parse-break"))
		case TOKEN_CONTINUE:
			Die(Todo("parse-continue"))
		case TOKEN_XNAME, TOKEN_AMPERSAND:
			// Variable assignment
			// a& += expr
			// a& -= expr
			// a& *= expr
			// a& /= expr
			// a&++
			// a&--
			// BUG: Won't work for
			// a[i].& <op> expr
			// a.b.& <op> expr
			k0 := p.CurrKind()
			k1 := p.PeekKind(1)
			k2 := p.PeekKind(2)
			if k0 == TOKEN_XNAME && k1 == TOKEN_AMPERSAND &&
				(k2 == TOKEN_PLUSEQ || k2 == TOKEN_MINUSEQ || k2 == TOKEN_STAREQ || k2 == TOKEN_SLASHEQ) {
				p.Consume(TOKEN_XNAME, 0)
				p.Consume(TOKEN_AMPERSAND, 0)
				p.Consume(k2, 0)
				ParseExpr(p, b)
				p.Maybe(TOKEN_SEMICOLON)
			} else {
				switch p.PeekKind(1) {
				// Single variable declaration
				// a = expr
				// Multi variable declaration
				// a, &b = expr
				// a, = expr
				// Loop iter range
				// a in expr
				case TOKEN_XNAME, TOKEN_EQ, TOKEN_IN, TOKEN_COMMA:
					for p.CurrKind() != TOKEN_EQ && p.CurrKind() != TOKEN_IN {
						hasVar := p.Exists(TOKEN_AMPERSAND)
						p.Consume(TOKEN_XNAME, 0)
						if p.CurrKind() != TOKEN_EQ && p.CurrKind() != TOKEN_IN {
							p.Consume(TOKEN_COMMA, 0)
						}
						Ignore(hasVar)
					}
					hasEq := p.Exists(TOKEN_EQ)
					hasIn := p.Exists(TOKEN_IN)
					ParseExpr(p, b)
					p.Maybe(TOKEN_SEMICOLON)
					Ignore(hasEq, hasIn)
				// Field access and call
				// a.b();
				// a.b
				case TOKEN_DOT:
					switch p.PeekKind(2) {
					case TOKEN_XNAME:
						Die(Todo("parse-path"))
					default:
						Die(Todo("wut?"))
					}
				// Variable updates
				// a& = expr
				case TOKEN_AMPERSAND:
					p.Consume(TOKEN_XNAME, 0)
					p.Consume(TOKEN_AMPERSAND, 0)
					p.Consume(TOKEN_EQ, 0)
					ParseExpr(p, b)
					p.Maybe(TOKEN_SEMICOLON)
				// Common function call
				// a()
				case TOKEN_LPAREN:
					Die(Todo("parse-call"))
				default:
					Die(Todo("wut?"))
				}
			}
		default:
			Die(Todo("parse-stmt"))
		}
	}
	return n
}

func ParseList(p *Parser, k TokenKind, sep TokenKind, end TokenKind) Node {
	n := Node{}
	for p.CurrKind() != end {
		p.Consume(k, 0)
		if p.CurrKind() != end {
			p.Consume(sep, 0)
		}
	}
	return n
}

func ParseExpr(p *Parser, b BlockKind) Node {
	n := Node{}
	switch p.CurrKind() {
	// Parse arrays: [3]int.{0, 1, 2}
	case TOKEN_LBRACK:
		switch p.PeekKind(1) {
		case TOKEN_RBRACK:
			Die(Todo("slice-exprs"))
		case TOKEN_XINT:
			switch p.PeekKind(2) {
			case TOKEN_RBRACK:
				n.SetCharIdx(p.CurrToken().Span[0])
				p.Consume(TOKEN_LBRACK, 0)
				p.Consume(TOKEN_XINT, 0)
				p.Consume(TOKEN_RBRACK, 0)
				typeNode := ParseType(p)
				p.Consume(TOKEN_DOT, 0)
				p.Consume(TOKEN_LBRACE, 0)
				for p.CurrKind() != TOKEN_RBRACE {
					exprNode := ParseExpr(p, b)
					if p.CurrKind() != TOKEN_RBRACE {
						p.Consume(TOKEN_COMMA, EParseArrayExpectComma)
					}
					Ignore(exprNode)
				}
				p.Consume(TOKEN_RBRACE, 0)
				n.Kind = NODE_XARRAY
				Ignore(typeNode)
			default:
				Die(Todo("wut?"))
			}
		default:
		}
	// Parse tuples: (T,U,V).{0, 1, 2}
	case TOKEN_LPAREN:
		switch p.PeekKind(2) {
		case TOKEN_COMMA:
			n.SetCharIdx(p.CurrToken().Span[0])
			p.Consume(TOKEN_LPAREN, 0)
			for p.CurrKind() != TOKEN_RPAREN {
				ParseType(p)
				if p.CurrKind() != TOKEN_RPAREN {
					p.Consume(TOKEN_COMMA, 0)
				}
			}
			p.Consume(TOKEN_RPAREN, 0)
			p.Consume(TOKEN_DOT, 0)
			p.Consume(TOKEN_LBRACE, 0)
			for p.CurrKind() != TOKEN_RBRACE {
				ParseExpr(p, b)
				if p.CurrKind() != TOKEN_RBRACE {
					p.Consume(TOKEN_COMMA, 0)
				}
			}
			p.Consume(TOKEN_RBRACE, 0)
			n.Kind = NODE_XTUPLE
		default:
			Die(Todo("wut?"))
		}
	case TOKEN_XNAME:
		if b == InStmt {
			switch p.PeekKind(1) {
			// Parse structs: A.{.a=0, .b=2, .c=3}
			case TOKEN_DOT:
				n.SetCharIdx(p.CurrToken().Span[0])
				p.Consume(TOKEN_XNAME, 0)
				p.Consume(TOKEN_DOT, 0)
				p.Consume(TOKEN_LBRACE, 0)
				for p.CurrKind() != TOKEN_RBRACE {
					p.Consume(TOKEN_DOT, EParseStructExpectDot)
					p.Consume(TOKEN_XNAME, 0)
					p.Consume(TOKEN_EQ, 0)
					ParseExpr(p, b)
					if p.CurrKind() != TOKEN_RBRACE {
						p.Consume(TOKEN_COMMA, EParseStructExpectComma)
					}
				}
				p.Consume(TOKEN_RBRACE, 0)
				n.Kind = NODE_XSTRUCT
			// Parse calls: A(0, 1, 2)
			case TOKEN_LPAREN:
				n.SetCharIdx(p.CurrToken().Span[0])
				p.Consume(TOKEN_XNAME, 0)
				p.Consume(TOKEN_LPAREN, 0)
				for p.CurrKind() != TOKEN_RPAREN {
					ParseExpr(p, b)
					if p.CurrKind() != TOKEN_RPAREN {
						p.Consume(TOKEN_COMMA, 0)
					}
				}
				n.Kind = NODE_XCALL
				p.Consume(TOKEN_RPAREN, 0)
			default:
				n.SetCharIdx(p.CurrToken().Span[0])
				p.Consume(TOKEN_XNAME, 0)
				n.Kind = NODE_XNAME
			}
		} else {
			n.SetCharIdx(p.CurrToken().Span[0])
			p.Consume(TOKEN_XNAME, 0)
			n.Kind = NODE_XNAME
		}
	case TOKEN_XINT:
		n.SetCharIdx(p.CurrToken().Span[0])
		p.Consume(TOKEN_XINT, 0)
		n.Kind = NODE_XINT
	case TOKEN_XTRUE, TOKEN_XFALSE:
		n.SetCharIdx(p.CurrToken().Span[0])
		p.Consume(p.CurrKind(), 0)
		n.Kind = NODE_XBOOL
	default:
		Die(Todo("parse-expr"))
	}
	p.Ast.AddNode(n)
	return n
}

func IsStmtOkay(k TokenKind, b BlockKind) bool {
	switch b {
	case InStmt:
		return IsStmt(k)
	case InFor:
		return IsStmtFor(k)
	default:
		Die(Todo("is-stmt-okay"))
	}
	return false
}

type BlockKind uint8

const (
	InStmt BlockKind = iota
	InFor
	InIf
)

func IsEntity(k TokenKind) bool {
	switch k {
	case TOKEN_PUB, TOKEN_FUNC, TOKEN_STRUCT:
		return true
	default:
		return false
	}
}
func IsEntityStruct(k TokenKind) bool {
	switch k {
	case TOKEN_PUB, TOKEN_FUNC:
		return true
	default:
		return false
	}
}

func IsStmt(k TokenKind) bool {
	switch k {
	case TOKEN_FOR, TOKEN_MATCH, TOKEN_IF, TOKEN_BREAK, TOKEN_CONTINUE:
		return true
	case TOKEN_XNAME, TOKEN_AMPERSAND:
		return true
	case TOKEN_LBRACE:
		return true
	default:
		return false
	}
}
func IsStmtFor(k TokenKind) bool {
	switch k {
	case TOKEN_XNAME, TOKEN_AMPERSAND:
		return true
	default:
		return false
	}
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

func TestLexer(p Parser) {
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
