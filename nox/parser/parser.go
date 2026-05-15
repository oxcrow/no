package parser

import (
	. "no/ast"
	. "no/core"

	"strconv"

	"fmt"
)

type Parser struct {
	text  string
	state parserState
	ast   Ast
}

type parserState struct {
	tokenIndex int
	charIndex  int
	lineIndex  int
	wordIndex  int
	exprIndex  int
	currToken  Token
	nextToken  Token
}

func (p Parser) isEOF() bool {
	return p.state.charIndex >= len(p.text)
}

func (p Parser) kind() TokenKind {
	return p.state.currToken.Kind
}

func (p Parser) peekKind() TokenKind {
	return p.state.nextToken.Kind
}

func (p Parser) kindIs(kind TokenKind) bool {
	return p.kind() == kind
}

func (p Parser) kindIsNot(kind TokenKind) bool {
	return p.kind() != kind
}

func (p Parser) peekKindIs(kind TokenKind) bool {
	return p.peekKind() == kind
}

func (p Parser) currToken() Token {
	return p.state.currToken
}

func (p *Parser) nextExprId() int {
	id := p.state.exprIndex
	p.state.exprIndex++
	return id
}

func (p *Parser) skip() {
	p.state.currToken = p.state.nextToken
	p.lexToken()
}

func (p *Parser) lexToken() {
	if p.isEOF() {
		return
	}

	c := p.text[p.state.charIndex]
	if c == '\n' {
		p.state.lineIndex += 1
		p.state.wordIndex = 0
	}

	token, e := LexWord(p.text, p.state.charIndex, p.state.lineIndex)
	if e != nil || token.Kind == TOKEN_WRONG {
		panic("Unable to tokenize word.")
	}

	switch token.Kind {
	case TOKEN_WHITESPACE, TOKEN_DOCUMENT, TOKEN_COMMENT:
		p.state.charIndex += token.Span.Len()
		p.lexToken()
	default:
		p.state.tokenIndex += 1
		p.state.charIndex += token.Span.Len()
		p.state.wordIndex += token.Span.Len()
		p.state.nextToken = token
	}
}

func (p *Parser) matchToken(kind TokenKind) Token {
	token := p.currToken()
	if p.kind() != kind {
		panic(
			fmt.Sprintf(
				"Unable to match token. (Expected Token: %d, Found Token: %d, On Line: %d)",
				kind, p.kind(), p.state.lineIndex+1),
		)
	}
	p.skip()
	return token
}

func (p *Parser) maybeToken(kind TokenKind) Token {
	token := p.currToken()
	if p.kind() != kind {
		return token
	}
	p.skip()
	return token
}

func (p Parser) nameValue() string {
	span := p.currToken().Span
	return p.text[span.Start:span.End]
}

func (p Parser) kindIsExpr() bool {
	return p.kindIs(TOKEN_MUTAMPERSAND) ||
		p.kindIs(TOKEN_AMPERSAND) ||
		p.kindIs(TOKEN_LPAREN) ||
		p.kindIs(TOKEN_XNAME) ||
		p.kindIs(TOKEN_FLOAT) ||
		p.kindIs(TOKEN_XINT) ||
		p.kindIs(TOKEN_XUNIT) ||
		p.kindIs(TOKEN_MATCH) ||
		p.kindIs(TOKEN_IF)
}

func (p Parser) kindIsLvalExpr() bool {
	return p.kindIs(TOKEN_XNAME)
}

func parseEntityList(p *Parser) {
	for !p.isEOF() {
		exportKind := func() TokenKind {
			if p.kindIs(TOKEN_EXPORT) {
				p.matchToken(TOKEN_EXPORT)
				return TOKEN_EXPORT
			} else if p.kindIs(TOKEN_LOCAL) {
				p.matchToken(TOKEN_LOCAL)
				return TOKEN_LOCAL
			}
			return TOKEN_HIDDEN
		}()

		entityToken := p.currToken()

		switch p.kind() {
		case TOKEN_FUNCTION:
			p.matchToken(TOKEN_FUNCTION)
			nameToken := p.matchToken(TOKEN_XNAME)
			p.matchToken(TOKEN_LPAREN)
			args := parseArgList(p)
			p.matchToken(TOKEN_RPAREN)
			typex := parseReturnType(p)
			stmts := parseBlock(p)
			p.ast.Entities = append(
				p.ast.Entities,
				&Function{
					Token:  entityToken,
					Export: exportKind,
					Name:   NameExpr{Token: nameToken, Value: nameToken.SpanText(p.text)},
					Args:   args,
					Block:  stmts,
					Type:   typex,
				},
			)
		case TOKEN_USE:
			p.matchToken(TOKEN_USE)
			nameToken := p.matchToken(TOKEN_XNAME)
			p.matchToken(TOKEN_SEMICOLON)
			p.ast.Entities = append(
				p.ast.Entities,
				&Use{
					Token: entityToken,
					Name:  NameExpr{Token: nameToken, Value: nameToken.SpanText(p.text), Id: p.nextExprId()},
				},
			)
		default:
			TODO("parse-entity")
		}
	}
}

func parseBlock(p *Parser) []AnyStatement {
	p.matchToken(TOKEN_LBRACE)
	stmts := parseBlockBody(p)
	p.matchToken(TOKEN_RBRACE)
	return stmts
}

func parseBlockBody(p *Parser) []AnyStatement {
	var stmts []AnyStatement

	for p.kindIs(TOKEN_LET) || p.kindIs(TOKEN_RETURN) || p.kindIs(TOKEN_XNAME) {
		switch p.kind() {
		case TOKEN_LET:
			stmtToken := p.matchToken(TOKEN_LET)
			vars := parseVarList(p)
			p.matchToken(TOKEN_EQ)
			expr := parseExpr(p, 0)
			p.matchToken(TOKEN_SEMICOLON)
			stmts = append(stmts,
				&LetStmt{
					Token: stmtToken, Vars: vars, Expr: expr,
				},
			)
		case TOKEN_RETURN:
			stmtToken := p.matchToken(TOKEN_RETURN)
			expr := parseExpr(p, 0)
			p.matchToken(TOKEN_SEMICOLON)
			stmts = append(stmts,
				&ReturnStmt{
					Token: stmtToken, Expr: expr,
				},
			)
		default:
			var lvals []AnyExpression
			switch p.kind() {
			case TOKEN_XNAME:
				lvalToken := p.currToken()
				lvalExpr := parseLvalExpr(p, 0)
				lvals = append(lvals, lvalExpr)
				switch p.kind() {
				case TOKEN_SEMICOLON:
					p.matchToken(TOKEN_SEMICOLON)
					stmts = append(stmts,
						&InvokeStmt{
							Token: lvalToken, Expr: lvalExpr,
						},
					)
				case TOKEN_COMMA:
					for p.kindIs(TOKEN_COMMA) {
						p.matchToken(TOKEN_COMMA)
						name := parseLvalExpr(p, 0)
						lvals = append(lvals, name)
					}
					p.matchToken(TOKEN_EQ)
					expr := parseExpr(p, 0)
					p.matchToken(TOKEN_SEMICOLON)
					stmts = append(stmts,
						&AssignStmt{Token: lvalToken, Vars: lvals, Expr: expr},
					)
				case TOKEN_EQ:
					p.matchToken(TOKEN_EQ)
					expr := parseExpr(p, 0)
					p.matchToken(TOKEN_SEMICOLON)
					stmts = append(stmts,
						&AssignStmt{Token: lvalToken, Vars: lvals, Expr: expr},
					)
				default:
					TODO("parse-lval-expr")
				}
			default:
				panic(
					fmt.Sprintf("Unable to parse code. Encountered Unknown token: %s, at Line Number: %d\n",
						p.nameValue(), p.state.lineIndex+1),
				)
			}
		}
	}

	if p.kindIs(TOKEN_COLON) {
		stmtToken := p.matchToken(TOKEN_COLON)
		expr := parseExpr(p, 0)
		stmts = append(stmts,
			&YieldStmt{
				Token: stmtToken, Expr: expr,
			},
		)
	}

	return stmts
}

func parseArgList(p *Parser) []Variable {
	args := parseVarList(p)
	return args
}

func parseVarList(p *Parser) []Variable {
	var vars []Variable

	for p.kindIs(TOKEN_MUT) || p.kindIs(TOKEN_PLUS) || p.kindIs(TOKEN_XNAME) {
		isMut := func() bool {
			if p.kindIs(TOKEN_MUT) {
				p.matchToken(TOKEN_MUT)
				return true
			} else {
				return false
			}
		}()
		isNew := func() bool {
			if p.kindIs(TOKEN_PLUS) {
				p.matchToken(TOKEN_PLUS)
				return true
			}
			return false
		}()

		nameToken := p.matchToken(TOKEN_XNAME)
		var typex AnyType = nil
		if p.kindIsNot(TOKEN_COMMA) {
			typex = parseType(p)
		}
		p.maybeToken(TOKEN_COMMA) // BUG: I DON'T LIKE THIS! Is this a bug?

		vars = append(vars,
			Variable{
				Token:    nameToken,
				Name:     NameExpr{Token: nameToken, Value: nameToken.SpanText(p.text)},
				IsMut:    isMut,
				IsShadow: isNew,
				Type:     &typex,
			},
		)
	}

	return vars
}

// Parse expression using Pratt's operator precedence method.
//  1. https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
//     https://ghostarchive.org/archive/BDR6F
func parseExpr(p *Parser, bindPower int) AnyExpression {
	lexpr := func() AnyExpression {
		var expr AnyExpression
		switch p.kind() {
		// Is expr a tuple(x,y,z), group(x+(y*(z))), or unit()?
		case TOKEN_LPAREN:
			lparenToken := p.matchToken(TOKEN_LPAREN)
			switch p.kind() {
			// Expr is unit
			case TOKEN_RPAREN:
				p.matchToken(TOKEN_RPAREN)
				expr = &UnitExpr{
					Token: lparenToken, Id: p.nextExprId(),
				}
			// Expr could be tuple or group; but not unit
			default:
				var tupleExprs []AnyExpression
				tupleExprs = append(tupleExprs, parseExpr(p, 0))
				// Is expr tuple?
				if p.kindIs(TOKEN_COMMA) {
					p.matchToken(TOKEN_COMMA)
					for p.kindIsExpr() {
						tupleExprs = append(tupleExprs, parseExpr(p, 0))
						if p.kindIs(TOKEN_RPAREN) {
							break
						} else {
							p.matchToken(TOKEN_COMMA)
						}
					}
					p.matchToken(TOKEN_RPAREN)
					expr = &TupleExpr{
						Token: lparenToken, Exprs: tupleExprs, Id: p.nextExprId(),
					}
				} else {
					// Is expr group?
					p.matchToken(TOKEN_RPAREN)
					expr = tupleExprs[0]
				}
			}
		// Expr doesn't depend on () so it's a normal expression.
		// NOTE: We call another function to prevent infinite recursion
		default:
			expr = parseCompExpr(p, bindPower)
		}
		return expr
	}()

outer:
	for {
		switch p.kind() {
		case
			TOKEN_PLUS, TOKEN_MINUS, TOKEN_STAR, TOKEN_SLASH,
			TOKEN_EQEQ, TOKEN_LTEQ, TOKEN_LT, TOKEN_GTEQ, TOKEN_GT,
			TOKEN_DOT, TOKEN_QUESTION, TOKEN_AMPERSAND:
			opToken := p.currToken()
			op := opToken.Kind
			{
				lbp, _, isPostfix := postfixBind(op)
				if isPostfix {
					if lbp < bindPower {
						break outer
					}
					p.matchToken(op)

					lexpr = &UnopExpr{Token: opToken, LExpr: lexpr, Id: p.nextExprId()}

					continue outer
				}
			}
			{
				lbp, rbp, isInfix := infixBind(op)
				if isInfix {
					if lbp < bindPower {
						break outer
					}
					p.matchToken(op)

					// Recurse until we find the highest precedence expression
					rexpr := parseExpr(p, rbp)

					lexpr = &BiopExpr{Token: opToken, LExpr: lexpr, RExpr: rexpr, Id: p.nextExprId()}
				}
			}
		default:
			break outer
		}
	}

	return lexpr
}

func parseCompExpr(p *Parser, bindPower int) AnyExpression {
	var expr AnyExpression

	switch p.kind() {
	// Is expr a block of code?
	case TOKEN_LBRACE:
		blockToken := p.matchToken(TOKEN_LBRACE)
		stmts := parseBlockBody(p)
		p.matchToken(TOKEN_RBRACE)
		expr = &BlockExpr{
			Token: blockToken,
			Block: stmts,
			Id:    p.nextExprId(),
		}
	// Is expr an if-else block?
	case TOKEN_IF:
		ifToken := p.matchToken(TOKEN_IF)
		ifCondExpr := parseExpr(p, 0)
		switch p.kind() {
		case TOKEN_LBRACE:
			p.matchToken(TOKEN_LBRACE)
			ifBlock := parseBlockBody(p)
			p.matchToken(TOKEN_RBRACE)
			elseToken := p.matchToken(TOKEN_ELSE)
			switch p.kind() {
			case TOKEN_IF:
				elseIfExpr := parseExpr(p, 0)
				expr = &IfExpr{
					Token: ifToken,
					Check: ifCondExpr,
					Block: ifBlock,
					Rest: &ElseIfExpr{
						Token: elseToken,
						Check: elseIfExpr.(*IfExpr).Check,
						Block: elseIfExpr.(*IfExpr).Block,
						Rest:  elseIfExpr.(*IfExpr).Rest,
						Id:    elseIfExpr.(*IfExpr).Id,
					},
					Id: p.nextExprId(),
				}
			default:
				p.matchToken(TOKEN_LBRACE)
				elseBlock := parseBlockBody(p)
				p.matchToken(TOKEN_RBRACE)
				expr = &IfExpr{
					Token: ifToken,
					Check: ifCondExpr,
					Block: ifBlock,
					Rest: &ElseExpr{
						Token: elseToken,
						Block: elseBlock,
						Id:    p.nextExprId(),
					},
					Id: p.nextExprId(),
				}
			}
		default:
			TODO("parse-expr-if")
		}
	// Is expr mutable reference?
	case TOKEN_MUTAMPERSAND:
		opToken := p.matchToken(TOKEN_MUTAMPERSAND)
		expr = &UnopExpr{
			Token: opToken,
			LExpr: parseExpr(p, 0),
			Id:    p.nextExprId(),
		}
	// Is expr reference?
	case TOKEN_AMPERSAND:
		opToken := p.matchToken(TOKEN_AMPERSAND)
		expr = &UnopExpr{
			Token: opToken,
			LExpr: parseExpr(p, 0),
			Id:    p.nextExprId(),
		}
	// Is expr an identifier, function, struct?
	case TOKEN_XNAME:
		nameToken := p.matchToken(TOKEN_XNAME)
		switch p.kind() {
		// Is expr macro expansion call?
		case TOKEN_EXCLAMATION:
			TODO("parse-expr-macro")
		// Is expr function call?
		case TOKEN_LPAREN:
			var args []AnyExpression
			p.matchToken(TOKEN_LPAREN)
			for p.kindIsExpr() {
				argExpr := parseExpr(p, 0)
				args = append(args, argExpr)
				if p.kindIs(TOKEN_RPAREN) {
					p.maybeToken(TOKEN_COMMA)
					break
				} else {
					p.matchToken(TOKEN_COMMA)
				}
			}
			p.matchToken(TOKEN_RPAREN)
			expr = &InvokeExpr{
				Token: nameToken,
				Name:  NameExpr{Token: nameToken, Value: nameToken.SpanText(p.text)},
				Args:  args,
				Id:    p.nextExprId(),
			}
		// Is expr struct?
		case TOKEN_LBRACE:
			TODO("parse-expr-struct")
		// Is expr array?
		case TOKEN_LBRACK:
			TODO("parse-expr-array")
		// Is expr an identifier?
		default:
			expr = &NameExpr{
				Token: nameToken, Value: nameToken.SpanText(p.text), Id: p.nextExprId(),
			}
		}
	// Is expr integer?
	case TOKEN_XINT:
		exprToken := p.matchToken(TOKEN_XINT)
		value, _ := strconv.ParseUint(exprToken.SpanText(p.text), 10, 64)
		expr = &IntExpr{Token: exprToken, Value: value, Id: p.nextExprId()}
	default:
		panic("(todo (parse-comp-expr))")
	}
	Ignore(p, bindPower)

	return expr
}

func parseLvalExpr(p *Parser, bindPower int) AnyExpression {
	lexpr := parseLvalCompExpr(p, bindPower)

outer:
	for {
		switch p.kind() {
		case
			TOKEN_DOT, TOKEN_QUESTION, TOKEN_AMPERSAND:
			opToken := p.currToken()
			op := opToken.Kind
			{
				lbp, _, isPostfix := postfixBind(op)
				if isPostfix {
					if lbp < bindPower {
						break outer
					}
					p.matchToken(op)

					lexpr = &UnopExpr{Token: opToken, LExpr: lexpr, Id: p.nextExprId()}

					continue outer
				}
			}
			{
				lbp, rbp, isInfix := infixBind(op)
				if isInfix {
					if lbp < bindPower {
						break outer
					}
					p.matchToken(op)

					// Recurse until we find the highest precedence expression
					rexpr := parseExpr(p, rbp)

					lexpr = &BiopExpr{Token: opToken, LExpr: lexpr, RExpr: rexpr, Id: p.nextExprId()}
				}
			}
		default:
			break outer
		}
	}

	return lexpr
}

func parseLvalCompExpr(p *Parser, bindPower int) AnyExpression {
	var expr AnyExpression

	switch p.kind() {
	// Is expr mutable reference?
	case TOKEN_MUTAMPERSAND:
		opToken := p.matchToken(TOKEN_MUTAMPERSAND)
		expr = &UnopExpr{
			Token: opToken,
			LExpr: parseExpr(p, 0),
			Id:    p.nextExprId(),
		}
	// Is expr constant reference?
	case TOKEN_AMPERSAND:
		opToken := p.matchToken(TOKEN_AMPERSAND)
		expr = &UnopExpr{
			Token: opToken,
			LExpr: parseExpr(p, 0),
			Id:    p.nextExprId(),
		}
	// Is expr an identifier, function, struct?
	case TOKEN_XNAME:
		nameToken := p.matchToken(TOKEN_XNAME)
		switch p.kind() {
		// Is expr macro expansion call?
		case TOKEN_EXCLAMATION:
			TODO("parse-lval-expr-macro")
		// Is expr function call?
		case TOKEN_LPAREN:
			var args []AnyExpression
			p.matchToken(TOKEN_LPAREN)
			for p.kindIsExpr() {
				argExpr := parseExpr(p, 0)
				args = append(args, argExpr)
				if p.kindIs(TOKEN_RPAREN) {
					p.maybeToken(TOKEN_COMMA)
					break
				} else {
					p.matchToken(TOKEN_COMMA)
				}
			}
			p.matchToken(TOKEN_RPAREN)
			expr = &InvokeExpr{
				Token: nameToken,
				Name:  NameExpr{Token: nameToken, Value: nameToken.SpanText(p.text), Id: p.nextExprId()},
				Args:  args,
			}
		// Is expr array?
		case TOKEN_LBRACK:
			TODO("parse-lval-expr-array")
		// Is expr an identifier?
		default:
			expr = &NameExpr{
				Token: nameToken, Value: nameToken.SpanText(p.text), Id: p.nextExprId(),
			}
		}
	default:
		panic("(todo (parse-lval-comp-expr))")
	}
	Ignore(p, bindPower)

	return expr
}

func parseReturnType(p *Parser) AnyType {
	var node AnyType
	switch p.kind() {
	case TOKEN_LBRACE:
		typeToken := p.currToken()
		node = &UnitType{Token: typeToken, Kind: TYPE_UNIT}
	default:
		node = parseType(p)
	}
	return node
}

func parseType(p *Parser) AnyType {
	var node AnyType
	switch p.kind() {
	case TOKEN_LPAREN:
		typeToken := p.matchToken(TOKEN_LPAREN)
		p.matchToken(TOKEN_RPAREN)
		node = &UnitType{Token: typeToken, Kind: TYPE_UNIT}
	case TOKEN_MUTAMPERSAND:
		typeToken := p.matchToken(TOKEN_MUTAMPERSAND)
		restType := parseType(p)
		node = &ReferType{Token: typeToken, IsMut: true, Rest: restType}
	case TOKEN_AMPERSAND:
		typeToken := p.matchToken(TOKEN_AMPERSAND)
		restType := parseType(p)
		node = &ReferType{Token: typeToken, IsMut: false, Rest: restType}
	case TOKEN_FLOAT:
		typeToken := p.matchToken(TOKEN_FLOAT)
		node = &FloatType{Token: typeToken, Kind: TYPE_FLOAT}
	case TOKEN_INT:
		typeToken := p.matchToken(TOKEN_INT)
		node = &IntType{Token: typeToken, Kind: TYPE_INT}
	}
	return node
}

func prefixBind(kind TokenKind) (error, int, bool) {
	switch kind {
	case TOKEN_PLUS, TOKEN_MINUS, TOKEN_EXCLAMATION, TOKEN_MUTAMPERSAND, TOKEN_AMPERSAND, TOKEN_STAR:
		return nil, 80, true
	}
	return nil, 0, false
}

func infixBind(kind TokenKind) (int, int, bool) {
	switch kind {
	case TOKEN_DOT:
		return 90, 91, true
	case TOKEN_STAR, TOKEN_SLASH:
		return 70, 71, true
	case TOKEN_PLUS, TOKEN_MINUS:
		return 60, 61, true
	case TOKEN_LTEQ, TOKEN_GTEQ, TOKEN_LT, TOKEN_GT:
		return 50, 51, true
	case TOKEN_NOTEQ, TOKEN_EQEQ:
		return 40, 41, true
	}
	return 0, 0, false
}

func postfixBind(kind TokenKind) (int, error, bool) {
	switch kind {
	case TOKEN_QUESTION, TOKEN_AMPERSAND:
		return 90, nil, true
	}
	return 0, nil, false
}

func ParseFile(filePath string) Ast {
	code := ReadFile(filePath)

	p := Parser{
		text:  code,
		state: parserState{tokenIndex: 0, charIndex: 0, wordIndex: 0},
		ast:   Ast{FilePath: filePath, Text: code},
	}

	p.lexToken()
	p.skip()

	parseEntityList(&p)

	return p.ast
}
