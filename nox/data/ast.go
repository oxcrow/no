package data

import (
	. "no/core"
)

type Ast struct {
	// All of the nodes that exist in file.
	Nodes []Node
	// Path to the file
	File string
}

type Node struct {
	// Index of this node in the AST
	SelfIdx uint32
	// Auxiliary data for different AST nodes, that we store in side arrays.
	// - For complex nodes, like function, struct, etc, we store the index to
	//   an array that packs the data in u8 blocks, in lhs. This allows us to
	//   keep a small memory footprint, while storing the AST in a flat form.
	// - For binary ops, we store index of the left/right exprs in lhs, and rhs.
	// - For unary ops, we store index of the expr in lhs.
	// - For Int, float, etc. have no other data.
	LhsIdx uint32
	RhsIdx uint32
	// Where does the node begin?
	CharIdx uint16
	// What kind of node is this?
	Kind NodeKind
	// Whatever flag we need.
	Flag uint8
}

type Token struct {
	// On which line is this token located?
	LineIdx uint32
	// Where in the file is this token?
	Span [2]uint32
	// What token is this?
	Kind TokenKind
}

type TokenKind uint8
type NodeKind uint8

const (
	TOKEN_WHITESPACE TokenKind = iota
	TOKEN_COMMENT

	TOKEN_AND
	TOKEN_BREAK
	TOKEN_CONTINUE
	TOKEN_DEF
	TOKEN_ELSE
	TOKEN_ENUM
	TOKEN_FOR
	TOKEN_FUNC
	TOKEN_IF
	TOKEN_IN
	TOKEN_LIB
	TOKEN_MACRO
	TOKEN_MATCH
	TOKEN_MOD
	TOKEN_OR
	TOKEN_PUB
	TOKEN_RETURN
	TOKEN_RISK
	TOKEN_SAFE
	TOKEN_STRUCT
	TOKEN_THEN
	TOKEN_TYPE
	TOKEN_UNION
	TOKEN_USE

	TOKEN_EXCLAMATION
	TOKEN_SEMICOLON
	TOKEN_AMPERSAND
	TOKEN_QUESTION
	TOKEN_COLON
	TOKEN_COMMA
	TOKEN_HASH
	TOKEN_PLUSEQ
	TOKEN_MINUSEQ
	TOKEN_STAREQ
	TOKEN_SLASHEQ
	TOKEN_PLUS
	TOKEN_MINUS
	TOKEN_STAR
	TOKEN_SLASH
	TOKEN_NOTEQ
	TOKEN_EQEQ
	TOKEN_EQ
	TOKEN_LTEQ
	TOKEN_GTEQ
	TOKEN_LT
	TOKEN_GT
	TOKEN_DOTDOT
	TOKEN_DOT
	TOKEN_BAR

	TOKEN_LPAREN
	TOKEN_RPAREN
	TOKEN_LBRACE
	TOKEN_RBRACE
	TOKEN_LBRACK
	TOKEN_RBRACK
	TOKEN_RARROW

	TOKEN_XNAME
	TOKEN_XSTR
	TOKEN_XINT
	TOKEN_XTRUE
	TOKEN_XFALSE

	TOKEN_UNIT

	TOKEN_NONE
)

const (
	NODE_NONE NodeKind = iota

	NODE_XSTRUCT
	NODE_XARRAY
	NODE_XTUPLE
	NODE_XNAME
	NODE_XBOOL
	NODE_XCALL
	NODE_XSTR
	NODE_XINT

	NODE_MOD
)

func (a *Ast) AddNode(n Node) {
	// So we don't blow up memory by looping infinitely
	if n.Kind.IsValid() {
		n.SelfIdx = uint32(len(a.Nodes))
		a.Nodes = append(a.Nodes, n)
	} else {
		Die(Todo("add-node"))
	}
}

func (a Ast) BytesPerNode(code string) float64 {
	return float64(len(code)) / float64(max(len(a.Nodes), 1.0))
}

func (t Token) Len() int {
	return int(t.Span[1] - t.Span[0])
}

func (n *Node) SetCharIdx(s uint32) {
	n.CharIdx = uint16(s)
}

func (k TokenKind) IsValid() bool {
	switch k {
	case TOKEN_NONE:
		return false
	default:
		return true
	}
}

func (k NodeKind) IsValid() bool {
	switch k {
	case NODE_NONE:
		return false
	default:
		return true
	}
}
