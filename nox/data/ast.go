package data

type Ast struct {
	// All of the nodes that exist in file.
	Nodes []Node
	// We only store the start character index of each nodes's span.
	// We can lex later, if we need the full span.
	Spans []uint32
	// Path to the file
	File string
}

type Node struct {
	// Auxiliary data for different AST nodes, that we store in side arrays.
	// - For complex nodes, like function, struct, etc, we store the index to
	//   an array that packs the data in u8 blocks, in lhs. This allows us to
	//   keep a small memory footprint, while storing the AST in a flat form.
	// - For binary ops, we store index of the left/right exprs in lhs, and rhs.
	// - For unary ops, we store index of the expr in lhs.
	// - For Int, float, etc. have no other data.
	LhsIdx uint32
	RhsIdx uint32
	// What node is this?
	Kind TokenKind
	// Wasted
	_ [3]uint8
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

	TOKEN_BOOL
	TOKEN_INT
	TOKEN_F64
	TOKEN_F32
	TOKEN_U64
	TOKEN_I64
	TOKEN_U32
	TOKEN_I32
	TOKEN_U16
	TOKEN_I16
	TOKEN_U8
	TOKEN_I8

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

func (a Ast) BytesPerNode(code string) float64 {
	return float64(len(code)) / float64(max(len(a.Nodes), 1.0))
}

func (k TokenKind) IsValid() bool {
	switch k {
	case TOKEN_NONE:
		return false
	default:
		return true
	}
}
