package ast

type Ast struct {
	FilePath string
	Entities []AnyEntity
	Text     string
}

type Function struct {
	Token  Token
	Export TokenKind
	Name   NameExpr
	Args   []Variable
	Block  []AnyStatement
	Type   AnyType
}

type Use struct {
	Token Token
	Name  NameExpr
}

func (x *Function) LineLoc() int { return x.Token.Line + 1 }
func (x *Use) LineLoc() int      { return x.Token.Line + 1 }

func (x *Function) entityNode() {}
func (x *Use) entityNode()      {}

type LetStmt struct {
	Token Token
	Vars  []Variable
	Expr  AnyExpression
}

type ReturnStmt struct {
	Token Token
	Expr  AnyExpression
}

type InvokeStmt struct {
	Token Token
	Expr  AnyExpression
}

type AssignStmt struct {
	Token Token
	Vars  []AnyExpression
	Expr  AnyExpression
}

type YieldStmt struct {
	Token Token
	Expr  AnyExpression
}

func (x *LetStmt) LineLoc() int    { return x.Token.Line + 1 }
func (x *ReturnStmt) LineLoc() int { return x.Token.Line + 1 }
func (x *InvokeStmt) LineLoc() int { return x.Token.Line + 1 }
func (x *AssignStmt) LineLoc() int { return x.Token.Line + 1 }
func (x *YieldStmt) LineLoc() int  { return x.Token.Line + 1 }

func (x *LetStmt) statementNode()    {}
func (x *ReturnStmt) statementNode() {}
func (x *InvokeStmt) statementNode() {}
func (x *AssignStmt) statementNode() {}
func (x *YieldStmt) statementNode()  {}

type Variable struct {
	Token    Token
	Name     NameExpr
	IsMut    bool
	IsShadow bool
	Type     *AnyType
}

type BlockExpr struct {
	Token Token
	Block []AnyStatement
	Id    int
}

type TupleExpr struct {
	Token Token
	Exprs []AnyExpression
	Id    int
}

type InvokeExpr struct {
	Token Token
	Name  NameExpr
	Args  []AnyExpression
	Id    int
}

type IfExpr struct {
	Token Token
	Check AnyExpression
	Block []AnyStatement
	Rest  AnyExpression
	Id    int
}

type ElseIfExpr struct {
	Token Token
	Check AnyExpression
	Block []AnyStatement
	Rest  AnyExpression
	Id    int
}

type ElseExpr struct {
	Token Token
	Block []AnyStatement
	Id    int
}

type NameExpr struct {
	Token Token
	Value string
	Id    int
}

type IntExpr struct {
	Token Token
	Value uint64
	Id    int
}

type UnitExpr struct {
	Token Token
	Id    int
}

func (x *BlockExpr) LineLoc() int  { return x.Token.Line + 1 }
func (x *TupleExpr) LineLoc() int  { return x.Token.Line + 1 }
func (x *InvokeExpr) LineLoc() int { return x.Token.Line + 1 }
func (x *IfExpr) LineLoc() int     { return x.Token.Line + 1 }
func (x *ElseIfExpr) LineLoc() int { return x.Token.Line + 1 }
func (x *ElseExpr) LineLoc() int   { return x.Token.Line + 1 }
func (x *NameExpr) LineLoc() int   { return x.Token.Line + 1 }
func (x *IntExpr) LineLoc() int    { return x.Token.Line + 1 }
func (x *UnitExpr) LineLoc() int   { return x.Token.Line + 1 }

func (x *BlockExpr) expressionNode()  {}
func (x *TupleExpr) expressionNode()  {}
func (x *InvokeExpr) expressionNode() {}
func (x *IfExpr) expressionNode()     {}
func (x *ElseIfExpr) expressionNode() {}
func (x *ElseExpr) expressionNode()   {}
func (x *NameExpr) expressionNode()   {}
func (x *IntExpr) expressionNode()    {}
func (x *UnitExpr) expressionNode()   {}

type BiopExpr struct {
	Token Token
	LExpr AnyExpression
	RExpr AnyExpression
	Id    int
}

type UnopExpr struct {
	Token Token
	LExpr AnyExpression
	Id    int
}

func (x *BiopExpr) LineLoc() int { return x.Token.Line + 1 }
func (x *UnopExpr) LineLoc() int { return x.Token.Line + 1 }

func (x *BiopExpr) expressionNode() {}
func (x *UnopExpr) expressionNode() {}

type ReferType struct {
	Token Token
	Rest  AnyType
	IsMut bool
}

type FloatType struct {
	Token Token
	Kind  TypeKind
}

type IntType struct {
	Token Token
	Kind  TypeKind
}

type UnitType struct {
	Token Token
	Kind  TypeKind
}

func (x *ReferType) LineLoc() int { return x.Token.Line + 1 }
func (x *FloatType) LineLoc() int { return x.Token.Line + 1 }
func (x *IntType) LineLoc() int   { return x.Token.Line + 1 }
func (x *UnitType) LineLoc() int  { return x.Token.Line + 1 }

func (x *ReferType) typeNode() {}
func (x *FloatType) typeNode() {}
func (x *IntType) typeNode()   {}
func (x *UnitType) typeNode()  {}

type TypeKind uint8

const (
	TYPE_UNIT TypeKind = iota
	TYPE_INT
	TYPE_FLOAT
	TYPE_MUTREF
	TYPE_CONREF
)

type AnyEntity interface {
	Node
	entityNode()
}

type AnyStatement interface {
	Node
	statementNode()
}

type AnyExpression interface {
	Node
	expressionNode()
}

type AnyType interface {
	Node
	typeNode()
}

type Node interface {
	Location
}

type Location interface {
	LineLoc() int
}
