package ast

type Ast struct {
	FilePath string
	Entities []AnyEntity
	Text     string
}

func (x *Ast) LineLoc() int { return 0 }
func (x *Ast) node()        {}

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

func (x *Function) node() {}
func (x *Use) node()      {}

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

func (x *LetStmt) GetExpr() AnyExpression    { return x.Expr }
func (x *ReturnStmt) GetExpr() AnyExpression { return x.Expr }
func (x *InvokeStmt) GetExpr() AnyExpression { return x.Expr }
func (x *AssignStmt) GetExpr() AnyExpression { return x.Expr }
func (x *YieldStmt) GetExpr() AnyExpression  { return x.Expr }

func (x *LetStmt) statementNode()    {}
func (x *ReturnStmt) statementNode() {}
func (x *InvokeStmt) statementNode() {}
func (x *AssignStmt) statementNode() {}
func (x *YieldStmt) statementNode()  {}

func (x *LetStmt) node()    {}
func (x *ReturnStmt) node() {}
func (x *InvokeStmt) node() {}
func (x *AssignStmt) node() {}
func (x *YieldStmt) node()  {}

type Variable struct {
	Token    Token
	Name     NameExpr
	IsMut    bool
	IsShadow bool
	Type     AnyType
}

func (x *Variable) LineLoc() int { return x.Token.Line + 1 }
func (x *Variable) node()        {}

type BlockExpr struct {
	Token Token
	Block []AnyStatement
	Type  AnyType
}

type TupleExpr struct {
	Token Token
	Exprs []AnyExpression
	Type  AnyType
}

type InvokeExpr struct {
	Token Token
	Name  NameExpr
	Args  []AnyExpression
	Type  AnyType
}

type IfExpr struct {
	Token Token
	Check AnyExpression
	Block []AnyStatement
	Rest  AnyExpression
	Type  AnyType
}

type ElseIfExpr struct {
	Token Token
	Check AnyExpression
	Block []AnyStatement
	Rest  AnyExpression
	Type  AnyType
}

type ElseExpr struct {
	Token Token
	Block []AnyStatement
	Type  AnyType
}

type NameExpr struct {
	Token Token
	Value string
	Type  AnyType
}

type IntExpr struct {
	Token Token
	Value uint64
	Type  AnyType
}

type UnitExpr struct {
	Token Token
	Type  AnyType
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

func (x *BlockExpr) GetType() AnyType  { return x.Type }
func (x *TupleExpr) GetType() AnyType  { return x.Type }
func (x *InvokeExpr) GetType() AnyType { return x.Type }
func (x *IfExpr) GetType() AnyType     { return x.Type }
func (x *ElseIfExpr) GetType() AnyType { return x.Type }
func (x *ElseExpr) GetType() AnyType   { return x.Type }
func (x *NameExpr) GetType() AnyType   { return x.Type }
func (x *IntExpr) GetType() AnyType    { return x.Type }
func (x *UnitExpr) GetType() AnyType   { return x.Type }

func (x *BlockExpr) expressionNode()  {}
func (x *TupleExpr) expressionNode()  {}
func (x *InvokeExpr) expressionNode() {}
func (x *IfExpr) expressionNode()     {}
func (x *ElseIfExpr) expressionNode() {}
func (x *ElseExpr) expressionNode()   {}
func (x *NameExpr) expressionNode()   {}
func (x *IntExpr) expressionNode()    {}
func (x *UnitExpr) expressionNode()   {}

func (x *BlockExpr) node()  {}
func (x *TupleExpr) node()  {}
func (x *InvokeExpr) node() {}
func (x *IfExpr) node()     {}
func (x *ElseIfExpr) node() {}
func (x *ElseExpr) node()   {}
func (x *NameExpr) node()   {}
func (x *IntExpr) node()    {}
func (x *UnitExpr) node()   {}

type BiopExpr struct {
	Token Token
	LExpr AnyExpression
	RExpr AnyExpression
	Type  AnyType
}

type UnopExpr struct {
	Token Token
	LExpr AnyExpression
	Type  AnyType
}

func (x *BiopExpr) LineLoc() int { return x.Token.Line + 1 }
func (x *UnopExpr) LineLoc() int { return x.Token.Line + 1 }

func (x *BiopExpr) GetType() AnyType { return x.Type }
func (x *UnopExpr) GetType() AnyType { return x.Type }

func (x *BiopExpr) expressionNode() {}
func (x *UnopExpr) expressionNode() {}

func (x *BiopExpr) node() {}
func (x *UnopExpr) node() {}

type TupleType struct {
	Token Token
	Types []AnyType
	Kind  TypeKind
}

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

func (x *TupleType) LineLoc() int { return x.Token.Line + 1 }
func (x *ReferType) LineLoc() int { return x.Token.Line + 1 }
func (x *FloatType) LineLoc() int { return x.Token.Line + 1 }
func (x *IntType) LineLoc() int   { return x.Token.Line + 1 }
func (x *UnitType) LineLoc() int  { return x.Token.Line + 1 }

func (x *TupleType) typeNode() {}
func (x *ReferType) typeNode() {}
func (x *FloatType) typeNode() {}
func (x *IntType) typeNode()   {}
func (x *UnitType) typeNode()  {}

func (x *TupleType) node() {}
func (x *ReferType) node() {}
func (x *FloatType) node() {}
func (x *IntType) node()   {}
func (x *UnitType) node()  {}

type TypeKind uint8

const (
	TYPE_UNIT TypeKind = iota
	TYPE_INT
	TYPE_FLOAT
	TYPE_TUPLE
	TYPE_MUTREF
	TYPE_CONREF
)

type AnyEntity interface {
	Node
	entityNode()
}

type AnyStatement interface {
	Node
	GetExpr() AnyExpression
	statementNode()
}

type AnyExpression interface {
	Node
	GetType() AnyType
	expressionNode()
}

type AnyType interface {
	Node
	typeNode()
}

type Node interface {
	Location
	node()
}

type Location interface {
	LineLoc() int
}
