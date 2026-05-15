package ast

import (
	. "no/core"
)

func Walk(n Node, f func(n Node)) {
	if n == nil {
		return
	}
	walkChild(n, f)
	f(n)
}

func walkChild(n Node, f func(n Node)) {
	switch x := n.(type) {
	case *Ast:
		walkSlice(x.Entities, f)
	case *Function:
		walkSlice(x.Args, f)
		walkSlice(x.Block, f)
	case *Use:
	case *LetStmt:
		walkSlice(x.Vars, f)
		Walk(x.Expr, f)
	case *InvokeStmt:
		Walk(x.Expr, f)
	case *ReturnStmt:
		Walk(x.Expr, f)
	case *YieldStmt:
		Walk(x.Expr, f)
	case *BlockExpr:
		walkSlice(x.Block, f)
	case *TupleExpr:
		walkSlice(x.Exprs, f)
	case *InvokeExpr:
		walkSlice(x.Args, f)
	case *IfExpr:
		Walk(x.Check, f)
		walkSlice(x.Block, f)
		Walk(x.Rest, f)
	case *ElseIfExpr:
		Walk(x.Check, f)
		walkSlice(x.Block, f)
		Walk(x.Rest, f)
	case *ElseExpr:
		walkSlice(x.Block, f)
	case *NameExpr:
	case *IntExpr:
	case *UnitExpr:
	case *BiopExpr:
		Walk(x.LExpr, f)
		Walk(x.RExpr, f)
	case *UnopExpr:
		Walk(x.LExpr, f)
	}
	Ignore(f)
}

func walkSlice[T any](nodes []T, f func(n Node)) {
	for i, x := range nodes {
		var n Node
		switch y := any(x).(type) {
		case Node:
			n = y
		case *Node:
			n = *y
		default:
			mem, ok := any(&nodes[i]).(Node)
			if ok {
				n = mem
			} else {
				panic("Unable to walk slice.")
			}
		}
		Walk(n, f)
	}
}
