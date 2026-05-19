package analyse

import (
	. "no/ast"
	. "no/core"

	"fmt"
)

// Environment
type Env struct {
	Names map[string][]TypeNode
	Scope [][]string
}

func (e *Env) AddScope() {
	e.Scope = append(e.Scope, []string{})
}

func (e *Env) DelScope() {
	for _, n := range e.Scope[len(e.Scope)-1] {
		e.Del(n)
	}
	e.Scope = e.Scope[:len(e.Scope)-1]

}

func (e *Env) GetType(name string) AnyType {
	nodes, ok := e.Names[name]
	if !ok {
		panic(fmt.Sprintf("Unable to find name '%s' in environment.", name))
	}
	return nodes[len(nodes)-1].GetType()
}

func (e *Env) Add(name string, node TypeNode) {
	e.Names[name] = append(e.Names[name], node)
	e.Scope[len(e.Scope)-1] = append(e.Scope[len(e.Scope)-1], name)
}

func (e *Env) Del(name string) {
	nodes, ok := e.Names[name]
	if ok {
		if len(nodes) > 0 {
			e.Names[name] = nodes[:len(nodes)-1]
		}
	}
}

// Semantically analyse the AST (type infer, type check, borrow check, etc.).
// We should probably analyse "one function at a time"; but later.
func AnalyseAst(a *Ast) {
	InferAst(a)
}

// Infer types of all expressions in the AST
// For the sake of deterministic results, we walk in post-order (depth-first)
// order through the AST, and then infer each expression's type, then store them
// inside each expression node, so the semantic analysis passes can access them.
func InferAst(a *Ast) {
	env := Env{
		Names: make(map[string][]TypeNode),
		Scope: [][]string{{}},
	}

	// Add functions to environment
	for i, e := range a.Entities {
		switch x := e.(type) {
		case *Function:
			env.Add(x.Name.Value, a.Entities[i])
		}
	}

	for _, e := range a.Entities {
		switch x := e.(type) {
		case *Function:
			inferFunction(env, x)
		}
	}
}

func inferFunction(env Env, f *Function) {
	env.AddScope()
	defer env.DelScope()

	// Add function arguments to environment
	for _, a := range f.Args {
		env.Add(a.Name.Value, &a)
	}

	returnType := inferBlock(env, f.Block)

	Ignore(returnType)
}

// TODO: Add/drop env scope when we enter/exit block
func inferBlock(env Env, stmts []AnyStatement) AnyType {
	var typex AnyType

	env.AddScope()
	defer env.DelScope()

	for _, s := range stmts {
		Walk(s, func(n Node) {
			switch x := n.(type) {
			case *LetStmt:
				if len(x.Vars) == 1 {
					x.Vars[0].Type = x.Expr.GetType()
					env.Add(x.Vars[0].Name.Value, &x.Vars[0])
				} else {
					for i, v := range x.Vars {
						switch e := x.Expr.GetType().(type) {
						case *TupleType:
							if len(e.Types) != len(x.Vars) {
								panic(fmt.Sprintf(
									"Unable to destructure %d tuple values to %d variables.",
									len(e.Types), len(x.Vars),
								))
							}
							x.Vars[i].Type = func() AnyType {
								switch e := x.Expr.GetType().(type) {
								case *TupleType:
									return e.Types[i]
								}
								panic("wut?")
							}()
							env.Add(v.Name.Value, &x.Vars[i])
						default:
							panic("Unable to destructure anything else except tuples.")
						}
					}
				}
			case *BlockExpr:
				x.Type = func() AnyType {
					s := x.Block[len(x.Block)-1]
					switch s.(type) {
					case *YieldStmt:
						return s.GetExpr().GetType()
					}
					return &UnitType{Kind: TYPE_UNIT}
				}()
			case *TupleExpr:
				tupleTypes := func() []AnyType {
					var types []AnyType
					for _, t := range x.Exprs {
						types = append(types, t.GetType())
					}
					return types
				}()
				x.Type = &TupleType{Types: tupleTypes, Kind: TYPE_TUPLE}
			case *InvokeExpr:
				x.Type = env.GetType(x.Name.Value)
			// BUG: What if the if, else/if and else branch types differ?
			case *IfExpr:
				x.Type = inferBlock(env, x.Block)
			case *ElseIfExpr:
				x.Type = inferBlock(env, x.Block)
			case *ElseExpr:
				x.Type = inferBlock(env, x.Block)
			case *NameExpr:
				x.Type = env.GetType(x.Value)
			case *IntExpr:
				x.Type = &IntType{Kind: TYPE_INT}
			case *UnitExpr:
				x.Type = &UnitType{Kind: TYPE_UNIT}
			case *BiopExpr:
				// BUG: What if lexpr and rexpr types differ?
				x.Type = x.LExpr.GetType()
			case *UnopExpr:
				x.Type = x.LExpr.GetType()
			}
		})
	}

outerLoop:
	for _, s := range stmts {
		switch x := s.(type) {
		case *YieldStmt:
			typex = x.Expr.GetType()
			if typex == nil {
				panic(fmt.Sprintf("BUG: Why is expression type nil? (Line: %d)", x.LineLoc()))
			}
			break outerLoop
		}
	}
	if typex == nil {
		typex = &UnitType{Kind: TYPE_UNIT}
	}

	return typex
}
