package analyse

import (
	. "no/ast"
	. "no/core"

	"fmt"
)

// Environment
type Env struct {
	Names map[string][]AnyType
	Scope [][]string
}

func (e *Env) Get(name string) AnyType {
	types, ok := e.Names[name]
	if !ok {
		panic(fmt.Sprintf("Unable to find name '%s' in environment.", name))
	}
	return types[len(types)-1]
}

func (e *Env) Add(name string, typex AnyType) {
	e.Names[name] = append(e.Names[name], typex)
	e.Scope[len(e.Scope)-1] = append(e.Scope[len(e.Scope)-1], name)
}

func (e *Env) Del(name string) {
	types, ok := e.Names[name]
	if ok {
		if len(types) > 0 {
			e.Names[name] = types[:len(types)-1]
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
		Names: make(map[string][]AnyType),
		Scope: [][]string{{}},
	}

	// Add functions to environment
	// TODO: This is not enough! We should also store function argument types.
	for _, e := range a.Entities {
		switch x := e.(type) {
		case *Function:
			env.Add(x.Name.Value, x.Type)
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
	// Add new scope
	env.Scope = append(env.Scope, []string{})

	// Add function arguments to environment
	for _, a := range f.Args {
		env.Add(a.Name.Value, a.Type)
	}

	returnType := inferBlock(env, f.Block)

	// Drop last scope
	for _, n := range env.Scope[len(env.Scope)-1] {
		env.Del(n)
		Dbg(n, "removed from scope")
	}
	env.Scope = env.Scope[:len(env.Scope)-1]
	Ignore(returnType)
}

// TODO: Add/drop env scope when we enter/exit block
func inferBlock(env Env, stmts []AnyStatement) AnyType {
	var typex AnyType = &UnitType{Kind: TYPE_UNIT}

	// Add new scope
	env.Scope = append(env.Scope, []string{})

	for _, s := range stmts {
		Walk(s, func(n Node) {
			switch x := n.(type) {
			case *LetStmt:
				if len(x.Vars) == 1 {
					x.Vars[0].Type = x.Expr.GetType()
					env.Add(x.Vars[0].Name.Value, x.Expr.GetType())
				} else {
					for i, v := range x.Vars {
						switch t := x.Expr.GetType().(type) {
						case *TupleType:
							if len(t.Types) != len(x.Vars) {
								panic(fmt.Sprintf(
									"Unable to destructure %d tuple values to %d variables.",
									len(t.Types), len(x.Vars),
								))
							}
							env.Add(v.Name.Value, t.Types[i])
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
				x.Type = env.Get(x.Name.Value)
			// BUG: What if the if, else/if and else branch types differ?
			case *IfExpr:
				x.Type = inferBlock(env, x.Block)
			case *ElseIfExpr:
				x.Type = inferBlock(env, x.Block)
			case *ElseExpr:
				x.Type = inferBlock(env, x.Block)
			case *NameExpr:
				x.Type = env.Get(x.Value)
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
			break outerLoop
		}
	}
	if typex == nil {
		Dbg("BUG: Why is type nil?", typex)
	}

	// Drop last scope
	for _, n := range env.Scope[len(env.Scope)-1] {
		env.Del(n)
		Dbg(n, "removed from scope")
	}
	env.Scope = env.Scope[:len(env.Scope)-1]

	return typex
}
