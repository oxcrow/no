package ast

import (
	. "no/core"
	"strings"

	"fmt"
)

func DumpAst(a Ast) {
	fmt.Printf("(ROOT: (FilePath: %s) (Entities: [\n", quote(a.FilePath))
	for _, e := range a.Entities {
		fmt.Printf("   (%s)\n", dumpEntity(e, 1)) // ╰ is U+2570, ─ is U+2500 │ is U+2502
	}
	fmt.Printf("]))\n")
	Ignore(a)
}

func dumpEntity(e AnyEntity, level int) string {
	line := e.LineLoc()
	var kind string
	var info string
	var rest []string

	switch x := e.(type) {
	case *Function:
		info = fmt.Sprintf("%s", quote(x.Name.Value))
		for _, s := range x.Block {
			stmt := dumpStmt(s, level+1)
			rest = append(rest, fmt.Sprintf("\n%s", stmt))
		}
		kind = "Function"
	case *Use:
		info = fmt.Sprintf("%s", quote(x.Name.Value))
		kind = "Use"
	}

	return fmt.Sprintf("%s %s (Stmts: [%s]) (Line: %d)", kind, info, strings.Join(rest, ""), line)
}

func dumpStmt(s AnyStatement, level int) string {
	pad := strings.Repeat("   ", level)
	line := s.LineLoc()
	var kind string

	switch s.(type) {
	case *LetStmt:
		kind = "Let"
	case *ReturnStmt:
		kind = "Return"
	case *InvokeStmt:
		kind = "Invoke"
	case *AssignStmt:
		kind = "Assign"
	case *YieldStmt:
		kind = "Yield"
	}

	return fmt.Sprintf("%s (%s (Line: %d))", pad, kind, line)
}

func DumpExprs(a Ast) {
	Walk(&a, func(n Node) {
		switch x := n.(type) {
		case *BlockExpr:
			fmt.Printf("%-20s At Line: %d\n", "BlockExpr", x.LineLoc())
		case *TupleExpr:
			fmt.Printf("%-20s At Line: %d\n", "TupleExpr", x.LineLoc())
		case *InvokeExpr:
			fmt.Printf("%-20s At Line: %d\n", "InvokeExpr", x.LineLoc())
		case *NameExpr:
			fmt.Printf("%-20s At Line: %d\n", "NameExpr", x.LineLoc())
		case *IntExpr:
			fmt.Printf("%-20s At Line: %d\n", "IntExpr", x.LineLoc())
		case *UnitExpr:
			fmt.Printf("%-20s At Line: %d\n", "UnitExpr", x.LineLoc())
		case *BiopExpr:
			switch x.Token.Kind {
			case TOKEN_PLUS:
				fmt.Printf("%-20s At Line: %d\n", "BiopExpr   (+)", x.LineLoc())
			case TOKEN_MINUS:
				fmt.Printf("%-20s At Line: %d\n", "BiopExpr   (-)", x.LineLoc())
			case TOKEN_STAR:
				fmt.Printf("%-20s At Line: %d\n", "BiopExpr   (*)", x.LineLoc())
			case TOKEN_SLASH:
				fmt.Printf("%-20s At Line: %d\n", "BiopExpr   (/)", x.LineLoc())
			case TOKEN_DOT:
				fmt.Printf("%-20s At Line: %d\n", "BiopExpr   (.)", x.LineLoc())
			default:
				fmt.Printf("%-20s At Line: %d\n", "BiopExpr   ( )", x.LineLoc())
			}
		case *UnopExpr:
			switch x.Token.Kind {
			case TOKEN_MUTAMPERSAND:
				fmt.Printf("%-20s At Line: %d\n", "UnopExpr   (mut&)", x.LineLoc())
			case TOKEN_AMPERSAND:
				fmt.Printf("%-20s At Line: %d\n", "UnopExpr   (&)", x.LineLoc())
			case TOKEN_QUESTION:
				fmt.Printf("%-20s At Line: %d\n", "UnopExpr   (?)", x.LineLoc())
			}
		default:
		}
	})
}

func quote(s string) string {
	return fmt.Sprintf("'%s'", s)
}
