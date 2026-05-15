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

func quote(s string) string {
	return fmt.Sprintf("'%s'", s)
}
