package parse

import (
	. "no/core"
	. "no/data"
)

func ParseFile(filePath string) (Ast, string) {
	code := ReadFile(filePath)

	guessNumNodes := guessNumNodes(len(code))
	a := Ast{
		Nodes: make([]Node, 0, guessNumNodes),
		Spans: make([]uint32, 0, guessNumNodes),
	}

	return a, code
}

func guessNumNodes(codeLen int) int {
	switch {
	// For small files upto 2 KB, we allocate enough nodes for almost all files
	case codeLen < 2048:
		return 256
	// For medium files upto 1 MB, we slightly reduce the number of nodes
	case codeLen < 1_048_576:
		return codeLen / 8
	// For extreme files, we aggressively reduce nodes to save memory
	default:
		return codeLen / 12
	}
}
