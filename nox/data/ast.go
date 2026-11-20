package data

type Ast struct {
	// All of the nodes that exist in file.
	Nodes []Node
	// We only store the start character index of each nodes's span.
	// We can lex later, if we need the full span.
	Spans []uint32
}

type Node struct {
}

func (a Ast) BytesPerNode(code string) float64 {
	return float64(len(code)) / float64(max(len(a.Nodes), 1.0))
}
