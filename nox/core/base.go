package core

import "fmt"

func Ignore(x ...any) {
	_ = x
}

func Dbg(x ...any) {
	fmt.Println(x...)
}

func TODO(thing string) {
	panic(fmt.Sprintf("(todo(%s))", thing))
}

// Optional data
type Option[T any] struct {
	x T
	e bool
}

func (o Option[T]) ok() T {
	if o.e {
		return o.x
	}
	panic("Unable to unwrap Option[T]")
}

// Index range
type Span struct {
	Start int
	End   int
}

func (s Span) Len() int {
	return s.End - s.Start
}
