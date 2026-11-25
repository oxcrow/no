package core

import (
	"fmt"
	"runtime"
	"strings"
)

func Ignore(x ...any) {
	_ = x
}

func Dbg(x ...any) {
	fmt.Println(x...)
}

func DieAt(m []string, file string, line int) {
	a := strings.Join(m, "\n")
	b := fmt.Sprintf("%s%s(Error emitted from compiler source: %s:%d)%s\n", ANSI_FAINT, ANSI_ITALIC, file, line, ANSI_RESET)
	c := fmt.Sprintf("%s%s(Believe in yourself, when no one else will)%s\n", ANSI_FAINT, ANSI_ITALIC, ANSI_RESET)
	panic(a + "\n\n" + b + c)
}

func Die(m []string) {
	file, line := func() (string, int) {
		_, file, line, _ := runtime.Caller(2)
		_, file, _ = strings.Cut(file, "nox")
		file = "nox" + file
		return file, line
	}()
	DieAt(m, file, line)
}

func Damn(m string) string {
	return fmt.Sprintf("%s <> %s%s", ANSI_RED, ANSI_RESET, m)
}

func Info(m string) string {
	return fmt.Sprintf("    %s", m)
}

func Hint(m string) string {
	return fmt.Sprintf("    %sHint:%s %s", ANSI_GREEN, ANSI_RESET, m)
}

func Line(i int, m string) string {
	return fmt.Sprintf("    %s%5d%s %s", ANSI_GREEN, i, ANSI_RESET, m)
}

func Todo(m string) []string {
	return []string{
		Damn("This section of code is not yet implemented."),
		"",
		Info("Sorry, this section of code is broken as fluff."),
		Info("My stupid devs haven't implemented it, yet."),
		Info("They only shared this error code."),
		"",
		Info(Info(ANSI_RED + "> " + ANSI_RESET + strings.ToUpper(m))),
		"",
		Hint("This is not your fault, dear user."),
		Hint("It's an internal compiler error."),
		Hint("We ducked up. Sorry. (~ 3 ~)"),
	}
}

type Option[T any] struct {
	Item T
	Flag bool
}

func (o Option[T]) Ok() T {
	if !o.Flag {
		return o.Item
	}
	panic("Unable to unwrap Option[T]")
}
