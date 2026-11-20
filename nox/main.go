package main

import (
	. "no/core"

	"fmt"
	"os"
)

func usage() {
	fmt.Fprintf(os.Stderr, "Usage: no <command> [options]\n")
	fmt.Fprintf(os.Stderr, "\n")
	//
	fmt.Fprintf(os.Stderr, "No: A language for the workers of the world.\n")
	fmt.Fprintf(os.Stderr, "\n")
	//
	fmt.Fprintf(os.Stderr, "Commands:\n")
	//                     "123-123-123-123-123-123-123-1"
	fmt.Fprintf(os.Stderr, "    %scompile, c%s          Compile into artifacts.\n", ANSI_GREEN, ANSI_RESET)
	fmt.Fprintf(os.Stderr, "    %shelp, h%s [command]   Display help message.\n", ANSI_GREEN, ANSI_RESET)
	fmt.Fprintf(os.Stderr, "\n")
	//
	fmt.Fprintf(os.Stderr, "(%sBelieve in yourself, when no one else will%s)\n", ANSI_ITALIC, ANSI_RESET)

}

func main() {
	argv := os.Args
	if len(argv) == 1 {
		usage()
		return
	}
}
