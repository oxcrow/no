package main

import (
	. "no/analyse"
	. "no/core"
	. "no/parser"

	"fmt"
	"os"
)

func usage() {
	fmt.Printf("No: A language for workers of the world.\n")
	fmt.Printf("\n")
	fmt.Printf("    $ no [COMMAND] <FILE.no> [OPTIONS]\n")
	fmt.Printf("\n")
	fmt.Printf("%sFile%s:                  An ASCII file with .no extension.\n", ANSI_GREEN, ANSI_RESET)
	fmt.Printf("\n")
	fmt.Printf("%sCommand%s:               What should the compiler do?\n", ANSI_GREEN, ANSI_RESET)
	fmt.Printf("    %s[build]%s            Build binary executable. (default)\n", ANSI_CYAN, ANSI_RESET)
	fmt.Printf("    %sclean%s              Clean build artifacts.\n", ANSI_CYAN, ANSI_RESET)
	fmt.Printf("    %stest%s               Build and run tests.\n", ANSI_CYAN, ANSI_RESET)
	fmt.Printf("    %srun%s                Run executable.\n", ANSI_CYAN, ANSI_RESET)
	fmt.Printf("\n")
	fmt.Printf("%sOptions%s:               What options should we configure?\n", ANSI_GREEN, ANSI_RESET)
	fmt.Printf("    %s-h --help%s          Show this help message.\n", ANSI_CYAN, ANSI_RESET)
	fmt.Printf("    %s[EMITTER]%s          IR/ASM/EXE emitter.\n", ANSI_CYAN, ANSI_RESET)
	fmt.Printf("\n")
	fmt.Printf("%sEmitter%s:               What should the compiler emit as result?\n", ANSI_GREEN, ANSI_RESET)
	fmt.Printf("    %s--emit-asm%s         Emit ASM by compiling QBE IR, then Exit.\n", ANSI_CYAN, ANSI_RESET)
	fmt.Printf("    %s--emit-qbe%s         Emit QBE IR by lowering AST, then Exit.\n", ANSI_CYAN, ANSI_RESET)
	fmt.Printf("    %s[]%s                 Emit binary executable. (default)\n", ANSI_CYAN, ANSI_RESET)
	fmt.Printf("\n")
}

func main() {
	args := Args{}

	// Parse command line arguments
	argv := os.Args
	if len(argv) == 1 {
		usage()
		return
	} else {
		ParseArgs(&args, argv)
	}

	// Print help if requested
	if args.Help {
		usage()
		return
	}
	// If command mode is anything except build, then exit.
	// TODO: Implement other command modes.
	if args.CommandMode != ARGS_COMMAND_BUILD {
		return
	}

	// Parse the root file for now
	// Later analyze its dependencies, and parse them too
	root := ParseFile(args.RootFilePath)

	AnalyseAst(&root)
	Ignore(root)
}
