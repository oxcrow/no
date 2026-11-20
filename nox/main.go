package main

import (
	. "no/core"
	. "no/data"

	"no/parse"

	"flag"
	"fmt"
	"os"
	"sync"
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
	defer func() {
		if msg := recover(); msg != nil {
			fmt.Print(msg)
		}
	}()

	var cmd string
	var path string

	if len(os.Args) == 1 {
		usage()
		return
	} else {
		flag.Parse()
		args := flag.Args()

		cmd = args[0]

		switch cmd {
		case "compile", "c":
			if len(args) > 1 {
				path = args[1]
			} else {
				Die([]string{
					Duck("Unable to parse filepath to compile from arguments."),
					"",
					Info("I can not compile if you don't give me a filepath."),
					Info("I expect the filepath to be given as,"),
					"",
					Info(Info("$ no compile <path/to/file>.no")),
					"",
					Hint("Give me the correct filepath, please."),
					Hint("Don't forget the .no extension."),
				})
			}
		}
	}

	switch cmd {
	case "compile", "c":
		// 1. Parse the root file of the module
		// 2. Parse each file in the module in parallel
		// 3. Combine all parsed files in a single AST
		ast, codes := func() (Ast, []string) {
			root, code := parse.ParseFile(path)
			files := []string{path, path, path} // BUG: Parse the module files

			type ParsedFile struct {
				ast  Ast
				code string
			}
			parseCh := make(chan ParsedFile, len(files))
			var wg sync.WaitGroup

			// Parse files in parallel
			for _, f := range files {
				wg.Add(1)
				go func(f string) {
					defer wg.Done()
					ast, code := parse.ParseFile(f)
					parseCh <- ParsedFile{
						ast:  ast,
						code: code,
					}
				}(f)
			}

			// Wait for all files to parse
			go func() {
				wg.Wait()
				close(parseCh)
			}()

			// Estimate the memory required for the AST, and store their code
			// We collect the offsets required for combining the AST
			offsets := make([]uint32, 0, len(files)+1)
			codes := make([]string, 0, len(files)+1)
			asts := make([]Ast, 0, len(files)+1)
			numNodes := 0
			// Store data from root file
			offsets = append(offsets, 0)
			codes = append(codes, code)
			asts = append(asts, root)
			// Store data from channel
			for r := range parseCh {
				codes = append(codes, r.code)
				asts = append(asts, r.ast)
				numNodes += len(r.ast.Nodes)
				offsets = append(offsets, uint32(numNodes))
			}

			module := Ast{} // Combined AST of the whole module

			// Renumber nodes with offsets
			for i, a := range asts {
				o := offsets[i]
				Ignore(a, o)
			}

			return module, codes
		}()
		Ignore(ast, codes)
	case "help", "h":
		usage()
	default:
		Die([]string{
			Duck("Unable to parse command for compiler to execute."),
			"",
			Info("I see that you want me to run \"" + cmd + "\" command."),
			Info("Sorry, I don't know what that means."),
			"",
			Hint("Run `$ no help` to show help message."),
			Hint("See what commands are available."),
		})
	}
}
