package main

import (
	. "no/core"
	. "no/data"

	"no/parse"

	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
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
			fmt.Fprintf(os.Stderr, "%s", msg)
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
					Damn("Unable to parse filepath to compile from arguments."),
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
		// 1. Parse the root file of the module, to find the module name (mod <name>;)
		// 2. Find other files in the module, by searching for the module name.
		// 3. Parse each file in the module in parallel, and get their asts.
		asts, codes := func() ([]Ast, []string) {
			files := func() []string {
				files := make([]string, 0, 64)
				// Parse the root file, and find out if it's part of a
				// module, or if it's a lone file, that's compiled alone.
				// BUG: I'm not happy with this. This will reparse root.
				rootIsModule := false
				if rootIsModule {
					dirpath := filepath.Dir(path)
					entrys, e := os.ReadDir(dirpath)
					if e != nil {
						Die([]string{Damn("Unable to read directory.")})
					}
					for _, f := range entrys {
						if !f.IsDir() {
							if strings.HasSuffix(f.Name(), ".no") {
								files = append(files, filepath.Join(dirpath, f.Name()))
							}
						}
					}
					return files
				}
				return []string{path}
			}()

			type ParsedFile struct {
				ast  Ast
				code string
			}
			parseCh := make(chan ParsedFile, len(files))
			var once sync.Once
			var wg sync.WaitGroup

			// Parse files in parallel
			for _, f := range files {
				wg.Add(1)
				go func(f string) {
					defer wg.Done()
					defer func() {
						if msg := recover(); msg != nil {
							once.Do(func() {
								fmt.Fprintf(os.Stderr, "%s", msg)
								os.Exit(1)
							})
						}
					}()
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
			codes := make([]string, 0, len(files)+1)
			asts := make([]Ast, 0, len(files)+1)

			// Store data from channel
			for r := range parseCh {
				codes = append(codes, r.code)
				asts = append(asts, r.ast)
			}

			return asts, codes
		}()
		Ignore(asts, codes)
	case "help", "h":
		usage()
	default:
		Die([]string{
			Damn("Unable to parse command for compiler to execute."),
			"",
			Info("I see that you want me to run \"" + cmd + "\" command."),
			Info("Sorry, I don't know what that means."),
			"",
			Hint("Run `$ no help` to show help message."),
			Hint("See what commands are available."),
		})
	}
}
