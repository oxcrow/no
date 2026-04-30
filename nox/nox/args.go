package nox

import (
	"slices"
)

type Args struct {
	RootFilePath string
	CommandMode  ArgsFlag
	EmitMode     ArgsFlag
	Help         bool
}

type ArgsFlag int

const (
	ARGS_COMMAND_BUILD ArgsFlag = iota
	ARGS_COMMAND_CLEAN
	ARGS_COMMAND_TEST
	ARGS_COMMAND_RUN
	//
	ARGS_EMIT_QBE
	ARGS_EMIT_ASM
	ARGS_EMIT_EXE
)

func ParseArgs(args *Args, argv []string) {
	keyExists := func(argv []string, key string) bool {
		return slices.Contains(argv, key)
	}

	keyHelp := keyExists(argv, "-h") || keyExists(argv, "--help")

	keyClean := argv[1] == "clean"
	keyTest := argv[1] == "test"
	keyRun := argv[1] == "run"
	keyBuild := (argv[1] == "build") || !(keyClean || keyTest || keyRun)

	keyEmitQBE := keyExists(argv, "--emit-qbe")
	keyEmitASM := keyExists(argv, "--emit-asm")
	keyEmitEXE := !(keyEmitQBE || keyEmitASM)

	if keyBuild {
		args.CommandMode = ARGS_COMMAND_BUILD
	} else {
		if keyClean {
			args.CommandMode = ARGS_COMMAND_CLEAN
		} else if keyTest {
			args.CommandMode = ARGS_COMMAND_TEST
		} else if keyRun {
			args.CommandMode = ARGS_COMMAND_RUN
		} else {
			panic("Unable to parse command (build, clean, test, run) from command line arguments.")
		}
	}

	if keyEmitEXE {
		args.CommandMode = ARGS_EMIT_EXE
	} else {
		if keyEmitQBE {
			args.CommandMode = ARGS_EMIT_QBE
		} else if keyEmitASM {
			args.CommandMode = ARGS_EMIT_ASM
		} else {
			panic("Unable to parse emmitter (qbe, asm) from command line arguments.")
		}
	}

	rootFileFound := false
	for _, arg := range argv[1:] {
		if arg[0] != '-' {
			if arg != "build" && arg != "clean" && arg != "test" && arg != "run" {
				args.RootFilePath = arg
				rootFileFound = true
				break
			}
		}
	}
	if !rootFileFound {
		panic("Unable to parse root file path from command line arguments.")
	}

	args.Help = keyHelp
}
