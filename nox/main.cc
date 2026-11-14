#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lib/core/args.hh"
#include "lib/core/base.hh"
//
#include "lib/parse/parse.hh"

void usage() {
    fprintf(stderr, "Usage: no <command> [options]\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "No: A language for the workers of the world.\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "Command:\n");
    // fprintf(stderr, "123-123-123-123-123-123-123-123-123-1\n");
    fprintf(stderr, "    %scompile, c%s                  Compile and build artifacts into an executable.\n", ANSI_GREEN, ANSI_RESET);
    fprintf(stderr, "    %shelp, h%s <command>           Display help message for command.\n", ANSI_GREEN, ANSI_RESET);
    fprintf(stderr, "\n");
    fprintf(stderr, "%s(Believe in yourself)%s\n", ANSI_ITALIC, ANSI_RESET);
}

void usageCompile() {
    fprintf(stderr, "Usage: no compile [options] -- <file>\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "Positional Arguments:\n");
    // fprintf(stderr, "123-123-123-123-123-123-123-123-123-1\n");
    fprintf(stderr, "    %sfile%s                        The file we want to compile.\n", ANSI_GREEN, ANSI_RESET);
    fprintf(stderr, "\n");
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "    %s--emit-qbe%s                  Emit QBE IR instead of compiling directly to binary executable.\n", ANSI_CYAN, ANSI_RESET);
    fprintf(stderr, "    %s--max-mem%s                   Max amount of memory (in bytes) that the compiler can allocate.\n", ANSI_CYAN, ANSI_RESET);
}

int main(int argc, char ** argv) {
    Argument args = {};
    Status s = {0};

    // Parse command line arguments. If nothing is found, print help.
    args.parse(argc, argv, s); stop(s);
    if (args.mCommand == Argument::ARG_CMD_HELP) {
        if (!args.mHelp.mCommand) {
            usage();
        } else {
            if (!strncmp(args.mHelp.mCommand, "compile", 7)) {
                usageCompile();
            } else if (!strncmp(args.mHelp.mCommand, "c", 1)) {
                usageCompile();
            }
        }
        exit(0);
    }

    Allocator mem = {
        Memory{
            calloc(args.mCompile.mMaxBytesAlloc, sizeof(char)),
            args.mCompile.mMaxBytesAlloc,
            0
        },
        args.mCompile.mMaxBytesAlloc,
        0,
    };

    switch (args.mCommand) {
    case Argument::ARG_CMD_COMPILE: {
        parseModule(mem, args.mCompile.mFilePath, s); stop(s);
        break;
    }
    default:
        break;
    }

    free(mem.mBase.mPtr);
    return 0;
}
