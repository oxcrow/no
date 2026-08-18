#include <stdio.h>

#include "base.h"
//
#include "argument.h"
#include "file.h"
#include "parse/parse.h"

void usage(FILE * f) {
    fprintf(f, "No - A language for workers of the world.\n");
    fprintf(f, "\n");

    fprintf(f, "Usage: no command [options] file\n");
    fprintf(f, "Command:\n");
    fprintf(f, "    %scompile, c%s                  Compile code\n", ANSI_GREEN, ANSI_RESET);
    fprintf(f, "    %srun, r%s                      Compile and run code\n", ANSI_GREEN, ANSI_RESET);
    fprintf(f, "    %shelp <command>%s              Display help message for command\n", ANSI_GREEN, ANSI_RESET);
    fprintf(f, "Options:\n");
    fprintf(f, "    %s--help, -h%s                  Display help message for compiler\n", ANSI_CYAN, ANSI_RESET);
    fprintf(f, "\n");

    fprintf(f, "%sBelieve in yourself.%s\n", ANSI_ITALIC, ANSI_RESET);
}

int dev(int argc, char ** argv) {
    struct Allocator mem;
    struct Argument arg;
    struct Status s;

    // Parse command line arguments
    if (argc == 1) {
        usage(stderr);
    } else {
        parseCmdlineArgs(&arg, argc, argv, &s); stop(&s);
    }

    // Create an allocator to safely manage memory
    allocatorInitHeap(&mem, 10e6);

    switch (arg.command) {
    case ARGUMENT_COMMAND_UNKNOWN:
        break;
    case ARGUMENT_COMMAND_COMPILE: {
        // Do we need a local arena for parsing AST?
        // Yes! Yes we do! Recursively parsing dependencies will be slow,
        // if we do not use arena allocated memory. Moreover, even inside
        // our code, we need short lived arenas for processing temporary
        // data, such as strings, lists, ast nodes, etc.
        struct Allocator arena;
        allocatorInitArena(&arena, memoryAlloc(&mem, 1e6, sizeof(char), 16), 1e6);

        // Read root file's code
        const char * rootFilePath = argv[arg.compile.rootFileArgvIndex];
        const char * rootCode = readFileText(&mem, rootFilePath, &s); stop(&s);

        // Parse root file's code
        void * rootAst = parseFile(&arena, rootFilePath, rootCode, &s); stop(&s);

        allocatorDrop(&arena);
        break;
    }
    case ARGUMENT_COMMAND_RUN:
        break;
    case ARGUMENT_COMMAND_CLEAN:
        break;
    case ARGUMENT_COMMAND_HELP:
        break;
    default:
        break;
    }

    // Cleanup
    allocatorDrop(&mem);

    return 0;
}

int main(int argc, char ** argv) {
    return dev(argc, argv);
}
