#include <stdio.h>

#include "base.h"
//
#include "argument.h"

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
        parseCmdlineArgs(&arg, argc, argv, &s);
        if (s.code) {
            die(&s, "Unable to parse command line arguments.");
        }
    }

    // Create an allocator to safely manage memory
    allocatorInit(&mem, ALLOCATOR_MODE_GENERAL, 10e6);

    // Cleanup
    allocatorDrop(&mem);

    return 0;
}

int main(int argc, char ** argv) {
    dev(argc, argv);
    return 0;
}
