#include <stdio.h>
#include <stdlib.h>
//
#include "parser.tab.h"

extern FILE * yyin;    // File to be opened
extern YYLTYPE yylloc; // Location

int main(int argc, char ** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: ng file\n");
        exit(EXIT_FAILURE);
    }

    yyin = fopen(argv[1], "r");
    if (!yyin) {
        fprintf(stderr, "Unable to open file.\n");
        exit(EXIT_FAILURE);
    }

    // Initialize location for first token
    yylloc.first_line = yylloc.last_line = 1;
    yylloc.first_column = yylloc.last_column = 1;
    yylloc.filename = argv[1];

    // Parse and verify grammar
    if (yyparse() == 0) {
        printf("Success: The grammar is valid! (File: %s)\n", argv[1]);
    }

    fclose(yyin);
    return 0;
}
