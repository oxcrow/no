#include <stdio.h>
#include <stdlib.h>
//
#include "base.h"

void usage(void) {
	printf("No: A language for workers of the world.\n");
	printf("\n");
	printf("    $ no <file.no> [option]\n");
	printf("\n");
	printf("file:                         An ASCII file with .no extension.\n");
	printf("\n");
	printf("option:                       Options to configure and tweak.\n");
	printf("    emit-option               How IR, Assembly, etc. emits.\n");
	printf("    -h --help                 Show help message.\n");
	printf("\n");
	printf("emit-option: (a|b)            Options to emit code.\n");
	printf("    --emit-qbe                Emit QBE IR, then Exit.\n");
	printf("    --emit-asm                Emit Assembly, then Exit.\n");
	printf("\n");
}

int main(int argc, char ** argv) {
	struct CmdLineArgs args = {0};

	// Parse command line arguments
	if (argc == 1) {
		usage();
		exit(EXIT_SUCCESS);
	} else {
		if (parseCmdLineArgs(&args, argc, argv)) {
			die("Unable to parse command line arguments.");
		}
	}

	return EXIT_SUCCESS;
}
