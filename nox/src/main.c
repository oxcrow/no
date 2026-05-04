#include <stdio.h>
#include <stdlib.h>
//
#include "base.h"

static void * CONTEXT = NULL;
static void (*CLEANUP)(void *) = NULL;

void cleanup(void) {
	if (CONTEXT != NULL) {
		CLEANUP(CONTEXT);
		CONTEXT = NULL;
	}
}

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

	// Register method to cleanup resources at exit
	if (atexit(cleanup)) {
		die(NULL, "Unable to register atexit(cleanup) method to clean resources at exit.");
	}

	// Parse command line arguments
	if (argc == 1) {
		usage();
		exit(EXIT_SUCCESS);
	} else {
		Status s = cmdParseArgs(&args, argc, argv);
		if (s.code) {
			die(&s, "Unable to parse command line arguments.");
		}
	}

	// Show help if requested with -h or --help flags
	if (args.help) {
		usage();
		return EXIT_SUCCESS;
	}

	return EXIT_SUCCESS;
}
