#include "argument.h"

void parseCmdlineArgs(struct Argument * arg, int argc, char ** argv, Status * s) {
    arg->argc = argc;
    arg->argv = argv;

    const char * command = argv[1];

    // Parse the command mode the compiler is expected to run
    if (stringEqual(command, "compile", stringLength(command, 8))) {
        arg->command = ARGUMENT_COMMAND_COMPILE;
    } else if (stringEqual(command, "c", stringLength(command, 2))) {
        arg->command = ARGUMENT_COMMAND_COMPILE;
    } else if (stringEqual(command, "run", stringLength(command, 4))) {
        arg->command = ARGUMENT_COMMAND_RUN;
    } else if (stringEqual(command, "r", stringLength(command, 2))) {
        arg->command = ARGUMENT_COMMAND_RUN;
    } else if (stringEqual(command, "help", stringLength(command, 8))) {
        arg->command = ARGUMENT_COMMAND_HELP;
    } else {
        duck(s, "Unable to parse any command from command line arguments.");
        return;
    }

    // Parse rest of the options required to run the expected commands
    switch (arg->command) {
    case ARGUMENT_COMMAND_COMPILE:
        break;
    case ARGUMENT_COMMAND_RUN:
        break;
    case ARGUMENT_COMMAND_CLEAN:
        break;
    case ARGUMENT_COMMAND_HELP:
        break;
    default:
        never("wut?");
    }

    return;
}
