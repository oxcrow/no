#pragma once
#include "base.h"

struct Argument {
    enum Command {
        ARGUMENT_COMMAND_COMPILE,
        ARGUMENT_COMMAND_RUN,
        //
        ARGUMENT_COMMAND_CLEAN,
        ARGUMENT_COMMAND_HELP,
    } command;

    usize argc;
    char ** argv;
};

void parseCmdlineArgs(struct Argument * arg, int argc, char ** argv, Status * s);
