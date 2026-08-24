#pragma once
#include "base.h"

struct Argument {
    enum Command {
        ARGUMENT_COMMAND_COMPILE = 1,
        ARGUMENT_COMMAND_RUN,
        //
        ARGUMENT_COMMAND_CLEAN,
        ARGUMENT_COMMAND_HELP,
    } command;

    struct {
        /// Index of the root file path in argv array
        u32 rootFileArgvIndex;
    } compile;

    u32 argc;
    char ** argv;
};

void parseCmdlineArgs(struct Argument * arg, int argc, char ** argv, Status * s);
