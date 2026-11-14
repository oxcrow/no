#pragma once
#include "base.hh"

struct Argument {
    /// Argument commands
    enum {
        ARG_CMD_HELP = 0,
        ARG_CMD_COMPILE,
    } mCommand;
    /// Data for compile command
    struct {
        const char * mFilePath = NULL;
        bool emitQbe = true;
        usize mMaxBytesAlloc = 10e6;
    } mCompile;
    /// Data for help command
    struct {
        const char * mCommand = NULL;
    } mHelp;

    /// Parse command line arguments
    void parse(int argc, char ** argv, Status & s);
};
