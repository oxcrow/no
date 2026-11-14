#include "args.hh"
#include "base.hh"

#include <string.h>

void Argument::parse(int argc, char ** argv, Status & s) {
    // Do we have anything to parse?
    if (argc == 1) {
        mCommand = ARG_CMD_HELP;
        return;
    }

    // Parse command mode
    const char * cmd = argv[1];
    if (!strncmp("compile", cmd, 7)) {
        mCommand = ARG_CMD_COMPILE;
    } else if (!strncmp("c", cmd, 1)) {
        mCommand = ARG_CMD_COMPILE;
    } else if (!strncmp("help", cmd, 4)) {
        mCommand = ARG_CMD_HELP;
    } else if (!strncmp("h", cmd, 1)) {
        mCommand = ARG_CMD_HELP;
    } else {
        mCommand = ARG_CMD_HELP;
    }

    // Parse command options
    switch (mCommand) {
    case ARG_CMD_COMPILE: {
        // Find the position of the root file path then extract the file path
        const char * filepath = [&]() -> const char * {
            usize fileIdx = 0;
            for (usize i = 2; i < (usize)argc; i++) {
                if (argv[i][0] != '-') {
                    fileIdx = i;
                    break;
                }
            }
            if (fileIdx == 0) {
                return NULL;
            }
            return argv[fileIdx];
        }();
        // Ensure we found the correct file path
        const bool fileNotFound = filepath == NULL; duck(
            s, void(), fileNotFound, "Unable to parse filepath from compile command."
        );
        // Ensure the file has .no extension
        const bool hasWrongExtension = strncmp(PTRMATH(filepath + (strlen(filepath) - 3)), ".no", 3); duck(
            s, void(), hasWrongExtension, "Unable to compile file without .no extension."
        );
        mCompile.mFilePath = filepath;
        break;
    }
    case ARG_CMD_HELP: {
        // Find the command for which we want to display help
        if (argc >= 3) {
            mHelp.mCommand = argv[2];
        }
        break;
    }
    default:;
    }
}
