#include "base.h"

bool keyExists(usize argCount, char ** argVec, const char * key);

Status cmdParseArgs(struct CmdLineArgs * args, usize argCount, char ** argVec) {
	// Parse and store help command flags
	const bool keyHelp = keyExists(argCount, argVec, "-h") || keyExists(argCount, argVec, "--help");
	args->help = keyHelp;
	// Return early if help is requested
	if (keyHelp) {
		return OK();
	}

	// The first argument that's not a flag, is the file to be compiled
	for (usize i = 1; i < argCount; i++) {
		const char c = argVec[at(i, argCount)][0];
		if (c != '-') {
			// Validate that commands are not in incorrect position.
			// So, the commands are not mistakenly identified as file path.
			char * filePath = argVec[at(i, argCount)];
			if (stringEqual(filePath, "clean", 32)
				|| stringEqual(filePath, "test", 32)
				|| stringEqual(filePath, "run", 32)) {
				if (i == 1) {
					continue;
				} else {
					return ERROR("Unable to enable command if it's not in correct position. (argv[1])");
				}
			}
			// Validate that file ends with .no extension
			const usize pathLen = stringLength(filePath, 4096);
			if (stringEqual(&filePath[pathLen - (3 + 1)], ".no", 3)) {
				args->filePath.ptr = filePath;
				args->filePath.len = stringLength(filePath, 4096);
			} else {
				return ERROR("Unable to parse file that does not end with \".no\" extension.");
			}
		}
	}
	// Validate that the file path was stored.
	if (args->filePath.ptr == NULL) {
		return ERROR("Unable to parse file path from command line arguments.");
	}

	// Parse command flags
	const bool keyCleanCmd = keyExists(argCount, argVec, "clean");
	const bool keyTestCmd = keyExists(argCount, argVec, "test");
	const bool keyRunCmd = keyExists(argCount, argVec, "run");
	const bool keyBuildCmd = !keyCleanCmd && !keyTestCmd && !keyRunCmd ? true : false;
	// Store command flag
	if (keyBuildCmd) {
		args->command = CMDLINE_COMMAND_BUILD;
	} else {
		if (keyCleanCmd) {
			args->command = CMDLINE_COMMAND_CLEAN;
		} else if (keyTestCmd) {
			args->command = CMDLINE_COMMAND_TEST;
		} else if (keyRunCmd) {
			args->command = CMDLINE_COMMAND_RUN;
		} else {
			return ERROR("Unable to parse command mode from command line.");
		}
	}

	// Parse emitter flags
	const bool keyEmitQbe = keyExists(argCount, argVec, "--emit-qbe");
	const bool keyEmitAsm = keyExists(argCount, argVec, "--emit-asm");
	const bool keyEmitExe = !keyEmitQbe && !keyEmitAsm ? true : false;
	// Store emitter flag
	if (keyEmitExe) {
		args->emit = CMDLINE_EMIT_EXE;
	} else {
		if (keyEmitQbe) {
			args->emit = CMDLINE_EMIT_QBE;
		} else if (keyEmitAsm) {
			args->emit = CMDLINE_EMIT_ASM;
		} else {
			return ERROR("Unable to parse emitter settings from command line.");
		}
	}

	return OK();
}

usize stringLength(const char * string, usize maxLength) {
	if (string == NULL) {
		return 0;
	}

	for (usize i = 0; i < maxLength; i++) {
		const char c = string[at(i, maxLength)];
		if (c == '\0') {
			return (i + 1);
		}
	}

	return maxLength;
}

bool stringEqual(const char * string, const char * other, usize maxLength) {
	if (string == NULL || other == NULL) {
		return false;
	}

	const usize stringLen = stringLength(string, maxLength);
	const usize otherLen = stringLength(other, maxLength);
	const usize minLen = MIN(stringLen, otherLen);
	if (stringLen != otherLen) {
		return false;
	}

	for (usize i = 0; i < minLen; i++) {
		const char a = string[at(i, maxLength)];
		const char b = other[at(i, maxLength)];
		if (a != b) {
			return false;
		}
	}

	return true;
}

bool keyExists(usize argCount, char ** argVec, const char * key) {
	const usize keyLen = stringLength(key, 32);

	for (usize i = 1; i < argCount; i++) {
		if (stringEqual(argVec[at(i, argCount)], key, keyLen)) {
			return true;
		}
	}

	return false;
}
