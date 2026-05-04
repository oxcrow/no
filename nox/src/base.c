#include "base.h"

Status parseCmdLineArgs(struct CmdLineArgs * args, usize argCount, char ** argVec) {
	// The first argument that's not a flag, is the file to be compiled
	for (usize i = 1; i < argCount; i++) {
		const char c = argVec[at(i, argCount)][0];
		if (c != '-') {
			char * filePath = argVec[at(i, argCount)];
			args->filePath.ptr = filePath;
			args->filePath.len = stringLength(filePath, 4096);
			break;
		}
	}
	return OK;
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
	for (usize i = 0; i < maxLength; i++) {
		const char a = string[at(i, maxLength)];
		const char b = other[at(i, maxLength)];
		if (a != b) {
			return false;
		}
	}
	return true;
}
