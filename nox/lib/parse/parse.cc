#include "parse.hh"
#include <ctype.h>

/// What is the character at given index?
static char charAtIndex(const char * text, usize charIndex, usize codeLen) {
    if (charIndex >= codeLen) {
        return ' ';
    }
    return text[at(charIndex, codeLen)];
}

/// Is character a delimiter?
static bool isdel(char c) {
    if (isalnum(c)) {
        return false;
    } else {
        return true;
    }
}

void parseModule(Allocator & mem, const char * filePath, Status & s) {
    const char * code = readFile(mem, filePath, s); back(s, void());
}
