#include "file.h"

char * readFileText(Allocator * mem, const char * filePath, Status * s) {
    FILE * f = fopen(filePath, "r");
    if (f == NULL) {
        duck(s, "Unable to open file from disk and read its text.");
        return NULL;
    }

    // Find the size of file by seeking until its end.
    fseek(f, 0, SEEK_END);
    const usize fileSize = ftell(f);
    fseek(f, 0, SEEK_SET);

    // Allocate memory for buffer (+1 for null terminator)
    char * buffer = memoryAlloc(mem, fileSize + 1, sizeof(char));

    // Read contents of file and append the null terminator to its end
    const usize bytesRead = fread(buffer, 1, fileSize, f);
    buffer[bytesRead] = '\0';
    fclose(f);

    return buffer;
}
