#include <stdio.h>     // for perror() and other stuff
#include <stdlib.h>    // for exit() and other stuff
#include <sys/types.h> // for pid_t type
#include <sys/wait.h>  // for waitpid()
#include <unistd.h>    // for fork()

#include "testAllocator.h"

typedef void (*Function)();

enum Expect {
    EXPECT_PASS = 0,
    EXPECT_FAIL,
};

/// Run tests in separate threads so they do not crash our test harness
void runTestHarness(Function f, enum Expect e, usize i) {
    pid_t pid = fork();
    if (pid == 0) {
        // Child fork: Run the test, and exit normally
        f();
        exit(0);
    } else if (pid > 0) {
        // Parent fork: Wait for test to suceed/fail, then report.
        int status;
        waitpid(pid, &status, 0);
        if (WIFSIGNALED(status)) {
            // Child fork crashed
            if (e == EXPECT_PASS) {
                fprintf(stderr, "Test #%zu: %sFAIL%s! (Expected test to pass, but it failed!)\n", i, ANSI_RED, ANSI_RESET);
            }
        } else if (WIFEXITED(status)) {
            // Child fork exited normally
            if (e == EXPECT_FAIL) {
                fprintf(stderr, "Test #%zu: %sPASS%s! (Expected test to fail, but it passed!)\n", i, ANSI_RED, ANSI_RESET);
            } else {
                fprintf(stderr, "Test #%zu: %sPASS%s!\n", i, ANSI_GREEN, ANSI_RESET);
            }
        }
    } else {
        perror("Unable to fork!");
    }
}

int main(void) {
    const Function f[] = {
        passTestAllocator001AA,
        passTestAllocator002AA,
    };

    for (usize i = 0; i < sizeof(f) / sizeof(f[0]); i++) {
        runTestHarness(f[i], EXPECT_PASS, i);
    }

    return 0;
}
