#pragma once

#include <assert.h>

#include "../../nox/src/base.h"

static void passTestHash001AA(void) {
    assert(fnv1a("", 0) == 2166136261);
}
