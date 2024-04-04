#include "config.h"
#include <stdio.h>

FILE *fgsl_cstdin() {
    return stdin;
}
FILE *fgsl_cstdout() {
    return stdout;
}
FILE *fgsl_cstderr() {
    return stderr;
}
