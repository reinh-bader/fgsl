#include "config.h"
#include <gsl/gsl_permutation.h>

size_t gsl_aux_sizeof_permutation() {
    return sizeof(gsl_permutation);
}
