#include "config.h"
#include <gsl/gsl_multiset.h>


size_t gsl_aux_sizeof_multiset() {
    return sizeof(gsl_multiset);
}
