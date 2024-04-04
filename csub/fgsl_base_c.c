#include "config.h"
#include <stddef.h>

size_t gsl_aux_sizeof_double() {
    return sizeof(double);
}
size_t gsl_aux_sizeof_float() {
    return sizeof(float);
}
size_t gsl_aux_sizeof_int() {
    return sizeof(int);
}
size_t gsl_aux_sizeof_long() {
    return sizeof(long);
}
size_t gsl_aux_sizeof_size_t() {
    return sizeof(size_t);
}
size_t gsl_aux_sizeof_char() {
    return sizeof(char);
}
