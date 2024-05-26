#include "config.h"
#include <gsl/gsl_ntuple.h>


gsl_ntuple_select_fn *fgsl_ntuple_select_fn_cinit(int (*func)(void *data, void *params), void *params) {
    gsl_ntuple_select_fn *result;
    result = (gsl_ntuple_select_fn *) malloc(sizeof(gsl_ntuple_select_fn));
    result->function = func;
    result->params = params;
    return result;
}

void fgsl_ntuple_select_fn_cfree(gsl_ntuple_select_fn *fun) {
    free(fun);
}
gsl_ntuple_value_fn *fgsl_ntuple_value_fn_cinit(double (*func)(void *data, void *params), void *params) {
    gsl_ntuple_value_fn *result;
    result = (gsl_ntuple_value_fn *) malloc(sizeof(gsl_ntuple_value_fn));
    result->function = func;
    result->params = params;
    return result;
}
void fgsl_ntuple_value_fn_cfree(gsl_ntuple_value_fn *fun) {
    free(fun);
}

void *fgsl_aux_ntuple_data(gsl_ntuple* ntuple) {
    return ntuple->ntuple_data;
}
size_t fgsl_aux_ntuple_size(gsl_ntuple* ntuple) {
    return ntuple->size;
}
