#include "config.h"
#include <gsl/gsl_multiroots.h>


gsl_multiroot_function *fgsl_multiroot_function_cinit(int (*f)(const gsl_vector *x, void *params,
							       gsl_vector *f), size_t n, void *params) {
    gsl_multiroot_function *result;
    result = (gsl_multiroot_function *) malloc(sizeof(gsl_multiroot_function));
    result->f = f;
    result->n = n;
    result->params = params;
    return result;
}

gsl_multiroot_function_fdf *fgsl_multiroot_function_fdf_cinit(
    int (*f)(const gsl_vector *x, void *params, gsl_vector *f),
    int (*df)(const gsl_vector *x, void *params, gsl_matrix *df),
    int (*fdf)(const gsl_vector *x, void *params, gsl_vector *f, gsl_matrix *df),
    size_t n, void *params) {
    gsl_multiroot_function_fdf *result;
    result = (gsl_multiroot_function_fdf *) malloc(sizeof(gsl_multiroot_function_fdf));
    result->f = f;
    result->df = df;
    result->fdf = fdf;
    result->n = n;
    result->params = params;
    return result;
}

const gsl_multiroot_fsolver_type *fgsl_aux_multiroot_fsolver_alloc(int i) {
    const gsl_multiroot_fsolver_type *res;
    switch(i) {
	case 1:
	    res = gsl_multiroot_fsolver_dnewton;
	    break;
	case 2:
	    res = gsl_multiroot_fsolver_broyden;
	    break;
	case 3:
	    res = gsl_multiroot_fsolver_hybrid;
	    break;
	case 4:
	    res = gsl_multiroot_fsolver_hybrids;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;
}

const gsl_multiroot_fdfsolver_type *fgsl_aux_multiroot_fdfsolver_alloc(int i) {
    const gsl_multiroot_fdfsolver_type *res;
    switch(i) {
	case 1:
	    res = gsl_multiroot_fdfsolver_newton;
	    break;
	case 2:
	    res = gsl_multiroot_fdfsolver_gnewton;
	    break;
	case 3:
	    res = gsl_multiroot_fdfsolver_hybridj;
	    break;
	case 4:
	    res = gsl_multiroot_fdfsolver_hybridsj;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;
}

void fgsl_multiroot_function_cfree(gsl_multiroot_function *fun) {
    free(fun);
}
void fgsl_multiroot_function_fdf_cfree(gsl_multiroot_function_fdf *fun) {
    free(fun);
}
