#include "config.h"
#include <stdlib.h>
#include <gsl/gsl_multifit.h>
#include <gsl/gsl_multifit_nlin.h>

const gsl_multifit_robust_type *fgsl_aux_multifit_robust_alloc(int i) {
    const gsl_multifit_robust_type *res;
    switch (i) {
	case 1:
	    res = gsl_multifit_robust_default;
	    break;
	case 2:
	    res = gsl_multifit_robust_bisquare;
	    break;
	case 3:
	    res = gsl_multifit_robust_cauchy;
	    break;
	case 4:
	    res = gsl_multifit_robust_fair;
	    break;
	case 5:
	    res = gsl_multifit_robust_huber;
	    break;
	case 6:
	    res = gsl_multifit_robust_ols;
	    break;
	case 7:
	    res = gsl_multifit_robust_welsch;
	    break;
	default :
	    res = NULL;
	    break;
    }
    return res;
}

gsl_multifit_function *fgsl_multifit_function_cinit(int (*f)(const gsl_vector *x, void *params,
							     gsl_vector *f),
						    size_t n, size_t p, void *params) {
    gsl_multifit_function *result;
    result = (gsl_multifit_function *) malloc(sizeof(gsl_multifit_function));
    result->f = f;
    result->n = n;
    result->p = p;
    result->params = params;
    return result;
}

gsl_multifit_function_fdf *fgsl_multifit_function_fdf_cinit(
    int (*f)(const gsl_vector *x, void *params, gsl_vector *f),
    int (*df)(const gsl_vector *x, void *params, gsl_matrix *df),
    int (*fdf)(const gsl_vector *x, void *params, gsl_vector *f, gsl_matrix *df),
    size_t n, size_t p, void *params) {
    gsl_multifit_function_fdf *result;
    result = (gsl_multifit_function_fdf *) malloc(sizeof(gsl_multifit_function_fdf));
    result->f = f;
    result->df = df;
    result->fdf = fdf;
    result->n = n;
    result->p = p;
    result->params = params;
    return result;
}

void fgsl_multifit_function_cfree(gsl_multifit_function *fun) {
    free(fun);
}
void fgsl_multifit_function_fdf_cfree(gsl_multifit_function_fdf *fun) {
    free(fun);
}

const gsl_multifit_fsolver_type *fgsl_aux_multifit_fsolver_alloc(int i) {
    const gsl_multifit_fsolver_type *res;
    switch(i) {
	default:
	    res = NULL;
	    break;
    }
    return res;

}

const gsl_multifit_fdfsolver_type *fgsl_aux_multifit_fdfsolver_alloc(int i) {
    const gsl_multifit_fdfsolver_type *res;
    switch(i) {
	case 1:
	    res = gsl_multifit_fdfsolver_lmder;
	    break;
	case 2:
	    res = gsl_multifit_fdfsolver_lmsder;
	    break;
	case 3:
	    res = gsl_multifit_fdfsolver_lmniel;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;

}
