#include "config.h"
#include <stdlib.h>
#include <gsl/gsl_multimin.h>



gsl_multimin_function *fgsl_multimin_function_cinit(double (*f)(const gsl_vector *, void *),
						    size_t n, void *params) {
    gsl_multimin_function *result;
    result = (gsl_multimin_function *) malloc(sizeof(gsl_multimin_function));
    result->f = f;
    result->n = n;
    result->params = params;
    return result;
}

gsl_multimin_function_fdf *fgsl_multimin_function_fdf_cinit(
    double (*f)(const gsl_vector *, void *),
    void (*df)(const gsl_vector *, void *, gsl_vector *),
    void (*fdf)(const gsl_vector *, void *, double *, gsl_vector *),
    size_t n, void *params) {
    gsl_multimin_function_fdf *result;
    result = (gsl_multimin_function_fdf *) malloc(sizeof(gsl_multimin_function_fdf));
    result->f = f;
    result->df = df;
    result->fdf = fdf;
    result->n = n;
    result->params = params;
    return result;
}

void fgsl_multimin_function_cfree(gsl_multimin_function *fun) {
    free(fun);
}
void fgsl_multimin_function_fdf_cfree(gsl_multimin_function_fdf *fun) {
    free(fun);
}

const gsl_multimin_fminimizer_type *fgsl_aux_multimin_fminimizer_alloc(int i) {
    const gsl_multimin_fminimizer_type *res;
    switch(i) {
	case 1:
	    res = gsl_multimin_fminimizer_nmsimplex;
	    break;
	case 2:
	    res = gsl_multimin_fminimizer_nmsimplex2;
	    break;
	case 3:
	    res = gsl_multimin_fminimizer_nmsimplex2rand;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;
}

const gsl_multimin_fdfminimizer_type *fgsl_aux_multimin_fdfminimizer_alloc(int i) {
    const gsl_multimin_fdfminimizer_type *res;
    switch(i) {
	case 1:
	    res = gsl_multimin_fdfminimizer_steepest_descent;
	    break;
	case 2:
	    res = gsl_multimin_fdfminimizer_conjugate_pr;
	    break;
	case 3:
	    res = gsl_multimin_fdfminimizer_conjugate_fr;
	    break;
	case 4:
	    res = gsl_multimin_fdfminimizer_vector_bfgs;
	    break;
	case 5:
	    res = gsl_multimin_fdfminimizer_vector_bfgs2;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;
}
