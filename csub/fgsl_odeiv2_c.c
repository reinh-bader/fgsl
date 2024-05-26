#include "config.h"
#include <stdlib.h>
#include <gsl/gsl_odeiv2.h>

gsl_odeiv2_system *fgsl_odeiv2_system_cinit(
			     int (*func)(double t, const double y[], double dydt[], void * params),
			     size_t dimension, void *params,
			     int (*jacobian)(double t, const double y[], double * dfdy, double dfdt[], void * params)) {
    gsl_odeiv2_system *result;
    result = (gsl_odeiv2_system *) malloc(sizeof(gsl_odeiv2_system));
    result->function = func;
    result->jacobian = jacobian;
    result->params = params;
    result->dimension = dimension;
    return result;
}

void fgsl_odeiv2_system_cfree(gsl_odeiv2_system *system) {
    free(system);
}

const gsl_odeiv2_step_type *fgsl_aux_odeiv2_step_alloc(int i) {
    const gsl_odeiv2_step_type *res;
    switch(i) {
	case 1:
	    res = gsl_odeiv2_step_rk2;
	    break;
	case 2:
	    res = gsl_odeiv2_step_rk4;
	    break;
	case 3:
	    res = gsl_odeiv2_step_rkf45;
	    break;
	case 4:
	    res = gsl_odeiv2_step_rkck;
	    break;
	case 5:
	    res = gsl_odeiv2_step_rk8pd;
	    break;
	case 6:
	    res = gsl_odeiv2_step_rk1imp;
	    break;
	case 7:
	    res = gsl_odeiv2_step_rk2imp;
	    break;
	case 8:
	    res = gsl_odeiv2_step_rk4imp;
	    break;
	case 9:
	    res = gsl_odeiv2_step_bsimp;
	    break;
	case 10:
	    res = gsl_odeiv2_step_msadams;
	    break;
	case 11:
	    res = gsl_odeiv2_step_msbdf;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;
}
