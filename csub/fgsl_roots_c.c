#include "config.h"
#include <gsl/gsl_roots.h>


const gsl_root_fsolver_type *fgsl_aux_fsolver_alloc(int i) {
    const gsl_root_fsolver_type *res;
    switch(i) {
	case 1:
	    res = gsl_root_fsolver_bisection;
	    break;
	case 2:
	    res = gsl_root_fsolver_brent;
	    break;
	case 3:
	    res = gsl_root_fsolver_falsepos;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;
}
const gsl_root_fdfsolver_type *fgsl_aux_fdfsolver_alloc(int i) {
    const gsl_root_fdfsolver_type *res;
    switch(i) {
	case 1:
	    res = gsl_root_fdfsolver_newton;
	    break;
	case 2:
	    res = gsl_root_fdfsolver_secant;
	    break;
	case 3:
	    res = gsl_root_fdfsolver_steffenson;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;
}
