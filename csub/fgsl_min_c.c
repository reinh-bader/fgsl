#include "config.h"
#include <gsl/gsl_min.h>

const gsl_min_fminimizer_type *fgsl_aux_fminimizer_alloc(int i) {
    const gsl_min_fminimizer_type *res;
    switch(i) {
	case 1:
	    res = gsl_min_fminimizer_goldensection;
	    break;
	case 2:
	    res = gsl_min_fminimizer_brent;
	    break;
	case 3:
	    res = gsl_min_fminimizer_quad_golden;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;
}
