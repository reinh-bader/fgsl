#include "config.h"
#include <gsl/gsl_qrng.h>

const gsl_qrng_type *fgsl_aux_qrng_assign(int i) {
    const gsl_qrng_type *res;
    switch(i) {
	case 1:
	    res = gsl_qrng_niederreiter_2;
	    break;
	case 2:
	    res = gsl_qrng_sobol;
	    break;
	case 3:
	    res = gsl_qrng_halton;
	    break;
	case 4:
	    res = gsl_qrng_reversehalton;
	    break;
	default :
	    res = NULL;
	    break;
    }
    return res;
}
