#include "config.h"
#include <gsl/gsl_integration.h>

size_t gsl_aux_sizeof_integration_workspace() {
    return sizeof(gsl_integration_workspace);
}
size_t gsl_aux_sizeof_integration_qaws_table() {
    return sizeof(gsl_integration_qaws_table);
}
size_t gsl_aux_sizeof_integration_qawo_table() {
    return sizeof(gsl_integration_qawo_table);
}

const gsl_integration_fixed_workspace *gsl_aux_integration_fixed_alloc(int t, const size_t n, const double a, const double b, const double alpha, const double beta) {
    const  gsl_integration_fixed_type *T;
    switch(t) {
	case 1:
	    T = gsl_integration_fixed_legendre;
	    break;
	case 2:
	    T = gsl_integration_fixed_chebyshev;
	    break;
	case 3:
	    T = gsl_integration_fixed_gegenbauer;
	    break;
	case 4:
	    T = gsl_integration_fixed_jacobi;
	    break;
	case 5:
	    T = gsl_integration_fixed_laguerre;
	    break;
	case 6:
	    T = gsl_integration_fixed_hermite;
	    break;
	case 7:
	    T = gsl_integration_fixed_exponential;
	    break;
	case 8:
	    T = gsl_integration_fixed_rational;
	    break;
	case 9:
	    T = gsl_integration_fixed_chebyshev2;
	    break;
	default :
	    T = NULL;
	    break;
    }
    return gsl_integration_fixed_alloc(T, n, a, b, alpha, beta);
}
