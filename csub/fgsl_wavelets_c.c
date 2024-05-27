#include "config.h"
#include <gsl/gsl_wavelet.h>

const gsl_wavelet_type *fgsl_aux_wavelet_alloc(int i) {
    const gsl_wavelet_type *res;
    switch(i) {
	case 1:
	    res = gsl_wavelet_daubechies;
	    break;
	case 2:
	    res = gsl_wavelet_daubechies_centered;
	    break;
	case 3:
	    res = gsl_wavelet_haar;
	    break;
	case 4:
	    res = gsl_wavelet_haar_centered;
	    break;
	case 5:
	    res = gsl_wavelet_bspline;
	    break;
	case 6:
	    res = gsl_wavelet_bspline_centered;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;
}

size_t gsl_aux_sizeof_wavelet() {
    return sizeof(gsl_wavelet);
}
size_t gsl_aux_sizeof_wavelet_workspace() {
    return sizeof(gsl_wavelet_workspace);
}
