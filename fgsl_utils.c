#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <gsl/gsl_interp.h>
#include <gsl/gsl_spline.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_odeiv.h>
#include <gsl/gsl_odeiv2.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_qrng.h>
#include <gsl/gsl_integration.h>
#include <gsl/gsl_wavelet.h>
#include <gsl/gsl_roots.h>
#include <gsl/gsl_min.h>
#include <gsl/gsl_multiroots.h>
#include <gsl/gsl_multimin.h>
#include <gsl/gsl_multifit_nlin.h>
#include <gsl/gsl_ntuple.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_monte.h>
#include <gsl/gsl_monte_miser.h>
#include <gsl/gsl_monte_vegas.h>
#include <gsl/gsl_permutation.h>
#include <gsl/gsl_combination.h>
#include <gsl/gsl_multiset.h>
#include <gsl/gsl_wavelet.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
// FIXME: remove after IBM compiler fixed
#include <gsl/gsl_sf.h>
#include <gsl/gsl_multifit.h>
#include <gsl/gsl_multifit_nlinear.h>
#include <gsl/gsl_multilarge.h>
#include <gsl/gsl_multilarge_nlinear.h>
#include <gsl/gsl_interp2d.h>
#include <gsl/gsl_spmatrix.h>
#include <gsl/gsl_splinalg.h>



gsl_vector *fgsl_aux_vector_double_init() {
    gsl_vector *result;
    result = (gsl_vector *) malloc(sizeof(gsl_vector));
    result->block = (gsl_block *) malloc(sizeof(gsl_block));
    result->owner = 0;
    result->stride = 0;
    result->size = 0;
    return result;
}
gsl_vector_int *fgsl_aux_vector_int_init() {
    gsl_vector_int *result;
    result = (gsl_vector_int *) malloc(sizeof(gsl_vector_int));
    result->block = (gsl_block_int *) malloc(sizeof(gsl_block_int));
    result->owner = 0;
    result->stride = 0;
    result->size = 0;
    return result;
}
void fgsl_aux_vector_double_free(gsl_vector *vec) {
    free(vec->block);
    free(vec);
}
void fgsl_aux_vector_int_free(gsl_vector_int *vec) {
    free(vec->block);
    free(vec);
}

int fgsl_aux_vector_double_align(double *a, size_t len, gsl_vector *fvec, size_t size,
				 size_t offset, size_t stride) {

    if (fvec == NULL || fvec->block == NULL) {
	return GSL_EFAULT;
    }
    if (offset + 1 + (size-1)*stride > len) {
	return GSL_EINVAL;
    }
    fvec->block->size = len;
    fvec->block->data = a;
    fvec->size = size;
    fvec->stride = stride;
    fvec->data = a+offset;
    fvec->owner = 0;
    return GSL_SUCCESS;
}
int fgsl_aux_vector_int_align(int *a, size_t len, gsl_vector_int *fvec, size_t size,
         size_t offset, size_t stride) {

    if (fvec == NULL || fvec->block == NULL) {
  return GSL_EFAULT;
    }
    if (offset + 1 + (size-1)*stride > len) {
  return GSL_EINVAL;
    }
    fvec->block->size = len;
    fvec->block->data = a;
    fvec->size = size;
    fvec->stride = stride;
    fvec->data = a+offset;
    fvec->owner = 0;
    return GSL_SUCCESS;
}


size_t fgsl_aux_vector_double_size(gsl_vector *fvec) {
    return fvec->size;
}
size_t fgsl_aux_vector_int_size(gsl_vector_int *fvec) {
    return fvec->size;
}

size_t fgsl_aux_vector_double_stride(gsl_vector *fvec) {
    return fvec->stride;
}
size_t fgsl_aux_vector_int_stride(gsl_vector_int *fvec) {
    return fvec->stride;
}


gsl_vector_complex *fgsl_aux_vector_complex_init() {
    gsl_vector_complex *result;
    result = (gsl_vector_complex *) malloc(sizeof(gsl_vector_complex));
    result->block = (gsl_block_complex *) malloc(sizeof(gsl_block_complex));
    result->owner = 0;
    result->stride = 0;
    result->size = 0;
    return result;
}
void fgsl_aux_vector_complex_free(gsl_vector_complex *vec) {
    free(vec->block);
    free(vec);
}

int fgsl_aux_vector_complex_align(double *a, size_t len,
				  gsl_vector_complex *fvec, size_t size,
				  size_t offset, size_t stride) {

    if (fvec == NULL || fvec->block == NULL) {
	return GSL_EFAULT;
    }
    if (offset + 1 + (size-1)*stride > len) {
	return GSL_EINVAL;
    }
    fvec->block->size = len;
    fvec->block->data = a;
    fvec->size = size;
    fvec->stride = stride;
    fvec->data = a+2*offset;
    fvec->owner = 0;
    return GSL_SUCCESS;
}

size_t fgsl_aux_vector_complex_size(gsl_vector_complex *fvec) {
    return fvec->size;
}

size_t fgsl_aux_vector_complex_stride(gsl_vector_complex *fvec) {
    return fvec->stride;
}



gsl_matrix *fgsl_aux_matrix_double_init() {
    gsl_matrix *result;
    result = (gsl_matrix *) malloc(sizeof(gsl_matrix));
    result->block = (gsl_block *) malloc(sizeof(gsl_block));
    result->owner = 0;
    return result;
}

void fgsl_aux_matrix_double_free(gsl_matrix *mat) {
    free(mat->block);
    free(mat);
}

int fgsl_aux_matrix_double_align(double *a, size_t lda, size_t n, size_t m, gsl_matrix *fvec) {

    if (fvec == NULL || fvec->block == NULL) {
	return GSL_EFAULT;
    }
    if (n > lda) {
	return GSL_EINVAL;
    }
    fvec->block->size = lda*m;
    fvec->block->data = a;
    fvec->size1 = m;
    fvec->size2 = n;
    fvec->tda = lda;
    fvec->data = a;
    fvec->owner = 0;
    return GSL_SUCCESS;
}

void fgsl_aux_matrix_double_size(gsl_matrix *fvec, size_t *lda, size_t *m, size_t *n) {
    if (m != NULL)
    	*m = fvec->size2;
    if (n != NULL)
    	*n = fvec->size1;
    if (lda != NULL)
    	*lda = fvec->tda;
}

gsl_matrix_complex *fgsl_aux_matrix_complex_init() {
    gsl_matrix_complex *result;
    result = (gsl_matrix_complex *) malloc(sizeof(gsl_matrix_complex));
    result->block = (gsl_block_complex *)
	malloc(sizeof(gsl_block_complex));
    result->owner = 0;
    return result;
}

void fgsl_aux_matrix_complex_free(gsl_matrix_complex *mat) {
    free(mat->block);
    free(mat);
}

int fgsl_aux_matrix_complex_align(double *a, size_t lda, size_t n,
				  size_t m, gsl_matrix_complex *fvec) {

    if (fvec == NULL || fvec->block == NULL) {
	return GSL_EFAULT;
    }
    if (n > lda) {
	return GSL_EINVAL;
    }
    fvec->block->size = lda*m;
    fvec->block->data = a;
    fvec->size1 = m;
    fvec->size2 = n;
    fvec->tda = lda;
    fvec->data = a;
    fvec->owner = 0;
    return GSL_SUCCESS;
}

void fgsl_aux_matrix_complex_size(gsl_matrix_complex *fvec,
				  size_t *lda, size_t *m, size_t *n) {
    if (m != NULL)
    	*m = fvec->size2;
    if (n != NULL)
    	*n = fvec->size1;
    if (lda != NULL)
    	*lda = fvec->tda;
}

const gsl_interp2d_type *fgsl_aux_interp2d_alloc(int i) {
  const gsl_interp2d_type *res;
  switch (i) {
    case 1:
      res = gsl_interp2d_bilinear;
      break;
    case 2:
      res = gsl_interp2d_bicubic;
      break;
	  default:
	    res = NULL;
	    break;
  }
  return res;
}

const gsl_interp_type *fgsl_aux_interp_alloc(int i) {
    const gsl_interp_type *res;
    switch(i) {
	case 1:
	    res = gsl_interp_linear;
	    break;
	case 2:
	    res = gsl_interp_polynomial;
	    break;
	case 3:
//	    printf("here we are: ");
	    res = gsl_interp_cspline;
	    break;
	case 4:
	    res = gsl_interp_cspline_periodic;
	    break;
	case 5:
	    res = gsl_interp_akima;
	    break;
	case 6:
	    res = gsl_interp_akima_periodic;
	    break;
	case 7:
	    res = gsl_interp_steffen;
	    break;
	default:
	    res = NULL;
	    break;
    }
//    printf("i had value %i\n",i);
//    printf("Address of interp type is %p\n",res);
//    printf("Address of cspline interp type is %p\n",gsl_interp_cspline);
    return res;
}

gsl_odeiv_system *fgsl_odeiv_system_cinit(
			     int (*func)(double t, const double y[], double dydt[], void * params),
			     size_t dimension, void *params,
			     int (*jacobian)(double t, const double y[], double * dfdy, double dfdt[], void * params)) {
    gsl_odeiv_system *result;
// debug
//    printf("cinit starts: \n");
//    printf(" params is %f\n",*(double *)params);
//    double y[2] = {1.0, 0.0};
//    double dydt[2];
//    printf(" call func\n");
//    func(0.0,y,dydt,params);
//    printf(" func done: %f %f\n",dydt[0],dydt[1]);
// debug ends
    result = (gsl_odeiv_system *) malloc(sizeof(gsl_odeiv_system));
    result->function = func;
    result->jacobian = jacobian;
    result->params = params;
    result->dimension = dimension;
//    printf(" call func2\n");
//    (*result->function)(0.0,y,dydt,params);
//    printf(" func done: %f %f\n",dydt[0],dydt[1]);
    return result;
}

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

void fgsl_odeiv_system_cfree(gsl_odeiv_system *system) {
    free(system);
}

void fgsl_odeiv2_system_cfree(gsl_odeiv2_system *system) {
    free(system);
}

const gsl_odeiv_step_type *fgsl_aux_odeiv_step_alloc(int i) {
    const gsl_odeiv_step_type *res;
    switch(i) {
	case 1:
	    res = gsl_odeiv_step_rk2;
	    break;
	case 2:
	    res = gsl_odeiv_step_rk4;
	    break;
	case 3:
	    res = gsl_odeiv_step_rkf45;
	    break;
	case 4:
	    res = gsl_odeiv_step_rkck;
	    break;
	case 5:
	    res = gsl_odeiv_step_rk8pd;
	    break;
	case 6:
	    res = gsl_odeiv_step_rk2imp;
	    break;
	case 7:
	    res = gsl_odeiv_step_rk2simp;
	    break;
	case 8:
	    res = gsl_odeiv_step_rk4imp;
	    break;
	case 9:
	    res = gsl_odeiv_step_bsimp;
	    break;
	case 10:
	    res = gsl_odeiv_step_gear1;
	    break;
	case 11:
	    res = gsl_odeiv_step_gear2;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;
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

const gsl_rng_type *fgsl_aux_rng_assign(int i) {
    const gsl_rng_type *res;
    switch(i) {
	case -1:
	    res = gsl_rng_default;
	    break;
	case 1:
	    res = gsl_rng_borosh13;
	    break;
	case 2:
	    res = gsl_rng_coveyou;
	    break;
	case 3:
	    res = gsl_rng_cmrg;
	    break;
	case 4:
	    res = gsl_rng_fishman18;
	    break;
	case 5:
	    res = gsl_rng_fishman20;
	    break;
	case 6:
	    res = gsl_rng_fishman2x;
	    break;
	case 7:
	    res = gsl_rng_gfsr4;
	    break;
	case 8:
	    res = gsl_rng_knuthran;
	    break;
	case 9:
	    res = gsl_rng_knuthran2;
	    break;
	case 10:
	    res = gsl_rng_lecuyer21;
	    break;
	case 11:
	    res = gsl_rng_minstd;
	    break;
	case 12:
	    res = gsl_rng_mrg;
	    break;
	case 13:
	    res = gsl_rng_mt19937;
	    break;
	case 14:
	    res = gsl_rng_mt19937_1999;
	    break;
	case 15:
	    res = gsl_rng_mt19937_1998;
	    break;
	case 16:
	    res = gsl_rng_r250;
	    break;
	case 17:
	    res = gsl_rng_ran0;
	    break;
	case 18:
	    res = gsl_rng_ran1;
	    break;
	case 19:
	    res = gsl_rng_ran2;
	    break;
	case 20:
	    res = gsl_rng_ran3;
	    break;
	case 21:
	    res = gsl_rng_rand;
	    break;
	case 22:
	    res = gsl_rng_rand48;
	    break;
	case 23:
	    res = gsl_rng_random128_bsd;
	    break;
	case 24:
	    res = gsl_rng_random128_glibc2;
	    break;
	case 25:
	    res = gsl_rng_random128_libc5;
	    break;
	case 26:
	    res = gsl_rng_random256_bsd;
	    break;
	case 27:
	    res = gsl_rng_random256_glibc2;
	    break;
	case 28:
	    res = gsl_rng_random256_libc5;
	    break;
	case 29:
	    res = gsl_rng_random32_bsd;
	    break;
	case 30:
	    res = gsl_rng_random32_glibc2;
	    break;
	case 31:
	    res = gsl_rng_random32_libc5;
	    break;
	case 32:
	    res = gsl_rng_random64_bsd;
	    break;
	case 33:
	    res = gsl_rng_random64_glibc2;
	    break;
	case 34:
	    res = gsl_rng_random64_libc5;
	    break;
	case 35:
	    res = gsl_rng_random8_bsd;
	    break;
	case 36:
	    res = gsl_rng_random8_glibc2;
	    break;
	case 37:
	    res = gsl_rng_random8_libc5;
	    break;
	case 38:
	    res = gsl_rng_random_bsd;
	    break;
	case 39:
	    res = gsl_rng_random_glibc2;
	    break;
	case 40:
	    res = gsl_rng_random_libc5;
	    break;
	case 41:
	    res = gsl_rng_randu;
	    break;
	case 42:
	    res = gsl_rng_ranf;
	    break;
	case 43:
	    res = gsl_rng_ranlux;
	    break;
	case 44:
	    res = gsl_rng_ranlux389;
	    break;
	case 45:
	    res = gsl_rng_ranlxd1;
	    break;
	case 46:
	    res = gsl_rng_ranlxd2;
	    break;
	case 47:
	    res = gsl_rng_ranlxs0;
	    break;
	case 48:
	    res = gsl_rng_ranlxs1;
	    break;
	case 49:
	    res = gsl_rng_ranlxs2;
	    break;
	case 50:
	    res = gsl_rng_ranmar;
	    break;
	case 51:
	    res = gsl_rng_slatec;
	    break;
	case 52:
	    res = gsl_rng_taus;
	    break;
	case 53:
	    res = gsl_rng_taus2;
	    break;
	case 54:
	    res = gsl_rng_taus113;
	    break;
	case 55:
	    res = gsl_rng_transputer;
	    break;
	case 56:
	    res = gsl_rng_tt800;
	    break;
	case 57:
	    res = gsl_rng_uni;
	    break;
	case 58:
	    res = gsl_rng_uni32;
	    break;
	case 59:
	    res = gsl_rng_vax;
	    break;
	case 60:
	    res = gsl_rng_waterman14;
	    break;
	case 61:
	    res = gsl_rng_zuf;
	    break;
	case 62:
	    res = gsl_rng_knuthran2002;
	    break;
	default :
	    res = NULL;
	    break;
    }
    return res;
}

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

gsl_monte_function *fgsl_monte_function_cinit(double (*func)(double *x, size_t dim, void *params), size_t dim, void *params) {
    gsl_monte_function *result;
    result = (gsl_monte_function *) malloc(sizeof(gsl_monte_function));
    result->f = func;
    result->dim = dim;
    result->params = params;
    return result;
}

void fgsl_monte_function_cfree(gsl_monte_function *fun) {
    free(fun);
}

void fgsl_monte_miser_csetparams(gsl_monte_miser_state *s, double estimate_frac,
				 size_t min_calls, size_t min_calls_per_bisection,
				 double alpha, double dither) {
    s->estimate_frac = estimate_frac;
    s->min_calls = min_calls;
    s->min_calls_per_bisection =  min_calls_per_bisection;
    s->alpha = alpha;
    s->dither = dither;
}

void fgsl_monte_miser_cgetparams(gsl_monte_miser_state *s, double *estimate_frac,
				 size_t *min_calls, size_t *min_calls_per_bisection,
				 double *alpha, double *dither) {
    *estimate_frac = s->estimate_frac;
    *min_calls = s->min_calls;
    *min_calls_per_bisection = s->min_calls_per_bisection;
    *alpha = s->alpha;
    *dither = s->dither;
}

void fgsl_monte_vegas_csetparams(gsl_monte_vegas_state *s, double result,
				 double sigma, double chisq, double alpha,
				 size_t iterations, int stage, int mode,
				 int verbose, FILE* ostream) {
    s->result = result;
    s->sigma = sigma;
    s->chisq = chisq;
    s->alpha = alpha;
    s->iterations = iterations;
    s->stage = stage;
    s->mode = mode;
    s->verbose = verbose;
    s->ostream = ostream;
}
void fgsl_monte_vegas_cgetparams(gsl_monte_vegas_state *s, double *result,
				 double *sigma, double *chisq, double *alpha,
				 size_t *iterations, int *stage, int *mode,
				 int *verbose, FILE* ostream) {
    *result = s->result;
    *sigma = s->sigma;
    *chisq = s->chisq;
    *alpha = s->alpha;
    *iterations = s->iterations;
    *stage = s->stage;
    *mode = s->mode;
    *verbose = s->verbose;
    ostream = s->ostream;
}



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


void fgsl_multiroot_function_cfree(gsl_multiroot_function *fun) {
    free(fun);
}
void fgsl_multiroot_function_fdf_cfree(gsl_multiroot_function_fdf *fun) {
    free(fun);
}

gsl_multifit_nlinear_fdf *fgsl_multifit_nlinear_fdf_cinit(
       size_t ndim, size_t p, void *params, 
       int (*f)(const gsl_vector *x, void *params, gsl_vector *f), 
       int (*df)(const gsl_vector *x, void *params, gsl_matrix *df), 
       int (*fvv)(const gsl_vector *x, const gsl_vector *v, void *params, gsl_vector *vv) 
       ) {
    gsl_multifit_nlinear_fdf *result;
    result = (gsl_multifit_nlinear_fdf *) malloc(sizeof(gsl_multifit_nlinear_fdf));
    result->f = f;
    result->df = df;
    result->fvv = fvv;
    result->n = ndim;
    result->p = p;
    result->params = params;
    return result;
}
void fgsl_multifit_nlinear_fdf_cfree(gsl_multifit_nlinear_fdf *fun) {
    free(fun);
}

const gsl_multifit_nlinear_trs *gsl_multifit_nlinear_get_trs(int i) {
    const gsl_multifit_nlinear_trs *res;
    switch(i) {
	case 1:
	    res = gsl_multifit_nlinear_trs_lm;
	    break;
	case 2:
	    res = gsl_multifit_nlinear_trs_lmaccel;
	    break;
	case 3:
	    res = gsl_multifit_nlinear_trs_dogleg;
	    break;
	case 4:
	    res = gsl_multifit_nlinear_trs_ddogleg;
	    break;
	case 5:
	    res = gsl_multifit_nlinear_trs_subspace2D;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;
}

const gsl_multifit_nlinear_scale *gsl_multifit_nlinear_get_scale(int i) {
    const gsl_multifit_nlinear_scale *res;
    switch(i) {
	case 1:
	    res = gsl_multifit_nlinear_scale_levenberg;
	    break;
	case 2:
	    res = gsl_multifit_nlinear_scale_marquardt;
	    break;
	case 3:
	    res = gsl_multifit_nlinear_scale_more;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;
}

const gsl_multifit_nlinear_solver *gsl_multifit_nlinear_get_solver(int i) {
    const gsl_multifit_nlinear_solver *res;
    switch(i) {
	case 1:
	    res = gsl_multifit_nlinear_solver_cholesky;
	    break;
	case 2:
	    res = gsl_multifit_nlinear_solver_qr;
	    break;
	case 3:
	    res = gsl_multifit_nlinear_solver_svd;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;
}

void gsl_multifit_nlinear_fdf_get(gsl_multifit_nlinear_fdf *fdf, 
       int (**fp)(const gsl_vector *x, void *params, gsl_vector *f),
       int (**dfp)(const gsl_vector *x, void *params, gsl_matrix *df),
       int (**fvvp)(const gsl_vector *x, const gsl_vector *v, void *params, gsl_vector *vv),
       size_t *n, size_t *p, void **params, size_t *nevalf, size_t *nevaldf, size_t *nevalfvv) {

       *fp = fdf->f; *dfp = fdf->df; *fvvp = fdf->fvv;
       *n = fdf->n; *p = fdf->p; 
       *params = fdf->params;
       *nevalf = fdf->nevalf; *nevaldf = fdf->nevaldf; *nevalfvv = fdf->nevalfvv;
}

gsl_multifit_nlinear_type *gsl_multifit_nlinear_setup(char *s) {
    return (gsl_multifit_nlinear_type *) gsl_multifit_nlinear_trust;
}
gsl_multilarge_nlinear_type *gsl_multilarge_nlinear_setup(char *s) {
    return (gsl_multilarge_nlinear_type *) gsl_multilarge_nlinear_trust;
}

gsl_multilarge_nlinear_fdf *fgsl_multilarge_nlinear_fdf_cinit(
       size_t ndim, size_t p, void *params, 
       int (*f)(const gsl_vector *x, void *params, gsl_vector *f), 
       int (*df)(CBLAS_TRANSPOSE_t TransJ, const gsl_vector * x,
              const gsl_vector * u, void * params, gsl_vector * v,
              gsl_matrix * JTJ),
       int (*fvv)(const gsl_vector *x, const gsl_vector *v, void *params, gsl_vector *vv) 
       ) {
    gsl_multilarge_nlinear_fdf *result;
    result = (gsl_multilarge_nlinear_fdf *) malloc(sizeof(gsl_multilarge_nlinear_fdf));
    result->f = f;
    result->df = df;
    result->fvv = fvv;
    result->n = ndim;
    result->p = p;
    result->params = params;
    return result;
}

void fgsl_multilarge_nlinear_fdf_cfree(gsl_multilarge_nlinear_fdf *fun) {
    free(fun);
}

void gsl_multilarge_nlinear_fdf_get(gsl_multilarge_nlinear_fdf *fdf, 
       int (**fp)(const gsl_vector *x, void *params, gsl_vector *f),
       int (**dfp)(CBLAS_TRANSPOSE_t TransJ, const gsl_vector *x, const gsl_vector *u, 
           void *params, gsl_vector *v, gsl_matrix *jtj),
       int (**fvvp)(const gsl_vector *x, const gsl_vector *v, void *params, gsl_vector *vv),
       size_t *n, size_t *p, void **params, size_t *nevalf, size_t *nevaldfu, 
       size_t *nevaldf2, size_t *nevalfvv) {

       *fp = fdf->f; *dfp = fdf->df; *fvvp = fdf->fvv;
       *n = fdf->n; *p = fdf->p; 
       *params = fdf->params;
       *nevalf = fdf->nevalf; *nevaldfu = fdf->nevaldfu; 
       *nevaldf2 = fdf->nevaldf2; *nevalfvv = fdf->nevalfvv;
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

const gsl_multilarge_nlinear_trs *gsl_multilarge_nlinear_get_trs(int i) {
    const gsl_multilarge_nlinear_trs *res;
    switch(i) {
	case 1:
	    res = gsl_multilarge_nlinear_trs_lm;
	    break;
	case 2:
	    res = gsl_multilarge_nlinear_trs_lmaccel;
	    break;
	case 3:
	    res = gsl_multilarge_nlinear_trs_dogleg;
	    break;
	case 4:
	    res = gsl_multilarge_nlinear_trs_ddogleg;
	    break;
	case 5:
	    res = gsl_multilarge_nlinear_trs_subspace2D;
	    break;
	case 6:
	    res = gsl_multilarge_nlinear_trs_cgst;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;
}

const gsl_multilarge_nlinear_scale *gsl_multilarge_nlinear_get_scale(int i) {
    const gsl_multilarge_nlinear_scale *res;
    switch(i) {
	case 1:
	    res = gsl_multilarge_nlinear_scale_levenberg;
	    break;
	case 2:
	    res = gsl_multilarge_nlinear_scale_marquardt;
	    break;
	case 3:
	    res = gsl_multilarge_nlinear_scale_more;
	    break;
	default:
	    res = NULL;
	    break;
    }
    return res;
}

const gsl_multilarge_nlinear_solver *gsl_multilarge_nlinear_get_solver(int i) {
    const gsl_multilarge_nlinear_solver *res;
    switch(i) {
	case 1:
	    res = gsl_multilarge_nlinear_solver_cholesky;
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

const gsl_multilarge_linear_type *fgsl_aux_multilarge_linear_alloc(int i) {
  const gsl_multilarge_linear_type *res;
  switch (i) {
    case 1:
      res = gsl_multilarge_linear_normal;
      break;
    case 2:
      res = gsl_multilarge_linear_tsqr;
      break;
    default:
      res = NULL;
      break;
  }
  return res;
}

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

const gsl_splinalg_itersolve_type *fgsl_aux_splinalg_itersolve_alloc(int i) {
  const gsl_splinalg_itersolve_type *res;
  switch(i) {
    case 1:
      res = gsl_splinalg_itersolve_gmres;
      break;
    default:
      res = NULL;
      break;
  }
  return res;
}

void gsl_spmatrix_size(gsl_spmatrix *m, size_t *n1, size_t *n2) {
  *n1 = m->size1;
  *n2 = m->size2;
}

void gsl_aux_spmatrix_getfields(gsl_spmatrix *m, int **i, double **data, int **p, size_t *psize) { 
  *i    = m->i;
  *data = m->data;
  *p    = m->p;
  switch (m->sptype) {
    case GSL_SPMATRIX_COO:
    *psize = m->nz;
    break;
    case GSL_SPMATRIX_CSC:
    *psize = m->size2+1;
    break;
    case GSL_SPMATRIX_CSR:
    *psize = m->size1+1;
    break;
    default:
    *psize = 0;
    break;
  }
}


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

gsl_vector *gsl_multifit_fdfsolver_dx(gsl_multifit_fdfsolver *s) {
    return s->dx;
}
gsl_vector *gsl_multifit_fdfsolver_f(gsl_multifit_fdfsolver *s) {
    return s->f;
}
/*gsl_matrix *gsl_multifit_fdfsolver_jac(gsl_multifit_fdfsolver *s) {
    return s->J;
}*/


size_t gsl_aux_sizeof_wavelet() {
    return sizeof(gsl_wavelet);
}
size_t gsl_aux_sizeof_wavelet_workspace() {
    return sizeof(gsl_wavelet_workspace);
}
size_t gsl_aux_sizeof_permutation() {
    return sizeof(gsl_permutation);
}
size_t gsl_aux_sizeof_combination() {
    return sizeof(gsl_combination);
}

size_t gsl_aux_sizeof_multiset() {
    return sizeof(gsl_multiset);
}
size_t gsl_aux_sizeof_vector() {
    return sizeof(gsl_vector);
}
size_t gsl_aux_sizeof_vector_complex() {
    return sizeof(gsl_vector_complex);
}
size_t gsl_aux_sizeof_matrix() {
    return sizeof(gsl_matrix);
}
size_t gsl_aux_sizeof_matrix_complex() {
    return sizeof(gsl_matrix_complex);
}
size_t gsl_aux_sizeof_interp() {
    return sizeof(gsl_interp);
}
size_t gsl_aux_sizeof_integration_workspace() {
    return sizeof(gsl_integration_workspace);
}
size_t gsl_aux_sizeof_integration_qaws_table() {
    return sizeof(gsl_integration_qaws_table);
}
size_t gsl_aux_sizeof_integration_qawo_table() {
    return sizeof(gsl_integration_qawo_table);
}



// The following only for testing

gsl_interp *
xgsl_interp_alloc (const gsl_interp_type * T, size_t size)
{
  gsl_interp * interp;
  int i;

  printf("Address of interp type is %p\n",T);
  printf("size is: %li , minimum is: %i\n",size,T->min_size);
  i = 0;
  while(T->name[i] != '\0') {
      printf("%c",T->name[i]);
      i++;
  }
  printf("\n - Name done\n");

  if (size < T->min_size)
    {
//	printf("size is: %i , minimum is: %i\n",size,T->min_size);
      GSL_ERROR_NULL ("insufficient number of points for interpolation type",
                      GSL_EINVAL);
    }

  interp = (gsl_interp *) malloc (sizeof(gsl_interp));

  if (interp == NULL)
    {
      GSL_ERROR_NULL ("failed to allocate space for interp struct",
                      GSL_ENOMEM);
    }

  interp->type = T;
  interp->size = size;

  if (interp->type->alloc == NULL)
    {
      interp->state = NULL;
      return interp;
    }
 interp->state = interp->type->alloc(size);

  if (interp->state == NULL)
    {
      free (interp);
      GSL_ERROR_NULL ("failed to allocate space for interp state", GSL_ENOMEM);
    };

  return interp;
}

FILE *fopenx(const char *path, const char *mode) {
    FILE *res;
    printf("path is %s\n",path);
    printf("mode is %s\n",mode);
    res = fopen(path, mode);
    if (res == NULL) {
//	printf("NULL!! Error is %s\n",strerror(errno));
	printf("NULL!! \n");
	perror("Null!!");
    }
    return res;
}
