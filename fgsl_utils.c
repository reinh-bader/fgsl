#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <gsl/gsl_interp.h>
#include <gsl/gsl_spline.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>

#include <gsl/gsl_wavelet.h>
#include <gsl/gsl_roots.h>
#include <gsl/gsl_min.h>
#include <gsl/gsl_multiroots.h>
#include <gsl/gsl_multimin.h>
#include <gsl/gsl_multifit_nlin.h>

#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>

#include <gsl/gsl_permutation.h>
#include <gsl/gsl_combination.h>
#include <gsl/gsl_multiset.h>
#include <gsl/gsl_sf.h>
#include <gsl/gsl_multifit.h>
#include <gsl/gsl_multifit_nlinear.h>
#include <gsl/gsl_multilarge.h>
#include <gsl/gsl_multilarge_nlinear.h>
#include <gsl/gsl_interp2d.h>
#include <gsl/gsl_spmatrix.h>
#include <gsl/gsl_splinalg.h>










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




gsl_vector *gsl_multifit_fdfsolver_dx(gsl_multifit_fdfsolver *s) {
    return s->dx;
}
gsl_vector *gsl_multifit_fdfsolver_f(gsl_multifit_fdfsolver *s) {
    return s->f;
}
/*gsl_matrix *gsl_multifit_fdfsolver_jac(gsl_multifit_fdfsolver *s) {
    return s->J;
}*/





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




