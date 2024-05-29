#include "config.h"
#include <stdlib.h>
#include <gsl/gsl_multifit_nlinear.h>
#include <gsl/gsl_multilarge_nlinear.h>
#include <gsl/gsl_multifit_nlin.h>

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

gsl_vector *gsl_multifit_fdfsolver_dx(gsl_multifit_fdfsolver *s) {
    return s->dx;
}
gsl_vector *gsl_multifit_fdfsolver_f(gsl_multifit_fdfsolver *s) {
    return s->f;
}
/*gsl_matrix *gsl_multifit_fdfsolver_jac(gsl_multifit_fdfsolver *s) {
    return s->J;
}*/

