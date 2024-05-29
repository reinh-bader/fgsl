#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <gsl/gsl_interp.h>
#include <gsl/gsl_spline.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>

#include <gsl/gsl_wavelet.h>
#include <gsl/gsl_multifit_nlin.h>

#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>

#include <gsl/gsl_permutation.h>
#include <gsl/gsl_combination.h>
#include <gsl/gsl_multiset.h>
#include <gsl/gsl_sf.h>

#include <gsl/gsl_interp2d.h>
#include <gsl/gsl_spmatrix.h>
#include <gsl/gsl_splinalg.h>





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




