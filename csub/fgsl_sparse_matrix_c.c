#include "config.h"
#include <stdlib.h>
#include <gsl/gsl_spmatrix.h>



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
