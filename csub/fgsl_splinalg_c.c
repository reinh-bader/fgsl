#include "config.h"
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
