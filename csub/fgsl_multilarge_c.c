#include "config.h"
#include <gsl/gsl_multilarge.h>



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
