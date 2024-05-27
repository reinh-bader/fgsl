#include "config.h"
#include <stdio.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_interp.h>
#include <gsl/gsl_interp2d.h>
#include <gsl/gsl_spline.h>


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

size_t gsl_aux_sizeof_interp() {
    return sizeof(gsl_interp);
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

