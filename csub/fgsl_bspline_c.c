#include "config.h"
#include <gsl/gsl_vector.h>
#include <gsl/gsl_bspline.h>

gsl_vector *gsl_bspline_return_knots_vector(gsl_bspline_workspace *w) {
	return w->knots;
}
