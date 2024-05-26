#include "config.h"
#include <stdlib.h>
#include <gsl/gsl_monte.h>
#include <gsl/gsl_monte_miser.h>
#include <gsl/gsl_monte_vegas.h>

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
