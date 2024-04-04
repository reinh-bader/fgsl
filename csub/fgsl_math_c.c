#include <gsl/gsl_math.h>

gsl_function *fgsl_function_cinit(double (*func)(double x, void *params), void *params) {
    gsl_function *result;
    result = (gsl_function *) malloc(sizeof(gsl_function));
    result->function = func;
    result->params = params;
    return result;
}
gsl_function_fdf *fgsl_function_fdf_cinit(double (*f)(double x, void *params),
					  double (*df)(double x, void *params),
					  void (*fdf)(double x, void *params, double *f, double *df),
					  void *params) {
    gsl_function_fdf *result;
    result = (gsl_function_fdf *) malloc(sizeof(gsl_function_fdf));
    result->f = f;
    result->df = df;
    result->fdf = fdf;
    result->params = params;
    return result;
}

double fgsl_fn_eval_aux(gsl_function *f, double x) {
    return GSL_FN_EVAL(f,x);
}
double fgsl_fn_fdf_eval_f_aux(gsl_function_fdf *f, double x) {
    return GSL_FN_FDF_EVAL_F(f,x);
}
double fgsl_fn_fdf_eval_df_aux(gsl_function_fdf *f, double x) {
    return GSL_FN_FDF_EVAL_DF(f,x);
}
void fgsl_fn_fdf_eval_f_df_aux(gsl_function_fdf *f, double x, double *y,
				double *dy) {
    GSL_FN_FDF_EVAL_F_DF(f,x,y,dy);
}

void fgsl_function_cfree(gsl_function *fun) {
    free(fun);
}
void fgsl_function_fdf_cfree(gsl_function_fdf *fun) {
    free(fun);
}
