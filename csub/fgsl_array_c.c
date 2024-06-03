#include "config.h"
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>

gsl_vector *fgsl_aux_vector_double_init() {
    gsl_vector *result;
    result = (gsl_vector *) malloc(sizeof(gsl_vector));
    result->block = (gsl_block *) malloc(sizeof(gsl_block));
    result->owner = 0;
    result->stride = 0;
    result->size = 0;
    return result;
}
gsl_vector_int *fgsl_aux_vector_int_init() {
    gsl_vector_int *result;
    result = (gsl_vector_int *) malloc(sizeof(gsl_vector_int));
    result->block = (gsl_block_int *) malloc(sizeof(gsl_block_int));
    result->owner = 0;
    result->stride = 0;
    result->size = 0;
    return result;
}
gsl_vector_uint *fgsl_aux_vector_uint_init() {
    gsl_vector_uint *result;
    result = (gsl_vector_uint *) malloc(sizeof(gsl_vector_uint));
    result->block = (gsl_block_uint *) malloc(sizeof(gsl_block_uint));
    result->owner = 0;
    result->stride = 0;
    result->size = 0;
    return result;
}

void fgsl_aux_vector_double_free(gsl_vector *vec) {
    free(vec->block);
    free(vec);
}
void fgsl_aux_vector_int_free(gsl_vector_int *vec) {
    free(vec->block);
    free(vec);
}
void fgsl_aux_vector_uint_free(gsl_vector_uint *vec) {
    free(vec->block);
    free(vec);
}

int fgsl_aux_vector_double_align(double *a, size_t len, gsl_vector *fvec, size_t size,
				 size_t offset, size_t stride) {

    if (fvec == NULL || fvec->block == NULL) {
	return GSL_EFAULT;
    }
    if (offset + 1 + (size-1)*stride > len) {
	return GSL_EINVAL;
    }
    fvec->block->size = len;
    fvec->block->data = a;
    fvec->size = size;
    fvec->stride = stride;
    fvec->data = a+offset;
    fvec->owner = 0;
    return GSL_SUCCESS;
}
int fgsl_aux_vector_int_align(int *a, size_t len, gsl_vector_int *fvec, size_t size,
         size_t offset, size_t stride) {

    if (fvec == NULL || fvec->block == NULL) {
  return GSL_EFAULT;
    }
    if (offset + 1 + (size-1)*stride > len) {
  return GSL_EINVAL;
    }
    fvec->block->size = len;
    fvec->block->data = a;
    fvec->size = size;
    fvec->stride = stride;
    fvec->data = a+offset;
    fvec->owner = 0;
    return GSL_SUCCESS;
}
int fgsl_aux_vector_uint_align(int *a, size_t len, gsl_vector_uint *fvec, size_t size,
         size_t offset, size_t stride) {

    if (fvec == NULL || fvec->block == NULL) {
  return GSL_EFAULT;
    }
    if (offset + 1 + (size-1)*stride > len) {
  return GSL_EINVAL;
    }
    fvec->block->size = len;
    fvec->block->data = a;
    fvec->size = size;
    fvec->stride = stride;
    fvec->data = a+offset;
    fvec->owner = 0;
    return GSL_SUCCESS;
}


size_t fgsl_aux_vector_double_size(gsl_vector *fvec) {
    return fvec->size;
}
size_t fgsl_aux_vector_int_size(gsl_vector_int *fvec) {
    return fvec->size;
}
size_t fgsl_aux_vector_uint_size(gsl_vector_uint *fvec) {
    return fvec->size;
}

size_t fgsl_aux_vector_double_stride(gsl_vector *fvec) {
    return fvec->stride;
}
size_t fgsl_aux_vector_int_stride(gsl_vector_int *fvec) {
    return fvec->stride;
}
size_t fgsl_aux_vector_uint_stride(gsl_vector_uint *fvec) {
    return fvec->stride;
}

gsl_vector_complex *fgsl_aux_vector_complex_init() {
    gsl_vector_complex *result;
    result = (gsl_vector_complex *) malloc(sizeof(gsl_vector_complex));
    result->block = (gsl_block_complex *) malloc(sizeof(gsl_block_complex));
    result->owner = 0;
    result->stride = 0;
    result->size = 0;
    return result;
}
void fgsl_aux_vector_complex_free(gsl_vector_complex *vec) {
    free(vec->block);
    free(vec);
}

int fgsl_aux_vector_complex_align(double *a, size_t len,
				  gsl_vector_complex *fvec, size_t size,
				  size_t offset, size_t stride) {

    if (fvec == NULL || fvec->block == NULL) {
	return GSL_EFAULT;
    }
    if (offset + 1 + (size-1)*stride > len) {
	return GSL_EINVAL;
    }
    fvec->block->size = len;
    fvec->block->data = a;
    fvec->size = size;
    fvec->stride = stride;
    fvec->data = a+2*offset;
    fvec->owner = 0;
    return GSL_SUCCESS;
}

size_t fgsl_aux_vector_complex_size(gsl_vector_complex *fvec) {
    return fvec->size;
}

size_t fgsl_aux_vector_complex_stride(gsl_vector_complex *fvec) {
    return fvec->stride;
}



gsl_matrix *fgsl_aux_matrix_double_init() {
    gsl_matrix *result;
    result = (gsl_matrix *) malloc(sizeof(gsl_matrix));
    result->block = (gsl_block *) malloc(sizeof(gsl_block));
    result->owner = 0;
    return result;
}

void fgsl_aux_matrix_double_free(gsl_matrix *mat) {
    free(mat->block);
    free(mat);
}

int fgsl_aux_matrix_double_align(double *a, size_t lda, size_t n, size_t m, gsl_matrix *fvec) {

    if (fvec == NULL || fvec->block == NULL) {
	return GSL_EFAULT;
    }
    if (n > lda) {
	return GSL_EINVAL;
    }
    fvec->block->size = lda*m;
    fvec->block->data = a;
    fvec->size1 = m;
    fvec->size2 = n;
    fvec->tda = lda;
    fvec->data = a;
    fvec->owner = 0;
    return GSL_SUCCESS;
}

void fgsl_aux_matrix_double_size(gsl_matrix *fvec, size_t *lda, size_t *m, size_t *n) {
    if (m != NULL)
    	*m = fvec->size2;
    if (n != NULL)
    	*n = fvec->size1;
    if (lda != NULL)
    	*lda = fvec->tda;
}

gsl_matrix_complex *fgsl_aux_matrix_complex_init() {
    gsl_matrix_complex *result;
    result = (gsl_matrix_complex *) malloc(sizeof(gsl_matrix_complex));
    result->block = (gsl_block_complex *)
	malloc(sizeof(gsl_block_complex));
    result->owner = 0;
    return result;
}

void fgsl_aux_matrix_complex_free(gsl_matrix_complex *mat) {
    free(mat->block);
    free(mat);
}

int fgsl_aux_matrix_complex_align(double *a, size_t lda, size_t n,
				  size_t m, gsl_matrix_complex *fvec) {

    if (fvec == NULL || fvec->block == NULL) {
	return GSL_EFAULT;
    }
    if (n > lda) {
	return GSL_EINVAL;
    }
    fvec->block->size = lda*m;
    fvec->block->data = a;
    fvec->size1 = m;
    fvec->size2 = n;
    fvec->tda = lda;
    fvec->data = a;
    fvec->owner = 0;
    return GSL_SUCCESS;
}

void fgsl_aux_matrix_complex_size(gsl_matrix_complex *fvec,
				  size_t *lda, size_t *m, size_t *n) {
    if (m != NULL)
    	*m = fvec->size2;
    if (n != NULL)
    	*n = fvec->size1;
    if (lda != NULL)
    	*lda = fvec->tda;
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

