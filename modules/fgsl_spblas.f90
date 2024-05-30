module fgsl_spblas
  !> Sparse BLAS support
  use fgsl_base
  use fgsl_array
  use fgsl_sparse_matrix
  use fgsl_linalg
  
  implicit none
  
  private :: gsl_spblas_dgemv, gsl_spblas_dgemm
  
  !
  !> C interfaces
  interface
    function gsl_spblas_dgemv(transa, alpha, a, x, beta, y) bind(c)
      import :: c_int, c_double, c_ptr
      integer(c_int), value :: transa
      real(c_double), value :: alpha, beta
      type(c_ptr), value :: a, x, y
      integer(c_int) :: gsl_spblas_dgemv
    end function
    function gsl_spblas_dgemm(alpha, a, b, c) bind(c)
      import :: c_int, c_double, c_ptr
      real(c_double), value :: alpha
      type(c_ptr), value :: a, b, c
      integer(c_int) :: gsl_spblas_dgemm
    end function
  end interface
contains
  function fgsl_spblas_dgemv(transa, alpha, a, x, beta, y) 
    integer(fgsl_int), intent(in) :: transa !> Use enum values cblas*trans 
                                            !> from fgsl_linalg
    real(fgsl_double), intent(in) :: alpha, beta
    type(fgsl_spmatrix), intent(in) :: a
    type(fgsl_vector), intent(in) :: x
    type(fgsl_vector), intent(inout) :: y
    integer(fgsl_int) :: fgsl_spblas_dgemv
    
    fgsl_spblas_dgemv = gsl_spblas_dgemv(transa, alpha, a%gsl_spmatrix, &
      x%gsl_vector, beta, y%gsl_vector)
  end function
  function fgsl_spblas_dgemm(alpha, a, b, c)
    real(fgsl_double), intent(in) :: alpha
    type(fgsl_spmatrix), intent(in) :: a, b
    type(fgsl_spmatrix), intent(inout) :: c
    integer(fgsl_int) :: fgsl_spblas_dgemm
    
    fgsl_spblas_dgemm = gsl_spblas_dgemm(alpha, a%gsl_spmatrix, b%gsl_spmatrix, &
      c%gsl_spmatrix)
  end function
end module
