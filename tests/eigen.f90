program eigen
  use fgsl
  use mod_unit
  implicit none
  real(fgsl_double), parameter :: eps10 = 1.0d-10
  type(fgsl_matrix) :: a, evec
  type(fgsl_vector) :: eval
  real(fgsl_double) :: af(3, 3), evalf(3), evecf(3, 3)
  type(fgsl_eigen_symm_workspace) :: wks
  type(fgsl_eigen_symmv_workspace) :: wksv
  integer(fgsl_int) :: status
!
! Test eigenvectors / eigenvalues
! remember that matrices are transposed vs. usual Fortran convention
!
  call unit_init(200)
!
  a = fgsl_matrix_init(af)
  evec = fgsl_matrix_init(evecf)
  eval = fgsl_vector_init(evalf)
  wks = fgsl_eigen_symm_alloc(3_fgsl_size_t)
  wksv = fgsl_eigen_symmv_alloc(3_fgsl_size_t)
  af = reshape((/ 1.0d0, -1.0d0, 0.0d0, -1.0d0, 2.0d0, 3.0d0, &
       0.0d0, 3.0d0, 1.0d0 /), (/3, 3/))
  status = fgsl_eigen_symm(a, eval, wks)
  call unit_assert_equal_within('fgsl_eigen_symm:eval',&
       (/(3.0d0+sqrt(41.0d0))/2.0d0, 1.0d0, (3.0d0-sqrt(41.0d0))/2.0d0 /), &
       evalf,eps10)
  af = reshape((/ 1.0d0, -1.0d0, 0.0d0, -1.0d0, 2.0d0, 3.0d0, &
       0.0d0, 3.0d0, 1.0d0 /), (/3, 3/))
  status = fgsl_eigen_symmv(a, eval, evec, wksv)
  call unit_assert_equal_within('fgsl_eigen_symmv:eval',&
       (/(3.0d0+sqrt(41.0d0))/2.0d0, 1.0d0, (3.0d0-sqrt(41.0d0))/2.0d0 /), &
       evalf,eps10)
! FIXME: remaining routines

! 
  call fgsl_matrix_free(a)
  call fgsl_vector_free(eval)
  call fgsl_matrix_free(evec)
  call fgsl_eigen_symm_free(wks)
  call fgsl_eigen_symmv_free(wksv)
   
! 
! Done
!
  call unit_finalize()
end program eigen
