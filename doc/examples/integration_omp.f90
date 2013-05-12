module mod_integration_omp
  use fgsl
  use, intrinsic :: iso_c_binding
  implicit none
contains
  function f(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: f
!
    real(c_double), pointer :: alpha
    call c_f_pointer(params, alpha)
    f = log(alpha * x) / sqrt(x)
  end function f
end module
program integration_omp
  use mod_integration_omp
  implicit none
  integer(fgsl_size_t), parameter :: nmax=1000
  integer, parameter :: imax=40
  real(fgsl_double), target :: alpha
  real(fgsl_double) :: result, error
  integer(fgsl_int) :: status, i
  type(c_ptr) :: ptr
  type(fgsl_function) :: f_obj
  type(fgsl_integration_workspace) :: wk

!$omp parallel private(ptr, f_obj, wk, alpha)
  ptr = c_loc(alpha)
  f_obj = fgsl_function_init(f, ptr)
  wk = fgsl_integration_workspace_alloc(nmax)
!$omp do private(result, error, status) ordered
  do i=1, imax
     alpha = 1.5d0 - dble(i-1)/dble(imax-1)
     status = fgsl_integration_qags(f_obj, 0.0_fgsl_double, 1.0_fgsl_double, &
          0.0_fgsl_double, 1.0e-7_fgsl_double, nmax, wk, result, error)
!$omp ordered
     write(6, fmt='(''Integration result        : '',F20.16)') result
     write(6, fmt='(''Integration error estimate: '',F20.16)') error
     write(6, fmt='(''Exact result              : '',F20.16)') &
          2*log(alpha)-4.0D0
!$omp end ordered
  end do
!$omp end do
  call fgsl_function_free(f_obj)
  call fgsl_integration_workspace_free(wk)
!$omp end parallel
end program integration_omp
