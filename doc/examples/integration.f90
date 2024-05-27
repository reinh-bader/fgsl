module mod_integration
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
program integration
  use mod_integration
  implicit none
  integer(fgsl_size_t), parameter :: nmax=1000
  real(fgsl_double), target :: alpha
  real(fgsl_double) :: result, error
  integer(fgsl_int) :: status
  type(c_ptr) :: ptr
  type(fgsl_function) :: f_obj
  type(fgsl_integration_workspace) :: wk
  alpha = 1.0D0
  ptr = c_loc(alpha)
  f_obj = fgsl_function_init(f, ptr)
  wk = fgsl_integration_workspace_alloc(nmax)
  status = fgsl_integration_qags(f_obj, 0.0_fgsl_double, 1.0_fgsl_double, &
       0.0_fgsl_double, 1.0e-7_fgsl_double, nmax, wk, result, error)
  write(*, fmt='(''Integration result        : '',F20.16)') result
  write(*, fmt='(''Integration error estimate: '',F20.16)') error
  write(*, fmt='(''Exact result              : '',F20.16)') -4.0D0
  call fgsl_function_free(f_obj)
  call fgsl_integration_workspace_free(wk)
end program integration
