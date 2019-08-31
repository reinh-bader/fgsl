module mod_integration_fixed
  use fgsl
  use, intrinsic :: iso_c_binding
  implicit none
contains
  function f(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: f
!
    integer(c_int), pointer :: m
    call c_f_pointer(params, m)
    f = x**m + 1.0_fgsl_double
  end function f
end module mod_integration_fixed
program integration_fixed
  use mod_integration_fixed
  implicit none
  
  type(fgsl_integration_fixed_workspace) :: wk
  type(fgsl_function) :: f_obj
  real(fgsl_double) :: expected, result
  integer(fgsl_int) :: n, m, status
  target :: m

  n = 6; m = 10
  f_obj = fgsl_function_init(f, c_loc(m))
  wk = fgsl_integration_fixed_alloc(fgsl_integration_fixed_hermite, int(n,fgsl_size_t), &
       0.0_fgsl_double, 1.0_fgsl_double, 0.0_fgsl_double, 0.0_fgsl_double)

  if (mod(m,2) == 0) then
     expected = M_SQRTPI + fgsl_sf_gamma(0.5_fgsl_double*(1.0 + m))
  else
     expected = M_SQRTPI
  end if

  status = fgsl_integration_fixed(f_obj, result, wk)
  
  write(6, fmt='(''m                         : '',I0)') m
  write(6, fmt='(''Intervals                 : '',I0)') n
  write(6, fmt='(''Integration result        : '',F20.16)') result
  write(6, fmt='(''Expected result           : '',F20.16)') expected
  write(6, fmt='(''Actual error              : '',F20.16)') result - expected

  call fgsl_function_free(f_obj)
  call fgsl_integration_fixed_free(wk)
end program integration_fixed
 
