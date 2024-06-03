module mod_diff
  use fgsl
  use, intrinsic :: iso_c_binding
  implicit none
contains
  function f(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: f
!
    if (c_associated(params)) continue
    f = x**1.5_c_double
  end function f
end module mod_diff
program diff
  use mod_diff
  implicit none
  real(fgsl_double) :: result, abserr
  integer(fgsl_int) :: status
  type(fgsl_function) :: pwr
!
  pwr = fgsl_function_init(f, c_null_ptr)
  write(*, *) 'f(x) = x^(3/2)'
  status = fgsl_deriv_central (pwr, 2.0_fgsl_double, 1.E-8_fgsl_double, &
       result, abserr)
  write(*, *) 'x = 2.0'
  write(*, '(''df/dx = '',F17.10,'' +/- '',F17.10)') result, abserr
  write(*, '(''exact = '',F17.10)') 1.5D0*sqrt(2.0D0)
 
  status = fgsl_deriv_forward (pwr, 0.0_fgsl_double, 1.E-8_fgsl_double, &
       result, abserr)
  write(*, *) 'x = 0.0'
  write(*, '(''df/dx = '',F17.10,'' +/- '',F17.10)') result, abserr
  write(*, '(''exact = '',F17.10)') 0.0D0
  
end program diff
