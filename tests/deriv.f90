module mod_deriv
  use, intrinsic :: iso_c_binding
  use fgsl
  use mod_unit
  implicit none
contains
  function power(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: power
!
    real(fgsl_double), pointer :: p
    call c_f_pointer(params, p)
    power = x ** p
  end function power
end module mod_deriv
program deriv
  use mod_deriv
  implicit none
  real(fgsl_double), parameter :: eps5 = 1.0d-5
  real(fgsl_double), target :: fpar(3)
  real(fgsl_double) :: abserr, result
  type(c_ptr) :: ptr
  type(fgsl_function) :: stdfunc
  integer(fgsl_int) :: status
!
! Test numerical derivative routines
!
  call unit_init(20)
!
  fpar = (/1.5_fgsl_double, 0.0_fgsl_double, 0.0_fgsl_double/)
  ptr = c_loc(fpar)
  stdfunc = fgsl_function_init(power, ptr)
  status = fgsl_deriv_central(stdfunc, 2.0_fgsl_double, eps5, result, abserr)
  call unit_assert_equal('fgsl_deriv_central:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_deriv_central',1.5d0*sqrt(2.0d0), &
       result,abserr)
  status = fgsl_deriv_forward(stdfunc, 2.0_fgsl_double, eps5, result, abserr)
  call unit_assert_equal('fgsl_deriv_within:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_deriv_within',1.5d0*sqrt(2.0d0), &
       result,abserr)
  status = fgsl_deriv_backward(stdfunc, 2.0_fgsl_double, eps5, result, abserr)
  call unit_assert_equal('fgsl_deriv_backward:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_deriv_within',1.5d0*sqrt(2.0d0), &
       result,abserr)
  call fgsl_function_free(stdfunc)
!
! Done
!
  call unit_finalize() 
end program deriv
