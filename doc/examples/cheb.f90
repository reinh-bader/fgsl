module mod_cheb
  use fgsl
  use, intrinsic :: iso_c_binding
  implicit none
contains
  function f(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: f
!
    if (x < 0.5_c_double) then
       f = 0.25_c_double
    else
       f = 0.75_c_double
    end if
  end function f
end module mod_cheb
program cheb
  use mod_cheb
  implicit none
  integer, parameter :: n = 10000
  integer(fgsl_int) :: i, status
  real(fgsl_double) :: x, r10, r40
  type(c_ptr) :: ptr
  type(fgsl_cheb_series) :: cs
  type(fgsl_function) :: fun
!
  cs = fgsl_cheb_alloc(40)
  fun = fgsl_function_init(f, ptr)
  status = fgsl_cheb_init(cs, fun, 0.0_fgsl_double, 1.0_fgsl_double)
  do i=1,n
     x = dble(i-1)/dble(n)
     r10 = fgsl_cheb_eval_n(cs, 10_fgsl_size_t, x)
     r40 = fgsl_cheb_eval(cs, x)
     write(6, fmt='(2(F7.4,1X),2(1PE13.6,1X))') x, f(x,ptr), r10, r40
  end do
  call fgsl_cheb_free(cs)
  call fgsl_function_free(fun)
end program cheb
