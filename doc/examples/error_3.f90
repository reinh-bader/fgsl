program error_3
  use fgsl
  use, intrinsic :: ieee_exceptions
  implicit none
  real(fgsl_double) :: x, y
  integer :: i
  type(fgsl_error_handler_t) :: default_errh
  logical :: have_invalid
  default_errh = fgsl_set_error_handler_off()
  x = -2.2d0
  do i=1,3
     x = x + 0.1d0
!    If your Fortran processor supports the IEEE stuff, 
!    you can do your own processing of the error condition
!    You need to know whether an FP error, and what kind
!    of FP error can happen.
     call ieee_set_flag(ieee_invalid, .false.)
     y = fgsl_sf_gamma(x)
     call ieee_get_flag(ieee_invalid, have_invalid)
     if (have_invalid) then
        write(*, *) x, 'Not a number'
     else
        write(*, *) x, y
     end if
  end do
end program error_3
