module mod_err
  use fgsl
  implicit none
  integer, protected, save :: error_state = fgsl_success
contains
! Here we define our own error handler. It is not thread-safe
! since global state is being maintained.
  subroutine err_sub (reason, file, line, errno) bind(c)
    use, intrinsic :: iso_c_binding
    type(c_ptr), value :: reason, file
    integer(c_int), value :: line, errno
    error_state = errno
  end subroutine err_sub
  subroutine err_reset()
    error_state = fgsl_success
  end subroutine err_reset
end module mod_err
program error_4
  use mod_err
  implicit none
  real(fgsl_double) :: x, y
  integer :: i
  type(fgsl_error_handler_t) :: default_errh, my_errh
  my_errh = fgsl_error_handler_init(err_sub)
  default_errh = fgsl_set_error_handler(my_errh)
! own handler initialized and installed. This is mostly 
! needed for routines which do not return an error status.
  x = -2.2d0
  do i=1,3
     x = x + 0.1d0
     y = fgsl_sf_gamma(x)
     if (error_state /= fgsl_success) then
        write(*, *) x, 'Error in result:', y
        call err_reset()
     else
        write(*, *) x, y
     end if
  end do
end program error_4
