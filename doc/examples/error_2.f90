program error_2
  use fgsl
  implicit none
  real(fgsl_double) :: x, y
  type(fgsl_error_handler_t) :: default_errh
  default_errh = fgsl_set_error_handler_off()
! switching the error handler off will allow to proceed,
! but at the cost of propagating NaN values
  x = -2.0d0
  y = fgsl_sf_gamma(x)
  write(*, *) x, y
end program error_2
