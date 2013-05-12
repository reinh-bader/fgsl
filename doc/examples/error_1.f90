program error_1
  use fgsl
  implicit none
  real(fgsl_double) :: x, y
  x = -2.0d0
  y = fgsl_sf_gamma(x)
! GSL default error handler will abort, hence the next line is never reached.
  write(*, *) x, y
end program error_1
