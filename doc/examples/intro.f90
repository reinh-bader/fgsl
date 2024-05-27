program intro
  use fgsl
  implicit none
  real(fgsl_double) :: x, y
!
  x = 5.0_fgsl_double
  y = fgsl_sf_bessel_jc0(x)
  write(*,fmt='(''J0(5) = '',1PE25.18E2)') y
end program intro
