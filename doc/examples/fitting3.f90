program fitting3
! generate fitting data for fitting2
  use fgsl
  implicit none
  integer(fgsl_int) :: i, n
  real(fgsl_double) :: x, y, dy, sigma
  type(fgsl_rng_type) :: t 
  type(fgsl_rng) :: r
  integer :: iu

  t = fgsl_rng_env_setup()
  t = fgsl_rng_default
  r = fgsl_rng_alloc(t)

  n = 20
  open(newunit=iu, file='fitting2.dat', form='formatted', status='unknown')
  write(iu, '(I8)') n
  do i=1, n
     x = 0.1d0 + dble(i-1)/dble(n) * 2.0d0
     y = exp(x)
     sigma = 0.1d0 * y
     dy = fgsl_ran_gaussian(r, sigma)
     write(iu, '(3(F10.5,1X))') x, y+dy, sigma
  end do
  close(iu)
end program fitting3
