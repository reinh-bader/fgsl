program randpoisson
  use fgsl
  implicit none
  integer(fgsl_int) :: i, k
  real(fgsl_double) :: mu
  type(fgsl_rng) :: r
  type(fgsl_rng_type) :: t
!
  mu = 3.0D0
  t = fgsl_rng_env_setup()
  t = fgsl_rng_default
  r = fgsl_rng_alloc (t)
! print n random variates chosen from 
! the poisson distribution with mean 
! parameter mu 
  do i=1, 10
     k = fgsl_ran_poisson (r, mu)
     write (6, advance='no', fmt='(I2)') k
  end do
  write(*, '('''')')
  call fgsl_rng_free(r)
end program randpoisson
