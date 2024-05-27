program rngunif
  use fgsl
  implicit none
  type(fgsl_rng) :: r
  type(fgsl_rng_type) :: t
  integer(fgsl_int) :: i
  real(fgsl_double) :: u
!
  t = fgsl_rng_env_setup()
  t = fgsl_rng_default
  r = fgsl_rng_alloc (t)
  do i=1, 10
     u = fgsl_rng_uniform (r)
     write(*, '(F7.5)') u
  end do
  call fgsl_rng_free(r)
end program rngunif
