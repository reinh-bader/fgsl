program randwalk
  use fgsl
  implicit none
  integer(fgsl_int) :: i
  real(fgsl_double) :: x, y, dx, dy
  type(fgsl_rng) :: r
  type(fgsl_rng_type) :: t
!
  x = 0.D0
  y = 0.D0
  t = fgsl_rng_env_setup()
  t = fgsl_rng_default
  r = fgsl_rng_alloc (t)
  write(*, '(''Starting with '',2(F10.5,1X))') x, y
  do i=1, 10
     call fgsl_ran_dir_2d(r, dx, dy)
     x = x + dx; y = y + dy
     write(*, '(4X,2(F10.5,1X))') x, y
  end do
  call fgsl_rng_free(r)
end program randwalk
