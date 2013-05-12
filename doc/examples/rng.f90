program rng
  use fgsl
  implicit none
  type(fgsl_rng) :: r
  type(fgsl_rng_type) :: t
!
  t = fgsl_rng_env_setup()
  t = fgsl_rng_default
  r = fgsl_rng_alloc (t)
  write(6, '(''generator type: '',A)') fgsl_rng_name(r)
  write(6, '(''seed = '',I0)') fgsl_rng_default_seed
  write(6, '(''first value = '',I0)') fgsl_rng_get(r)
  call fgsl_rng_free(r)
end program rng
