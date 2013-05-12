program histogram2d
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: nx = 10, ny = 10
  integer(fgsl_int) :: i, status
  real(fgsl_double) :: x, y, u, v
  type(fgsl_rng_type) :: t
  type(fgsl_rng) :: r
  type(fgsl_histogram2d) :: h
  type(fgsl_histogram2d_pdf) :: p
!
  h = fgsl_histogram2d_alloc(nx, ny)
  status = fgsl_histogram2d_set_ranges_uniform(h, &
       0.0_fgsl_double, 1.0_fgsl_double, &
       0.0_fgsl_double, 1.0_fgsl_double)
  status = fgsl_histogram2d_accumulate (h, 0.3_fgsl_double, &
       0.3_fgsl_double, 1.0_fgsl_double)
  status = fgsl_histogram2d_accumulate (h, 0.8_fgsl_double, &
       0.1_fgsl_double, 5.0_fgsl_double)
  status = fgsl_histogram2d_accumulate (h, 0.7_fgsl_double, &
       0.9_fgsl_double, 0.5_fgsl_double)
  t = fgsl_rng_env_setup()
  t = fgsl_rng_default
  r = fgsl_rng_alloc(t)
  p = fgsl_histogram2d_pdf_alloc (nx, ny)
  status = fgsl_histogram2d_pdf_init(p, h)
  do i=1,1000
     u = fgsl_rng_uniform(r)
     v = fgsl_rng_uniform(r)
     status = fgsl_histogram2d_pdf_sample (p, u, v, x, y)
     if (status /= FGSL_SUCCESS) exit
     write(6, '(2(F15.8,2X))') x, y
  end do
  call fgsl_histogram2d_free(h)
  call fgsl_histogram2d_pdf_free(p)
  call fgsl_rng_free(r)
end program histogram2d
