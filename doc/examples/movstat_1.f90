program movstat_1
  use fgsl
  implicit none

! length of time series and window size
  integer(fgsl_size_t), parameter :: n = 500, k = 11

  type(fgsl_movstat_workspace) :: w
  type(fgsl_vector) :: x, xmean, xmin, xmax
  real(fgsl_double), target :: vx(n), vxmean(n), vxmin(n), vxmax(n)
  real(fgsl_double) :: xi, ei
  type(fgsl_rng) :: r
  integer(fgsl_size_t) :: i
  integer(fgsl_int) :: status

  w = fgsl_movstat_alloc(k)
  r = fgsl_rng_alloc(fgsl_rng_default)

  do i = 1, n
     xi = cos( 2.0_fgsl_double * m_pi * real(i-1,fgsl_double) / real(n,fgsl_double) )
     ei = fgsl_ran_gaussian(r, 0.1_fgsl_double)
     vx(i) = xi + ei
  end do
  
  x = fgsl_vector_init(vx)
  xmean = fgsl_vector_init(vxmean)
  xmin = fgsl_vector_init(vxmin)
  xmax = fgsl_vector_init(vxmax)

! compute moving statistics
  status = fgsl_movstat_mean(FGSL_MOVSTAT_END_PADVALUE, x, xmean, w)
  status = fgsl_movstat_minmax(FGSL_MOVSTAT_END_PADVALUE, x, xmin, xmax, w)

! print results

  do i = 1, n
     write(*, fmt='(i0,4(1x,f11.6))') i, vx(i), vxmean(i), vxmin(i), vxmax(i)
  end do

  call fgsl_vector_free(x)
  call fgsl_vector_free(xmean)
  call fgsl_vector_free(xmin)
  call fgsl_vector_free(xmax)
  call fgsl_rng_free(r)
  call fgsl_movstat_free(w)
  
end program movstat_1
