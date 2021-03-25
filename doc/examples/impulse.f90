program impulse
  use fgsl
  implicit none

  ! length of time series and window size
  integer(fgsl_size_t), parameter :: n = 1000, k = 25
  ! tuning parameter for impulse detection
  real(fgsl_double), parameter :: t = 4.0d0
  ! vectors: input, filtered output, window medians and scale estimates
  type(fgsl_vector) :: x, y, xmedian, xsigma
  real(fgsl_double) :: v_x(n), v_y(n), v_xmedian(n), v_xsigma(n)
  ! outlier register
  type(fgsl_vector_int) :: ioutlier
  integer(fgsl_int) :: v_ioutlier(n)
  

  type(fgsl_filter_impulse_workspace) :: w
  type(fgsl_rng) :: r
  real(fgsl_double) :: xi, ei, u, outlier

  integer(fgsl_size_t) :: noutlier, i
  integer(fgsl_int) :: status

  x = fgsl_vector_init(v_x)
  y = fgsl_vector_init(v_y)
  xmedian = fgsl_vector_init(v_xmedian)
  xsigma = fgsl_vector_init(v_xsigma)
  ioutlier = fgsl_vector_init(v_ioutlier)

  w = fgsl_filter_impulse_alloc(k)
  r = fgsl_rng_alloc(fgsl_rng_default)

  ! generate input signal
  setup : do i = 1, n
     xi = 1.d1 * sin(2.d0 * m_pi * (i-1) / real(n, fgsl_double))
     ei = fgsl_ran_gaussian(r, 2.0d0)
     u = fgsl_rng_uniform(r)
     if (u < 0.01) then
        outlier = sign(15.0d0, ei)
     else
        outlier = 0.d0
     end if
     v_x(i) = xi + ei + outlier
  end do setup

  ! apply impulse detection filter */
  status = fgsl_filter_impulse(FGSL_FILTER_END_TRUNCATE, &
       FGSL_FILTER_SCALE_QN, t, x, y, xmedian, xsigma, &
       noutlier, ioutlier, w)

  print : do i = 1, n
     write(*, fmt='(i6,4(1x,f11.6),1x,i6)') i, v_x(i), v_y(i), &
          v_xmedian(i) + t * v_xsigma(i), &
          v_xmedian(i) - t * v_xsigma(i), &
          v_ioutlier(i)
  end do print
  
  call fgsl_vector_free(x)
  call fgsl_vector_free(y)
  call fgsl_vector_free(xmedian)
  call fgsl_vector_free(xsigma)
  call fgsl_vector_free(ioutlier)
  call fgsl_rng_free(r)
  call fgsl_filter_impulse_free(w)
end program impulse
