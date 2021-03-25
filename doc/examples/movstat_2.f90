program movstat_2
  use fgsl
  implicit none

  ! length of time series and window size
  integer(fgsl_size_t), parameter :: n = 1000, k = 41
  ! variances
  real(fgsl_double), parameter :: sigma(5) = [ 1.d0, 5.d0, 1.d0, 3.d0, 5.d0 ]
  ! samples where variance changes
  integer(fgsl_size_t), parameter :: n_sigma(5) = &
       [ 200, 450, 600, 850, 1000 ]

  type(fgsl_vector) :: x, xmedian, xmad, xiqr, xsn, xqn, xsd
  real(fgsl_double), target :: v_x(n), v_xmedian(n), v_xmad(n), v_xiqr(n), &
       v_xsn(n), v_xqn(n), v_xsd(n)
  type(fgsl_rng) :: r
  type(fgsl_movstat_workspace) :: w
  integer(fgsl_size_t) :: idx, i
  integer(fgsl_int) :: status
  real(fgsl_double) :: gi, u, outlier, xi

  x = fgsl_vector_init(v_x)
  xmedian = fgsl_vector_init(v_xmedian)
  xmad = fgsl_vector_init(v_xmad)
  xiqr = fgsl_vector_init(v_xiqr)
  xsn = fgsl_vector_init(v_xsn)
  xqn = fgsl_vector_init(v_xqn)
  xsd = fgsl_vector_init(v_xsd)
  r = fgsl_rng_alloc(fgsl_rng_default)
  w = fgsl_movstat_alloc(k)

  idx = 1
  setup : do i = 1, n
     gi = fgsl_ran_gaussian(r, sigma(idx))
     u = fgsl_rng_uniform(r)
     if (u < 0.01) then
        outlier = sign(15.0d0, gi)
     else
        outlier = 0.0d0
     end if
     xi = gi + outlier

     v_x(i) = xi

     if ( i == n_sigma(idx) ) idx = idx + 1
  end do setup
  
  ! compute moving statistics

  status = fgsl_movstat_mad(FGSL_MOVSTAT_END_TRUNCATE, x, xmedian, xmad, w)
  status = fgsl_movstat_qqr(FGSL_MOVSTAT_END_TRUNCATE, x, 0.25d0, xiqr, w)
  status = fgsl_movstat_Sn(FGSL_MOVSTAT_END_TRUNCATE, x, xsn, w)
  status = fgsl_movstat_Qn(FGSL_MOVSTAT_END_TRUNCATE, x, xqn, w);
  status = fgsl_movstat_sd(FGSL_MOVSTAT_END_TRUNCATE, x, xsd, w);

  ! scale IQR by factor to approximate standard deviation
  v_xiqr = v_xiqr * 0.7413d0

  idx = 1
  print : do i = 1, n
     write(*, fmt='(i0,7(1x,f11.6))') i, v_x(i), sigma(idx), v_xmad(i), &
          v_xiqr(i), v_xsn(i), v_xqn(i), v_xsd(i)
     if ( i == n_sigma(idx) ) idx = idx + 1
  end do print

  call fgsl_vector_free(x)
  call fgsl_vector_free(xmedian)
  call fgsl_vector_free(xmad)
  call fgsl_vector_free(xiqr)
  call fgsl_vector_free(xSn)
  call fgsl_vector_free(xQn)
  call fgsl_vector_free(xsd)
  call fgsl_rng_free(r)
  call fgsl_movstat_free(w)

end program movstat_2
