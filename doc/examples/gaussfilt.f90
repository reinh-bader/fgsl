program gaussfilt
  use fgsl
  implicit none

  ! length of time series and window size
  integer(fgsl_size_t), parameter :: n = 500, k = 51
  ! alpha values
  real(fgsl_double), parameter :: alpha(3) = [ .5d0, 3.d0, 1.d1 ]
  ! vectors: input, filtered output for all alpha,
  ! gaussian kernels for all alpha
  type(fgsl_vector) :: x, y1, y2, y3, k1, k2, k3
  real(fgsl_double), target :: v_x(n), v_y1(n), v_y2(n), v_y3(n), &
       v_k1(k), v_k2(k), v_k3(k)

  type(fgsl_rng) :: r
  type(fgsl_filter_gaussian_workspace) :: gauss_p
  integer(fgsl_size_t) :: i
  integer(fgsl_int) :: status
  real(fgsl_double) :: sum, ui

  x = fgsl_vector_init(v_x)
  y1 = fgsl_vector_init(v_y1)
  y2 = fgsl_vector_init(v_y2)
  y3 = fgsl_vector_init(v_y3)
  k1 = fgsl_vector_init(v_k1)
  k2 = fgsl_vector_init(v_k2)
  k3 = fgsl_vector_init(v_k3)

  r = fgsl_rng_alloc(fgsl_rng_default)
  gauss_p = fgsl_filter_gaussian_alloc(k)
  sum = 0.0d0

  ! generate input signal
  setup : do i = 1, n
     ui = fgsl_ran_gaussian(r, 1.0d0)
     sum = sum + ui
     v_x(i) = sum
  end do setup

  ! compute kernels without normalization
  status = fgsl_filter_gaussian_kernel(alpha(1), 0_fgsl_size_t, 0, k1);
  status = fgsl_filter_gaussian_kernel(alpha(2), 0_fgsl_size_t, 0, k2);
  status = fgsl_filter_gaussian_kernel(alpha(3), 0_fgsl_size_t, 0, k3);
  
  ! apply filters
  status = fgsl_filter_gaussian(FGSL_FILTER_END_PADVALUE, alpha(1), &
       0_fgsl_size_t, x, y1, gauss_p)
  status = fgsl_filter_gaussian(FGSL_FILTER_END_PADVALUE, alpha(2), &
       0_fgsl_size_t, x, y2, gauss_p)
  status = fgsl_filter_gaussian(FGSL_FILTER_END_PADVALUE, alpha(3), &
       0_fgsl_size_t, x, y3, gauss_p)

  write(*,*) 'kernels:'
  print_kernels : do i = 1, k
     write(*, fmt='(i0,1p,3(1x,e13.6))') i, v_k1(i), v_k2(i), v_k3(i)
  end do print_kernels
  write(*,*) 'filter results:'
  print_res : do i = 1, N
     write(*, fmt='(4(1x,1p,e19.12))') v_x(i), v_y1(i), v_y2(i), v_y3(i)
  end do print_res
  
  call fgsl_vector_free(x)
  call fgsl_vector_free(y1)
  call fgsl_vector_free(y2)
  call fgsl_vector_free(y3)
  call fgsl_vector_free(k1)
  call fgsl_vector_free(k2)
  call fgsl_vector_free(k3)
  call fgsl_rng_free(r)
  call fgsl_filter_gaussian_free(gauss_p)
end program gaussfilt
