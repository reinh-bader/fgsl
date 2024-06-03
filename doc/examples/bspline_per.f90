program bspline_per
  !> Transcription of seventh bspline example to Fortran
  !> Least squares and periodic splines
  use fgsl_bspline
  use fgsl_rngen
  use fgsl_cdf
  use, intrinsic :: iso_fortran_env, only : error_unit
  implicit none
  
  integer(fgsl_size_t), parameter :: n = 500           ! number of data points to fit
  integer(fgsl_size_t), parameter :: spline_order = 6  ! spline order
  integer(fgsl_size_t), parameter :: ncontrol = 15     ! number of control points
                                                       ! data interval [a,b] 
  real(fgsl_double), parameter :: a = 0._fgsl_double
  real(fgsl_double), parameter :: b = 2._fgsl_double * m_pi
  real(fgsl_double), parameter :: sigma = 0.2_fgsl_double ! noise
  
  type(fgsl_bspline_workspace) :: w, wper
  
  real(fgsl_double), allocatable, target :: cf(:), cperf(:), xf(:), yf(:), wtsf(:)
  type(fgsl_vector) :: c, cper, x, y, wts

  type(fgsl_rng) :: r
  type(fgsl_rng_type) :: t
  
  integer(fgsl_int) :: i, iu, status
  real(fgsl_double) :: chisq, dyi, result, result_per, result0, result1, &
                       xi, yi

  ! set up all objects
  w = fgsl_bspline_alloc_ncontrol(spline_order, ncontrol)
  wper = fgsl_bspline_alloc_ncontrol(spline_order, ncontrol)
  
  t = fgsl_rng_env_setup() ! t not otherwise used
  r = fgsl_rng_alloc(fgsl_rng_default)
  
  allocate(cf(ncontrol), cperf(ncontrol), xf(n), yf(n), wtsf(n))
  c = fgsl_vector_init(cf)        ! non-periodic coefficients
  cper = fgsl_vector_init(cperf)  ! periodic coefficients
  x = fgsl_vector_init(xf)
  y = fgsl_vector_init(yf)
  wts = fgsl_vector_init(wtsf)
  
  ! prepare I/O
  open(newunit=iu, file='bspline_per_data.txt', form='FORMATTED', &
	   status='REPLACE', action='WRITE')  
  
  ! create the data to be fitted
  do i = 1, int(n, fgsl_int)
    xi = (b - a) / (n - 1.0_fgsl_double) * (real(i-1, fgsl_double)) + a
    yi = sin(xi) - cos(2.0_fgsl_double * xi)
    
    dyi = fgsl_ran_gaussian(r, sigma)
    yi = yi + dyi 
    
    xf(i) = xi; yf(i) = yi; wtsf(i) = 1.0_fgsl_double/sigma**2
    
    write(iu, fmt='(2(F0.6,1X))') xi, yi
  end do 
  write(iu, fmt='(/)')

  
  ! use uniform non-periodic knots on [a, b] 
  status = fgsl_bspline_init_uniform(a, b, w)

  ! solve least squares problem for non-periodic spline 
  status = fgsl_bspline_wlssolve(x, y, wts, c, chisq, w)

  ! use periodic knots on [a, b] 
  status = fgsl_bspline_init_periodic(a, b, wper);

  ! solve least squares problem for periodic spline 
  status = fgsl_bspline_pwlssolve(x, y, wts, cper, chisq, wper)
  
  ! write spline curves to external file
  xi = a
  do 
    if (xi >= b) exit
    
    status = fgsl_bspline_calc(xi, c, result, w)
    status = fgsl_bspline_calc(xi, cper, result_per, wper)

    write(iu, fmt='(3(F0.6,1X))') xi, result, result_per 
    xi = xi + .01_fgsl_double
  end do
  
  write(error_unit, fmt=*) "=== Non-periodic spline endpoint derivatives ==="
  do i = 1, spline_order
    status = fgsl_bspline_calc_deriv(a, c, int(i-1, fgsl_size_t), result0, w)
    status = fgsl_bspline_calc_deriv(b, c, int(i-1, fgsl_size_t), result1, w)
    write(error_unit, fmt='("deriv ", I0, ": [",ES14.6,",",ES14.6,"]")') &
                    i, result0, result1 
  end do   
  write(error_unit, fmt=*) "=== Periodic spline endpoint derivatives ==="
  do i = 1, spline_order
    status = fgsl_bspline_calc_deriv(a, cper, int(i-1, fgsl_size_t), result0, wper)
    status = fgsl_bspline_calc_deriv(b, cper, int(i-1, fgsl_size_t), result1, wper)
    write(error_unit, fmt='("deriv ", I0, ": [",ES14.6,",",ES14.6,"]")') &
                    i, result0, result1 
  end do   
    
  call fgsl_rng_free(r)
  call fgsl_bspline_free(w)
  call fgsl_bspline_free(wper)
  
  deallocate(cf, cperf, xf, yf, wtsf)
  close(iu)
  
end program bspline_per
