program bspline_lsbreak
  !> Transcription of third bspline example to Fortran
  !> Least squares and breakpoints
  use fgsl_bspline
  use fgsl_rngen
  use fgsl_cdf
  implicit none
  
  integer(c_size_t), parameter :: n = 500   ! number of data points to fit
  integer(c_size_t), parameter :: k = 4     ! spline order
                                            ! data interval [a,b] 
  real(fgsl_double), parameter :: a = 0._fgsl_double
  real(fgsl_double), parameter :: b = 15._fgsl_double
  real(fgsl_double), parameter :: sigma = 0.2_fgsl_double ! noise
  
  type(fgsl_bspline_workspace) :: work1, work2
  real(fgsl_double), allocatable, target :: c1f(:), c2f(:), xf(:), yf(:), wf(:)
  type(fgsl_vector) :: c1, c2, x, y, w
  
  type(fgsl_rng) :: r
  type(fgsl_rng_type) :: t
  
  integer(c_size_t) :: dof1, dof2, i, p1, p2
  integer(c_int) :: iu_data, iu_fit, status
  real(fgsl_double) :: chisq1, chisq2, dyi, result1, result2, xi, yi
  
  ! work spaces with 40 and 10 breakbpoints, respectively
  work1 = fgsl_bspline_alloc(k, 40_fgsl_size_t)
  work2 = fgsl_bspline_alloc(k, 10_fgsl_size_t)
  ! number of control points
  p1 = fgsl_bspline_ncontrol(work1)
  p2 = fgsl_bspline_ncontrol(work2)
  ! degrees of freedom
  dof1 = n - p1
  dof2 = n - p2
  
  allocate(c1f(p1), c2f(p2), xf(n), yf(n), wf(n))
  c1 = fgsl_vector_init(c1f)
  c2 = fgsl_vector_init(c2f)
  x = fgsl_vector_init(xf)
  y = fgsl_vector_init(yf)
  w = fgsl_vector_init(wf)

  t = fgsl_rng_env_setup() ! t not otherwise used
  r = fgsl_rng_alloc(fgsl_rng_default)
  
  ! prepare I/O
  open(newunit=iu_data, file='bspline3_data.txt', form='FORMATTED', &
	   status='REPLACE', action='WRITE')
  open(newunit=iu_fit, file='bspline3_fit.txt', form='FORMATTED', &
	   status='REPLACE', action='WRITE')
  
  ! create the data to be fitted
  do i = 1, n
    xi = (b - a) / (n - 1.0_fgsl_double) * (real(i-1, fgsl_double)) + a
    yi = cos(xi) * exp(-0.1_fgsl_double * xi)
    
    dyi = fgsl_ran_gaussian(r, sigma)
    yi = yi + dyi 
    
    xf(i) = xi; yf(i) = yi; wf(i) = 1.0_fgsl_double/sigma**2
    
    write(iu_data, fmt='(2(F0.6,1X))') xi, yi

  end do 
  
  ! use uniform breakpoints on [a, b] 
  status = fgsl_bspline_init_uniform(a, b, work1)
  status = fgsl_bspline_init_uniform(a, b, work2)
  
  ! solve least squares problem 
  status = fgsl_bspline_wlssolve(x, y, w, c1, chisq1, work1)
  status = fgsl_bspline_wlssolve(x, y, w, c2, chisq2, work2)

  write(*, fmt='("40 breakpoints: chisq/dof = ", ES0.6)') chisq1 / dof1
  write(*, fmt='("10 breakpoints: chisq/dof = ", ES0.6)') chisq2 / dof2
  
  ! write spline curves to external file
  xi = a
  do 
    if (xi >= b) exit
    
    status = fgsl_bspline_calc(xi, c1, result1, work1)
    status = fgsl_bspline_calc(xi, c2, result2, work2)

    write(iu_fit, fmt='(3(F0.6,1X))') xi, result1, result2
        
    xi = xi + .1_fgsl_double
  end do
  
  
  close(iu_data)
  close(iu_fit)
  
  call fgsl_rng_free(r)
  call fgsl_bspline_free(work1)
  call fgsl_bspline_free(work2)
  deallocate(c1f, c2f, xf, yf, wf)
end program
