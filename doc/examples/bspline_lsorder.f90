program bspline_lsorder
  !> Transcription of fourth bspline example to Fortran
  !> Least squares and Spline order
  use fgsl_bspline
  use fgsl_rngen
  use fgsl_cdf
  implicit none
  
  type :: array_container
    real(fgsl_double), allocatable :: c(:)
  end type
  
  integer(c_size_t), parameter :: n = 500     ! number of data points to fit
                                              ! maximum spline order
  integer(c_size_t), parameter :: max_spline_order = 5   
                                              ! data interval [a,b] 
  integer(c_size_t), parameter :: nbreak = 10 ! number of breakpoints
  real(fgsl_double), parameter :: a = 0._fgsl_double
  real(fgsl_double), parameter :: b = 15._fgsl_double
  real(fgsl_double), parameter :: sigma = 0.2_fgsl_double ! noise
  
  type(fgsl_bspline_workspace) :: work(max_spline_order)
  type(array_container), target :: cf(max_spline_order)
  real(fgsl_double), allocatable, target :: xf(:), yf(:), wf(:)
  type(fgsl_vector) :: c(max_spline_order), x, y, w
  
  type(fgsl_rng) :: r
  type(fgsl_rng_type) :: t
  
  integer(c_size_t) :: i
  integer(c_int) :: iu_fit, status
  real(fgsl_double) :: chisq, dyi, result, xi, yi
  
  allocate(xf(n), yf(n), wf(n))
  x = fgsl_vector_init(xf)
  y = fgsl_vector_init(yf)
  w = fgsl_vector_init(wf)

  t = fgsl_rng_env_setup() ! t not otherwise used
  r = fgsl_rng_alloc(fgsl_rng_default)
  
  ! prepare I/O
  open(newunit=iu_fit, file='bspline4_fit.txt', form='FORMATTED', &
	   status='REPLACE', action='WRITE')
  
  ! create the data to be fitted
  ! (same as for third bspline example)
  do i = 1, n
    xi = (b - a) / (n - 1.0_fgsl_double) * (real(i-1, fgsl_double)) + a
    yi = cos(xi) * exp(-0.1_fgsl_double * xi)
    
    dyi = fgsl_ran_gaussian(r, sigma)
    yi = yi + dyi 
    
    xf(i) = xi; yf(i) = yi; wf(i) = 1.0_fgsl_double/sigma**2
  end do 
  
  do i = 1, max_spline_order
    ! allocate workspace for this spline order
    work(i) = fgsl_bspline_alloc(i, nbreak)
    allocate(cf(i)%c(fgsl_bspline_ncontrol(work(i))))
    c(i) = fgsl_vector_init(cf(i)%c)

    ! use uniform breakpoints on [a, b] 
    status = fgsl_bspline_init_uniform(a, b, work(i))
  
    ! solve least squares problem 
    status = fgsl_bspline_wlssolve(x, y, w, c(i), chisq, work(i))
  end do
  
  ! write spline curves to external file
  xi = a
  do 
    if (xi >= b) exit
    
    write(iu_fit, advance='NO', fmt='(F0.6,1X)') xi
    do i = 1, max_spline_order
      status = fgsl_bspline_calc(xi, c(i), result, work(i))

      write(iu_fit, advance='NO', fmt='(F0.6,1X)') result
    end do
    write(iu_fit, fmt=*)
    xi = xi + .1_fgsl_double
  end do
  
  close(iu_fit)
  
  call fgsl_rng_free(r)
  do i = 1, max_spline_order
    call fgsl_bspline_free(work(i))
    deallocate(cf(i)%c)
  end do
  deallocate(xf, yf, wf)
end program
