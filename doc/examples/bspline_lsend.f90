module mod_bspline_lsend
  use fgsl_bspline
  use fgsl_rngen
  use fgsl_cdf
  use fgsl_linalg
  implicit none
  
  integer(c_size_t), parameter :: n = 500        ! number of data points to fit
  integer(c_size_t), parameter :: k = 10         ! spline order
                                                 ! data interval [a,b] 
  real(fgsl_double), parameter :: a = -1.0_fgsl_double
  real(fgsl_double), parameter :: b =  1.0_fgsl_double
  real(fgsl_double), parameter :: sigma = 0.03_fgsl_double     ! noise
  real(fgsl_double), parameter :: lambda_sq = 10.0_fgsl_double ! regularization parameter
  integer(c_size_t), parameter :: nderiv = 1     ! derivative order to regularize at endpoints
  integer(c_size_t), parameter :: nbreak = 20    ! breakpoints
 
contains
  function f(x) bind(c)
    real(c_double), value :: x
    real(c_double) :: f
    
    f = 1.0_c_double / (1.0_c_double + 25.0_c_double*x**2)
  end function
end module mod_bspline_lsend

program bspline_lsend
  !> Transcription of fifth bspline example to Fortran
  !> Least squares and endpoint regularization
  use, intrinsic :: iso_fortran_env, only : error_unit
  use mod_bspline_lsend
  implicit none
  
  integer(c_int) :: i, iu, status
  type(fgsl_bspline_workspace) :: work
  integer(fgsl_size_t) :: p                     ! number of control points
  real(fgsl_double) :: ui, xi, yi, dyi  
  real(fgsl_double) :: result_unreg, result_reg, err_unreg, err_reg
  real(fgsl_double) :: result0, result1
  
  ! control points for unregularized and regularized models, respectively
  real(fgsl_double), allocatable, target :: cf(:), cregf(:)
  type(fgsl_vector) :: c, creg
  ! x, y, weights
  real(fgsl_double), allocatable, target  :: xf(:), yf(:), wf(:)
  type(fgsl_vector) :: x, y, w
  ! normal equation matrix and rhs
  real(fgsl_double), allocatable, target  :: xtxf(:,:), xtyf(:)
  type(fgsl_matrix) :: xtx
  type(fgsl_vector) :: xty
  ! covariance matrices and regularization matrices
  real(fgsl_double), allocatable, target  :: covf(:,:), cov_regf(:,:), a1f(:,:), a2f(:,:)
  type(fgsl_matrix) :: cov, cov_reg, a1, a2
  ! random numbers
  type(fgsl_rng) :: r
  type(fgsl_rng_type) :: t


  ! set up all objects
  work = fgsl_bspline_alloc(k, nbreak)
  p = fgsl_bspline_ncontrol(work)
  
  allocate(cf(p), cregf(p), xf(n), yf(n), wf(n), xtxf(k, p), xtyf(p), &
           covf(p, p), cov_regf(p, p), a1f(k, p), a2f(k, p))
  c = fgsl_vector_init(cf)
  creg = fgsl_vector_init(cregf)
  x = fgsl_vector_init(xf)
  y = fgsl_vector_init(yf)
  w = fgsl_vector_init(wf)
  xtx = fgsl_matrix_init(xtxf)
  xty = fgsl_vector_init(xtyf)
  cov = fgsl_matrix_init(covf)
  cov_reg = fgsl_matrix_init(cov_regf)
  a1 = fgsl_matrix_init(a1f)
  a2 = fgsl_matrix_init(a2f)
  
  ! set up I/O
  open(newunit=iu, file='bspline_lsend.txt', form='FORMATTED', &
	   status='REPLACE', action='WRITE') 
	   
	   
  t = fgsl_rng_env_setup() ! t not otherwise used
  r = fgsl_rng_alloc(fgsl_rng_default)
    
  ! create the data to be fitted
  data : do i = 1, int(n, fgsl_int)
    ui = fgsl_rng_uniform(r)
    xi = (b - a) * ui + a
    
    dyi = fgsl_ran_gaussian(r, sigma)
    yi = f(xi) + dyi
    xf(i) = xi; yf(i) = yi; wf(i) = 1.0_fgsl_double/sigma**2

    write(iu, fmt='(2(F0.6,1X))') xi, yi
    
  end do data
  write(iu, fmt='(/)') 
  
  ! use uniform breakpoints on [a, b]
  status = fgsl_bspline_init_uniform(a, b, work)
  ! form normal equations matrix
  status = fgsl_bspline_lsnormal(x, y, w, xty, xtx, work)
  ! banded Cholesky decomposition
  status = fgsl_linalg_cholesky_band_decomp(xtx)
  ! solve for unregularized solution
  status = fgsl_linalg_cholesky_band_solve(xtx, xty, c)
  ! compute covariance matrix
  status = fgsl_bspline_covariance(xtx, cov, work)
  ! compute regularization matrices
  status = fgsl_bspline_oprod(nderiv, a, a1, work)
  status = fgsl_bspline_oprod(nderiv, b, a2, work)
  ! multiply by lambda^2 
  a1f = a1f * lambda_sq
  a2f = a2f * lambda_sq
  ! form normal equations matrix 
  status = fgsl_bspline_lsnormal(x, y, w, xty, xtx, work)
  ! add regularization term
  xtxf = xtxf + a1f
  xtxf = xtxf + a2f
  ! banded Cholesky decomposition 
  status = fgsl_linalg_cholesky_band_decomp(xtx)
  ! solve for regularized solution
  status = fgsl_linalg_cholesky_band_solve(xtx, xty, creg)
  ! compute covariance matrix 
  status = fgsl_bspline_covariance(xtx, cov_reg, work)
  
  ! write spline curves to external file
  xi = a
  do 
    if (xi >= b) exit
    
    ! compute unregularized spline value and error at xi 
    status = fgsl_bspline_calc(xi, c, result_unreg, work)
    status = fgsl_bspline_err(xi, 0_fgsl_size_t, cov, err_unreg, work)
    
    ! compute regularized spline value and error at xi */
    status = fgsl_bspline_calc(xi, creg, result_reg, work)
    status = fgsl_bspline_err(xi, 0_fgsl_size_t, cov_reg, err_reg, work)

    write(iu, fmt='(F0.6,1X, 5(ES0.6E2,1X))') xi, f(xi), result_unreg,  &
                                       err_unreg, result_reg, err_reg
    xi = xi + .001_fgsl_double
  end do
  

  ! 
  status = fgsl_bspline_calc_deriv(a, c, nderiv, result0, work)
  status = fgsl_bspline_calc_deriv(b, c, nderiv, result1, work)
  write(error_unit, fmt='("unregularized endpoint deriv ", I0, ": [",ES14.6,",",ES14.6,"]")') &
                    nderiv, result0, result1
  status = fgsl_bspline_calc_deriv(a, creg, nderiv, result0, work)
  status = fgsl_bspline_calc_deriv(b, creg, nderiv, result1, work)
  write(error_unit, fmt='("  regularized endpoint deriv ", I0, ": [",ES14.6,",",ES14.6,"]")') &
                    nderiv, result0, result1
  
  call fgsl_rng_free(r)
  call fgsl_bspline_free(work)
  deallocate(cf, cregf, xf, yf, wf, xtxf, xtyf, covf, cov_regf, a1f, a2f)
  close(iu)
   
end program bspline_lsend
