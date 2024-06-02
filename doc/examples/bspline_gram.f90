program bspline_gram
  !> Transcription of fifth bspline example to Fortran
  !> Least squares, regularization and the Gram matrix
  use fgsl_bspline
  use fgsl_rngen
  use fgsl_cdf
  use fgsl_linalg
  implicit none
    
  integer(c_size_t), parameter :: n = 500        ! number of data points to fit
  integer(c_size_t), parameter :: nbreak = 25    ! breakpoints
  integer(c_size_t), parameter :: k = 4          ! spline order
                                                 ! data interval [a,b] 
  real(fgsl_double), parameter :: a = -1.5_fgsl_double
  real(fgsl_double), parameter :: b =  1.5_fgsl_double
  real(fgsl_double), parameter :: sigma = 0.02_fgsl_double    ! noise
  real(fgsl_double), parameter :: lambda_sq = 0.1_fgsl_double ! regularization parameter
  
  real(fgsl_double) :: reg_a, reg_b              ! regularization interval [reg_a,reg_b]
  namelist / reg_nml / reg_a, reg_b
  logical :: reg_interval                        ! whether to apply regularization on smaller interval
  integer(c_int) :: i, iu, status
  real(fgsl_double) :: ui, xi, yi, dyi
  real(fgsl_double) :: result_unreg, result_reg, err_unreg, err_reg
   
  type(fgsl_bspline_workspace) :: work
  integer(fgsl_size_t) :: p                      ! number of control points
  
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
  ! covariance matrices and regularization matrix
  real(fgsl_double), allocatable, target  :: covf(:,:), cov_regf(:,:), gf(:,:)
  type(fgsl_matrix) :: cov, cov_reg, g
  ! random numbers
  type(fgsl_rng) :: r
  type(fgsl_rng_type) :: t
   
  ! set up all objects
  work = fgsl_bspline_alloc(k, nbreak)
  p = fgsl_bspline_ncontrol(work)
  
  allocate(cf(p), cregf(p), xf(n), yf(n), wf(n), xtxf(k, p), xtyf(p), &
           covf(p, p), cov_regf(p, p), gf(k, p))
  c = fgsl_vector_init(cf)
  creg = fgsl_vector_init(cregf)
  x = fgsl_vector_init(xf)
  y = fgsl_vector_init(yf)
  w = fgsl_vector_init(wf)
  xtx = fgsl_matrix_init(xtxf)
  xty = fgsl_vector_init(xtyf)
  cov = fgsl_matrix_init(covf)
  cov_reg = fgsl_matrix_init(cov_regf)
  g = fgsl_matrix_init(gf)
  
  ! determine whether to regularize
  reg_interval = .false.
  try_nml : block
    open(newunit=iu, form='FORMATTED', action='READ', status='OLD', &
         file='bspline_gram.nml', iostat=status) 
    if (status /= 0) exit try_nml
    read(iu, nml=reg_nml, iostat=status) 
    if (status /= 0) exit try_nml
    reg_interval = .true.
  end block try_nml
  close(iu)
  write(*, fmt='("Regularization values are: ",2(F0.3,1X),L3)') &
                    reg_a, reg_b, reg_interval
  ! set up I/O
  open(newunit=iu, file='bspline4.txt', form='FORMATTED', &
	   status='REPLACE', action='WRITE') 
	   
	   
  t = fgsl_rng_env_setup() ! t not otherwise used
  r = fgsl_rng_alloc(fgsl_rng_default)
  
  ! create the data to be fitted
  i = 1
  data : do 
    ui = fgsl_rng_uniform(r)
    xi = (b - a) * ui + a
    
    ! data gaps between [-1.1,-0.7] and [0.1,0.5] 
    if ((xi >= -1.1_fgsl_double .and. xi <= -0.7_fgsl_double) .or. &
        (xi >= 0.1_fgsl_double .and. xi <= 0.55_fgsl_double)) then
      cycle data
    end if 

    dyi = fgsl_ran_gaussian(r, sigma)
    yi = exp(-xi**2) + dyi
    xf(i) = xi; yf(i) = yi; wf(i) = 1.0_fgsl_double/sigma**2

    write(iu, fmt='(2(F0.6,1X))') xi, yi
    
    i = i+1; if (i > n) exit data
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
  ! compute regularization matrix 
  if (reg_interval) then
    status = fgsl_bspline_gram_interval(reg_a, reg_b, 2_fgsl_size_t, g, work)
  else
    status = fgsl_bspline_gram(2_fgsl_size_t, g, work);
  end if
  ! multiply by lambda^2 
  gf = gf * lambda_sq
  ! form normal equations matrix 
  status = fgsl_bspline_lsnormal(x, y, w, xty, xtx, work)
  ! add regularization term
  xtxf = xtxf + gf
  ! banded Cholesky decomposition 
  status = fgsl_linalg_cholesky_band_decomp(xtx)
  ! solve for regularized solution
  status = fgsl_linalg_cholesky_band_solve(xtx, xty, creg)
  ! compute covariance matrix 
  status = fgsl_bspline_covariance(xtx, cov_reg, work);
  
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

    write(iu, fmt='(F0.6,1X, 5(ES0.6E2,1X))') xi, exp(-xi*xi), result_unreg,  &
                                       err_unreg, result_reg, err_reg
    xi = xi + .01_fgsl_double
  end do
  
  
  call fgsl_rng_free(r)
  call fgsl_bspline_free(work)
  deallocate(cf, cregf, xf, yf, wf, xtxf, xtyf, covf, cov_regf, gf)
  close(iu)
    
end program bspline_gram
