module fgsl_multifit
  !> Linear least-squeares fitting - multi-parameter regression
  use fgsl_base
  use fgsl_array
  use fgsl_permutations
  implicit none

  private :: gsl_multifit_fsolver_alloc, gsl_multifit_fdfsolver_alloc, &
    gsl_multifit_fsolver_set, gsl_multifit_fdfsolver_set, &
    gsl_multifit_fdfsolver_wset, gsl_multifit_fsolver_free, &
    gsl_multifit_fdfsolver_free, gsl_multifit_fsolver_name, &
    gsl_multifit_fdfsolver_name, gsl_multifit_fsolver_iterate, &
    gsl_multifit_fdfsolver_iterate, gsl_multifit_fsolver_position, &
    gsl_multifit_fdfsolver_position, gsl_multifit_fdfsolver_dx, &
    gsl_multifit_fdfsolver_f, gsl_multifit_fdfsolver_jac, &
    gsl_multifit_test_delta, gsl_multifit_test_gradient, &
    gsl_multifit_gradient, gsl_multifit_covar, gsl_multifit_covar_qrpt, &
    gsl_multifit_fsolver_driver, gsl_multifit_fdfsolver_driver, &
    gsl_multifit_fdfsolver_dif_df, gsl_multifit_robust_alloc, &
    gsl_multifit_robust_free, gsl_multifit_robust_tune, &
    gsl_multifit_robust_name, gsl_multifit_robust_statistics, &
    gsl_multifit_robust, gsl_multifit_robust_est, &
    gsl_multifit_fdfsolver_residual, gsl_multifit_fdfsolver_niter, &
    gsl_multifit_eval_wf, gsl_multifit_eval_wdf, gsl_multifit_fdfsolver_test, &
    gsl_multifit_linear_alloc, gsl_multifit_linear_free, gsl_multifit_linear, &
    gsl_multifit_linear_tsvd, gsl_multifit_linear_svd, gsl_multifit_linear_bsvd, &
    gsl_multifit_linear_solve, gsl_multifit_linear_applyw, &
    gsl_multifit_linear_stdform1, gsl_multifit_linear_wstdform1, &
    gsl_multifit_linear_l_decomp, gsl_multifit_linear_stdform2, &
    gsl_multifit_linear_wstdform2, gsl_multifit_linear_genform1, &
    gsl_multifit_linear_genform2, gsl_multifit_linear_wgenform2, &
    gsl_multifit_linear_lreg, gsl_multifit_linear_lcurve, &
    gsl_multifit_linear_lcurvature, gsl_multifit_linear_lcorner, &
    gsl_multifit_linear_gcv_init, &
    gsl_multifit_linear_gcv_curve, gsl_multifit_linear_gcv_min, &
    gsl_multifit_linear_gcv_calc, gsl_multifit_linear_gcv, &
    gsl_multifit_linear_lk, gsl_multifit_linear_lsobolev, &
    gsl_multifit_linear_rcond, gsl_multifit_robust_maxiter, &
    gsl_multifit_robust_weights, gsl_multifit_robust_residuals, &
    gsl_multifit_wlinear, gsl_multifit_wlinear_tsvd, &
    gsl_multifit_wlinear_svd, gsl_multifit_wlinear_usvd, &
    gsl_multifit_linear_est, gsl_multifit_linear_residuals, &
    gsl_multifit_linear_rank, gsl_multifit_fdfridge_alloc, &
    gsl_multifit_fdfridge_free, gsl_multifit_fdfridge_name, &
    gsl_multifit_fdfridge_position, &
    gsl_multifit_fdfridge_residual, gsl_multifit_fdfridge_niter, &
    gsl_multifit_fdfridge_set, gsl_multifit_fdfridge_wset, &
    gsl_multifit_fdfridge_set2, gsl_multifit_fdfridge_wset2, &
    gsl_multifit_fdfridge_set3, gsl_multifit_fdfridge_wset3, &
    gsl_multifit_fdfridge_iterate, gsl_multifit_fdfridge_driver
   
    
  private :: fgsl_aux_multifit_robust_alloc, fgsl_multifit_function_cinit, &
    fgsl_multifit_function_cfree, fgsl_multifit_function_fdf_cfree, &
    fgsl_aux_multifit_fsolver_alloc, fgsl_aux_multifit_fdfsolver_alloc
    
  !
  !> Types
  !
  type, public :: fgsl_multifit_linear_workspace
     private
     type(c_ptr) :: gsl_multifit_linear_workspace = c_null_ptr
  end type fgsl_multifit_linear_workspace

  !
  !> legacy interface
  type, public :: fgsl_multifit_function
     private
     type(c_ptr) :: gsl_multifit_function = c_null_ptr
  end type fgsl_multifit_function
  type, public :: fgsl_multifit_function_fdf
     private
     type(c_ptr) :: gsl_multifit_function_fdf = c_null_ptr
  end type fgsl_multifit_function_fdf
  type, public :: fgsl_multifit_fsolver
     private
     type(c_ptr) :: gsl_multifit_fsolver = c_null_ptr
  end type fgsl_multifit_fsolver
  type, public :: fgsl_multifit_fsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multifit_fsolver_type
  type, public :: fgsl_multifit_fdfsolver
     private
     type(c_ptr) :: gsl_multifit_fdfsolver = c_null_ptr
  end type fgsl_multifit_fdfsolver
  type, public :: fgsl_multifit_fdfsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multifit_fdfsolver_type
  type(fgsl_multifit_fdfsolver_type), public, parameter :: &
       fgsl_multifit_fdfsolver_lmder = fgsl_multifit_fdfsolver_type(1), &
       fgsl_multifit_fdfsolver_lmsder = fgsl_multifit_fdfsolver_type(2), &
       fgsl_multifit_fdfsolver_lmniel = fgsl_multifit_fdfsolver_type(3)
  type, public:: fgsl_multifit_fdfridge
    private
    type(c_ptr) :: gsl_multifit_fdfridge = c_null_ptr
  end type fgsl_multifit_fdfridge
  !
  !> robust multifit
  type, public :: fgsl_multifit_robust_type
     private
     integer(fgsl_int) :: which = 0
  end type fgsl_multifit_robust_type
  type(fgsl_multifit_robust_type), parameter, public :: &
       fgsl_multifit_robust_default = fgsl_multifit_robust_type(1), &
       fgsl_multifit_robust_bisquare = fgsl_multifit_robust_type(2), &
       fgsl_multifit_robust_cauchy = fgsl_multifit_robust_type(3), &
       fgsl_multifit_robust_fair = fgsl_multifit_robust_type(4), &
       fgsl_multifit_robust_huber = fgsl_multifit_robust_type(5), &
       fgsl_multifit_robust_ols = fgsl_multifit_robust_type(6), &
       fgsl_multifit_robust_welsch = fgsl_multifit_robust_type(7)
  type, public :: fgsl_multifit_robust_workspace
       private
       type(c_ptr) :: gsl_multifit_robust_workspace
  end type fgsl_multifit_robust_workspace
  type, public :: fgsl_multifit_robust_stats
       real(fgsl_double) :: sigma_ols
       real(fgsl_double) :: sigma_mad
       real(fgsl_double) :: sigma_rob
       real(fgsl_double) :: sigma
       real(fgsl_double) :: Rsq
       real(fgsl_double) :: adj_Rsq
       real(fgsl_double) :: rmse
       real(fgsl_double) :: sse
       real(fgsl_double) :: dof
       real(fgsl_double) :: numit
       type(fgsl_vector) :: weights
       type(fgsl_vector) :: r
  end type fgsl_multifit_robust_stats
  type, bind(c) :: gsl_multifit_robust_stats
       real(c_double) :: sigma_ols
       real(c_double) :: sigma_mad
       real(c_double) :: sigma_rob
       real(c_double) :: sigma
       real(c_double) :: Rsq
       real(c_double) :: adj_Rsq
       real(c_double) :: rmse
       real(c_double) :: sse
       real(c_double) :: dof
       real(c_double) :: numit
       type(c_ptr) :: weights
       type(c_ptr) :: r
  end type gsl_multifit_robust_stats
  !
  !> Generics
  interface fgsl_well_defined
     module procedure fgsl_multifit_status
     module procedure fgsl_multifit_fsolver_status
     module procedure fgsl_multifit_fdfsolver_status
  end interface
  !
  ! Multifit: with or without weights
  ! The C API allows a user to pass NULL if weights are not required,
  ! however this cannot be done easily with Fortran. This seems to be
  ! an elegant solution
  ! FIXME Fortran 2018+ should permit this. Check whether this can be used here.
  interface fgsl_multifit_fdfsolver_dif_df
    module procedure fgsl_multifit_fdfsolver_dif_df_wts
    module procedure fgsl_multifit_fdfsolver_dif_df_nowts
  end interface fgsl_multifit_fdfsolver_dif_df
  interface fgsl_multifit_eval_wf
    module procedure fgsl_multifit_eval_wf_wts
    module procedure fgsl_multifit_eval_wf_nowts
  end interface fgsl_multifit_eval_wf
  interface fgsl_multifit_eval_wdf
    module procedure fgsl_multifit_eval_wdf_wts
    module procedure fgsl_multifit_eval_wdf_nowts
  end interface fgsl_multifit_eval_wdf
  !
  !> C interfaces
  interface
	  function gsl_multifit_fsolver_alloc(t, n, p) bind(c)
	    import
	    type(c_ptr), value :: t
	    integer(c_size_t), value :: n, p
	    type(c_ptr) :: gsl_multifit_fsolver_alloc
	  end function gsl_multifit_fsolver_alloc
	  function gsl_multifit_fdfsolver_alloc(t, n, p) bind(c)
	    import
	    type(c_ptr), value :: t
	    integer(c_size_t), value :: n, p
	    type(c_ptr) :: gsl_multifit_fdfsolver_alloc
	  end function gsl_multifit_fdfsolver_alloc
	  function gsl_multifit_fsolver_set(s, f, x) bind(c)
	    import
	    type(c_ptr), value :: s, f, x
	    integer(c_int) :: gsl_multifit_fsolver_set
	  end function gsl_multifit_fsolver_set
	  function gsl_multifit_fdfsolver_set(s, f, x) bind(c)
	    import
	    type(c_ptr), value :: s, f, x
	    integer(c_int) :: gsl_multifit_fdfsolver_set
	  end function gsl_multifit_fdfsolver_set
	  function gsl_multifit_fdfsolver_wset(s, f, x, wts) bind(c)
	    import
	    type(c_ptr), value :: s, f, x, wts
	    integer(c_int) :: gsl_multifit_fdfsolver_wset
	  end function gsl_multifit_fdfsolver_wset
	  subroutine gsl_multifit_fsolver_free(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	  end subroutine gsl_multifit_fsolver_free
	  subroutine gsl_multifit_fdfsolver_free(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	  end subroutine gsl_multifit_fdfsolver_free
	  function gsl_multifit_fsolver_name(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multifit_fsolver_name
	  end function gsl_multifit_fsolver_name
	  function gsl_multifit_fdfsolver_name(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multifit_fdfsolver_name
	  end function gsl_multifit_fdfsolver_name
	  function gsl_multifit_fsolver_iterate(s) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: s
	    integer(c_int) :: gsl_multifit_fsolver_iterate
	  end function gsl_multifit_fsolver_iterate
	  function gsl_multifit_fdfsolver_iterate(s) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: s
	    integer(c_int) :: gsl_multifit_fdfsolver_iterate
	  end function gsl_multifit_fdfsolver_iterate
	  function gsl_multifit_fsolver_position(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multifit_fsolver_position
	  end function gsl_multifit_fsolver_position
	  function gsl_multifit_fdfsolver_position(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multifit_fdfsolver_position
	  end function gsl_multifit_fdfsolver_position
	  function gsl_multifit_fdfsolver_dx(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multifit_fdfsolver_dx
	  end function gsl_multifit_fdfsolver_dx
	  function gsl_multifit_fdfsolver_f(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multifit_fdfsolver_f
	  end function gsl_multifit_fdfsolver_f
	  function gsl_multifit_fdfsolver_jac(s, J) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: s, J
	    integer(c_int) :: gsl_multifit_fdfsolver_jac
	  end function gsl_multifit_fdfsolver_jac
	  function gsl_multifit_test_delta(dx, x, epsabs, epsrel) bind(c)
	    import :: c_ptr, c_int, c_double
	    type(c_ptr), value :: dx, x
	    real(c_double), value :: epsabs, epsrel
	    integer(c_int) :: gsl_multifit_test_delta
	  end function gsl_multifit_test_delta
	  function gsl_multifit_test_gradient(g, epsabs) bind(c)
	    import :: c_ptr, c_int, c_double
	    type(c_ptr), value :: g
	    real(c_double), value :: epsabs
	    integer(c_int) :: gsl_multifit_test_gradient
	  end function gsl_multifit_test_gradient
	  function gsl_multifit_gradient(j, f, g) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: j, f, g
	    integer(c_int) :: gsl_multifit_gradient
	  end function gsl_multifit_gradient
	  function gsl_multifit_covar(j, epsrel, cov) bind(c)
	    import :: c_ptr, c_int, c_double
	    type(c_ptr), value :: j, cov
	    real(c_double), value :: epsrel
	    integer(c_int) :: gsl_multifit_covar
	  end function gsl_multifit_covar
	  function gsl_multifit_covar_qrpt(r, perm, epsrel, cov) &
	    bind(c, name='gsl_multifit_covar_QRPT')
	    import :: c_ptr, c_int, c_double
	    type(c_ptr), value :: r, perm, cov
	    real(c_double), value :: epsrel
	    integer(c_int) :: gsl_multifit_covar_qrpt
	  end function gsl_multifit_covar_qrpt
	  function fgsl_aux_multifit_robust_alloc(i) bind(c)
	    import :: c_int, c_ptr
	    type(c_ptr) :: fgsl_aux_multifit_robust_alloc
	    integer(c_int), value :: i
	  end function fgsl_aux_multifit_robust_alloc
	  function gsl_multifit_fsolver_driver(s, maxiter, epsabs, epsrel) bind(c)
	    import :: c_ptr, c_size_t, c_double, c_int
	    type(c_ptr), value :: s
	    integer(c_size_t), value :: maxiter
	    real(c_double), value :: epsabs, epsrel
	    integer(c_int) :: gsl_multifit_fsolver_driver
	  end function gsl_multifit_fsolver_driver
	  function gsl_multifit_fdfsolver_driver(s, maxiter, xtol, gtol,&
	    ftol, info) bind(c)
	    import :: c_ptr, c_size_t, c_double, c_int
	    type(c_ptr), value :: s
	    integer(c_size_t), value :: maxiter
	    real(c_double), value :: xtol, gtol, ftol
	    integer(c_int) :: info
	    integer(c_int) :: gsl_multifit_fdfsolver_driver
	  end function gsl_multifit_fdfsolver_driver
	  function gsl_multifit_fdfsolver_dif_df(x, wts, fdf, f, J) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: x, wts, fdf, f, J
	    integer(c_int) :: gsl_multifit_fdfsolver_dif_df
	  end function gsl_multifit_fdfsolver_dif_df
	  function gsl_multifit_robust_alloc(T, n, p) bind(c)
	    import :: c_ptr, c_size_t
	    type(c_ptr), value :: T
	    integer(c_size_t), value :: n, p
	    type(c_ptr) :: gsl_multifit_robust_alloc
	  end function gsl_multifit_robust_alloc
	  subroutine gsl_multifit_robust_free(w) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: w
	  end subroutine gsl_multifit_robust_free
	  function gsl_multifit_robust_tune(tune, w) bind(c)
	    import :: c_ptr, c_double, c_int
	    real(c_double), value :: tune
	    type(c_ptr), value :: w
	    integer(c_int) :: gsl_multifit_robust_tune
	  end function gsl_multifit_robust_tune
	  function gsl_multifit_robust_name(w) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: w
	    type(c_ptr) :: gsl_multifit_robust_name
	  end function gsl_multifit_robust_name
	  function gsl_multifit_robust_statistics(w) bind(c)
	    import :: c_ptr, gsl_multifit_robust_stats
	    type(c_ptr), value :: w
	    type(gsl_multifit_robust_stats) :: gsl_multifit_robust_statistics
	  end function gsl_multifit_robust_statistics
	  function gsl_multifit_robust(X, y, c, cov, w) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: X, y, c, cov, w
	    integer(c_int) :: gsl_multifit_robust
	  end function gsl_multifit_robust
	  function gsl_multifit_robust_est(x, c, cov, y, y_err) bind(c)
	    import :: c_ptr, c_int, c_double
	    type(c_ptr), value :: x, c, cov
	    real(c_double), intent(out) :: y, y_err
	    integer(c_int) :: gsl_multifit_robust_est
	  end function gsl_multifit_robust_est
	!
	  function fgsl_multifit_function_cinit(fp, ndim, p, params) bind(c)
	    import
	    type(c_funptr), value :: fp
	    integer(c_size_t), value :: ndim, p
	    type(c_ptr), value :: params
	    type(c_ptr) :: fgsl_multifit_function_cinit
	  end function fgsl_multifit_function_cinit
	  function fgsl_multifit_function_fdf_cinit(fp, dfp, fdfp, ndim, p, params) bind(c)
	    import
	    type(c_funptr), value :: fp, dfp, fdfp
	    integer(c_size_t), value :: ndim, p
	    type(c_ptr), value :: params
	    type(c_ptr) :: fgsl_multifit_function_fdf_cinit
	  end function fgsl_multifit_function_fdf_cinit
	  subroutine fgsl_multifit_function_cfree(f) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: f
	  end subroutine fgsl_multifit_function_cfree
	  subroutine fgsl_multifit_function_fdf_cfree(f) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: f
	  end subroutine fgsl_multifit_function_fdf_cfree
	  function fgsl_aux_multifit_fsolver_alloc(it) bind(c)
	    import
	    integer(c_int), value :: it
	    type(c_ptr) :: fgsl_aux_multifit_fsolver_alloc
	  end function fgsl_aux_multifit_fsolver_alloc
	  function fgsl_aux_multifit_fdfsolver_alloc(it) bind(c)
	    import
	    integer(c_int), value :: it
	    type(c_ptr) :: fgsl_aux_multifit_fdfsolver_alloc
	  end function fgsl_aux_multifit_fdfsolver_alloc
	
	  function gsl_multifit_fdfsolver_residual(s) bind(c)
	    import c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multifit_fdfsolver_residual
	  end function gsl_multifit_fdfsolver_residual
	  function gsl_multifit_fdfsolver_niter(s) bind(c)
	    import c_ptr, c_size_t
	    type(c_ptr), value :: s
	    integer(c_size_t) :: gsl_multifit_fdfsolver_niter
	  end function gsl_multifit_fdfsolver_niter
	  function gsl_multifit_eval_wf(fdf, x, wts, y) bind(c)
	    import c_ptr, c_int
	    type(c_ptr), value :: fdf, x, wts, y
	    integer(c_int) :: gsl_multifit_eval_wf
	  end function gsl_multifit_eval_wf
	  function gsl_multifit_eval_wdf(fdf, x, wts, dy) bind(c)
	    import c_ptr, c_int
	    type(c_ptr), value :: fdf, x, wts, dy
	    integer(c_int) :: gsl_multifit_eval_wdf
	  end function gsl_multifit_eval_wdf
	  function gsl_multifit_fdfsolver_test(s, xtol, gtol, ftol, info) bind(c)
	    import c_ptr, c_double, c_int
	    type(c_ptr), value :: s
	    real(c_double), value :: xtol, gtol, ftol
	    integer(c_int) :: info
	    integer(c_int) :: gsl_multifit_fdfsolver_test
	  end function gsl_multifit_fdfsolver_test
	
	  function gsl_multifit_linear_alloc(n, p) bind(c)
	    import
	    integer(c_size_t), value :: n, p
	    type(c_ptr) :: gsl_multifit_linear_alloc
	  end function gsl_multifit_linear_alloc
	  subroutine gsl_multifit_linear_free(w) bind(c)
	    import
	    type(c_ptr), value :: w
	  end subroutine gsl_multifit_linear_free
	  function gsl_multifit_linear(x, y, c, cov, chisq, work) bind(c)
	    import :: c_ptr, c_int, c_double
	    type(c_ptr), value :: x, y, c, cov, work
	    real(c_double) :: chisq
	    integer(c_int) :: gsl_multifit_linear
	  end function gsl_multifit_linear
	  function gsl_multifit_linear_tsvd(x, y, tol, c, cov, chisq, rank, work) bind(c)
	    import :: c_ptr, c_int, c_double, c_size_t
	    type(c_ptr), value :: x, y, c, cov, work
	    real(c_double), value :: tol
	    real(c_double) :: chisq
	    integer(c_size_t) :: rank
	    integer(c_int) :: gsl_multifit_linear_tsvd
	  end function gsl_multifit_linear_tsvd
	  function gsl_multifit_linear_svd(x, work) bind(c)
	    import :: c_ptr, c_int, c_double, c_size_t
	    type(c_ptr), value :: x, work
	    integer(c_int) :: gsl_multifit_linear_svd
	  end function gsl_multifit_linear_svd
	  function gsl_multifit_linear_bsvd(x, work) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: x, work
	    integer(c_int) :: gsl_multifit_linear_bsvd
	  end function gsl_multifit_linear_bsvd
	  function gsl_multifit_linear_solve(lambda, x, y, c, rnorm, snorm, work) bind(c)
	    import :: c_double, c_ptr, c_int
	    real(c_double), value :: lambda
	    type(c_ptr), value :: X, y, c, work
	    real(c_double) :: rnorm, snorm
	    integer(c_int) :: gsl_multifit_linear_solve
	  end function gsl_multifit_linear_solve
	  function gsl_multifit_linear_applyw(X, w, y, WX, Wy) &
	    bind(c, name='gsl_multifit_linear_applyW')
	    import :: c_ptr, c_int
	    type(c_ptr), value :: X, w, y, WX, Wy
	    integer(c_int) :: gsl_multifit_linear_applyw
	  end function gsl_multifit_linear_applyw
	  function gsl_multifit_linear_stdform1(L, X, y, Xs, ys, work) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: L, X, y, Xs, ys, work
	    integer(c_int) :: gsl_multifit_linear_stdform1
	  end function gsl_multifit_linear_stdform1
	  function gsl_multifit_linear_wstdform1(L, X, w, y, Xs, ys, work) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: L, X, w, y, Xs, ys, work
	    integer(c_int) :: gsl_multifit_linear_wstdform1
	  end function gsl_multifit_linear_wstdform1
	  function gsl_multifit_linear_l_decomp(L, tau) &
	    bind(c, name='gsl_multifit_linear_L_decomp')
	    import :: c_ptr, c_int
	    type(c_ptr), value :: L, tau
	    integer(c_int) :: gsl_multifit_linear_l_decomp
	  end function gsl_multifit_linear_l_decomp
	  function gsl_multifit_linear_stdform2(LQR, Ltau, X, y, Xs, ys, M, work) bind(c)
	    import :: c_int, c_ptr
	    type(c_ptr), value :: LQR, Ltau, X, y, Xs, ys, M, work
	    integer(c_int) :: gsl_multifit_linear_stdform2
	  end function gsl_multifit_linear_stdform2
	  function gsl_multifit_linear_wstdform2(LQR, Ltau, X, w, y, Xs, ys, M, work) bind(c)
	    import :: c_int, c_ptr
	    type(c_ptr), value :: LQR, Ltau, X, w, y, Xs, ys, M, work
	    integer(c_int) :: gsl_multifit_linear_wstdform2
	  end function gsl_multifit_linear_wstdform2
	  function gsl_multifit_linear_genform1(L, cs, c, work) bind(c)
	    import :: c_int, c_ptr
	    type(c_ptr), value :: L, cs, c, work
	    integer(c_int) :: gsl_multifit_linear_genform1
	  end function gsl_multifit_linear_genform1
	  function gsl_multifit_linear_genform2(LQR, Ltau, X, y, cs, M, c, work) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: LQR, Ltau, X, y, cs, M, c, work
	    integer(c_int) :: gsl_multifit_linear_genform2
	  end function gsl_multifit_linear_genform2
	  function gsl_multifit_linear_wgenform2(LQR, Ltau, X, w, y, cs, M, c, work) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: LQR, Ltau, X, w, y, cs, M, c, work
	    integer(c_int) :: gsl_multifit_linear_wgenform2
	  end function gsl_multifit_linear_wgenform2
	  function gsl_multifit_linear_lreg(smin, smax, reg_param) bind(c)
	    import :: c_double, c_ptr, c_int
	    real(c_double), value :: smin, smax
	    type(c_ptr), value :: reg_param
	    integer(c_int) :: gsl_multifit_linear_lreg
	  end function gsl_multifit_linear_lreg
	  function gsl_multifit_linear_lcurve(y, reg_param, rho, eta, work) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: y, reg_param, rho, eta, work
	    integer(c_int) :: gsl_multifit_linear_lcurve
	  end function gsl_multifit_linear_lcurve
	  function gsl_multifit_linear_lcurvature(y, reg_param, rho, eta, kappa, work) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: y, reg_param, rho, eta, kappa, work
	    integer(c_int) :: gsl_multifit_linear_lcurvature
	  end function gsl_multifit_linear_lcurvature
	  function gsl_multifit_linear_lcorner(rho, eta, idx) bind(c)
	    import :: c_int, c_size_t, c_ptr
	    type(c_ptr), value :: rho, eta
	    integer(c_size_t) :: idx
	    integer(c_int) :: gsl_multifit_linear_lcorner
	  end function gsl_multifit_linear_lcorner
	  function gsl_multifit_linear_lcorner2(reg_param, eta, idx) bind(c)
	    import :: c_int, c_size_t, c_ptr
	    type(c_ptr), value :: reg_param, eta
	    integer(c_size_t) :: idx
	    integer(c_int) :: gsl_multifit_linear_lcorner2
	  end function gsl_multifit_linear_lcorner2
	  integer(c_int) function gsl_multifit_linear_gcv_init(y, reg_param, uty, delta0, work) bind(c)
	    import :: c_int, c_ptr, c_double
	    type(c_ptr), value :: y, reg_param, uty, work
	    real(c_double) :: delta0
	  end function
	  integer(c_int) function gsl_multifit_linear_gcv_curve(reg_param, uty, delta0, g, work) bind(c)
	    import :: c_int, c_ptr, c_double
	    type(c_ptr), value :: reg_param, uty, g, work
	    real(c_double), value :: delta0
	  end function
	  integer(c_int) function gsl_multifit_linear_gcv_min(reg_param, uty, delta0, g, &
	          lambda, work) bind(c)
	    import :: c_int, c_ptr, c_double
	    type(c_ptr), value :: reg_param, uty, g, work
	    real(c_double), value :: delta0
	    real(c_double) :: lambda
	  end function
	  real(c_double) function gsl_multifit_linear_gcv_calc(lambda, uty, delta0, work) bind(c)
	    import :: c_ptr, c_double
	    type(c_ptr), value :: uty, work
	    real(c_double), value :: delta0, lambda
	  end function
	  integer(c_int) function gsl_multifit_linear_gcv(y, reg_param, g, lambda, g_lambda, work) bind(c)
	    import :: c_int, c_ptr, c_double
	    type(c_ptr), value :: y, reg_param, g, work
	    real(c_double) :: lambda, g_lambda
	  end function
	  function gsl_multifit_linear_lk(p, k, L) &
	    bind(c, name='gsl_multifit_linear_Lk')
	    import :: c_size_t, c_int, c_ptr
	    integer(c_size_t), value :: p, k
	    type(c_ptr), value :: L
	    integer(c_int) :: gsl_multifit_linear_lk
	  end function gsl_multifit_linear_lk
	  function gsl_multifit_linear_lsobolev(p, kmax, alpha, L, work) &
	    bind(c, name='gsl_multifit_linear_Lsobolev')
	    import :: c_size_t, c_int, c_ptr
	    integer(c_size_t), value :: p, kmax
	    type(c_ptr), value :: alpha, L, work
	    integer(c_int) :: gsl_multifit_linear_lsobolev
	  end function gsl_multifit_linear_lsobolev
	  function gsl_multifit_linear_rcond(w) bind(c)
	    import :: c_double, c_ptr
	    type(c_ptr), value :: w
	    real(c_double) :: gsl_multifit_linear_rcond
	  end function gsl_multifit_linear_rcond
	  function gsl_multifit_robust_maxiter(maxiter, w) bind(c)
	    import :: c_size_t, c_int, c_ptr
	    integer(c_size_t), value :: maxiter
	    type(c_ptr), value :: w
	    integer(c_int) :: gsl_multifit_robust_maxiter
	  end function gsl_multifit_robust_maxiter
	  function gsl_multifit_robust_weights(r, wts, w) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: r, wts, w
	    integer(c_int) :: gsl_multifit_robust_weights
	  end function gsl_multifit_robust_weights
	  function gsl_multifit_robust_residuals(X, y, c, r, w) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: X, y, c, r, w
	    integer(c_int) :: gsl_multifit_robust_residuals
	  end function gsl_multifit_robust_residuals
	
	  function gsl_multifit_wlinear(x, w, y, c, cov, chisq, work) bind(c)
	    import :: c_ptr, c_int, c_double
	    type(c_ptr), value :: x, w, y, c, cov, work
	    real(c_double) :: chisq
	    integer(c_int) :: gsl_multifit_wlinear
	  end function gsl_multifit_wlinear
	  function gsl_multifit_wlinear_tsvd(x, w, y, tol, c, cov, chisq, rank, work) bind(c)
	    import :: c_ptr, c_int, c_double, c_size_t
	    type(c_ptr), value :: x, w, y, c, cov, work
	    real(c_double), value :: tol
	    real(c_double) :: chisq
	    integer(c_size_t) :: rank
	    integer(c_int) :: gsl_multifit_wlinear_tsvd
	  end function gsl_multifit_wlinear_tsvd
	  function gsl_multifit_wlinear_svd(x, w, y, tol, rank, c, cov, chisq, work) bind(c)
	    import :: c_ptr, c_int, c_double, c_size_t
	    type(c_ptr), value :: x, w, y, c, cov, work
	    real(c_double), value :: tol
	    integer(c_size_t) :: rank
	    real(c_double) :: chisq
	    integer(c_int) :: gsl_multifit_wlinear_svd
	  end function gsl_multifit_wlinear_svd
	  function gsl_multifit_wlinear_usvd(x, w, y, tol, rank, c, cov, chisq, work) bind(c)
	    import :: c_ptr, c_int, c_double, c_size_t
	    type(c_ptr), value :: x, w, y, c, cov, work
	    real(c_double), value :: tol
	    integer(c_size_t) :: rank
	    real(c_double) :: chisq
	    integer(c_int) :: gsl_multifit_wlinear_usvd
	  end function gsl_multifit_wlinear_usvd
	  function gsl_multifit_linear_est(x, c, cov, y, y_err) bind(c)
	    import :: c_ptr, c_double, c_int
	    type(c_ptr), value :: x, c, cov
	    real(c_double) :: y, y_err
	    integer(c_int) :: gsl_multifit_linear_est
	  end function gsl_multifit_linear_est
	  function gsl_multifit_linear_residuals(x, y, c, r) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: x
	    type(c_ptr), value :: y, c
	    type(c_ptr), value :: r
	    integer(c_int) :: gsl_multifit_linear_residuals
	  end function gsl_multifit_linear_residuals
	  integer(c_size_t) function gsl_multifit_linear_rank(tol, work) bind(c)
	    import :: c_ptr, c_size_t, c_double
	    type(c_ptr), value :: work
	    real(c_double), value :: tol
	  end function
	  function gsl_multifit_fdfridge_alloc(T, n, p) bind(c)
	    import :: c_ptr, c_size_t
	    type(c_ptr), value :: T
	    integer(c_size_t), value :: n, p
	    type(c_ptr) :: gsl_multifit_fdfridge_alloc
	  end function gsl_multifit_fdfridge_alloc
	  subroutine gsl_multifit_fdfridge_free(work) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: work
	  end subroutine gsl_multifit_fdfridge_free
	  function gsl_multifit_fdfridge_name(w) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: w
	    type(c_ptr) :: gsl_multifit_fdfridge_name
	  end function gsl_multifit_fdfridge_name
	  function gsl_multifit_fdfridge_position(w) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: w
	    type(c_ptr) :: gsl_multifit_fdfridge_position
	  end function gsl_multifit_fdfridge_position
	  function gsl_multifit_fdfridge_residual(w) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: w
	    type(c_ptr) :: gsl_multifit_fdfridge_residual
	  end function gsl_multifit_fdfridge_residual
	  function gsl_multifit_fdfridge_niter(w) bind(c)
	    import :: c_ptr, c_size_t
	    type(c_ptr), value :: w
	    integer(c_size_t) :: gsl_multifit_fdfridge_niter
	  end function gsl_multifit_fdfridge_niter
	  function gsl_multifit_fdfridge_set(w, f, x, lambda) bind(c)
	    import :: c_ptr, c_int, c_double
	    type(c_ptr), value :: w, f, x
	    real(c_double), value :: lambda
	    integer(c_int) :: gsl_multifit_fdfridge_set
	  end function gsl_multifit_fdfridge_set
	  function gsl_multifit_fdfridge_wset(w, f, x, lambda, wts) bind(c)
	    import :: c_ptr, c_int, c_double
	    type(c_ptr), value :: w, f, x, wts
	    real(c_double), value :: lambda
	    integer(c_int) :: gsl_multifit_fdfridge_wset
	  end function gsl_multifit_fdfridge_wset
	  function gsl_multifit_fdfridge_set2(w, f, x, lambda) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: w, f, x, lambda
	    integer(c_int) :: gsl_multifit_fdfridge_set2
	  end function gsl_multifit_fdfridge_set2
	  function gsl_multifit_fdfridge_wset2(w, f, x, lambda, wts) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: w, f, x, wts, lambda
	    integer(c_int) :: gsl_multifit_fdfridge_wset2
	  end function gsl_multifit_fdfridge_wset2
	  function gsl_multifit_fdfridge_set3(w, f, x, L) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: w, f, x, L
	    integer(c_int) :: gsl_multifit_fdfridge_set3
	  end function gsl_multifit_fdfridge_set3
	  function gsl_multifit_fdfridge_wset3(w, f, x, L, wts) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: w, f, x, L, wts
	    integer(c_int) :: gsl_multifit_fdfridge_wset3
	  end function gsl_multifit_fdfridge_wset3
	  function gsl_multifit_fdfridge_iterate(w) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: w
	    integer(c_int) :: gsl_multifit_fdfridge_iterate
	  end function gsl_multifit_fdfridge_iterate
	  function gsl_multifit_fdfridge_driver(w, maxiter, xtol, gtol, ftol, info) bind(c)
	    import :: c_ptr, c_int, c_size_t, c_double
	    type(c_ptr), value :: w
	    integer(c_size_t), value :: maxiter
	    real(c_double), value :: xtol, gtol, ftol
	    integer(c_int) :: info
	    integer(c_int) :: gsl_multifit_fdfridge_driver
	  end function gsl_multifit_fdfridge_driver
  end interface
  
contains
!
!>  API
  function fgsl_multifit_function_init(func, ndim, p, params)
    interface
       function func(x, params, f) bind(c)
         import :: c_ptr, c_int
         type(c_ptr), value :: x, params, f
         integer(c_int) :: func
       end function func
    end interface
    integer(fgsl_size_t), intent(in) :: ndim, p
    type(c_ptr), intent(in) :: params
    type(fgsl_multifit_function) :: fgsl_multifit_function_init
!
    type(c_funptr) :: fp
    fp = c_funloc(func)
    fgsl_multifit_function_init%gsl_multifit_function = &
         fgsl_multifit_function_cinit(fp, ndim, p, params)
  end function fgsl_multifit_function_init
  function fgsl_multifit_function_fdf_init(func, dfunc, fdfunc, ndim, p, &
    params)
    interface
       function func(x, params, f) bind(c)
         import :: c_ptr, c_int
         type(c_ptr), value :: x, params, f
         integer(c_int) :: func
       end function func
       function dfunc(x, params, df) bind(c)
         import :: c_ptr, c_int
         type(c_ptr), value :: x, params, df
         integer(c_int) :: dfunc
       end function dfunc
       function fdfunc(x, params, f, df) bind(c)
         import :: c_ptr, c_int
         type(c_ptr), value :: x, params, f, df
         integer(c_int) :: fdfunc
       end function fdfunc
    end interface
    integer(fgsl_size_t), intent(in) :: ndim, p
    type(c_ptr), intent(in) :: params
    type(fgsl_multifit_function_fdf) :: fgsl_multifit_function_fdf_init
!
    type(c_funptr) :: fp, dfp, fdfp
    fp = c_funloc(func)
    dfp = c_funloc(dfunc)
    fdfp = c_funloc(fdfunc)
    fgsl_multifit_function_fdf_init%gsl_multifit_function_fdf = &
         fgsl_multifit_function_fdf_cinit(fp, dfp, fdfp, ndim, p, params)
  end function fgsl_multifit_function_fdf_init
  subroutine fgsl_multifit_function_free(fun)
    type(fgsl_multifit_function), intent(inout) :: fun
    call fgsl_multifit_function_cfree(fun%gsl_multifit_function)
  end subroutine fgsl_multifit_function_free
  subroutine fgsl_multifit_function_fdf_free(fun)
    type(fgsl_multifit_function_fdf), intent(inout) :: fun
    call fgsl_multifit_function_fdf_cfree(fun%gsl_multifit_function_fdf)
  end subroutine fgsl_multifit_function_fdf_free
  function fgsl_multifit_fsolver_alloc(t, n, p)
    type(fgsl_multifit_fsolver_type), intent(in) :: t
    integer(fgsl_size_t), intent(in) :: n, p
    type(fgsl_multifit_fsolver) :: fgsl_multifit_fsolver_alloc
!
    type(c_ptr) :: ftype
    ftype = fgsl_aux_multifit_fsolver_alloc(t%which)
    fgsl_multifit_fsolver_alloc%gsl_multifit_fsolver = &
         gsl_multifit_fsolver_alloc(ftype, n, p)
  end function fgsl_multifit_fsolver_alloc
  function fgsl_multifit_fdfsolver_alloc(t, n, p)
    type(fgsl_multifit_fdfsolver_type), intent(in) :: t
    integer(fgsl_size_t), intent(in) :: n, p
    type(fgsl_multifit_fdfsolver) :: fgsl_multifit_fdfsolver_alloc
!
    type(c_ptr) :: ftype
    ftype = fgsl_aux_multifit_fdfsolver_alloc(t%which)
    fgsl_multifit_fdfsolver_alloc%gsl_multifit_fdfsolver = &
         gsl_multifit_fdfsolver_alloc(ftype, n, p)
  end function fgsl_multifit_fdfsolver_alloc
  subroutine fgsl_multifit_fsolver_free(s)
    type(fgsl_multifit_fsolver), intent(inout) :: s
    call gsl_multifit_fsolver_free(s%gsl_multifit_fsolver)
  end subroutine fgsl_multifit_fsolver_free
  subroutine fgsl_multifit_fdfsolver_free(s)
    type(fgsl_multifit_fdfsolver), intent(inout) :: s
    call gsl_multifit_fdfsolver_free(s%gsl_multifit_fdfsolver)
  end subroutine fgsl_multifit_fdfsolver_free
  function fgsl_multifit_fsolver_set(s, f, x)
    type(fgsl_multifit_fsolver), intent(inout) :: s
    type(fgsl_multifit_function), intent(in)  :: f
    type(fgsl_vector), intent(in)  :: x
    integer(fgsl_int) :: fgsl_multifit_fsolver_set
    fgsl_multifit_fsolver_set = gsl_multifit_fsolver_set(s%gsl_multifit_fsolver, &
         f%gsl_multifit_function, x%gsl_vector)
  end function fgsl_multifit_fsolver_set
  function fgsl_multifit_fdfsolver_set(s, fdf, x)
    type(fgsl_multifit_fdfsolver), intent(inout) :: s
    type(fgsl_multifit_function_fdf), intent(in)  :: fdf
    type(fgsl_vector), intent(in)  :: x
    integer(fgsl_int) :: fgsl_multifit_fdfsolver_set
    fgsl_multifit_fdfsolver_set = gsl_multifit_fdfsolver_set(s%gsl_multifit_fdfsolver, &
         fdf%gsl_multifit_function_fdf, x%gsl_vector)
  end function fgsl_multifit_fdfsolver_set
  function fgsl_multifit_fdfsolver_wset(s, fdf, x, wts)
    type(fgsl_multifit_fdfsolver), intent(inout) :: s
    type(fgsl_multifit_function_fdf), intent(in)  :: fdf
    type(fgsl_vector), intent(in)  :: x, wts
    integer(fgsl_int) :: fgsl_multifit_fdfsolver_wset
    fgsl_multifit_fdfsolver_wset = gsl_multifit_fdfsolver_wset(s%gsl_multifit_fdfsolver, &
         fdf%gsl_multifit_function_fdf, x%gsl_vector, wts%gsl_vector)
  end function fgsl_multifit_fdfsolver_wset
  function fgsl_multifit_fsolver_name(s)
    type(fgsl_multifit_fsolver), intent(in) :: s
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_multifit_fsolver_name
!
    type(c_ptr) :: name
!
    name = gsl_multifit_fsolver_name(s%gsl_multifit_fsolver)
    fgsl_multifit_fsolver_name = fgsl_name(name)
  end function fgsl_multifit_fsolver_name
  function fgsl_multifit_fdfsolver_name(s)
    type(fgsl_multifit_fdfsolver), intent(in) :: s
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_multifit_fdfsolver_name
!
    type(c_ptr) :: name
!
    name = gsl_multifit_fdfsolver_name(s%gsl_multifit_fdfsolver)
    fgsl_multifit_fdfsolver_name = fgsl_name(name)
  end function fgsl_multifit_fdfsolver_name
  function fgsl_multifit_fsolver_iterate(s)
    type(fgsl_multifit_fsolver), intent(in) :: s
    integer(fgsl_int) :: fgsl_multifit_fsolver_iterate
    fgsl_multifit_fsolver_iterate = &
         gsl_multifit_fsolver_iterate(s%gsl_multifit_fsolver)
  end function fgsl_multifit_fsolver_iterate
  function fgsl_multifit_fdfsolver_iterate(s)
    type(fgsl_multifit_fdfsolver), intent(in) :: s
    integer(fgsl_int) :: fgsl_multifit_fdfsolver_iterate
    fgsl_multifit_fdfsolver_iterate = &
         gsl_multifit_fdfsolver_iterate(s%gsl_multifit_fdfsolver)
  end function fgsl_multifit_fdfsolver_iterate
  function fgsl_multifit_fsolver_position(s)
    type(fgsl_multifit_fsolver), intent(in) :: s
    type(fgsl_vector) :: fgsl_multifit_fsolver_position
    fgsl_multifit_fsolver_position%gsl_vector = &
         gsl_multifit_fsolver_position(s%gsl_multifit_fsolver)
  end function fgsl_multifit_fsolver_position
  function fgsl_multifit_fdfsolver_position(s)
    type(fgsl_multifit_fdfsolver), intent(in) :: s
    type(fgsl_vector) :: fgsl_multifit_fdfsolver_position
    fgsl_multifit_fdfsolver_position%gsl_vector = &
         gsl_multifit_fdfsolver_position(s%gsl_multifit_fdfsolver)
  end function fgsl_multifit_fdfsolver_position
  function fgsl_multifit_fdfsolver_dx(s)
    type(fgsl_multifit_fdfsolver), intent(in) :: s
    type(fgsl_vector) :: fgsl_multifit_fdfsolver_dx
    fgsl_multifit_fdfsolver_dx%gsl_vector = &
         gsl_multifit_fdfsolver_dx(s%gsl_multifit_fdfsolver)
  end function fgsl_multifit_fdfsolver_dx
  function fgsl_multifit_fdfsolver_f(s)
    type(fgsl_multifit_fdfsolver), intent(in) :: s
    type(fgsl_vector) :: fgsl_multifit_fdfsolver_f
    fgsl_multifit_fdfsolver_f%gsl_vector = &
         gsl_multifit_fdfsolver_f(s%gsl_multifit_fdfsolver)
  end function fgsl_multifit_fdfsolver_f
  function fgsl_multifit_fdfsolver_jac(s, J)
    type(fgsl_multifit_fdfsolver), intent(in) :: s
    type(fgsl_matrix), intent(inout) :: J
    integer(fgsl_int) :: fgsl_multifit_fdfsolver_jac
    fgsl_multifit_fdfsolver_jac = &
         gsl_multifit_fdfsolver_jac(s%gsl_multifit_fdfsolver, &
         J%gsl_matrix)
  end function fgsl_multifit_fdfsolver_jac
  function fgsl_multifit_test_delta(dx, x, epsabs, epsrel)
    type(fgsl_vector), intent(in) :: dx, x
    real(fgsl_double), intent(in) :: epsabs, epsrel
    integer(fgsl_int) :: fgsl_multifit_test_delta
    fgsl_multifit_test_delta = gsl_multifit_test_delta(dx%gsl_vector, x%gsl_vector, &
         epsabs, epsrel)
  end function fgsl_multifit_test_delta
  function fgsl_multifit_test_gradient(g, epsabs)
    type(fgsl_vector), intent(in) :: g
    real(fgsl_double), intent(in) :: epsabs
    integer(fgsl_int) :: fgsl_multifit_test_gradient
    fgsl_multifit_test_gradient = gsl_multifit_test_gradient(g%gsl_vector, epsabs)
  end function fgsl_multifit_test_gradient
  function fgsl_multifit_gradient(j, f, g)
    type(fgsl_matrix), intent(in) :: j
    type(fgsl_vector), intent(in) :: f
    type(fgsl_vector), intent(inout) :: g
    integer(fgsl_int) :: fgsl_multifit_gradient
    fgsl_multifit_gradient = gsl_multifit_gradient(j%gsl_matrix, f%gsl_vector, &
         g%gsl_vector)
  end function fgsl_multifit_gradient
  function fgsl_multifit_covar(j, epsrel, covar)
    type(fgsl_matrix), intent(in) :: j
    real(fgsl_double), intent(in) :: epsrel
    type(fgsl_matrix), intent(inout) :: covar
    integer(fgsl_int) :: fgsl_multifit_covar
    fgsl_multifit_covar = gsl_multifit_covar(j%gsl_matrix, epsrel, &
         covar%gsl_matrix)
  end function fgsl_multifit_covar
  function fgsl_multifit_covar_qrpt(r, perm, epsrel, covar)
    type(fgsl_matrix), intent(inout) :: r
    type(fgsl_permutation), intent(inout) :: perm
    real(fgsl_double), intent(in) :: epsrel
    type(fgsl_matrix), intent(inout) :: covar
    integer(fgsl_int) :: fgsl_multifit_covar_qrpt
    fgsl_multifit_covar_qrpt = gsl_multifit_covar_qrpt(r%gsl_matrix, &
         perm%gsl_permutation, epsrel, &
         covar%gsl_matrix)
  end function fgsl_multifit_covar_qrpt
  function fgsl_multifit_fsolver_status(s)
    type(fgsl_multifit_fsolver), intent(in) :: s
    logical :: fgsl_multifit_fsolver_status
    fgsl_multifit_fsolver_status = .false.
    if (c_associated(s%gsl_multifit_fsolver)) &
         fgsl_multifit_fsolver_status = .true.
  end function fgsl_multifit_fsolver_status
  function fgsl_multifit_fdfsolver_status(s)
    type(fgsl_multifit_fdfsolver), intent(in) :: s
    logical :: fgsl_multifit_fdfsolver_status
    fgsl_multifit_fdfsolver_status = .false.
    if (c_associated(s%gsl_multifit_fdfsolver)) &
         fgsl_multifit_fdfsolver_status = .true.
  end function fgsl_multifit_fdfsolver_status

  function fgsl_multifit_fsolver_driver(s, maxiter, epsabs, epsrel)
    type(fgsl_multifit_fsolver), intent(inout) :: s
    integer(fgsl_size_t), intent(in) :: maxiter
    real(fgsl_double), intent(in) :: epsabs, epsrel
    integer(fgsl_int) :: fgsl_multifit_fsolver_driver
    fgsl_multifit_fsolver_driver = gsl_multifit_fsolver_driver(&
    s%gsl_multifit_fsolver, maxiter, epsabs, epsrel)
  end function fgsl_multifit_fsolver_driver
  function fgsl_multifit_fdfsolver_driver(s, maxiter, xtol, gtol, ftol, info)
    type(fgsl_multifit_fdfsolver), intent(inout) :: s
    integer(fgsl_size_t), intent(in) :: maxiter
    real(fgsl_double), intent(in) :: xtol, gtol, ftol
    integer(fgsl_int), intent(out) :: info
    integer(fgsl_int) :: fgsl_multifit_fdfsolver_driver
    fgsl_multifit_fdfsolver_driver = gsl_multifit_fdfsolver_driver(&
    s%gsl_multifit_fdfsolver, maxiter, xtol, gtol, ftol, info)
  end function fgsl_multifit_fdfsolver_driver
  function fgsl_multifit_fdfsolver_dif_df_wts(x, wts, fdf, f, J)
    type(fgsl_vector), intent(in) :: x, f, wts
    type(fgsl_multifit_function_fdf), intent(inout) :: fdf
    type(fgsl_matrix), intent(inout) :: J
    integer(fgsl_int) :: fgsl_multifit_fdfsolver_dif_df_wts
    fgsl_multifit_fdfsolver_dif_df_wts = gsl_multifit_fdfsolver_dif_df(&
        x%gsl_vector, wts%gsl_vector, fdf%gsl_multifit_function_fdf, f%gsl_vector,&
        J%gsl_matrix)
  end function fgsl_multifit_fdfsolver_dif_df_wts
  function fgsl_multifit_fdfsolver_dif_df_nowts(x, fdf, f, J)
    type(fgsl_vector), intent(in) :: x, f
    type(fgsl_multifit_function_fdf), intent(inout) :: fdf
    type(fgsl_matrix), intent(inout) :: J
    integer(fgsl_int) :: fgsl_multifit_fdfsolver_dif_df_nowts
    fgsl_multifit_fdfsolver_dif_df_nowts = gsl_multifit_fdfsolver_dif_df(&
        x%gsl_vector, c_null_ptr, fdf%gsl_multifit_function_fdf, f%gsl_vector,&
        J%gsl_matrix)
  end function fgsl_multifit_fdfsolver_dif_df_nowts
  function fgsl_multifit_robust_alloc(t, n, p)
    type(fgsl_multifit_robust_type), intent(in) :: t
    integer(fgsl_size_t), intent(in) :: n, p
    type(fgsl_multifit_robust_workspace) :: fgsl_multifit_robust_alloc
    type(c_ptr) :: ftype
    ftype = fgsl_aux_multifit_robust_alloc(t%which)
    fgsl_multifit_robust_alloc%gsl_multifit_robust_workspace = &
         gsl_multifit_robust_alloc(ftype, n, p)
  end function fgsl_multifit_robust_alloc
  subroutine fgsl_multifit_robust_free(w)
    type(fgsl_multifit_robust_workspace), intent(inout) :: w
    call gsl_multifit_robust_free(w%gsl_multifit_robust_workspace)
  end subroutine fgsl_multifit_robust_free
  function fgsl_multifit_robust_tune(tune, w)
    real(fgsl_double), intent(in) :: tune
    type(fgsl_multifit_robust_workspace), intent(in) :: w
    integer(fgsl_int) :: fgsl_multifit_robust_tune
    fgsl_multifit_robust_tune = gsl_multifit_robust_tune(tune,&
    w%gsl_multifit_robust_workspace)
  end function fgsl_multifit_robust_tune
  function fgsl_multifit_robust_name(w)
    type(fgsl_multifit_robust_workspace), intent(in) :: w
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_multifit_robust_name
    type(c_ptr) :: name
    name = gsl_multifit_robust_name(w%gsl_multifit_robust_workspace)
    fgsl_multifit_robust_name = fgsl_name(name)
  end function fgsl_multifit_robust_name
  function fgsl_multifit_robust_statistics(w)
    type(fgsl_multifit_robust_workspace), intent(in) :: w
    type(fgsl_multifit_robust_stats) :: fgsl_multifit_robust_statistics
    type(gsl_multifit_robust_stats) :: stats

    stats = gsl_multifit_robust_statistics(w%gsl_multifit_robust_workspace)
    fgsl_multifit_robust_statistics%sigma_ols = stats%sigma_ols
    fgsl_multifit_robust_statistics%sigma_mad = stats%sigma_mad
    fgsl_multifit_robust_statistics%sigma_rob = stats%sigma_rob
    fgsl_multifit_robust_statistics%sigma = stats%sigma
    fgsl_multifit_robust_statistics%Rsq = stats%Rsq
    fgsl_multifit_robust_statistics%adj_Rsq = stats%adj_Rsq
    fgsl_multifit_robust_statistics%rmse = stats%rmse
    fgsl_multifit_robust_statistics%sse = stats%sse
    fgsl_multifit_robust_statistics%dof = stats%dof
    fgsl_multifit_robust_statistics%numit = stats%numit
    fgsl_multifit_robust_statistics%r%gsl_vector = stats%r
    fgsl_multifit_robust_statistics%weights%gsl_vector = stats%weights
  end function fgsl_multifit_robust_statistics
  function fgsl_multifit_robust(X, y, c, cov, w)
    type(fgsl_matrix), intent(in) :: X
    type(fgsl_vector), intent(in) :: y
    type(fgsl_vector), intent(inout) :: c
    type(fgsl_matrix), intent(inout) :: cov
    type(fgsl_multifit_robust_workspace), intent(inout) :: w
    integer(c_int) :: fgsl_multifit_robust

    fgsl_multifit_robust = gsl_multifit_robust(X%gsl_matrix,&
    y%gsl_vector, c%gsl_vector,cov%gsl_matrix,&
    w%gsl_multifit_robust_workspace)
  end function fgsl_multifit_robust
  function fgsl_multifit_robust_est(x, c, cov, y, y_err)
    type(fgsl_vector), intent(in) :: x, c
    type(fgsl_matrix), intent(in) :: cov
    real(c_double), intent(out) :: y, y_err
    integer(c_int) :: fgsl_multifit_robust_est
    fgsl_multifit_robust_est = gsl_multifit_robust_est(x%gsl_vector, c%gsl_vector,&
    cov%gsl_matrix, y, y_err)
  end function fgsl_multifit_robust_est

  function fgsl_multifit_fdfsolver_residual(s)
    type(fgsl_multifit_fdfsolver), intent(in) :: s
    type(fgsl_vector) :: fgsl_multifit_fdfsolver_residual
    fgsl_multifit_fdfsolver_residual%gsl_vector = &
    gsl_multifit_fdfsolver_residual(s%gsl_multifit_fdfsolver)
  end function fgsl_multifit_fdfsolver_residual
  function fgsl_multifit_fdfsolver_niter(s)
    type(fgsl_multifit_fdfsolver), intent(in) :: s
    integer(fgsl_size_t) :: fgsl_multifit_fdfsolver_niter
    fgsl_multifit_fdfsolver_niter = &
    gsl_multifit_fdfsolver_niter(s%gsl_multifit_fdfsolver)
  end function fgsl_multifit_fdfsolver_niter
  function fgsl_multifit_eval_wf_wts(fdf, x, wts, y)
    type(fgsl_multifit_function_fdf), intent(inout) :: fdf
    type(fgsl_vector), intent(in) :: x, wts
    type(fgsl_vector), intent(inout) :: y
    integer(fgsl_int) :: fgsl_multifit_eval_wf_wts
    fgsl_multifit_eval_wf_wts = gsl_multifit_eval_wf(fdf%gsl_multifit_function_fdf, &
    x%gsl_vector, wts%gsl_vector, y%gsl_vector)
  end function fgsl_multifit_eval_wf_wts
  function fgsl_multifit_eval_wf_nowts(fdf, x, y)
    type(fgsl_multifit_function_fdf), intent(inout) :: fdf
    type(fgsl_vector), intent(in) :: x
    type(fgsl_vector), intent(inout) :: y
    integer(fgsl_int) :: fgsl_multifit_eval_wf_nowts
    fgsl_multifit_eval_wf_nowts = gsl_multifit_eval_wf(fdf%gsl_multifit_function_fdf, &
    x%gsl_vector, c_null_ptr, y%gsl_vector)
  end function fgsl_multifit_eval_wf_nowts
  function fgsl_multifit_eval_wdf_wts(fdf, x, wts, dy)
    type(fgsl_multifit_function_fdf), intent(inout) :: fdf
    type(fgsl_vector), intent(in) :: x, wts
    type(fgsl_matrix), intent(inout) :: dy
    integer(fgsl_int) :: fgsl_multifit_eval_wdf_wts
    fgsl_multifit_eval_wdf_wts = gsl_multifit_eval_wdf(fdf%gsl_multifit_function_fdf, &
    x%gsl_vector, wts%gsl_vector, dy%gsl_matrix)
  end function fgsl_multifit_eval_wdf_wts
  function fgsl_multifit_eval_wdf_nowts(fdf, x, dy)
    type(fgsl_multifit_function_fdf), intent(inout) :: fdf
    type(fgsl_vector), intent(in) :: x
    type(fgsl_matrix), intent(inout) :: dy
    integer(fgsl_int) :: fgsl_multifit_eval_wdf_nowts
    fgsl_multifit_eval_wdf_nowts = gsl_multifit_eval_wdf(fdf%gsl_multifit_function_fdf, &
    x%gsl_vector, c_null_ptr, dy%gsl_matrix)
  end function fgsl_multifit_eval_wdf_nowts
  function fgsl_multifit_fdfsolver_test(s, xtol, gtol, ftol, info)
    type(fgsl_multifit_fdfsolver), intent(in) :: s
    real(fgsl_double), intent(in) :: xtol, gtol, ftol
    integer(fgsl_int), intent(out) :: info
    integer(fgsl_int) :: fgsl_multifit_fdfsolver_test
    fgsl_multifit_fdfsolver_test = gsl_multifit_fdfsolver_test(s%gsl_multifit_fdfsolver, xtol, gtol, ftol, info)
  end function fgsl_multifit_fdfsolver_test

  function fgsl_multifit_linear_alloc(n, p)
    integer(fgsl_size_t), intent(in) :: n, p
    type(fgsl_multifit_linear_workspace) :: fgsl_multifit_linear_alloc
    fgsl_multifit_linear_alloc%gsl_multifit_linear_workspace = &
         gsl_multifit_linear_alloc(n, p)
  end function fgsl_multifit_linear_alloc
  subroutine fgsl_multifit_linear_free(w)
    type(fgsl_multifit_linear_workspace), intent(inout) :: w
    call gsl_multifit_linear_free(w%gsl_multifit_linear_workspace)
  end subroutine fgsl_multifit_linear_free
  function fgsl_multifit_linear(x, y, c, cov, chisq, work)
    type(fgsl_matrix), intent(in) :: x
    type(fgsl_vector), intent(in) :: y
    type(fgsl_vector), intent(inout) :: c
    type(fgsl_matrix), intent(inout) :: cov
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    real(fgsl_double), intent(inout) :: chisq
    integer(fgsl_int) :: fgsl_multifit_linear
    fgsl_multifit_linear = gsl_multifit_linear(x%gsl_matrix, y%gsl_vector, &
         c%gsl_vector, cov%gsl_matrix, chisq, work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear
  function fgsl_multifit_linear_tsvd(x, y, tol, c, cov, chisq, rank, work)
    type(fgsl_matrix), intent(in) :: x
    type(fgsl_vector), intent(in) :: y
    real(fgsl_double), intent(in) :: tol
    type(fgsl_vector), intent(inout) :: c
    type(fgsl_matrix), intent(inout) :: cov
    integer(fgsl_size_t), intent(inout) :: rank
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    real(fgsl_double), intent(inout) :: chisq
    integer(fgsl_int) :: fgsl_multifit_linear_tsvd
    fgsl_multifit_linear_tsvd = gsl_multifit_linear_tsvd(x%gsl_matrix, y%gsl_vector, tol, &
         c%gsl_vector, cov%gsl_matrix, chisq, rank, work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear_tsvd
  function fgsl_multifit_linear_svd(x, work)
    type(fgsl_matrix), intent(in) :: x
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_multifit_linear_svd
    fgsl_multifit_linear_svd = gsl_multifit_linear_svd(x%gsl_matrix, work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear_svd
  function fgsl_multifit_linear_bsvd(X, work)
    type(fgsl_matrix), intent(in) :: X
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_multifit_linear_bsvd
    fgsl_multifit_linear_bsvd = gsl_multifit_linear_bsvd(X%gsl_matrix, &
    work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear_bsvd
  function fgsl_multifit_linear_solve(lambda, X, y, c, rnorm, snorm, work)
    real(fgsl_double), intent(in) :: lambda
    type(fgsl_matrix), intent(in) :: X
    type(fgsl_vector), intent(in) :: y
    type(fgsl_vector), intent(inout) :: c
    real(fgsl_double), intent(out) :: rnorm, snorm
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_multifit_linear_solve
    fgsl_multifit_linear_solve = gsl_multifit_linear_solve(lambda, &
    X%gsl_matrix, y%gsl_vector, c%gsl_vector, rnorm, snorm, &
    work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear_solve
  function fgsl_multifit_linear_applyw(X, w, y, WX, Wy)
    type(fgsl_matrix), intent(in) :: X
    type(fgsl_vector), intent(in) :: w, y
    type(fgsl_matrix), intent(inout) :: WX
    type(fgsl_vector), intent(inout) :: Wy
    integer(fgsl_int) :: fgsl_multifit_linear_applyw
    fgsl_multifit_linear_applyw = gsl_multifit_linear_applyw( &
    X%gsl_matrix, w%gsl_vector, y%gsl_vector, WX%gsl_matrix, &
    Wy%gsl_vector)
  end function fgsl_multifit_linear_applyw
  function fgsl_multifit_linear_stdform1(L, X, y, Xs, ys, work)
    type(fgsl_vector), intent(in) :: L, y
    type(fgsl_matrix), intent(in) :: X
    type(fgsl_matrix), intent(inout) :: Xs
    type(fgsl_vector), intent(inout) :: ys
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_multifit_linear_stdform1
    fgsl_multifit_linear_stdform1 = gsl_multifit_linear_stdform1(L%gsl_vector, &
    X%gsl_matrix, y%gsl_vector, Xs%gsl_matrix, ys%gsl_vector, &
    work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear_stdform1
  function fgsl_multifit_linear_wstdform1(L, X, w, y, Xs, ys, work)
    type(fgsl_vector), intent(in) :: L, w, y
    type(fgsl_matrix), intent(in) :: X
    type(fgsl_matrix), intent(inout) :: Xs
    type(fgsl_vector), intent(inout) :: ys
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_multifit_linear_wstdform1
    fgsl_multifit_linear_wstdform1 = gsl_multifit_linear_wstdform1(L%gsl_vector, &
    X%gsl_matrix, w%gsl_vector, y%gsl_vector, Xs%gsl_matrix, ys%gsl_vector, &
    work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear_wstdform1
  function fgsl_multifit_linear_l_decomp(L, tau)
    type(fgsl_matrix), intent(inout) :: l
    type(fgsl_vector), intent(inout) :: tau
    integer(fgsl_int) :: fgsl_multifit_linear_l_decomp
    fgsl_multifit_linear_l_decomp = gsl_multifit_linear_l_decomp(&
    l%gsl_matrix, tau%gsl_vector)
  end function fgsl_multifit_linear_l_decomp
  function fgsl_multifit_linear_stdform2(LQR, Ltau, X, y, Xs, ys, M, work)
    type(fgsl_matrix), intent(in) :: LQR, X
    type(fgsl_vector), intent(in) :: Ltau, y
    type(fgsl_matrix), intent(inout) :: Xs, M
    type(fgsl_vector), intent(inout) :: ys
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_multifit_linear_stdform2
    fgsl_multifit_linear_stdform2 = gsl_multifit_linear_stdform2(&
    LQR%gsl_matrix, Ltau%gsl_vector, X%gsl_matrix, y%gsl_vector, &
    Xs%gsl_matrix, ys%gsl_vector, M%gsl_matrix, work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear_stdform2
  function fgsl_multifit_linear_wstdform2(LQR, Ltau, X, w, y, Xs, ys, M, work)
    type(fgsl_matrix), intent(in) :: LQR, X
    type(fgsl_vector), intent(in) :: Ltau, w, y
    type(fgsl_matrix), intent(inout) :: Xs, M
    type(fgsl_vector), intent(inout) :: ys
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_multifit_linear_wstdform2
    fgsl_multifit_linear_wstdform2 = gsl_multifit_linear_wstdform2(&
    LQR%gsl_matrix, Ltau%gsl_vector, X%gsl_matrix, w%gsl_vector, y%gsl_vector, &
    Xs%gsl_matrix, ys%gsl_vector, M%gsl_matrix, work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear_wstdform2
  function fgsl_multifit_linear_genform1(L, cs, c, work)
    type(fgsl_vector), intent(in) :: L, cs
    type(fgsl_vector), intent(inout) :: c
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_multifit_linear_genform1
    fgsl_multifit_linear_genform1 = gsl_multifit_linear_genform1(&
    L%gsl_vector, cs%gsl_vector, c%gsl_vector, &
    work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear_genform1
  function fgsl_multifit_linear_genform2(LQR, Ltau, X, y, cs, M, c, work)
    type(fgsl_matrix), intent(in) :: LQR, X, M
    type(fgsl_vector), intent(in) :: Ltau, y, cs
    type(fgsl_vector), intent(inout) :: c
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_multifit_linear_genform2
    fgsl_multifit_linear_genform2 = gsl_multifit_linear_genform2(&
    LQR%gsl_matrix, Ltau%gsl_vector, X%gsl_matrix, y%gsl_vector, &
    cs%gsl_vector, M%gsl_matrix, c%gsl_vector, work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear_genform2
  function fgsl_multifit_linear_wgenform2(LQR, Ltau, X, w, y, cs, M, c, work)
    type(fgsl_matrix), intent(in) :: LQR, X, M
    type(fgsl_vector), intent(in) :: Ltau, w, y, cs
    type(fgsl_vector), intent(inout) :: c
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_multifit_linear_wgenform2
    fgsl_multifit_linear_wgenform2 = gsl_multifit_linear_wgenform2(&
    LQR%gsl_matrix, Ltau%gsl_vector, X%gsl_matrix, &
    w%gsl_vector, y%gsl_vector, cs%gsl_vector, &
    M%gsl_matrix, c%gsl_vector, work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear_wgenform2
  function fgsl_multifit_linear_lreg(smin, smax, reg_param)
    real(fgsl_double), intent(in) :: smin, smax
    type(fgsl_vector), intent(inout) :: reg_param
    integer(fgsl_int) :: fgsl_multifit_linear_lreg
    fgsl_multifit_linear_lreg = gsl_multifit_linear_lreg(smin, smax, &
    reg_param%gsl_vector)
  end function fgsl_multifit_linear_lreg
  function fgsl_multifit_linear_lcurve(y, reg_param, rho, eta, work)
    type(fgsl_vector), intent(in) :: y
    type(fgsl_vector), intent(inout) :: reg_param, rho, eta
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_multifit_linear_lcurve
    fgsl_multifit_linear_lcurve = gsl_multifit_linear_lcurve(y%gsl_vector, &
    reg_param%gsl_vector, rho%gsl_vector, eta%gsl_vector, &
    work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear_lcurve
  function fgsl_multifit_linear_lcurvature(y, reg_param, rho, eta, kappa, work)
    type(fgsl_vector), intent(in) :: y, reg_param, rho, eta
    type(fgsl_vector), intent(inout) :: kappa
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_multifit_linear_lcurvature
    fgsl_multifit_linear_lcurvature = gsl_multifit_linear_lcurvature( &
         y%gsl_vector, reg_param%gsl_vector, rho%gsl_vector, eta%gsl_vector, &
         kappa%gsl_vector, work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear_lcurvature
  function fgsl_multifit_linear_lcorner(rho, eta, idx)
    type(fgsl_vector), intent(in) :: rho, eta
    integer(fgsl_size_t), intent(out) :: idx
    integer(fgsl_int) :: fgsl_multifit_linear_lcorner
    fgsl_multifit_linear_lcorner = gsl_multifit_linear_lcorner(&
    rho%gsl_vector, eta%gsl_vector, idx)
  end function fgsl_multifit_linear_lcorner
  function fgsl_multifit_linear_lcorner2(reg_param, eta, idx)
    type(fgsl_vector), intent(in) :: reg_param, eta
    integer(fgsl_size_t), intent(out) :: idx
    integer(fgsl_int) :: fgsl_multifit_linear_lcorner2
    fgsl_multifit_linear_lcorner2 = gsl_multifit_linear_lcorner2(&
    reg_param%gsl_vector, eta%gsl_vector, idx)
  end function fgsl_multifit_linear_lcorner2
  integer(fgsl_int) function fgsl_multifit_linear_gcv_init(y, reg_param, uty, delta0, work)
    type(fgsl_vector), intent(in) :: y
    type(fgsl_vector), intent(inout) :: reg_param, uty
    real(fgsl_double), intent(inout) :: delta0
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    fgsl_multifit_linear_gcv_init = gsl_multifit_linear_gcv_init(y%gsl_vector, &
          reg_param%gsl_vector, uty%gsl_vector, delta0, work%gsl_multifit_linear_workspace)
  end function
  integer(fgsl_int) function fgsl_multifit_linear_gcv_curve(reg_param, uty, delta0, g, work)
    type(fgsl_vector), intent(in) :: reg_param, uty
    type(fgsl_vector), intent(inout) :: g
    real(fgsl_double), intent(in) :: delta0
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    fgsl_multifit_linear_gcv_curve = gsl_multifit_linear_gcv_curve(reg_param%gsl_vector, &
          uty%gsl_vector, delta0, g%gsl_vector, work%gsl_multifit_linear_workspace)
  end function
  integer(fgsl_int) function fgsl_multifit_linear_gcv_min(reg_param, uty, delta0, g, & 
          lambda, work)
    type(fgsl_vector), intent(in) :: reg_param, uty, g
    real(fgsl_double), intent(in) :: delta0
    real(fgsl_double), intent(inout) :: lambda
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    fgsl_multifit_linear_gcv_min = gsl_multifit_linear_gcv_min(reg_param%gsl_vector, &
          uty%gsl_vector, delta0, g%gsl_vector, lambda, work%gsl_multifit_linear_workspace)
  end function
  real(fgsl_double) function fgsl_multifit_linear_gcv_calc(lambda, uty, delta0, work)
    type(fgsl_vector), intent(in) :: uty
    real(fgsl_double), intent(in) :: delta0, lambda
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    fgsl_multifit_linear_gcv_calc = gsl_multifit_linear_gcv_calc(lambda, &
          uty%gsl_vector, delta0, work%gsl_multifit_linear_workspace)
  end function
  integer(fgsl_int) function fgsl_multifit_linear_gcv(y, reg_param, g, lambda, g_lambda, work)
    type(fgsl_vector), intent(in) :: y
    type(fgsl_vector), intent(inout) :: reg_param, g
    real(fgsl_double), intent(inout) :: lambda, g_lambda
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    fgsl_multifit_linear_gcv = gsl_multifit_linear_gcv(y%gsl_vector, reg_param%gsl_vector, &
          g%gsl_vector, lambda, g_lambda, work%gsl_multifit_linear_workspace)
  end function
  function fgsl_multifit_linear_lk(p, k, l)
    integer(fgsl_size_t), intent(in) :: p, k
    type(fgsl_matrix), intent(inout) :: l
    integer(fgsl_int) :: fgsl_multifit_linear_lk
    fgsl_multifit_linear_lk = gsl_multifit_linear_lk(p, k, l%gsl_matrix)
  end function fgsl_multifit_linear_lk
  function fgsl_multifit_linear_lsobolev(p, kmax, alpha, l, work)
    integer(fgsl_size_t), intent(in) :: p, kmax
    type(fgsl_vector), intent(in) :: alpha
    type(fgsl_matrix), intent(inout) :: L
    type(fgsl_multifit_linear_workspace) :: work
    integer(fgsl_int) :: fgsl_multifit_linear_lsobolev
    fgsl_multifit_linear_lsobolev = gsl_multifit_linear_lsobolev(p, kmax, &
    alpha%gsl_vector, L%gsl_matrix, work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear_lsobolev
  function fgsl_multifit_linear_rcond(w)
    type(fgsl_multifit_linear_workspace), intent(in) :: w
    real(fgsl_double) :: fgsl_multifit_linear_rcond
    fgsl_multifit_linear_rcond = gsl_multifit_linear_rcond(&
    w%gsl_multifit_linear_workspace)
  end function fgsl_multifit_linear_rcond
  function fgsl_multifit_robust_maxiter(maxiter, w)
    integer(fgsl_size_t), intent(in) :: maxiter
    type(fgsl_multifit_robust_workspace), intent(inout) :: w
    integer(fgsl_int) :: fgsl_multifit_robust_maxiter
    fgsl_multifit_robust_maxiter = gsl_multifit_robust_maxiter(&
    maxiter, w%gsl_multifit_robust_workspace)
  end function fgsl_multifit_robust_maxiter
  function fgsl_multifit_robust_residuals(X, y, c, r, w)
    type(fgsl_matrix), intent(in) :: X
    type(fgsl_vector), intent(in) :: y, c
    type(fgsl_vector), intent(inout) :: r
    type(fgsl_multifit_robust_workspace), intent(inout) :: w
    integer(fgsl_int) :: fgsl_multifit_robust_residuals
    fgsl_multifit_robust_residuals = gsl_multifit_robust_residuals(&
    X%gsl_matrix, y%gsl_vector, c%gsl_vector, r%gsl_vector, &
    w%gsl_multifit_robust_workspace)
  end function fgsl_multifit_robust_residuals
  function fgsl_multifit_robust_weights(r, wts, w)
    type(fgsl_vector), intent(in) :: r
    type(fgsl_vector), intent(inout) :: wts
    type(fgsl_multifit_robust_workspace), intent(inout) :: w
    integer(fgsl_int) :: fgsl_multifit_robust_weights
    fgsl_multifit_robust_weights = gsl_multifit_robust_weights(&
    r%gsl_vector, wts%gsl_vector, w%gsl_multifit_robust_workspace)
  end function fgsl_multifit_robust_weights

  function fgsl_multifit_wlinear(x, w, y, c, cov, chisq, work)
    type(fgsl_matrix), intent(in) :: x
    type(fgsl_vector), intent(in) :: w, y
    type(fgsl_vector), intent(inout) :: c
    type(fgsl_matrix), intent(inout) :: cov
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    real(fgsl_double), intent(inout) :: chisq
    integer(fgsl_int) :: fgsl_multifit_wlinear
    fgsl_multifit_wlinear = gsl_multifit_wlinear(x%gsl_matrix, w%gsl_vector, y%gsl_vector, &
         c%gsl_vector, cov%gsl_matrix, chisq, work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_wlinear
  function fgsl_multifit_wlinear_tsvd(x, w, y, tol, c, cov, chisq, rank, work)
    type(fgsl_matrix), intent(in) :: x
    type(fgsl_vector), intent(in) :: w, y
    real(fgsl_double), intent(in) :: tol
    type(fgsl_vector), intent(inout) :: c
    type(fgsl_matrix), intent(inout) :: cov
    integer(fgsl_size_t), intent(inout) :: rank
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    real(fgsl_double), intent(inout) :: chisq
    integer(fgsl_int) :: fgsl_multifit_wlinear_tsvd
    fgsl_multifit_wlinear_tsvd = gsl_multifit_wlinear_tsvd( &
         x%gsl_matrix, w%gsl_vector, y%gsl_vector, tol, &
         c%gsl_vector, cov%gsl_matrix, chisq, rank, work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_wlinear_tsvd
  function fgsl_multifit_wlinear_svd(x, w, y, tol, rank, c, cov, chisq, work)
    type(fgsl_matrix), intent(in) :: x
    type(fgsl_vector), intent(in) :: w, y
    real(fgsl_double), intent(in) :: tol
    integer(fgsl_size_t), intent(inout) :: rank
    type(fgsl_vector), intent(inout) :: c
    type(fgsl_matrix), intent(inout) :: cov
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    real(fgsl_double), intent(inout) :: chisq
    integer(fgsl_int) :: fgsl_multifit_wlinear_svd
    fgsl_multifit_wlinear_svd = gsl_multifit_wlinear_svd(x%gsl_matrix, w%gsl_vector, &
         y%gsl_vector, tol, rank, c%gsl_vector, cov%gsl_matrix, chisq, &
         work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_wlinear_svd
  function fgsl_multifit_wlinear_usvd(x, w, y, tol, rank, c, cov, chisq, work)
    type(fgsl_matrix), intent(in) :: x
    type(fgsl_vector), intent(in) :: w, y
    real(fgsl_double), intent(in) :: tol
    integer(fgsl_size_t), intent(inout) :: rank
    type(fgsl_vector), intent(inout) :: c
    type(fgsl_matrix), intent(inout) :: cov
    type(fgsl_multifit_linear_workspace), intent(inout) :: work
    real(fgsl_double), intent(inout) :: chisq
    integer(fgsl_int) :: fgsl_multifit_wlinear_usvd
    fgsl_multifit_wlinear_usvd = gsl_multifit_wlinear_usvd(x%gsl_matrix, w%gsl_vector, &
         y%gsl_vector, tol, rank, c%gsl_vector, cov%gsl_matrix, chisq, &
         work%gsl_multifit_linear_workspace)
  end function fgsl_multifit_wlinear_usvd
  function fgsl_multifit_linear_est(x, c, cov, y, y_err)
    type(fgsl_vector), intent(in) :: x, c
    type(fgsl_matrix), intent(in) :: cov
    real(fgsl_double), intent(inout) :: y, y_err
    integer(fgsl_int) :: fgsl_multifit_linear_est
    fgsl_multifit_linear_est = gsl_multifit_linear_est(x%gsl_vector, c%gsl_vector, &
         cov%gsl_matrix, y, y_err)
  end function fgsl_multifit_linear_est
  function fgsl_multifit_linear_residuals(x, y, c, r)
    type(fgsl_matrix), intent(in) :: x
    type(fgsl_vector), intent(in) :: y, c
    type(fgsl_vector), intent(inout) :: r
    integer(fgsl_int) :: fgsl_multifit_linear_residuals
    fgsl_multifit_linear_residuals = gsl_multifit_linear_residuals( &
         x%gsl_matrix, y%gsl_vector, c%gsl_vector, r%gsl_vector)
  end function fgsl_multifit_linear_residuals
  integer(fgsl_size_t) function fgsl_multifit_linear_rank(tol, work)
    real(fgsl_double), intent(in) :: tol
    type(fgsl_multifit_linear_workspace), intent(in) :: work
    fgsl_multifit_linear_rank = gsl_multifit_linear_rank(tol, work%gsl_multifit_linear_workspace)
  end function
  function fgsl_multifit_status(multifit)
    type(fgsl_multifit_linear_workspace), intent(in) :: multifit
    logical :: fgsl_multifit_status
    fgsl_multifit_status = .true.
    if (.not. c_associated(multifit%gsl_multifit_linear_workspace)) fgsl_multifit_status = .false.
  end function fgsl_multifit_status
  function fgsl_multifit_fdfridge_alloc(T, n, p)
    type(fgsl_multifit_fdfsolver_type), intent(in) :: T
    integer(fgsl_size_t), intent(in) :: n, p
    type(fgsl_multifit_fdfridge) :: fgsl_multifit_fdfridge_alloc
    type(c_ptr) :: ftype
    ftype = fgsl_aux_multifit_fdfsolver_alloc(t%which)
    fgsl_multifit_fdfridge_alloc%gsl_multifit_fdfridge= &
    gsl_multifit_fdfridge_alloc(ftype, n, p)
  end function fgsl_multifit_fdfridge_alloc
  subroutine fgsl_multifit_fdfridge_free(work)
    type(fgsl_multifit_fdfridge), intent(inout) :: work
    call gsl_multifit_fdfridge_free(work%gsl_multifit_fdfridge)
  end subroutine fgsl_multifit_fdfridge_free
  function fgsl_multifit_fdfridge_name(w)
    type(fgsl_multifit_fdfridge), intent(in) :: w
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_multifit_fdfridge_name
    type(c_ptr) :: name
    name = gsl_multifit_fdfridge_name(w%gsl_multifit_fdfridge)
    fgsl_multifit_fdfridge_name = fgsl_name(name)
  end function fgsl_multifit_fdfridge_name
  function fgsl_multifit_fdfridge_position(w)
    type(fgsl_multifit_fdfridge), intent(in) :: w
    type(fgsl_vector) :: fgsl_multifit_fdfridge_position
    fgsl_multifit_fdfridge_position%gsl_vector = gsl_multifit_fdfridge_position(w%gsl_multifit_fdfridge)
  end function fgsl_multifit_fdfridge_position
  function fgsl_multifit_fdfridge_residual(w)
    type(fgsl_multifit_fdfridge), intent(in) :: w
    type(fgsl_vector) :: fgsl_multifit_fdfridge_residual
    fgsl_multifit_fdfridge_residual%gsl_vector = gsl_multifit_fdfridge_residual(w%gsl_multifit_fdfridge)
  end function fgsl_multifit_fdfridge_residual
  function fgsl_multifit_fdfridge_niter(w)
    type(fgsl_multifit_fdfridge), intent(in) :: w
    integer(fgsl_size_t) :: fgsl_multifit_fdfridge_niter
    fgsl_multifit_fdfridge_niter = gsl_multifit_fdfridge_niter(w%gsl_multifit_fdfridge)
  end function fgsl_multifit_fdfridge_niter
  function fgsl_multifit_fdfridge_set(w, f, x, lambda)
    type(fgsl_multifit_fdfridge), intent(inout) :: w
    type(fgsl_multifit_function_fdf), intent(inout) :: f
    type(fgsl_vector), intent(in) :: x
    real(fgsl_double), intent(in) :: lambda
    integer(fgsl_int) :: fgsl_multifit_fdfridge_set
    fgsl_multifit_fdfridge_set = gsl_multifit_fdfridge_set(&
    w%gsl_multifit_fdfridge, f%gsl_multifit_function_fdf, &
    x%gsl_vector, lambda)
  end function fgsl_multifit_fdfridge_set
  function fgsl_multifit_fdfridge_wset(w, f, x, lambda, wts)
    type(fgsl_multifit_fdfridge), intent(inout) :: w
    type(fgsl_multifit_function_fdf), intent(inout) :: f
    type(fgsl_vector), intent(in) :: x, wts
    real(fgsl_double), intent(in) :: lambda
    integer(fgsl_int) :: fgsl_multifit_fdfridge_wset
    fgsl_multifit_fdfridge_wset = gsl_multifit_fdfridge_wset(&
    w%gsl_multifit_fdfridge, f%gsl_multifit_function_fdf, &
    x%gsl_vector, lambda, wts%gsl_vector)
  end function fgsl_multifit_fdfridge_wset
  function fgsl_multifit_fdfridge_set2(w, f, x, lambda)
    type(fgsl_multifit_fdfridge), intent(inout) :: w
    type(fgsl_multifit_function_fdf), intent(inout) :: f
    type(fgsl_vector), intent(in) :: x, lambda
    integer(fgsl_int) :: fgsl_multifit_fdfridge_set2
    fgsl_multifit_fdfridge_set2 = gsl_multifit_fdfridge_set2(&
    w%gsl_multifit_fdfridge, f%gsl_multifit_function_fdf, &
    x%gsl_vector, lambda%gsl_vector)
  end function fgsl_multifit_fdfridge_set2
  function fgsl_multifit_fdfridge_wset2(w, f, x, lambda, wts)
    type(fgsl_multifit_fdfridge), intent(inout) :: w
    type(fgsl_multifit_function_fdf), intent(inout) :: f
    type(fgsl_vector), intent(in) :: x, wts, lambda
    integer(fgsl_int) :: fgsl_multifit_fdfridge_wset2
    fgsl_multifit_fdfridge_wset2 = gsl_multifit_fdfridge_wset2(&
    w%gsl_multifit_fdfridge, f%gsl_multifit_function_fdf, &
    x%gsl_vector, lambda%gsl_vector, wts%gsl_vector)
  end function fgsl_multifit_fdfridge_wset2
  function fgsl_multifit_fdfridge_set3(w, f, x, L)
    type(fgsl_multifit_fdfridge), intent(inout) :: w
    type(fgsl_multifit_function_fdf), intent(inout) :: f
    type(fgsl_vector), intent(in) :: x
    type(fgsl_matrix), intent(in) :: L
    integer(fgsl_int) :: fgsl_multifit_fdfridge_set3
    fgsl_multifit_fdfridge_set3 = gsl_multifit_fdfridge_set3(&
    w%gsl_multifit_fdfridge, f%gsl_multifit_function_fdf, &
    x%gsl_vector, L%gsl_matrix)
  end function fgsl_multifit_fdfridge_set3
  function fgsl_multifit_fdfridge_wset3(w, f, x, L, wts)
    type(fgsl_multifit_fdfridge), intent(inout) :: w
    type(fgsl_multifit_function_fdf), intent(inout) :: f
    type(fgsl_vector), intent(in) :: x, wts
    type(fgsl_matrix), intent(in) :: L
    integer(fgsl_int) :: fgsl_multifit_fdfridge_wset3
    fgsl_multifit_fdfridge_wset3 = gsl_multifit_fdfridge_wset3(&
    w%gsl_multifit_fdfridge, f%gsl_multifit_function_fdf, &
    x%gsl_vector, L%gsl_matrix, wts%gsl_vector)
  end function fgsl_multifit_fdfridge_wset3
  function fgsl_multifit_fdfridge_iterate(w)
    type(fgsl_multifit_fdfridge), intent(inout) :: w
    integer(fgsl_int) :: fgsl_multifit_fdfridge_iterate
    fgsl_multifit_fdfridge_iterate = gsl_multifit_fdfridge_iterate(&
    w%gsl_multifit_fdfridge)
  end function fgsl_multifit_fdfridge_iterate
  function fgsl_multifit_fdfridge_driver(w, maxiter, xtol, gtol, ftol, info)
    type(fgsl_multifit_fdfridge), intent(inout) :: w
    integer(fgsl_size_t), intent(in) :: maxiter
    real(fgsl_double), intent(in) :: xtol, gtol, ftol
    integer(fgsl_int), intent(out) :: info
    integer(fgsl_int) :: fgsl_multifit_fdfridge_driver
    fgsl_multifit_fdfridge_driver = gsl_multifit_fdfridge_driver(&
    w%gsl_multifit_fdfridge, maxiter, xtol, gtol, ftol, info)
  end function fgsl_multifit_fdfridge_driver
end module fgsl_multifit
