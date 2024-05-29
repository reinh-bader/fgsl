module fgsl_multi_nlinear
  !> NonLinear least-squeares fitting 
  !> Note: This module contains the interfaces to gsl_multifit_nlinear.h
  !> and gsl_multilarge_nlinear.h
  use fgsl_base
  use fgsl_array

  implicit none

  private :: gsl_multifit_nlinear_setup, gsl_multilarge_nlinear_setup, &
    gsl_multifit_nlinear_alloc, gsl_multilarge_nlinear_alloc, &
    gsl_multifit_nlinear_default_parameters, gsl_multilarge_nlinear_default_parameters, &
    gsl_multifit_nlinear_init, gsl_multilarge_nlinear_init, &
    gsl_multifit_nlinear_winit, gsl_multilarge_nlinear_winit, &
    gsl_multifit_nlinear_free, gsl_multilarge_nlinear_free, &
    gsl_multifit_nlinear_name, gsl_multilarge_nlinear_name, &
    gsl_multifit_nlinear_trs_name, gsl_multilarge_nlinear_trs_name, &
    gsl_multifit_nlinear_iterate, gsl_multilarge_nlinear_iterate, &
    gsl_multifit_nlinear_position, gsl_multilarge_nlinear_position, &
    gsl_multifit_nlinear_residual, gsl_multilarge_nlinear_residual, &
    gsl_multifit_nlinear_jac, &
    gsl_multifit_nlinear_niter, gsl_multilarge_nlinear_niter, &
    gsl_multifit_nlinear_rcond, gsl_multilarge_nlinear_rcond, &
    gsl_multifit_nlinear_test, gsl_multilarge_nlinear_test, &
    gsl_multifit_nlinear_driver, gsl_multilarge_nlinear_driver, &
    gsl_multifit_nlinear_covar, gsl_multilarge_nlinear_covar, &
    gsl_multifit_nlinear_fdf_get, gsl_multifit_nlinear_get_trs, &
    gsl_multifit_nlinear_get_scale, gsl_multifit_nlinear_get_solver, &
    gsl_multilarge_nlinear_fdf_get, gsl_multilarge_nlinear_get_trs, &
    gsl_multilarge_nlinear_get_scale, gsl_multilarge_nlinear_get_solver
    
    
  private :: fgsl_multifit_nlinear_fdf_cinit, fgsl_multifit_nlinear_fdf_cfree, &
    fgsl_multilarge_nlinear_fdf_cinit, fgsl_multilarge_nlinear_fdf_cfree
    
  !
  !> Types:
  !> multifit new nonlinear interfaces for both small and large problems
  type, public :: fgsl_multifit_nlinear_type
     private
     type(c_ptr) :: gsl_multifit_nlinear_type = c_null_ptr
  end type fgsl_multifit_nlinear_type
  type, public :: fgsl_multifit_nlinear_workspace
     type(c_ptr) :: gsl_multifit_nlinear_workspace = c_null_ptr
  end type fgsl_multifit_nlinear_workspace
  type, BIND(C) :: gsl_multifit_nlinear_parameters
     type(c_ptr) :: trs, scale, solver
     integer(c_int) :: fdtype
     real(c_double) :: factor_up, factor_down, avmax, h_df, h_fvv
  end type
  type, public :: fgsl_multifit_nlinear_parameters
     private
     type(gsl_multifit_nlinear_parameters) :: gsl_multifit_nlinear_parameters 
  end type fgsl_multifit_nlinear_parameters

  type, public :: fgsl_multilarge_nlinear_type
     private
     type(c_ptr) :: gsl_multilarge_nlinear_type = c_null_ptr
  end type fgsl_multilarge_nlinear_type
  type, public :: fgsl_multilarge_nlinear_workspace
     type(c_ptr) :: gsl_multilarge_nlinear_workspace = c_null_ptr
  end type fgsl_multilarge_nlinear_workspace
  type, BIND(C) :: gsl_multilarge_nlinear_parameters
     type(c_ptr) :: trs, scale, solver
     integer(c_int) :: fdtype
     real(c_double) :: factor_up, factor_down, avmax, h_df, h_fvv
     integer(c_size_t) :: max_iter    
     real(c_double) :: tol
  end type
  type, public :: fgsl_multilarge_nlinear_parameters
     private
     type(gsl_multilarge_nlinear_parameters) :: gsl_multilarge_nlinear_parameters
  end type fgsl_multilarge_nlinear_parameters
  type, public :: fgsl_multifit_nlinear_fdf
     private
     type(c_ptr) :: gsl_multifit_nlinear_fdf = c_null_ptr
  end type fgsl_multifit_nlinear_fdf
  type, public :: fgsl_multilarge_nlinear_fdf
     private
     type(c_ptr) :: gsl_multilarge_nlinear_fdf = c_null_ptr
  end type fgsl_multilarge_nlinear_fdf
  abstract interface
    subroutine fgsl_nlinear_callback(iter, params, w) BIND(C)
      import :: fgsl_size_t, c_ptr, c_funptr
      integer(fgsl_size_t), value :: iter
      type(c_ptr), value :: params, w
    end subroutine
    integer(c_int) function fgsl_nlinear_fdf_func(x, params, f) bind(c)
      import :: c_ptr, c_int
      type(c_ptr), value :: x, params, f
    end function 
    integer(c_int) function fgsl_nlinear_fdf_dfunc(x, params, df) bind(c)
      import :: c_ptr, c_int
      type(c_ptr), value :: x, params, df
    end function 
    integer(c_int) function fgsl_nlinear_fdf_dlfunc(t, x, u, params, v, jtj) bind(c)
      import :: c_ptr, c_int
!     assuming int is the correct integer for enum type
      integer(c_int), value :: t
      type(c_ptr), value :: x, u, params, v, jtj
    end function 
    integer(c_int) function fgsl_nlinear_fdf_fvv(x, v, params, vv) bind(c)
      import :: c_ptr, c_int
      type(c_ptr), value :: x, v, params, vv
    end function
  end interface
  !
  !> trust region subproblem methods
  type, private :: fgsl_multifit_nlinear_trs
    integer(c_int) :: which = 0
  end type
  type(fgsl_multifit_nlinear_trs), public, parameter :: &
          fgsl_multifit_nlinear_trs_lm = fgsl_multifit_nlinear_trs(1), &
          fgsl_multifit_nlinear_trs_lmaccel = fgsl_multifit_nlinear_trs(2), &
          fgsl_multifit_nlinear_trs_dogleg = fgsl_multifit_nlinear_trs(3), &
          fgsl_multifit_nlinear_trs_ddogleg = fgsl_multifit_nlinear_trs(4), &
          fgsl_multifit_nlinear_trs_subspace2d = fgsl_multifit_nlinear_trs(5)
  type, private :: fgsl_multilarge_nlinear_trs
    integer(c_int) :: which = 0
  end type
  type(fgsl_multilarge_nlinear_trs), public, parameter :: &
          fgsl_multilarge_nlinear_trs_lm = fgsl_multilarge_nlinear_trs(1), &
          fgsl_multilarge_nlinear_trs_lmaccel = fgsl_multilarge_nlinear_trs(2), &
          fgsl_multilarge_nlinear_trs_dogleg = fgsl_multilarge_nlinear_trs(3), &
          fgsl_multilarge_nlinear_trs_ddogleg = fgsl_multilarge_nlinear_trs(4), &
          fgsl_multilarge_nlinear_trs_subspace2d = fgsl_multilarge_nlinear_trs(5), &
          fgsl_multilarge_nlinear_trs_cgst = fgsl_multilarge_nlinear_trs(6)
  ! 
  !> scaling matrix strategies
  type, private :: fgsl_multifit_nlinear_scale
    integer(c_int) :: which = 0
  end type
  type(fgsl_multifit_nlinear_scale), public, parameter :: &
          fgsl_multifit_nlinear_scale_levenberg = fgsl_multifit_nlinear_scale(1), &
          fgsl_multifit_nlinear_scale_marquardt = fgsl_multifit_nlinear_scale(2), &
          fgsl_multifit_nlinear_scale_more = fgsl_multifit_nlinear_scale(3)
  type, private :: fgsl_multilarge_nlinear_scale
    integer(c_int) :: which = 0
  end type
  type(fgsl_multilarge_nlinear_scale), public, parameter :: &
          fgsl_multilarge_nlinear_scale_levenberg = fgsl_multilarge_nlinear_scale(1), &
          fgsl_multilarge_nlinear_scale_marquardt = fgsl_multilarge_nlinear_scale(2), &
          fgsl_multilarge_nlinear_scale_more = fgsl_multilarge_nlinear_scale(3)
  !
  !> least square solvers
  type, private :: fgsl_multifit_nlinear_solver
    integer(c_int) :: which = 0
  end type
  type(fgsl_multifit_nlinear_solver), public, parameter :: &
          fgsl_multifit_nlinear_solver_cholesky = fgsl_multifit_nlinear_solver(1), &
          fgsl_multifit_nlinear_solver_qr = fgsl_multifit_nlinear_solver(2), &
          fgsl_multifit_nlinear_solver_svd = fgsl_multifit_nlinear_solver(3)
  integer(fgsl_int), parameter, public :: FGSL_MULTIFIT_NLINEAR_FWDIFF = 0, & 
                                  FGSL_MULTIFIT_NLINEAR_CTRDIFF = 1
  type, private :: fgsl_multilarge_nlinear_solver
    integer(c_int) :: which = 0
  end type
  type(fgsl_multilarge_nlinear_solver), public, parameter :: &
          fgsl_multilarge_nlinear_solver_cholesky = fgsl_multilarge_nlinear_solver(1)

  !
  !> Generics
  interface fgsl_multifit_nlinear_type
     module procedure fgsl_multifit_nlinear_setup
  end interface
  interface fgsl_multilarge_nlinear_type
     module procedure fgsl_multilarge_nlinear_setup
  end interface
  interface fgsl_well_defined
     module procedure fgsl_multifit_nlinear_status
  end interface  
  !
  !> C interfaces
  interface
	 type(c_ptr) function gsl_multifit_nlinear_setup(s) BIND(C)
	    import
	    character(c_char) :: s
	 end function
	 type(c_ptr) function gsl_multilarge_nlinear_setup(s) BIND(C)
	    import
	    character(c_char) :: s
	 end function
	 function gsl_multifit_nlinear_alloc(t, params, n, p) BIND(C)
	    import
	    type(c_ptr), value :: t
	    type(gsl_multifit_nlinear_parameters) :: params
	    type(c_ptr) :: gsl_multifit_nlinear_alloc
	    integer(c_size_t), value :: n, p
	 end function 
	 function gsl_multilarge_nlinear_alloc(t, params, n, p) BIND(C)
	    import
	    type(c_ptr), value :: t
	    type(gsl_multilarge_nlinear_parameters) :: params
	    type(c_ptr) :: gsl_multilarge_nlinear_alloc
	    integer(c_size_t), value :: n, p
	 end function 
	 function gsl_multifit_nlinear_default_parameters() BIND(C)
	    import
	    type(gsl_multifit_nlinear_parameters) :: gsl_multifit_nlinear_default_parameters
	 end function
	 function gsl_multilarge_nlinear_default_parameters() BIND(C)
	    import
	    type(gsl_multilarge_nlinear_parameters) :: gsl_multilarge_nlinear_default_parameters
	 end function
	 integer(c_int) function gsl_multifit_nlinear_init(x, fdf, w) BIND(C)
	    import
	    type(c_ptr), value :: x, fdf, w
	 end function
	 integer(c_int) function gsl_multilarge_nlinear_init(x, fdf, w) BIND(C)
	    import
	    type(c_ptr), value :: x, fdf, w
	 end function
	 integer(c_int) function gsl_multifit_nlinear_winit(x, wts, fdf, w) BIND(C)
	    import
	    type(c_ptr), value :: x, wts, fdf, w
	 end function
	 integer(c_int) function gsl_multilarge_nlinear_winit(x, wts, fdf, w) BIND(C)
	    import
	    type(c_ptr), value :: x, wts, fdf, w
	 end function
	 subroutine gsl_multifit_nlinear_free(w) BIND(C)
	    import
	    type(c_ptr), value :: w
	 end subroutine
	 subroutine gsl_multilarge_nlinear_free(w) BIND(C)
	    import
	    type(c_ptr), value :: w
	 end subroutine
	 function gsl_multifit_nlinear_name(w) BIND(C)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr) :: gsl_multifit_nlinear_name
	 end function
	 function gsl_multilarge_nlinear_name(w) BIND(C)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr) :: gsl_multilarge_nlinear_name
	 end function
	 function gsl_multifit_nlinear_trs_name(w) BIND(C)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr) :: gsl_multifit_nlinear_trs_name
	 end function
	 function gsl_multilarge_nlinear_trs_name(w) BIND(C)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr) :: gsl_multilarge_nlinear_trs_name
	 end function
	 integer(c_int) function gsl_multifit_nlinear_iterate(w) BIND(C)
	    import
	    type(c_ptr), value :: w
	 end function
	 integer(c_int) function gsl_multilarge_nlinear_iterate(w) BIND(C)
	    import
	    type(c_ptr), value :: w
	 end function
	 function gsl_multifit_nlinear_position(w) BIND(C)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr) :: gsl_multifit_nlinear_position
	 end function
	 function gsl_multilarge_nlinear_position(w) BIND(C)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr) :: gsl_multilarge_nlinear_position
	 end function
	 function gsl_multifit_nlinear_residual(w) BIND(C)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr) :: gsl_multifit_nlinear_residual
	 end function
	 function gsl_multilarge_nlinear_residual(w) BIND(C)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr) :: gsl_multilarge_nlinear_residual
	 end function
	 function gsl_multifit_nlinear_jac(w) BIND(C)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr) :: gsl_multifit_nlinear_jac
	 end function
	 integer (c_int) function gsl_multifit_nlinear_niter(w) BIND(C)
	    import
	    type(c_ptr), value :: w
	 end function
	 integer (c_int) function gsl_multilarge_nlinear_niter(w) BIND(C)
	    import
	    type(c_ptr), value :: w
	 end function
	 integer (c_int) function gsl_multifit_nlinear_rcond(rcond, w) BIND(C)
	    import
	    real(c_double), intent(inout) :: rcond
	    type(c_ptr), value :: w
	 end function
	 integer (c_int) function gsl_multilarge_nlinear_rcond(rcond, w) BIND(C)
	    import
	    real(c_double), intent(inout) :: rcond
	    type(c_ptr), value :: w
	 end function
	 integer(c_int) function gsl_multifit_nlinear_test(xtol, gtol, ftol, info, w) BIND(C)
	    import :: c_ptr, c_double, c_int
	    real(c_double), value :: xtol, gtol, ftol
	    integer(c_int), intent(inout) :: info
	    type(c_ptr), value :: w
	 end function
	 integer(c_int) function gsl_multilarge_nlinear_test(xtol, gtol, ftol, info, w) BIND(C)
	    import :: c_ptr, c_double, c_int
	    real(c_double), value :: xtol, gtol, ftol
	    integer(c_int), intent(inout) :: info
	    type(c_ptr), value :: w
	 end function
	 integer(c_int) function gsl_multifit_nlinear_driver(maxiter, xtol, gtol, ftol, &
	                       callback, callback_params, info, w) BIND(C)
	    import :: c_ptr, c_double, c_int, c_size_t, c_funptr
	    integer(c_size_t), value :: maxiter
	    real(c_double), value :: xtol, gtol, ftol
	    type(c_funptr), value :: callback
	    type(c_ptr), value :: callback_params, w
	    integer(c_int), intent(inout) :: info
	 end function
	 integer(c_int) function gsl_multilarge_nlinear_driver(maxiter, xtol, gtol, ftol, &
	                       callback, callback_params, info, w) BIND(C)
	    import :: c_ptr, c_double, c_int, c_size_t, c_funptr
	    integer(c_size_t), value :: maxiter
	    real(c_double), value :: xtol, gtol, ftol
	    type(c_funptr), value :: callback
	    type(c_ptr), value :: callback_params, w
	    integer(c_int), intent(inout) :: info
	 end function
	 integer(c_int) function gsl_multifit_nlinear_covar(j, epsrel, covar) BIND(C)
	    import
	    type(c_ptr), value :: j, covar
	    real(c_double), value :: epsrel
	 end function
	 integer(c_int) function gsl_multilarge_nlinear_covar(covar, w) BIND(C)
	    import
	    type(c_ptr), value :: covar, w
	 end function
	 type(c_ptr) function fgsl_multifit_nlinear_fdf_cinit(ndim, p, params, fp, dfp, fvvp) BIND(C)
	    import
	    integer(c_size_t), value :: ndim, p
	    type(c_ptr), value :: params
	    type(c_funptr), value :: fp, dfp, fvvp
	 end function
	 subroutine gsl_multifit_nlinear_fdf_get(fdf, fp, dfp, fvvp, &
	          n, p, params, nevalf, nevaldf, nevalfvv) BIND(C)
	    import
	    type(c_ptr), value :: fdf
	    type(c_funptr) :: fp, dfp, fvvp
	    integer(c_size_t) :: n, p, nevalf, nevaldf, nevalfvv
	    type(c_ptr) :: params
	 end subroutine
	 subroutine fgsl_multifit_nlinear_fdf_cfree(fdf) BIND(C)
	    import
	    type(c_ptr), value :: fdf
	 end subroutine
	 type(c_ptr) function gsl_multifit_nlinear_get_trs(which) BIND(C)
	    import
	    integer(c_int), value :: which
	 end function
	 type(c_ptr) function gsl_multifit_nlinear_get_scale(which) BIND(C)
	    import
	    integer(c_int), value :: which
	 end function
	 type(c_ptr) function gsl_multifit_nlinear_get_solver(which) BIND(C)
	    import
	    integer(c_int), value :: which
	 end function
	 type(c_ptr) function fgsl_multilarge_nlinear_fdf_cinit(ndim, p, params, fp, dfp, fvvp) BIND(C)
	    import
	    integer(c_size_t), value :: ndim, p
	    type(c_ptr), value :: params
	    type(c_funptr), value :: fp, dfp, fvvp
	 end function
	 subroutine fgsl_multilarge_nlinear_fdf_cfree(fdf) BIND(C)
	    import
	    type(c_ptr), value :: fdf
	 end subroutine
	 subroutine gsl_multilarge_nlinear_fdf_get(fdf, fp, dfp, fvvp, &
	          n, p, params, nevalf, nevaldfu, nevaldf2, nevalfvv) BIND(C)
	    import
	    type(c_ptr), value :: fdf
	    type(c_funptr) :: fp, dfp, fvvp
	    integer(c_size_t) :: n, p, nevalf, nevaldfu, nevaldf2, nevalfvv
	    type(c_ptr) :: params
	 end subroutine
	 type(c_ptr) function gsl_multilarge_nlinear_get_trs(which) BIND(C)
	    import
	    integer(c_int), value :: which
	 end function
	 type(c_ptr) function gsl_multilarge_nlinear_get_scale(which) BIND(C)
	    import
	    integer(c_int), value :: which
	 end function
	 type(c_ptr) function gsl_multilarge_nlinear_get_solver(which) BIND(C)
	    import
	    integer(c_int), value :: which
	 end function

  end interface
contains
!
!>  API
  type(fgsl_multifit_nlinear_type) function fgsl_multifit_nlinear_setup(s)
    character(kind=fgsl_char, len=*) :: s 
    fgsl_multifit_nlinear_setup%gsl_multifit_nlinear_type = gsl_multifit_nlinear_setup(s(1:1))
  end function
  type(fgsl_multilarge_nlinear_type) function fgsl_multilarge_nlinear_setup(s)
    character(kind=fgsl_char, len=*) :: s
    fgsl_multilarge_nlinear_setup%gsl_multilarge_nlinear_type = gsl_multilarge_nlinear_setup(s(1:1))
  end function
  function fgsl_multifit_nlinear_alloc(t, params, n, p)
    type(fgsl_multifit_nlinear_type), intent(in) :: t
    type(fgsl_multifit_nlinear_parameters), intent(in) :: params
    integer(fgsl_size_t), intent(in) :: n, p
    type(fgsl_multifit_nlinear_workspace) :: fgsl_multifit_nlinear_alloc
!
    fgsl_multifit_nlinear_alloc%gsl_multifit_nlinear_workspace = &
         gsl_multifit_nlinear_alloc(t%gsl_multifit_nlinear_type, &
                 params%gsl_multifit_nlinear_parameters, n, p)
  end function fgsl_multifit_nlinear_alloc
  function fgsl_multilarge_nlinear_alloc(t, params, n, p)
    type(fgsl_multilarge_nlinear_type), intent(in) :: t
    type(fgsl_multilarge_nlinear_parameters), intent(in) :: params
    integer(fgsl_size_t), intent(in) :: n, p
    type(fgsl_multilarge_nlinear_workspace) :: fgsl_multilarge_nlinear_alloc
!
    fgsl_multilarge_nlinear_alloc%gsl_multilarge_nlinear_workspace = &
         gsl_multilarge_nlinear_alloc(t%gsl_multilarge_nlinear_type, &
                 params%gsl_multilarge_nlinear_parameters, n, p)
  end function fgsl_multilarge_nlinear_alloc
  function fgsl_multifit_nlinear_default_parameters()
    type(fgsl_multifit_nlinear_parameters) :: fgsl_multifit_nlinear_default_parameters
    fgsl_multifit_nlinear_default_parameters%gsl_multifit_nlinear_parameters = &
         gsl_multifit_nlinear_default_parameters()
  end function
  function fgsl_multilarge_nlinear_default_parameters()
    type(fgsl_multilarge_nlinear_parameters) :: fgsl_multilarge_nlinear_default_parameters
    fgsl_multilarge_nlinear_default_parameters%gsl_multilarge_nlinear_parameters = &
         gsl_multilarge_nlinear_default_parameters()
  end function
  integer(fgsl_int) function fgsl_multifit_nlinear_init(x, fdf, w)
    type(fgsl_vector), intent(in) :: x
    type(fgsl_multifit_nlinear_fdf), intent(in) :: fdf
    type(fgsl_multifit_nlinear_workspace), intent(inout) :: w
    fgsl_multifit_nlinear_init = gsl_multifit_nlinear_init(x%gsl_vector, &
         fdf%gsl_multifit_nlinear_fdf, w%gsl_multifit_nlinear_workspace)
  end function
  integer(fgsl_int) function fgsl_multifit_nlinear_winit(x, wts, fdf, w)
    type(fgsl_vector), intent(in) :: x, wts
    type(fgsl_multifit_nlinear_fdf), intent(in) :: fdf
    type(fgsl_multifit_nlinear_workspace), intent(inout) :: w
    fgsl_multifit_nlinear_winit = gsl_multifit_nlinear_winit(x%gsl_vector, &
         wts%gsl_vector, fdf%gsl_multifit_nlinear_fdf, w%gsl_multifit_nlinear_workspace)
  end function
  integer(fgsl_int) function fgsl_multilarge_nlinear_init(x, fdf, w)
    type(fgsl_vector), intent(in) :: x
    type(fgsl_multilarge_nlinear_fdf), intent(in) :: fdf
    type(fgsl_multilarge_nlinear_workspace), intent(inout) :: w
    fgsl_multilarge_nlinear_init = gsl_multilarge_nlinear_init(x%gsl_vector, &
         fdf%gsl_multilarge_nlinear_fdf, w%gsl_multilarge_nlinear_workspace)
  end function
  integer(fgsl_int) function fgsl_multilarge_nlinear_winit(x, wts, fdf, w)
    type(fgsl_vector), intent(in) :: x, wts
    type(fgsl_multilarge_nlinear_fdf), intent(in) :: fdf
    type(fgsl_multilarge_nlinear_workspace), intent(inout) :: w
    fgsl_multilarge_nlinear_winit = gsl_multilarge_nlinear_winit(x%gsl_vector, &
         wts%gsl_vector, fdf%gsl_multilarge_nlinear_fdf, w%gsl_multilarge_nlinear_workspace)
  end function
  subroutine fgsl_multifit_nlinear_free(w)
    type(fgsl_multifit_nlinear_workspace), intent(inout) :: w
    call gsl_multifit_nlinear_free(w%gsl_multifit_nlinear_workspace)
  end subroutine
  subroutine fgsl_multilarge_nlinear_free(w)
    type(fgsl_multilarge_nlinear_workspace), intent(inout) :: w
    call gsl_multilarge_nlinear_free(w%gsl_multilarge_nlinear_workspace)
  end subroutine
  function fgsl_multifit_nlinear_name(w)
    type(fgsl_multifit_nlinear_workspace), intent(in) :: w
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_multifit_nlinear_name
    type(c_ptr) :: name
    name = gsl_multifit_nlinear_name(w%gsl_multifit_nlinear_workspace)
    fgsl_multifit_nlinear_name = fgsl_name(name)
  end function
  function fgsl_multilarge_nlinear_name(w)
    type(fgsl_multilarge_nlinear_workspace), intent(in) :: w
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_multilarge_nlinear_name
    type(c_ptr) :: name
    name = gsl_multilarge_nlinear_name(w%gsl_multilarge_nlinear_workspace)
    fgsl_multilarge_nlinear_name = fgsl_name(name)
  end function
  function fgsl_multifit_nlinear_trs_name(w)
    type(fgsl_multifit_nlinear_workspace), intent(in) :: w
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_multifit_nlinear_trs_name
    type(c_ptr) :: name
    name = gsl_multifit_nlinear_trs_name(w%gsl_multifit_nlinear_workspace)
    fgsl_multifit_nlinear_trs_name = fgsl_name(name)
  end function
  function fgsl_multilarge_nlinear_trs_name(w)
    type(fgsl_multilarge_nlinear_workspace), intent(in) :: w
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_multilarge_nlinear_trs_name
    type(c_ptr) :: name
    name = gsl_multilarge_nlinear_trs_name(w%gsl_multilarge_nlinear_workspace)
    fgsl_multilarge_nlinear_trs_name = fgsl_name(name)
  end function
  integer(fgsl_int) function fgsl_multifit_nlinear_iterate(w)
    type(fgsl_multifit_nlinear_workspace), intent(inout) :: w
    fgsl_multifit_nlinear_iterate = gsl_multifit_nlinear_iterate(w%gsl_multifit_nlinear_workspace)
  end function
  integer(fgsl_int) function fgsl_multilarge_nlinear_iterate(w)
    type(fgsl_multilarge_nlinear_workspace), intent(inout) :: w
    fgsl_multilarge_nlinear_iterate = gsl_multilarge_nlinear_iterate(&
               w%gsl_multilarge_nlinear_workspace)
  end function
  function fgsl_multifit_nlinear_position(w)
    type(fgsl_multifit_nlinear_workspace), intent(in) :: w
    type(fgsl_vector) :: fgsl_multifit_nlinear_position
    fgsl_multifit_nlinear_position%gsl_vector = &
         gsl_multifit_nlinear_position(w%gsl_multifit_nlinear_workspace)
  end function fgsl_multifit_nlinear_position
  function fgsl_multilarge_nlinear_position(w)
    type(fgsl_multilarge_nlinear_workspace), intent(in) :: w
    type(fgsl_vector) :: fgsl_multilarge_nlinear_position
    fgsl_multilarge_nlinear_position%gsl_vector = &
         gsl_multilarge_nlinear_position(w%gsl_multilarge_nlinear_workspace)
  end function fgsl_multilarge_nlinear_position
  function fgsl_multifit_nlinear_residual(w)
    type(fgsl_multifit_nlinear_workspace), intent(in) :: w
    type(fgsl_vector) :: fgsl_multifit_nlinear_residual
    fgsl_multifit_nlinear_residual%gsl_vector = &
         gsl_multifit_nlinear_residual(w%gsl_multifit_nlinear_workspace)
  end function fgsl_multifit_nlinear_residual
  function fgsl_multilarge_nlinear_residual(w)
    type(fgsl_multilarge_nlinear_workspace), intent(in) :: w
    type(fgsl_vector) :: fgsl_multilarge_nlinear_residual
    fgsl_multilarge_nlinear_residual%gsl_vector = &
         gsl_multilarge_nlinear_residual(w%gsl_multilarge_nlinear_workspace)
  end function fgsl_multilarge_nlinear_residual
  function fgsl_multifit_nlinear_jac(w)
    type(fgsl_multifit_nlinear_workspace), intent(in) :: w
    type(fgsl_matrix) :: fgsl_multifit_nlinear_jac
    fgsl_multifit_nlinear_jac%gsl_matrix = &
         gsl_multifit_nlinear_jac(w%gsl_multifit_nlinear_workspace)
  end function fgsl_multifit_nlinear_jac
  integer(fgsl_size_t) function fgsl_multifit_nlinear_niter(w)
    type(fgsl_multifit_nlinear_workspace), intent(in) :: w
    fgsl_multifit_nlinear_niter = &
         gsl_multifit_nlinear_niter(w%gsl_multifit_nlinear_workspace)
  end function fgsl_multifit_nlinear_niter
  integer(fgsl_size_t) function fgsl_multilarge_nlinear_niter(w)
    type(fgsl_multilarge_nlinear_workspace), intent(in) :: w
    fgsl_multilarge_nlinear_niter = &
         gsl_multilarge_nlinear_niter(w%gsl_multilarge_nlinear_workspace)
  end function fgsl_multilarge_nlinear_niter
  integer(fgsl_int) function fgsl_multifit_nlinear_rcond(rcond, w)
    real(fgsl_double), intent(inout) :: rcond
    type(fgsl_multifit_nlinear_workspace), intent(in) :: w
    fgsl_multifit_nlinear_rcond = &
         gsl_multifit_nlinear_rcond(rcond, w%gsl_multifit_nlinear_workspace)
  end function fgsl_multifit_nlinear_rcond
  integer(fgsl_int) function fgsl_multilarge_nlinear_rcond(rcond, w)
    real(fgsl_double), intent(inout) :: rcond
    type(fgsl_multilarge_nlinear_workspace), intent(in) :: w
    fgsl_multilarge_nlinear_rcond = &
         gsl_multilarge_nlinear_rcond(rcond, w%gsl_multilarge_nlinear_workspace)
  end function fgsl_multilarge_nlinear_rcond
  integer(fgsl_int) function fgsl_multifit_nlinear_test(xtol, gtol, ftol, info, w)
    real(fgsl_double), intent(in) :: xtol, gtol, ftol
    integer(fgsl_int), intent(inout) :: info
    type(fgsl_multifit_nlinear_workspace), intent(in) :: w
    fgsl_multifit_nlinear_test = &
         gsl_multifit_nlinear_test(xtol, gtol, ftol, info, w%gsl_multifit_nlinear_workspace)
  end function fgsl_multifit_nlinear_test
  integer(fgsl_int) function fgsl_multilarge_nlinear_test(xtol, gtol, ftol, info, w)
    real(fgsl_double), intent(in) :: xtol, gtol, ftol
    integer(fgsl_int), intent(inout) :: info
    type(fgsl_multilarge_nlinear_workspace), intent(in) :: w
    fgsl_multilarge_nlinear_test = &
         gsl_multilarge_nlinear_test(xtol, gtol, ftol, info, w%gsl_multilarge_nlinear_workspace)
  end function fgsl_multilarge_nlinear_test
  integer(fgsl_int) function fgsl_multifit_nlinear_driver(maxiter, xtol, gtol, ftol, &
                       callback, callback_params, info, w)
    integer(fgsl_size_t), intent(in) :: maxiter
    real(fgsl_double), intent(in) :: xtol, gtol, ftol
    procedure(fgsl_nlinear_callback), optional :: callback
    type(c_ptr), value :: callback_params
    integer(fgsl_int), intent(inout) :: info
    type(fgsl_multifit_nlinear_workspace), intent(in) :: w
    type(c_funptr) :: callback_ptr
    if (present(callback)) then
      callback_ptr = c_funloc(callback)
    else
      callback_ptr = c_null_funptr
    end if
    fgsl_multifit_nlinear_driver = gsl_multifit_nlinear_driver(maxiter, xtol, gtol, &
                         ftol, callback_ptr, callback_params, info, &
                         w%gsl_multifit_nlinear_workspace)
  end function fgsl_multifit_nlinear_driver
  integer(fgsl_int) function fgsl_multilarge_nlinear_driver(maxiter, xtol, gtol, ftol, &
                       callback, callback_params, info, w)
    integer(fgsl_size_t), intent(in) :: maxiter
    real(fgsl_double), intent(in) :: xtol, gtol, ftol
    procedure(fgsl_nlinear_callback), optional :: callback
    type(c_ptr), value :: callback_params
    integer(fgsl_int), intent(inout) :: info
    type(fgsl_multilarge_nlinear_workspace), intent(in) :: w
    type(c_funptr) :: callback_ptr
    if (present(callback)) then
      callback_ptr = c_funloc(callback)
    else
      callback_ptr = c_null_funptr
    end if
    fgsl_multilarge_nlinear_driver = gsl_multilarge_nlinear_driver(maxiter, xtol, gtol, &
                         ftol, callback_ptr, callback_params, info, &
                         w%gsl_multilarge_nlinear_workspace)
  end function fgsl_multilarge_nlinear_driver
  integer(fgsl_int) function fgsl_multifit_nlinear_covar(j, epsrel, covar)
    type(fgsl_matrix), intent(in) :: j
    real(fgsl_double), intent(in) :: epsrel
    type(fgsl_matrix), intent(inout) :: covar
    fgsl_multifit_nlinear_covar = gsl_multifit_nlinear_covar(j%gsl_matrix, &
                         epsrel, covar%gsl_matrix)
  end function fgsl_multifit_nlinear_covar
  integer(fgsl_int) function fgsl_multilarge_nlinear_covar(covar, w)
    type(fgsl_matrix), intent(inout) :: covar
    type(fgsl_multilarge_nlinear_workspace), intent(in) :: w
    fgsl_multilarge_nlinear_covar = gsl_multilarge_nlinear_covar(covar%gsl_matrix, &
                         w%gsl_multilarge_nlinear_workspace)
  end function fgsl_multilarge_nlinear_covar
  function fgsl_multifit_nlinear_fdf_init(ndim, p, params, func, dfunc, fvv)
    procedure(fgsl_nlinear_fdf_func), optional :: func
    procedure(fgsl_nlinear_fdf_dfunc), optional :: dfunc
    procedure(fgsl_nlinear_fdf_fvv), optional :: fvv
    integer(fgsl_size_t), intent(in) :: ndim, p
    type(c_ptr), intent(in) :: params
    type(fgsl_multifit_nlinear_fdf) :: fgsl_multifit_nlinear_fdf_init
!
    type(c_funptr) :: fp, dfp, fvvp
    fp = c_funloc(func)
    if (present(dfunc)) then
       dfp = c_funloc(dfunc)
    else
       dfp = c_null_funptr
    end if
    if (present(fvv)) then
       fvvp = c_funloc(fvv)
    else
       fvvp = c_null_funptr
    end if
    fgsl_multifit_nlinear_fdf_init%gsl_multifit_nlinear_fdf = &
         fgsl_multifit_nlinear_fdf_cinit(ndim, p, params, fp, dfp, fvvp)
  end function fgsl_multifit_nlinear_fdf_init
  subroutine fgsl_multifit_nlinear_fdf_get(fdf, func, dfunc, fvv, n, p, params, &
         nevalf, nevaldf, nevalfvv)
    type(fgsl_multifit_nlinear_fdf), intent(in) :: fdf
    procedure(fgsl_nlinear_fdf_func), pointer, optional :: func
    procedure(fgsl_nlinear_fdf_dfunc), pointer, optional :: dfunc
    procedure(fgsl_nlinear_fdf_fvv), pointer, optional :: fvv
    integer(fgsl_size_t), intent(out), optional :: n, p, nevalf, nevaldf, nevalfvv
    type(c_ptr), intent(out), optional :: params
    type(c_funptr) :: pfunc, pdfunc, pfvv
    integer(fgsl_size_t) :: nl, pl, nevalfl, nevaldfl, nevalfvvl
    type(c_ptr) :: paramsl
    call gsl_multifit_nlinear_fdf_get(fdf%gsl_multifit_nlinear_fdf, pfunc, pdfunc, pfvv, &
          nl, pl, paramsl, nevalfl, nevaldfl, nevalfvvl)
    if (present(func)) call c_f_procpointer(pfunc, func)
    if (present(dfunc)) call c_f_procpointer(pdfunc, dfunc)
    if (present(fvv)) call c_f_procpointer(pfvv, fvv)
    if (present(n)) n = nl
    if (present(p)) p = pl
    if (present(params)) params = paramsl
    if (present(nevalf)) nevalf = nevalfl
    if (present(nevaldf)) nevaldf = nevaldfl
    if (present(nevalfvv)) nevalfvv = nevalfvvl
  end subroutine fgsl_multifit_nlinear_fdf_get
  subroutine fgsl_multifit_nlinear_fdf_free(fun)
    type(fgsl_multifit_nlinear_fdf), intent(inout) :: fun
    call fgsl_multifit_nlinear_fdf_cfree(fun%gsl_multifit_nlinear_fdf)
  end subroutine fgsl_multifit_nlinear_fdf_free
  function fgsl_multifit_nlinear_status(s)
    type(fgsl_multifit_nlinear_workspace), intent(in) :: s
    logical :: fgsl_multifit_nlinear_status
    fgsl_multifit_nlinear_status = .false.
    if (c_associated(s%gsl_multifit_nlinear_workspace)) &
         fgsl_multifit_nlinear_status = .true.
  end function fgsl_multifit_nlinear_status
  subroutine fgsl_multifit_nlinear_parameters_set(params, trs, scale, solver, &
         fdtype, factor_up, factor_down, avmax, h_df, h_fvv)
    type(fgsl_multifit_nlinear_parameters) :: params
    type(fgsl_multifit_nlinear_trs), optional :: trs
    type(fgsl_multifit_nlinear_scale), optional :: scale
    type(fgsl_multifit_nlinear_solver), optional :: solver
    integer(fgsl_int), optional :: fdtype
    real(c_double), optional :: factor_up, factor_down, avmax, h_df, h_fvv
    associate(p => params%gsl_multifit_nlinear_parameters)
      if (present(trs)) p%trs = gsl_multifit_nlinear_get_trs(trs%which)
      if (present(scale)) p%scale = gsl_multifit_nlinear_get_scale(scale%which)
      if (present(solver)) p%solver = gsl_multifit_nlinear_get_solver(solver%which)
      if (present(fdtype)) p%fdtype = fdtype
      if (present(factor_up)) p%factor_up = factor_up
      if (present(factor_down)) p%factor_down = factor_down
      if (present(avmax)) p%avmax = avmax
      if (present(h_df)) p%h_df = h_df
      if (present(h_fvv)) p%h_fvv = h_fvv
    end associate
  end subroutine
  function fgsl_multilarge_nlinear_fdf_init(ndim, p, params, func, dfunc, fvv)
    procedure(fgsl_nlinear_fdf_func), optional :: func
    procedure(fgsl_nlinear_fdf_dlfunc), optional :: dfunc
    procedure(fgsl_nlinear_fdf_fvv), optional :: fvv
    integer(fgsl_size_t), intent(in) :: ndim, p
    type(c_ptr), intent(in) :: params
    type(fgsl_multilarge_nlinear_fdf) :: fgsl_multilarge_nlinear_fdf_init
!
    type(c_funptr) :: fp, dfp, fvvp
    fp = c_funloc(func)
    if (present(dfunc)) then
       dfp = c_funloc(dfunc)
    else
       dfp = c_null_funptr
    end if
    if (present(fvv)) then
       fvvp = c_funloc(fvv)
    else
       fvvp = c_null_funptr
    end if
    fgsl_multilarge_nlinear_fdf_init%gsl_multilarge_nlinear_fdf = &
         fgsl_multilarge_nlinear_fdf_cinit(ndim, p, params, fp, dfp, fvvp)
  end function fgsl_multilarge_nlinear_fdf_init
  subroutine fgsl_multilarge_nlinear_fdf_free(fun)
    type(fgsl_multilarge_nlinear_fdf), intent(inout) :: fun
    call fgsl_multilarge_nlinear_fdf_cfree(fun%gsl_multilarge_nlinear_fdf)
  end subroutine fgsl_multilarge_nlinear_fdf_free
  subroutine fgsl_multilarge_nlinear_fdf_get(fdf, func, dfunc, fvv, n, p, params, &
         nevalf, nevaldfu, nevaldf2, nevalfvv)
    type(fgsl_multilarge_nlinear_fdf), intent(in) :: fdf
    procedure(fgsl_nlinear_fdf_func), pointer, optional :: func
    procedure(fgsl_nlinear_fdf_dlfunc), pointer, optional :: dfunc
    procedure(fgsl_nlinear_fdf_fvv), pointer, optional :: fvv
    integer(fgsl_size_t), intent(out), optional :: n, p, nevalf, nevaldfu, nevaldf2, nevalfvv
    type(c_ptr), intent(out), optional :: params
    type(c_funptr) :: pfunc, pdfunc, pfvv
    integer(fgsl_size_t) :: nl, pl, nevalfl, nevaldful, nevaldf2l, nevalfvvl
    type(c_ptr) :: paramsl
    call gsl_multilarge_nlinear_fdf_get(fdf%gsl_multilarge_nlinear_fdf, pfunc, pdfunc, pfvv, &
          nl, pl, paramsl, nevalfl, nevaldful, nevaldf2l, nevalfvvl)
    if (present(func)) call c_f_procpointer(pfunc, func)
    if (present(dfunc)) call c_f_procpointer(pdfunc, dfunc)
    if (present(fvv)) call c_f_procpointer(pfvv, fvv)
    if (present(n)) n = nl
    if (present(p)) p = pl
    if (present(params)) params = paramsl
    if (present(nevalf)) nevalf = nevalfl
    if (present(nevaldfu)) nevaldfu = nevaldful
    if (present(nevaldf2)) nevaldf2 = nevaldf2l
    if (present(nevalfvv)) nevalfvv = nevalfvvl
  end subroutine fgsl_multilarge_nlinear_fdf_get
  subroutine fgsl_multilarge_nlinear_parameters_set(params, trs, scale, solver, &
         fdtype, factor_up, factor_down, avmax, h_df, h_fvv, max_iter, tol)
    type(fgsl_multilarge_nlinear_parameters) :: params
    type(fgsl_multilarge_nlinear_trs), optional :: trs
    type(fgsl_multilarge_nlinear_scale), optional :: scale
    type(fgsl_multilarge_nlinear_solver), optional :: solver
    integer(fgsl_int), optional :: fdtype
    integer(fgsl_size_t), optional :: max_iter
    real(c_double), optional :: factor_up, factor_down, avmax, h_df, h_fvv, tol
    associate(p => params%gsl_multilarge_nlinear_parameters)
      if (present(trs)) p%trs = gsl_multilarge_nlinear_get_trs(trs%which)
      if (present(scale)) p%scale = gsl_multilarge_nlinear_get_scale(scale%which)
      if (present(solver)) p%solver = gsl_multilarge_nlinear_get_solver(solver%which)
      if (present(fdtype)) p%fdtype = fdtype
      if (present(factor_up)) p%factor_up = factor_up
      if (present(factor_down)) p%factor_down = factor_down
      if (present(avmax)) p%avmax = avmax
      if (present(h_df)) p%h_df = h_df
      if (present(h_fvv)) p%h_fvv = h_fvv
      if (present(max_iter)) p%max_iter = max_iter
      if (present(tol)) p%tol = tol
    end associate
  end subroutine
end module fgsl_multi_nlinear
