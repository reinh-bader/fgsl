module fgsl_montecarlo
  !> Monte Carlo Integration
  !> Note: this module contains the routines for plain, vegas and miser
  use fgsl_base
  use fgsl_io
  use fgsl_rngen
  
  implicit none
  
  private :: fgsl_monte_function_cinit, fgsl_monte_function_cfree, &
    gsl_monte_plain_alloc, gsl_monte_plain_init, gsl_monte_plain_integrate, &
    gsl_monte_plain_free, gsl_monte_miser_alloc, gsl_monte_miser_init, &
    gsl_monte_miser_integrate, gsl_monte_miser_free, gsl_monte_vegas_alloc, &
    gsl_monte_vegas_init, gsl_monte_vegas_integrate, gsl_monte_vegas_free, &
    gsl_monte_vegas_chisq, gsl_monte_vegas_runval, fgsl_monte_miser_csetparams, &
    fgsl_monte_miser_cgetparams, fgsl_monte_vegas_csetparams, &
    fgsl_monte_vegas_cgetparams
  
  !
  ! Types
  type, public :: fgsl_monte_function
     private
     type(c_ptr) :: gsl_monte_function = c_null_ptr
  end type fgsl_monte_function
  type, public :: fgsl_monte_plain_state
     private
     type(c_ptr) :: gsl_monte_plain_state = c_null_ptr
  end type fgsl_monte_plain_state
  type, public :: fgsl_monte_miser_state
     private
     type(c_ptr) :: gsl_monte_miser_state = c_null_ptr
  end type fgsl_monte_miser_state
  type, public :: fgsl_monte_vegas_state
     private
     type(c_ptr) :: gsl_monte_vegas_state = c_null_ptr
  end type fgsl_monte_vegas_state
! NOTE: not all compilers support enum yet
  integer(c_int), parameter, public :: fgsl_vegas_mode_importance = 1
  integer(c_int), parameter, public :: fgsl_vegas_mode_importance_only = 0
  integer(c_int), parameter, public :: fgsl_vegas_mode_stratified = -1
  
  !
  ! Generics
  interface fgsl_well_defined
     module procedure fgsl_monte_function_status
     module procedure fgsl_monte_plain_status
     module procedure fgsl_monte_miser_status
     module procedure fgsl_monte_vegas_status
  end interface fgsl_well_defined
  !
  ! C interface
  interface
	  function fgsl_monte_function_cinit(func, dim, params) bind(c)
	    import :: c_funptr, c_ptr, c_size_t
	    type(c_funptr), value :: func
	    integer(c_size_t), value :: dim
	    type(c_ptr), value :: params
	    type(c_ptr) :: fgsl_monte_function_cinit
	  end function fgsl_monte_function_cinit
	  subroutine fgsl_monte_function_cfree(func) bind(c)
	    import
	    type(c_ptr), value :: func
	  end subroutine fgsl_monte_function_cfree
	  function gsl_monte_plain_alloc(dim) bind(c)
	    import
	    integer(c_size_t), value :: dim
	    type(c_ptr) :: gsl_monte_plain_alloc
	  end function gsl_monte_plain_alloc
	  function gsl_monte_plain_init(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    integer(c_int) :: gsl_monte_plain_init
	  end function gsl_monte_plain_init
	  function gsl_monte_plain_integrate(f, xl, xu, dim, calls, r, s, result, abserr) bind(c)
	    import
	    type(c_ptr), value :: f
	    integer(c_size_t), value :: dim, calls
	    real(c_double), intent(in) :: xl(dim), xu(dim)
	    type(c_ptr), value :: r, s
	    real(c_double), intent(out) :: result, abserr
	    integer(c_int) :: gsl_monte_plain_integrate
	  end function gsl_monte_plain_integrate
	  subroutine gsl_monte_plain_free(s) bind(c)
	    import
	    type(c_ptr), value :: s
	  end subroutine gsl_monte_plain_free
	  function gsl_monte_miser_alloc(dim) bind(c)
	    import
	    integer(c_size_t), value :: dim
	    type(c_ptr) :: gsl_monte_miser_alloc 
	  end function gsl_monte_miser_alloc
	  function gsl_monte_miser_init(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    integer(c_int) :: gsl_monte_miser_init
	  end function gsl_monte_miser_init
	  function gsl_monte_miser_integrate(f, xl, xu, dim, calls, r, s, result, abserr) bind(c)
	    import
	    type(c_ptr), value :: f
	    integer(c_size_t), value :: dim, calls
	    real(c_double), intent(in) :: xl(dim), xu(dim)
	    type(c_ptr), value :: r, s
	    real(c_double), intent(out) :: result, abserr
	    integer(c_int) :: gsl_monte_miser_integrate
	  end function gsl_monte_miser_integrate
	  subroutine gsl_monte_miser_free(s) bind(c)
	    import
	    type(c_ptr), value :: s
	  end subroutine gsl_monte_miser_free
	  function gsl_monte_vegas_alloc(dim) bind(c)
	    import
	    integer(c_size_t), value :: dim
	    type(c_ptr) :: gsl_monte_vegas_alloc 
	  end function gsl_monte_vegas_alloc
	  function gsl_monte_vegas_init(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    integer(c_int) :: gsl_monte_vegas_init
	  end function gsl_monte_vegas_init
	  function gsl_monte_vegas_integrate(f, xl, xu, dim, calls, r, s, result, abserr) bind(c)
	    import
	    type(c_ptr), value :: f
	    integer(c_size_t), value :: dim, calls
	    real(c_double), intent(in) :: xl(dim), xu(dim)
	    type(c_ptr), value :: r, s
	    real(c_double), intent(out) :: result, abserr
	    integer(c_int) :: gsl_monte_vegas_integrate
	  end function gsl_monte_vegas_integrate
	  subroutine gsl_monte_vegas_free(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	  end subroutine gsl_monte_vegas_free
	  function gsl_monte_vegas_chisq(s) bind(c)
	    import :: c_double, c_ptr 
	    real(c_double) :: gsl_monte_vegas_chisq
	    type(c_ptr), value :: s
	  end function gsl_monte_vegas_chisq
	  subroutine gsl_monte_vegas_runval(s, result, sigma) bind(c)
	    import :: c_double, c_ptr 
	    type(c_ptr), value :: s
	    real(c_double) :: result, sigma
	  end subroutine gsl_monte_vegas_runval
	! add-on routines
	  subroutine fgsl_monte_miser_csetparams(s, estimate_frac, min_calls, &
	       min_calls_per_bisection, alpha, dither) bind(c)
	    import :: c_size_t, c_double, c_ptr
	    type(c_ptr), value :: s
	    real(c_double), value :: estimate_frac, alpha, dither
	    integer(c_size_t), value ::  min_calls, min_calls_per_bisection
	  end subroutine fgsl_monte_miser_csetparams
	  subroutine fgsl_monte_miser_cgetparams(s, estimate_frac, min_calls, &
	       min_calls_per_bisection, alpha, dither) bind(c)
	    import :: c_size_t, c_double, c_ptr
	    type(c_ptr), value :: s
	    real(c_double), intent(out) :: estimate_frac, alpha, dither
	    integer(c_size_t), intent(out) ::  min_calls, min_calls_per_bisection
	  end subroutine fgsl_monte_miser_cgetparams
	  subroutine fgsl_monte_vegas_csetparams(s, result, sigma, chisq, alpha, &
	       iterations, stage, mode, verbose, ostream) bind(c)
	    import :: c_size_t, c_double, c_ptr, c_int
	    type(c_ptr), value :: s, ostream
	    real(c_double), value :: result, sigma, chisq, alpha
	    integer(c_size_t), value ::  iterations
	    integer(c_int), value :: stage, mode, verbose
	  end subroutine fgsl_monte_vegas_csetparams
	  subroutine fgsl_monte_vegas_cgetparams(s, result, sigma, chisq, alpha, &
	       iterations, stage, mode, verbose, ostream) bind(c)
	    import :: c_size_t, c_double, c_ptr, c_int
	    type(c_ptr), value :: s, ostream
	    real(c_double), intent(out) :: result, sigma, chisq, alpha
	    integer(c_size_t), intent(out) ::  iterations
	    integer(c_int), intent(out) :: stage, mode, verbose
	  end subroutine fgsl_monte_vegas_cgetparams
  end interface
  
contains
!> Note: in GSL 1.13, accessors were also added to GSL. They're
!> slightly different named and have a differing interface from
!> fgsl_monte_*_?etparams routines already existing in FGSL.
!> To preserve backward compatibility, the FGSL accessors are retained.
! FIXME: add the new accessors and deprecate the older ones

  function fgsl_monte_function_init(func, dim, params)
    interface
       function func(xke, dim, params) bind(c)
         use, intrinsic :: iso_c_binding
         type(c_ptr), value :: xke
         integer(c_size_t), value :: dim
         type(c_ptr), value :: params
         real(c_double) :: func
       end function func
    end interface
    integer(fgsl_size_t), intent(in) :: dim
    type(c_ptr), intent(in) :: params
    type(fgsl_monte_function) :: fgsl_monte_function_init
!
    type(c_funptr) :: fp
    fp = c_funloc(func)
    fgsl_monte_function_init%gsl_monte_function = &
         fgsl_monte_function_cinit(fp, dim, params)
  end function fgsl_monte_function_init
  subroutine fgsl_monte_function_free(func)
    type(fgsl_monte_function), intent(inout) :: func
    call fgsl_monte_function_cfree(func%gsl_monte_function)
  end subroutine fgsl_monte_function_free
  function fgsl_monte_plain_alloc(dim)
    integer(fgsl_size_t), intent(in) :: dim
    type(fgsl_monte_plain_state) :: fgsl_monte_plain_alloc
    fgsl_monte_plain_alloc%gsl_monte_plain_state = &
         gsl_monte_plain_alloc(dim)
  end function fgsl_monte_plain_alloc
  function fgsl_monte_plain_init(s)
    type(fgsl_monte_plain_state), intent(in) :: s
    integer(fgsl_int) :: fgsl_monte_plain_init
    fgsl_monte_plain_init = gsl_monte_plain_init(s%gsl_monte_plain_state)
  end function fgsl_monte_plain_init
  function fgsl_monte_plain_integrate(f, xl, xu, dim, calls, r, s, result, abserr)
    type(fgsl_monte_function), intent(in) :: f
    integer(fgsl_size_t), intent(in) :: dim, calls
    real(fgsl_double), intent(in) :: xl(dim), xu(dim)
    type(fgsl_rng), intent(in) :: r
    type(fgsl_monte_plain_state), intent(in) :: s
    real(fgsl_double), intent(out) :: result, abserr
    integer(fgsl_int) :: fgsl_monte_plain_integrate
    fgsl_monte_plain_integrate = &
         gsl_monte_plain_integrate(f%gsl_monte_function, xl, xu, dim, calls, &
         r%gsl_rng, s%gsl_monte_plain_state, result, abserr)
  end function fgsl_monte_plain_integrate
  subroutine fgsl_monte_plain_free(s)
    type(fgsl_monte_plain_state), intent(inout) :: s
    call gsl_monte_plain_free(s%gsl_monte_plain_state)
  end subroutine fgsl_monte_plain_free
  function fgsl_monte_miser_alloc(dim)
    integer(fgsl_size_t), value :: dim
    type(fgsl_monte_miser_state) :: fgsl_monte_miser_alloc
    fgsl_monte_miser_alloc%gsl_monte_miser_state = gsl_monte_miser_alloc(dim)
  end function fgsl_monte_miser_alloc
  function fgsl_monte_miser_init(s)
    type(fgsl_monte_miser_state), intent(in) :: s
    integer(fgsl_int) :: fgsl_monte_miser_init
    fgsl_monte_miser_init = gsl_monte_miser_init(s%gsl_monte_miser_state)
  end function fgsl_monte_miser_init
  function fgsl_monte_miser_integrate(f, xl, xu, dim, calls, r, s, result, abserr)
    type(fgsl_monte_function), intent(in) :: f
    integer(fgsl_size_t), intent(in) :: dim, calls
    real(fgsl_double), intent(in) :: xl(dim), xu(dim)
    type(fgsl_rng), intent(in) :: r
    type(fgsl_monte_miser_state), intent(in) :: s
    real(fgsl_double), intent(out) :: result, abserr
    integer(fgsl_int) :: fgsl_monte_miser_integrate
    fgsl_monte_miser_integrate = &
         gsl_monte_miser_integrate(f%gsl_monte_function, xl, xu, dim, calls, &
         r%gsl_rng, s%gsl_monte_miser_state, result, abserr)
  end function fgsl_monte_miser_integrate
  subroutine fgsl_monte_miser_free(s)
    type(fgsl_monte_miser_state), intent(inout) :: s
    call gsl_monte_miser_free(s%gsl_monte_miser_state)
  end subroutine fgsl_monte_miser_free
!
  function fgsl_monte_vegas_alloc(dim)
    integer(fgsl_size_t), value :: dim
    type(fgsl_monte_vegas_state) :: fgsl_monte_vegas_alloc
    fgsl_monte_vegas_alloc%gsl_monte_vegas_state = gsl_monte_vegas_alloc(dim)
  end function fgsl_monte_vegas_alloc
  function fgsl_monte_vegas_init(s)
    type(fgsl_monte_vegas_state), intent(in) :: s
    integer(fgsl_int) :: fgsl_monte_vegas_init
    fgsl_monte_vegas_init = gsl_monte_vegas_init(s%gsl_monte_vegas_state)
  end function fgsl_monte_vegas_init
  function fgsl_monte_vegas_integrate(f, xl, xu, dim, calls, r, s, result, abserr)
    type(fgsl_monte_function), intent(in) :: f
    integer(fgsl_size_t), intent(in) :: dim, calls
    real(fgsl_double), intent(in) :: xl(dim), xu(dim)
    type(fgsl_rng), intent(in) :: r
    type(fgsl_monte_vegas_state), intent(in) :: s
    real(fgsl_double), intent(out) :: result, abserr
    integer(fgsl_int) :: fgsl_monte_vegas_integrate
    fgsl_monte_vegas_integrate = &
         gsl_monte_vegas_integrate(f%gsl_monte_function, xl, xu, dim, calls, &
         r%gsl_rng, s%gsl_monte_vegas_state, result, abserr)
  end function fgsl_monte_vegas_integrate
  subroutine fgsl_monte_vegas_free(s)
    type(fgsl_monte_vegas_state), intent(inout) :: s
    call gsl_monte_vegas_free(s%gsl_monte_vegas_state)
  end subroutine fgsl_monte_vegas_free
  function fgsl_monte_vegas_chisq(s)
    real(fgsl_double) :: fgsl_monte_vegas_chisq
    type(fgsl_monte_vegas_state), intent(in) :: s
    fgsl_monte_vegas_chisq = gsl_monte_vegas_chisq(s%gsl_monte_vegas_state)
  end function fgsl_monte_vegas_chisq
  subroutine fgsl_monte_vegas_runval(s, result, sigma)
    type(fgsl_monte_vegas_state), intent(in) :: s
    real(fgsl_double), intent(out) :: result, sigma
    call gsl_monte_vegas_runval(s%gsl_monte_vegas_state, result, sigma)
  end subroutine fgsl_monte_vegas_runval
  function fgsl_monte_function_status(monte_function)
    type(fgsl_monte_function), intent(in) :: monte_function
    logical :: fgsl_monte_function_status
    fgsl_monte_function_status = .true.
    if (.not. c_associated(monte_function%gsl_monte_function)) &
         fgsl_monte_function_status = .false.
  end function fgsl_monte_function_status
  function fgsl_monte_plain_status(monte_plain)
    type(fgsl_monte_plain_state), intent(in) :: monte_plain
    logical :: fgsl_monte_plain_status
    fgsl_monte_plain_status = .true.
    if (.not. c_associated(monte_plain%gsl_monte_plain_state)) &
         fgsl_monte_plain_status = .false.
  end function fgsl_monte_plain_status
  function fgsl_monte_miser_status(monte_miser)
    type(fgsl_monte_miser_state), intent(in) :: monte_miser
    logical :: fgsl_monte_miser_status
    fgsl_monte_miser_status = .true.
    if (.not. c_associated(monte_miser%gsl_monte_miser_state)) &
         fgsl_monte_miser_status = .false.
  end function fgsl_monte_miser_status
  function fgsl_monte_vegas_status(monte_vegas)
    type(fgsl_monte_vegas_state), intent(in) :: monte_vegas
    logical :: fgsl_monte_vegas_status
    fgsl_monte_vegas_status = .true.
    if (.not. c_associated(monte_vegas%gsl_monte_vegas_state)) &
         fgsl_monte_vegas_status = .false.
  end function fgsl_monte_vegas_status
!
!> Accessor routine for setting the parameters for the MISER algorithm
  subroutine fgsl_monte_miser_setparams(s, estimate_frac, min_calls, &
       min_calls_per_bisection, alpha, dither)
    type(fgsl_monte_miser_state), intent(inout) :: s
    real(fgsl_double), intent(in) :: estimate_frac, alpha, dither
    integer(fgsl_size_t), intent(in) ::  min_calls, min_calls_per_bisection
    call fgsl_monte_miser_csetparams(s%gsl_monte_miser_state, estimate_frac, &
         min_calls, min_calls_per_bisection, alpha, dither)
  end subroutine fgsl_monte_miser_setparams
!> Accessor routine for reading out the parameters for the MISER algorithm
  subroutine fgsl_monte_miser_getparams(s, estimate_frac, min_calls, &
       min_calls_per_bisection, alpha, dither)
    type(fgsl_monte_miser_state), intent(in) :: s
    real(fgsl_double), intent(out) :: estimate_frac, alpha, dither
    integer(fgsl_size_t), intent(out) ::  min_calls, min_calls_per_bisection
    call fgsl_monte_miser_cgetparams(s%gsl_monte_miser_state, estimate_frac, &
         min_calls, min_calls_per_bisection, alpha, dither)
  end subroutine fgsl_monte_miser_getparams
!> Accessor routine for setting the parameters for the VEGAS algorithm
  subroutine fgsl_monte_vegas_setparams(s, result, sigma, chisq, alpha, &
       iterations, stage, mode, verbose, ostream)
    type(fgsl_monte_vegas_state), intent(inout) :: s
    real(fgsl_double), intent(in) :: result, sigma, chisq, alpha
    integer(fgsl_size_t), intent(in) ::  iterations
    integer(fgsl_int), intent(in) :: stage, mode, verbose
    type(fgsl_file), intent(in) :: ostream
    call fgsl_monte_vegas_csetparams(s%gsl_monte_vegas_state, result, &
         sigma, chisq, alpha, iterations, stage, mode, verbose, &
         ostream%gsl_file)
  end subroutine fgsl_monte_vegas_setparams
!> Accessor routine for reading out the parameters for the VEGAS algorithm
  subroutine fgsl_monte_vegas_getparams(s, result, sigma, chisq, alpha, &
       iterations, stage, mode, verbose, ostream)
    type(fgsl_monte_vegas_state), intent(in) :: s
    real(fgsl_double), intent(out) :: result, sigma, chisq, alpha
    integer(fgsl_size_t), intent(out) ::  iterations
    integer(fgsl_int), intent(out) :: stage, mode, verbose
    type(fgsl_file), intent(out) :: ostream
    call fgsl_monte_vegas_cgetparams(s%gsl_monte_vegas_state, result, &
         sigma, chisq, alpha, iterations, stage, mode, verbose, &
         ostream%gsl_file)
  end subroutine fgsl_monte_vegas_getparams

end module fgsl_montecarlo
