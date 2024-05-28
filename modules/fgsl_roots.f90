module fgsl_roots
  !> One-dimensional Root Finding
  use fgsl_base
  use fgsl_math
  implicit none

  private :: gsl_root_fsolver_alloc, fgsl_aux_fsolver_alloc, gsl_root_fdfsolver_alloc, &
    fgsl_aux_fdfsolver_alloc, gsl_root_fsolver_set, gsl_root_fdfsolver_set, &
    gsl_root_fsolver_free, gsl_root_fdfsolver_free, gsl_root_fsolver_name, &
    gsl_root_fdfsolver_name, gsl_root_fsolver_iterate, gsl_root_fdfsolver_iterate, &
    gsl_root_fsolver_root, gsl_root_fdfsolver_root, gsl_root_fsolver_x_lower, &
    gsl_root_fsolver_x_upper, gsl_root_test_interval, gsl_root_test_delta, &
    gsl_root_test_residual

!
!> Types
  type, public :: fgsl_root_fsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_root_fsolver_type
  type(fgsl_root_fsolver_type), public, parameter :: &
       fgsl_root_fsolver_bisection = fgsl_root_fsolver_type(1), &
       fgsl_root_fsolver_brent = fgsl_root_fsolver_type(2), &
       fgsl_root_fsolver_falsepos = fgsl_root_fsolver_type(3)
  type, public :: fgsl_root_fdfsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_root_fdfsolver_type
  type(fgsl_root_fdfsolver_type), public, parameter :: &
       fgsl_root_fdfsolver_newton = fgsl_root_fdfsolver_type(1), &
       fgsl_root_fdfsolver_secant = fgsl_root_fdfsolver_type(2), &
       fgsl_root_fdfsolver_steffenson = fgsl_root_fdfsolver_type(3)
  type, public :: fgsl_root_fsolver
     private
     type(c_ptr) :: gsl_root_fsolver = c_null_ptr
  end type fgsl_root_fsolver
  type, public :: fgsl_root_fdfsolver
     private
     type(c_ptr) :: gsl_root_fdfsolver = c_null_ptr
  end type fgsl_root_fdfsolver
  !
  !> Generics
  interface fgsl_well_defined
     module procedure fgsl_root_fsolver_status
     module procedure fgsl_root_fdfsolver_status
  end interface
  ! 
  !> C interfaces
  interface
	  function gsl_root_fsolver_alloc(t) bind(c)
	    import
	    type(c_ptr), value :: t
	    type(c_ptr) :: gsl_root_fsolver_alloc
	  end function gsl_root_fsolver_alloc
	  function fgsl_aux_fsolver_alloc(it) bind(c)
	    import
	    integer(c_int), value :: it
	    type(c_ptr) :: fgsl_aux_fsolver_alloc
	  end function fgsl_aux_fsolver_alloc
	  function gsl_root_fdfsolver_alloc(t) bind(c)
	    import
	    type(c_ptr), value :: t
	    type(c_ptr) :: gsl_root_fdfsolver_alloc
	  end function gsl_root_fdfsolver_alloc
	  function fgsl_aux_fdfsolver_alloc(it) bind(c)
	    import
	    integer(c_int), value :: it
	    type(c_ptr) :: fgsl_aux_fdfsolver_alloc
	  end function fgsl_aux_fdfsolver_alloc
	  function gsl_root_fsolver_set(s, f, x_lower, x_upper) bind(c)
	    import
	    type(c_ptr), value :: s, f
	    real(c_double), value :: x_lower, x_upper
	    integer(c_int) :: gsl_root_fsolver_set
	  end function gsl_root_fsolver_set
	  function gsl_root_fdfsolver_set(s, f, x) bind(c)
	    import
	    type(c_ptr), value :: s, f
	    real(c_double), value :: x
	    integer(c_int) :: gsl_root_fdfsolver_set
	  end function gsl_root_fdfsolver_set
	  subroutine gsl_root_fsolver_free(s) bind(c)
	    import
	    type(c_ptr), value :: s
	  end subroutine gsl_root_fsolver_free
	  subroutine gsl_root_fdfsolver_free(s) bind(c)
	    import
	    type(c_ptr), value :: s
	  end subroutine gsl_root_fdfsolver_free
	  function gsl_root_fsolver_name(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_root_fsolver_name
	  end function gsl_root_fsolver_name
	  function gsl_root_fdfsolver_name(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_root_fdfsolver_name
	  end function gsl_root_fdfsolver_name
	  function gsl_root_fsolver_iterate(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    integer(c_int) :: gsl_root_fsolver_iterate
	  end function gsl_root_fsolver_iterate
	  function gsl_root_fdfsolver_iterate(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    integer(c_int) :: gsl_root_fdfsolver_iterate
	  end function gsl_root_fdfsolver_iterate
	  function gsl_root_fsolver_root(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    real(c_double) :: gsl_root_fsolver_root
	  end function gsl_root_fsolver_root
	  function gsl_root_fdfsolver_root(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    real(c_double) :: gsl_root_fdfsolver_root
	  end function gsl_root_fdfsolver_root
	  function gsl_root_fsolver_x_lower(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    real(c_double) :: gsl_root_fsolver_x_lower
	  end function gsl_root_fsolver_x_lower
	  function gsl_root_fsolver_x_upper(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    real(c_double) :: gsl_root_fsolver_x_upper
	  end function gsl_root_fsolver_x_upper
	  function gsl_root_test_interval(x_lower, x_upper, epsabs, epsrel) bind(c)
	    import
	    real(c_double), value :: x_lower, x_upper, epsabs, epsrel
	    integer(c_int) :: gsl_root_test_interval
	  end function gsl_root_test_interval
	  function gsl_root_test_delta(x1, x0, epsabs, epsrel) bind(c)
	    import
	    real(c_double), value :: x1, x0, epsabs, epsrel
	    integer(c_int) :: gsl_root_test_delta
	  end function gsl_root_test_delta
	  function gsl_root_test_residual(f, epsabs) bind(c)
	    import
	    real(c_double), value :: f, epsabs
	    integer(c_int) :: gsl_root_test_residual
	  end function gsl_root_test_residual
  end interface
contains
  function fgsl_root_fsolver_alloc(t)
    type(fgsl_root_fsolver_type), intent(in) :: t
    type(fgsl_root_fsolver) :: fgsl_root_fsolver_alloc
    type(c_ptr) :: it
    it = fgsl_aux_fsolver_alloc(t%which)
    if (c_associated(it)) then
       fgsl_root_fsolver_alloc%gsl_root_fsolver = gsl_root_fsolver_alloc(it)
    else
       fgsl_root_fsolver_alloc%gsl_root_fsolver = c_null_ptr
    end if
  end function fgsl_root_fsolver_alloc
  function fgsl_root_fdfsolver_alloc(t)
    type(fgsl_root_fdfsolver_type), intent(in) :: t
    type(fgsl_root_fdfsolver) :: fgsl_root_fdfsolver_alloc
    type(c_ptr) :: it
    it = fgsl_aux_fdfsolver_alloc(t%which)
    if (c_associated(it)) then
       fgsl_root_fdfsolver_alloc%gsl_root_fdfsolver = gsl_root_fdfsolver_alloc(it)
    else
       fgsl_root_fdfsolver_alloc%gsl_root_fdfsolver = c_null_ptr
    end if
  end function fgsl_root_fdfsolver_alloc
  function fgsl_root_fsolver_set(s, f, x_lower, x_upper) 
    type(fgsl_root_fsolver), intent(in) :: s
    type(fgsl_function), intent(in) :: f
    real(fgsl_double), intent(in) :: x_lower, x_upper
    integer(fgsl_int) :: fgsl_root_fsolver_set
    fgsl_root_fsolver_set = gsl_root_fsolver_set(s%gsl_root_fsolver, &
         f%gsl_function, x_lower, x_upper)
  end function fgsl_root_fsolver_set
  function fgsl_root_fdfsolver_set(s, fdf, x) 
    type(fgsl_root_fdfsolver), intent(in) :: s
    type(fgsl_function_fdf), intent(in) :: fdf
    real(fgsl_double), intent(in) :: x
    integer(fgsl_int) :: fgsl_root_fdfsolver_set
    fgsl_root_fdfsolver_set = gsl_root_fdfsolver_set(s%gsl_root_fdfsolver, &
         fdf%gsl_function_fdf, x)
  end function fgsl_root_fdfsolver_set
  subroutine fgsl_root_fsolver_free(s) 
    type(fgsl_root_fsolver), intent(inout) :: s
    call gsl_root_fsolver_free(s%gsl_root_fsolver)
  end subroutine fgsl_root_fsolver_free
  subroutine fgsl_root_fdfsolver_free(s) 
    type(fgsl_root_fdfsolver), intent(inout) :: s
    call gsl_root_fdfsolver_free(s%gsl_root_fdfsolver)
  end subroutine fgsl_root_fdfsolver_free
  function fgsl_root_fsolver_name(s) 
    type(fgsl_root_fsolver), intent(in) :: s
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_root_fsolver_name
!
    type(c_ptr) :: name
!
    name = gsl_root_fsolver_name(s%gsl_root_fsolver)
    fgsl_root_fsolver_name = fgsl_name(name)
  end function fgsl_root_fsolver_name
  function fgsl_root_fdfsolver_name(s) 
    type(fgsl_root_fdfsolver), intent(in) :: s
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_root_fdfsolver_name
!
    type(c_ptr) :: name
!
    name = gsl_root_fdfsolver_name(s%gsl_root_fdfsolver)
    fgsl_root_fdfsolver_name = fgsl_name(name)
  end function fgsl_root_fdfsolver_name
  function fgsl_root_fsolver_iterate(s) 
    type(fgsl_root_fsolver), intent(inout) :: s
    integer(fgsl_int) :: fgsl_root_fsolver_iterate
    fgsl_root_fsolver_iterate = gsl_root_fsolver_iterate(s%gsl_root_fsolver)
  end function fgsl_root_fsolver_iterate
  function fgsl_root_fdfsolver_iterate(s) 
    type(fgsl_root_fdfsolver), intent(inout) :: s
    integer(fgsl_int) :: fgsl_root_fdfsolver_iterate
    fgsl_root_fdfsolver_iterate = gsl_root_fdfsolver_iterate(s%gsl_root_fdfsolver)
  end function fgsl_root_fdfsolver_iterate
  function fgsl_root_fsolver_root(s) 
    type(fgsl_root_fsolver), intent(inout) :: s
    real(fgsl_double) :: fgsl_root_fsolver_root
    fgsl_root_fsolver_root = gsl_root_fsolver_root(s%gsl_root_fsolver)
  end function fgsl_root_fsolver_root
  function fgsl_root_fdfsolver_root(s) 
    type(fgsl_root_fdfsolver), intent(inout) :: s
    real(fgsl_double) :: fgsl_root_fdfsolver_root
    fgsl_root_fdfsolver_root = gsl_root_fdfsolver_root(s%gsl_root_fdfsolver)
  end function fgsl_root_fdfsolver_root
  function fgsl_root_fsolver_x_lower(s) 
    type(fgsl_root_fsolver), intent(inout) :: s
    real(fgsl_double) :: fgsl_root_fsolver_x_lower
    fgsl_root_fsolver_x_lower = gsl_root_fsolver_x_lower(s%gsl_root_fsolver)
  end function fgsl_root_fsolver_x_lower
  function fgsl_root_fsolver_x_upper(s) 
    type(fgsl_root_fsolver), intent(inout) :: s
    real(fgsl_double) :: fgsl_root_fsolver_x_upper
    fgsl_root_fsolver_x_upper = gsl_root_fsolver_x_upper(s%gsl_root_fsolver)
  end function fgsl_root_fsolver_x_upper
  function fgsl_root_test_interval(x_lower, x_upper, epsabs, epsrel) 
    real(fgsl_double), intent(in) :: x_lower, x_upper, epsabs, epsrel
    integer(fgsl_int) :: fgsl_root_test_interval
    fgsl_root_test_interval = gsl_root_test_interval(x_lower, x_upper, epsabs, epsrel)
  end function fgsl_root_test_interval
  function fgsl_root_test_delta(x1, x0, epsabs, epsrel) 
    real(fgsl_double), intent(in) :: x1, x0, epsabs, epsrel
    integer(fgsl_int) :: fgsl_root_test_delta
    fgsl_root_test_delta = gsl_root_test_delta(x1, x0, epsabs, epsrel)
  end function fgsl_root_test_delta
  function fgsl_root_test_residual(f, epsabs) 
    real(fgsl_double), intent(in) :: f, epsabs
    integer(fgsl_int) :: fgsl_root_test_residual
    fgsl_root_test_residual = gsl_root_test_residual(f, epsabs) 
  end function fgsl_root_test_residual
!
  function fgsl_root_fsolver_status(s)
    type(fgsl_root_fsolver), intent(in) :: s
    logical :: fgsl_root_fsolver_status
    fgsl_root_fsolver_status = .false.
    if (c_associated(s%gsl_root_fsolver)) &
         fgsl_root_fsolver_status = .true.
  end function fgsl_root_fsolver_status
  function fgsl_root_fdfsolver_status(s)
    type(fgsl_root_fdfsolver), intent(in) :: s
    logical :: fgsl_root_fdfsolver_status
    fgsl_root_fdfsolver_status = .false.
    if (c_associated(s%gsl_root_fdfsolver)) &
         fgsl_root_fdfsolver_status = .true.
  end function fgsl_root_fdfsolver_status
end module fgsl_roots
