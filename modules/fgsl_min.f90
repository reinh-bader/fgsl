module fgsl_min
  !> One-dimensional Minimization
  use fgsl_base
  use fgsl_math
  implicit none

  private :: gsl_min_fminimizer_alloc, fgsl_aux_fminimizer_alloc, &
    gsl_min_fminimizer_free, gsl_min_fminimizer_set, &
    gsl_min_fminimizer_set_with_values, gsl_min_fminimizer_iterate, &
    gsl_min_fminimizer_name, gsl_min_fminimizer_x_minimum, &
    gsl_min_fminimizer_x_lower, gsl_min_fminimizer_x_upper, &
    gsl_min_fminimizer_f_minimum, gsl_min_fminimizer_f_lower, &
    gsl_min_test_interval 
    
  !
  !> Types
  type, public :: fgsl_min_fminimizer_type
     private
     integer(c_int) :: which = 0
  end type fgsl_min_fminimizer_type
  type(fgsl_min_fminimizer_type), public, parameter :: &
       fgsl_min_fminimizer_goldensection = fgsl_min_fminimizer_type(1), &
       fgsl_min_fminimizer_brent = fgsl_min_fminimizer_type(2), &
       fgsl_min_fminimizer_quad_golden = fgsl_min_fminimizer_type(3)
  type, public :: fgsl_min_fminimizer
     private
     type(c_ptr) :: gsl_min_fminimizer = c_null_ptr
  end type fgsl_min_fminimizer
  !
  !> Generics
  interface fgsl_well_defined
     module procedure fgsl_min_fminimizer_status
  end interface fgsl_well_defined
  !
  !> C interfaces
  interface
	  function gsl_min_fminimizer_alloc(t) bind(c)
	    import
	    type(c_ptr), value :: t
	    type(c_ptr) :: gsl_min_fminimizer_alloc
	  end function gsl_min_fminimizer_alloc
	  function fgsl_aux_fminimizer_alloc(it) bind(c)
	    import
	    integer(c_int), value :: it
	    type(c_ptr) :: fgsl_aux_fminimizer_alloc
	  end function fgsl_aux_fminimizer_alloc
	  subroutine gsl_min_fminimizer_free(s) bind(c)
	    import
	    type(c_ptr), value :: s
	  end subroutine gsl_min_fminimizer_free
	  function gsl_min_fminimizer_set(s, f, x_minimum, x_lower, x_upper) bind(c)
	    import
	    type(c_ptr), value :: s, f
	    real(c_double), value :: x_minimum, x_lower, x_upper
	    integer(c_int) :: gsl_min_fminimizer_set
	  end function gsl_min_fminimizer_set
	  function gsl_min_fminimizer_set_with_values(s, f, x_minimum, f_minimum, &
	           x_lower, f_lower, x_upper, f_upper) bind(c)
	    import
	    type(c_ptr), value :: s, f
	    real(c_double), value :: x_minimum, f_minimum, &
	           x_lower, f_lower, x_upper, f_upper
	    integer(c_int) :: gsl_min_fminimizer_set_with_values
	  end function gsl_min_fminimizer_set_with_values
	  function  gsl_min_fminimizer_iterate(s) bind(c)
	    import 
	    type(c_ptr), value :: s
	    integer(c_int) :: gsl_min_fminimizer_iterate
	  end function gsl_min_fminimizer_iterate
	  function gsl_min_fminimizer_name(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_min_fminimizer_name
	  end function gsl_min_fminimizer_name
	  function gsl_min_fminimizer_x_minimum(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    real(c_double) ::  gsl_min_fminimizer_x_minimum
	  end function gsl_min_fminimizer_x_minimum
	  function gsl_min_fminimizer_x_lower(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    real(c_double) ::  gsl_min_fminimizer_x_lower
	  end function gsl_min_fminimizer_x_lower
	  function gsl_min_fminimizer_x_upper(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    real(c_double) ::  gsl_min_fminimizer_x_upper
	  end function gsl_min_fminimizer_x_upper
	  function gsl_min_fminimizer_f_minimum(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    real(c_double) ::  gsl_min_fminimizer_f_minimum
	  end function gsl_min_fminimizer_f_minimum
	  function gsl_min_fminimizer_f_lower(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    real(c_double) ::  gsl_min_fminimizer_f_lower
	  end function gsl_min_fminimizer_f_lower
	  function gsl_min_fminimizer_f_upper(s) bind(c)
	    import
	    type(c_ptr), value :: s
	    real(c_double) ::  gsl_min_fminimizer_f_upper
	  end function gsl_min_fminimizer_f_upper
	  function gsl_min_test_interval(x_lower, x_upper, epsabs, epsrel) bind(c)
	    import
	    real(c_double), value :: x_lower, x_upper, epsabs, epsrel
	    integer(c_int) :: gsl_min_test_interval
	  end function gsl_min_test_interval
  end interface
contains
!> API: Minimization

  function fgsl_min_fminimizer_alloc(t) 
    type(fgsl_min_fminimizer_type), intent(in) :: t
    type(fgsl_min_fminimizer) :: fgsl_min_fminimizer_alloc
    type(c_ptr) :: it
    it = fgsl_aux_fminimizer_alloc(t%which)
    if (c_associated(it)) then
       fgsl_min_fminimizer_alloc%gsl_min_fminimizer = gsl_min_fminimizer_alloc(it)
    else
       fgsl_min_fminimizer_alloc%gsl_min_fminimizer = c_null_ptr
    end if
  end function fgsl_min_fminimizer_alloc
  subroutine fgsl_min_fminimizer_free(s)
    type(fgsl_min_fminimizer), intent(inout) :: s
    call gsl_min_fminimizer_free(s%gsl_min_fminimizer)
  end subroutine fgsl_min_fminimizer_free
  function fgsl_min_fminimizer_set(s, f, x_minimum, x_lower, x_upper)
    type(fgsl_min_fminimizer), intent(inout) :: s
    type(fgsl_function), intent(in) :: f
    real(fgsl_double), intent(in) :: x_minimum, x_lower, x_upper
    integer(fgsl_int) :: fgsl_min_fminimizer_set
    fgsl_min_fminimizer_set = gsl_min_fminimizer_set(s%gsl_min_fminimizer, f%gsl_function, &
                              x_minimum, x_lower, x_upper)
  end function fgsl_min_fminimizer_set
  function fgsl_min_fminimizer_set_with_values(s, f, x_minimum, f_minimum, &
           x_lower, f_lower, x_upper, f_upper) 
    type(fgsl_min_fminimizer), intent(inout) :: s
    type(fgsl_function), intent(in) :: f
    real(fgsl_double), intent(in) :: x_minimum, f_minimum, &
           x_lower, f_lower, x_upper, f_upper
    integer(fgsl_int) :: fgsl_min_fminimizer_set_with_values
    fgsl_min_fminimizer_set_with_values = gsl_min_fminimizer_set_with_values( &
           s%gsl_min_fminimizer, f%gsl_function, x_minimum, f_minimum, &
           x_lower, f_lower, x_upper, f_upper) 
  end function fgsl_min_fminimizer_set_with_values
  function fgsl_min_fminimizer_iterate(s) 
    type(fgsl_min_fminimizer), intent(in) :: s
    integer(fgsl_int) :: fgsl_min_fminimizer_iterate
    fgsl_min_fminimizer_iterate = gsl_min_fminimizer_iterate(s%gsl_min_fminimizer)
  end function fgsl_min_fminimizer_iterate
  function fgsl_min_fminimizer_name(s)
    type(fgsl_min_fminimizer), intent(in) :: s
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_min_fminimizer_name
!
    type(c_ptr) :: name
!
    name = gsl_min_fminimizer_name(s%gsl_min_fminimizer)
    fgsl_min_fminimizer_name = fgsl_name(name)
  end function fgsl_min_fminimizer_name
  function fgsl_min_fminimizer_x_minimum(s) 
    type(fgsl_min_fminimizer), intent(in) :: s
    real(fgsl_double) ::  fgsl_min_fminimizer_x_minimum
    fgsl_min_fminimizer_x_minimum = gsl_min_fminimizer_x_minimum(s%gsl_min_fminimizer)
  end function fgsl_min_fminimizer_x_minimum
  function fgsl_min_fminimizer_x_lower(s) 
    type(fgsl_min_fminimizer), intent(in) :: s
    real(fgsl_double) ::  fgsl_min_fminimizer_x_lower
    fgsl_min_fminimizer_x_lower = gsl_min_fminimizer_x_lower(s%gsl_min_fminimizer)
  end function fgsl_min_fminimizer_x_lower
  function fgsl_min_fminimizer_x_upper(s)
    type(fgsl_min_fminimizer), intent(in) :: s
    real(fgsl_double) ::  fgsl_min_fminimizer_x_upper
    fgsl_min_fminimizer_x_upper = gsl_min_fminimizer_x_upper(s%gsl_min_fminimizer)
  end function fgsl_min_fminimizer_x_upper
  function fgsl_min_fminimizer_f_minimum(s)
    type(fgsl_min_fminimizer), intent(in) :: s
    real(fgsl_double) ::  fgsl_min_fminimizer_f_minimum
    fgsl_min_fminimizer_f_minimum = gsl_min_fminimizer_f_minimum(s%gsl_min_fminimizer)
  end function fgsl_min_fminimizer_f_minimum
  function fgsl_min_fminimizer_f_lower(s) 
    type(fgsl_min_fminimizer), intent(in) :: s
    real(fgsl_double) ::  fgsl_min_fminimizer_f_lower
    fgsl_min_fminimizer_f_lower = gsl_min_fminimizer_f_lower(s%gsl_min_fminimizer)
  end function fgsl_min_fminimizer_f_lower
  function fgsl_min_fminimizer_f_upper(s)
    type(fgsl_min_fminimizer), intent(in) :: s
    real(fgsl_double) ::  fgsl_min_fminimizer_f_upper
    fgsl_min_fminimizer_f_upper = gsl_min_fminimizer_f_upper(s%gsl_min_fminimizer)
  end function fgsl_min_fminimizer_f_upper
  function fgsl_min_test_interval(x_lower, x_upper, epsabs, epsrel) 
    real(fgsl_double), intent(in) :: x_lower, x_upper, epsabs, epsrel
    integer(fgsl_int) :: fgsl_min_test_interval
    fgsl_min_test_interval = gsl_min_test_interval(x_lower, x_upper, epsabs, epsrel)
  end function fgsl_min_test_interval
!
  function fgsl_min_fminimizer_status(s)
    type(fgsl_min_fminimizer), intent(in) :: s
    logical :: fgsl_min_fminimizer_status
    fgsl_min_fminimizer_status = .false.
    if (c_associated(s%gsl_min_fminimizer)) &
         fgsl_min_fminimizer_status = .true.
  end function fgsl_min_fminimizer_status
end module fgsl_min
