module fgsl_multimin
  !> Multi-dimensional Minimization
  use fgsl_base
  use fgsl_array
  use fgsl_math
  implicit none

  private :: gsl_multimin_fminimizer_alloc, gsl_multimin_fdfminimizer_alloc, &
    gsl_multimin_fminimizer_set, gsl_multimin_fdfminimizer_set, &
    gsl_multimin_fminimizer_free, gsl_multimin_fdfminimizer_free, &
    gsl_multimin_fminimizer_name, gsl_multimin_fdfminimizer_name, &
    gsl_multimin_fminimizer_iterate, gsl_multimin_fdfminimizer_iterate, &
    gsl_multimin_fminimizer_x, gsl_multimin_fdfminimizer_x, &
    gsl_multimin_fminimizer_minimum, gsl_multimin_fdfminimizer_minimum, &
    gsl_multimin_fdfminimizer_gradient, gsl_multimin_fminimizer_size, &
    gsl_multimin_fdfminimizer_restart, gsl_multimin_test_gradient, &
    gsl_multimin_test_size
  private :: fgsl_multimin_function_cinit, fgsl_multimin_function_fdf_cinit, &
    fgsl_multimin_function_cfree, fgsl_aux_multimin_fminimizer_alloc, &
    fgsl_aux_multimin_fdfminimizer_alloc
  !
  !> Types
  type, public :: fgsl_multimin_function
     private
     type(c_ptr) :: gsl_multimin_function = c_null_ptr
  end type fgsl_multimin_function
  type, public :: fgsl_multimin_function_fdf
     private
     type(c_ptr) :: gsl_multimin_function_fdf = c_null_ptr
  end type fgsl_multimin_function_fdf
  type, public :: fgsl_multimin_fminimizer
     private
     type(c_ptr) :: gsl_multimin_fminimizer = c_null_ptr
  end type fgsl_multimin_fminimizer
  type, public :: fgsl_multimin_fminimizer_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multimin_fminimizer_type
  type(fgsl_multimin_fminimizer_type), public, parameter :: &
       fgsl_multimin_fminimizer_nmsimplex = fgsl_multimin_fminimizer_type(1), &
       fgsl_multimin_fminimizer_nmsimplex2 = fgsl_multimin_fminimizer_type(2), &
       fgsl_multimin_fminimizer_nmsimplex2rand = fgsl_multimin_fminimizer_type(3)
  type, public :: fgsl_multimin_fdfminimizer
     private
     type(c_ptr) :: gsl_multimin_fdfminimizer = c_null_ptr
  end type fgsl_multimin_fdfminimizer
  type, public :: fgsl_multimin_fdfminimizer_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multimin_fdfminimizer_type
  type(fgsl_multimin_fdfminimizer_type), public, parameter :: &
       fgsl_multimin_fdfminimizer_steepest_descent = fgsl_multimin_fdfminimizer_type(1), &
       fgsl_multimin_fdfminimizer_conjugate_pr = fgsl_multimin_fdfminimizer_type(2), &
       fgsl_multimin_fdfminimizer_conjugate_fr = fgsl_multimin_fdfminimizer_type(3), &
       fgsl_multimin_fdfminimizer_vector_bfgs = fgsl_multimin_fdfminimizer_type(4), &
       fgsl_multimin_fdfminimizer_vector_bfgs2 = fgsl_multimin_fdfminimizer_type(5)
  !
  !> Generics
  interface fgsl_well_defined
     module procedure fgsl_multimin_fminimizer_status
     module procedure fgsl_multimin_fdfminimizer_status
  end interface
  !
  !> C interfaces
  interface
	  function gsl_multimin_fminimizer_alloc(t, n) bind(c)
	    import
	    type(c_ptr), value :: t
	    integer(c_size_t), value :: n
	    type(c_ptr) :: gsl_multimin_fminimizer_alloc
	  end function gsl_multimin_fminimizer_alloc
	  function gsl_multimin_fdfminimizer_alloc(t, n) bind(c)
	    import
	    type(c_ptr), value :: t
	    integer(c_size_t), value :: n
	    type(c_ptr) :: gsl_multimin_fdfminimizer_alloc
	  end function gsl_multimin_fdfminimizer_alloc
	  function gsl_multimin_fminimizer_set(s, f, x, step) bind(c)
	    import
	    type(c_ptr), value :: s, f, x, step
	    integer(c_int) :: gsl_multimin_fminimizer_set
	  end function gsl_multimin_fminimizer_set
	  function gsl_multimin_fdfminimizer_set(s, f, x, step, tol) bind(c)
	    import
	    type(c_ptr), value :: s, f, x
	    real(c_double), value :: step, tol
	    integer(c_int) :: gsl_multimin_fdfminimizer_set
	  end function gsl_multimin_fdfminimizer_set
	  subroutine gsl_multimin_fminimizer_free(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	  end subroutine gsl_multimin_fminimizer_free
	  subroutine gsl_multimin_fdfminimizer_free(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	  end subroutine gsl_multimin_fdfminimizer_free
	  function gsl_multimin_fminimizer_name(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multimin_fminimizer_name
	  end function gsl_multimin_fminimizer_name
	  function gsl_multimin_fdfminimizer_name(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multimin_fdfminimizer_name
	  end function gsl_multimin_fdfminimizer_name
	  function gsl_multimin_fminimizer_iterate(s) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: s
	    integer(c_int) :: gsl_multimin_fminimizer_iterate
	  end function gsl_multimin_fminimizer_iterate
	  function gsl_multimin_fdfminimizer_iterate(s) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: s
	    integer(c_int) :: gsl_multimin_fdfminimizer_iterate
	  end function gsl_multimin_fdfminimizer_iterate
	  function gsl_multimin_fminimizer_x(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multimin_fminimizer_x
	  end function gsl_multimin_fminimizer_x
	  function gsl_multimin_fdfminimizer_x(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multimin_fdfminimizer_x
	  end function gsl_multimin_fdfminimizer_x
	  function gsl_multimin_fminimizer_minimum(s) bind(c)
	    import :: c_ptr, c_double
	    type(c_ptr), value :: s
	    real(c_double) :: gsl_multimin_fminimizer_minimum
	  end function gsl_multimin_fminimizer_minimum
	  function gsl_multimin_fdfminimizer_minimum(s) bind(c)
	    import :: c_ptr, c_double
	    type(c_ptr), value :: s
	    real(c_double) :: gsl_multimin_fdfminimizer_minimum
	  end function gsl_multimin_fdfminimizer_minimum
	  function gsl_multimin_fdfminimizer_gradient(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multimin_fdfminimizer_gradient
	  end function gsl_multimin_fdfminimizer_gradient
	  function gsl_multimin_fminimizer_size(s) bind(c)
	    import :: c_ptr, c_double
	    type(c_ptr), value :: s
	    real(c_double) :: gsl_multimin_fminimizer_size
	  end function gsl_multimin_fminimizer_size
	  function gsl_multimin_fdfminimizer_restart(s) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: s
	    integer(c_int) :: gsl_multimin_fdfminimizer_restart
	  end function gsl_multimin_fdfminimizer_restart
	  function gsl_multimin_test_gradient(g, epsabs) bind(c)
	    import :: c_ptr, c_int, c_double
	    type(c_ptr), value :: g
	    real(c_double), value :: epsabs
	    integer(c_int) :: gsl_multimin_test_gradient
	  end function gsl_multimin_test_gradient
	  function gsl_multimin_test_size(size, epsabs) bind(c)
	    import :: c_int, c_double
	    real(c_double), value :: size, epsabs
	    integer(c_int) :: gsl_multimin_test_size
	  end function gsl_multimin_test_size
	!
	  function fgsl_multimin_function_cinit(fp, ndim, params) bind(c)
	    import 
	    type(c_funptr), value :: fp
	    integer(c_size_t), value :: ndim
	    type(c_ptr), value :: params
	    type(c_ptr) :: fgsl_multimin_function_cinit
	  end function fgsl_multimin_function_cinit
	  function fgsl_multimin_function_fdf_cinit(fp, dfp, fdfp, ndim, params) bind(c)
	    import
	    type(c_funptr), value :: fp, dfp, fdfp
	    integer(c_size_t), value :: ndim
	    type(c_ptr), value :: params
	    type(c_ptr) :: fgsl_multimin_function_fdf_cinit
	  end function fgsl_multimin_function_fdf_cinit
	  subroutine fgsl_multimin_function_cfree(f) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: f
	  end subroutine fgsl_multimin_function_cfree
	  subroutine fgsl_multimin_function_fdf_cfree(f) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: f
	  end subroutine fgsl_multimin_function_fdf_cfree
	  function fgsl_aux_multimin_fminimizer_alloc(it) bind(c)
	    import
	    integer(c_int), value :: it
	    type(c_ptr) :: fgsl_aux_multimin_fminimizer_alloc
	  end function fgsl_aux_multimin_fminimizer_alloc
	  function fgsl_aux_multimin_fdfminimizer_alloc(it) bind(c)
	    import
	    integer(c_int), value :: it
	    type(c_ptr) :: fgsl_aux_multimin_fdfminimizer_alloc
	  end function fgsl_aux_multimin_fdfminimizer_alloc
  end interface
contains
!
!> API
  function fgsl_multimin_function_init(func, ndim, params)
    interface
       function func(x, params) bind(c)
         import :: c_ptr, c_double
         type(c_ptr), value :: x, params
         real(c_double) :: func
       end function func
    end interface
    integer(fgsl_size_t), intent(in) :: ndim
    type(c_ptr), intent(in) :: params
    type(fgsl_multimin_function) :: fgsl_multimin_function_init
!
    type(c_funptr) :: fp
    fp = c_funloc(func)
    fgsl_multimin_function_init%gsl_multimin_function = &
         fgsl_multimin_function_cinit(fp, ndim, params)
  end function fgsl_multimin_function_init
  function fgsl_multimin_function_fdf_init(func, dfunc, fdfunc, ndim, params)
    interface
       function func(x, params) bind(c)
         import :: c_ptr, c_double
         type(c_ptr), value :: x, params
         real(c_double) :: func
       end function func
       subroutine dfunc(x, params, df) bind(c)
         import :: c_ptr
         type(c_ptr), value :: x, params, df
       end subroutine  dfunc
       subroutine fdfunc(x, params, f, df) bind(c)
         import :: c_ptr, c_double
         real(c_double) :: f
         type(c_ptr), value :: x, params, df
       end subroutine fdfunc
    end interface
    integer(fgsl_size_t), intent(in) :: ndim
    type(c_ptr), intent(in) :: params
    type(fgsl_multimin_function_fdf) :: fgsl_multimin_function_fdf_init
!
    type(c_funptr) :: fp, dfp, fdfp
    fp = c_funloc(func)
    dfp = c_funloc(dfunc)
    fdfp = c_funloc(fdfunc)
    fgsl_multimin_function_fdf_init%gsl_multimin_function_fdf = &
         fgsl_multimin_function_fdf_cinit(fp, dfp, fdfp, ndim, params)
  end function fgsl_multimin_function_fdf_init
  subroutine fgsl_multimin_function_free(fun) 
    type(fgsl_multimin_function), intent(inout) :: fun
    call fgsl_multimin_function_cfree(fun%gsl_multimin_function)
  end subroutine fgsl_multimin_function_free
  subroutine fgsl_multimin_function_fdf_free(fun) 
    type(fgsl_multimin_function_fdf), intent(inout) :: fun
    call fgsl_multimin_function_fdf_cfree(fun%gsl_multimin_function_fdf)
  end subroutine fgsl_multimin_function_fdf_free
  function fgsl_multimin_fminimizer_alloc(t, n) 
    type(fgsl_multimin_fminimizer_type), intent(in) :: t
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_multimin_fminimizer) :: fgsl_multimin_fminimizer_alloc
! 
    type(c_ptr) :: ftype
    ftype = fgsl_aux_multimin_fminimizer_alloc(t%which)
    fgsl_multimin_fminimizer_alloc%gsl_multimin_fminimizer = &
         gsl_multimin_fminimizer_alloc(ftype,n)
  end function fgsl_multimin_fminimizer_alloc
  function fgsl_multimin_fdfminimizer_alloc(t, n) 
    type(fgsl_multimin_fdfminimizer_type), intent(in) :: t
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_multimin_fdfminimizer) :: fgsl_multimin_fdfminimizer_alloc
! 
    type(c_ptr) :: ftype
    ftype = fgsl_aux_multimin_fdfminimizer_alloc(t%which)
    fgsl_multimin_fdfminimizer_alloc%gsl_multimin_fdfminimizer = &
         gsl_multimin_fdfminimizer_alloc(ftype,n)
  end function fgsl_multimin_fdfminimizer_alloc
  subroutine fgsl_multimin_fminimizer_free(s) 
    type(fgsl_multimin_fminimizer), intent(inout) :: s
    call gsl_multimin_fminimizer_free(s%gsl_multimin_fminimizer)
  end subroutine fgsl_multimin_fminimizer_free
  subroutine fgsl_multimin_fdfminimizer_free(s) 
    type(fgsl_multimin_fdfminimizer), intent(inout) :: s
    call gsl_multimin_fdfminimizer_free(s%gsl_multimin_fdfminimizer)
  end subroutine fgsl_multimin_fdfminimizer_free
  function fgsl_multimin_fminimizer_set(s, f, x, step)
    type(fgsl_multimin_fminimizer), intent(inout) :: s
    type(fgsl_multimin_function), intent(in)  :: f
    type(fgsl_vector), intent(in)  :: x, step
    integer(fgsl_int) :: fgsl_multimin_fminimizer_set
    fgsl_multimin_fminimizer_set = gsl_multimin_fminimizer_set(s%gsl_multimin_fminimizer, &
         f%gsl_multimin_function, x%gsl_vector, step%gsl_vector)
  end function fgsl_multimin_fminimizer_set
  function fgsl_multimin_fdfminimizer_set(s, fdf, x, step, tol)
    type(fgsl_multimin_fdfminimizer), intent(inout) :: s
    type(fgsl_multimin_function_fdf), intent(in)  :: fdf
    type(fgsl_vector), intent(in)  :: x
    real(fgsl_double), intent(in) :: step, tol
    integer(fgsl_int) :: fgsl_multimin_fdfminimizer_set
    fgsl_multimin_fdfminimizer_set = gsl_multimin_fdfminimizer_set(s%gsl_multimin_fdfminimizer, &
         fdf%gsl_multimin_function_fdf, x%gsl_vector, step, tol)
  end function fgsl_multimin_fdfminimizer_set
  function fgsl_multimin_fminimizer_name(s)
    type(fgsl_multimin_fminimizer), intent(in) :: s
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_multimin_fminimizer_name
!
    type(c_ptr) :: name
!
    name = gsl_multimin_fminimizer_name(s%gsl_multimin_fminimizer)
    fgsl_multimin_fminimizer_name = fgsl_name(name)
  end function fgsl_multimin_fminimizer_name
  function fgsl_multimin_fdfminimizer_name(s)
    type(fgsl_multimin_fdfminimizer), intent(in) :: s
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_multimin_fdfminimizer_name
!
    type(c_ptr) :: name
!
    name = gsl_multimin_fdfminimizer_name(s%gsl_multimin_fdfminimizer)
    fgsl_multimin_fdfminimizer_name = fgsl_name(name)
  end function fgsl_multimin_fdfminimizer_name
  function fgsl_multimin_fminimizer_iterate(s)
    type(fgsl_multimin_fminimizer), intent(in) :: s
    integer(fgsl_int) :: fgsl_multimin_fminimizer_iterate
    fgsl_multimin_fminimizer_iterate = &
         gsl_multimin_fminimizer_iterate(s%gsl_multimin_fminimizer)
  end function fgsl_multimin_fminimizer_iterate
  function fgsl_multimin_fdfminimizer_iterate(s)
    type(fgsl_multimin_fdfminimizer), intent(in) :: s
    integer(fgsl_int) :: fgsl_multimin_fdfminimizer_iterate
    fgsl_multimin_fdfminimizer_iterate = &
         gsl_multimin_fdfminimizer_iterate(s%gsl_multimin_fdfminimizer)
  end function fgsl_multimin_fdfminimizer_iterate
  function fgsl_multimin_fminimizer_x(s)
    type(fgsl_multimin_fminimizer), intent(in) :: s
    type(fgsl_vector) :: fgsl_multimin_fminimizer_x
    fgsl_multimin_fminimizer_x%gsl_vector = &
         gsl_multimin_fminimizer_x(s%gsl_multimin_fminimizer)
  end function fgsl_multimin_fminimizer_x
  function fgsl_multimin_fdfminimizer_x(s)
    type(fgsl_multimin_fdfminimizer), intent(in) :: s
    type(fgsl_vector) :: fgsl_multimin_fdfminimizer_x
    fgsl_multimin_fdfminimizer_x%gsl_vector = &
         gsl_multimin_fdfminimizer_x(s%gsl_multimin_fdfminimizer)
  end function fgsl_multimin_fdfminimizer_x
  function fgsl_multimin_fminimizer_minimum(s)
    type(fgsl_multimin_fminimizer), intent(in) :: s
    real(fgsl_double) :: fgsl_multimin_fminimizer_minimum
    fgsl_multimin_fminimizer_minimum = &
         gsl_multimin_fminimizer_minimum(s%gsl_multimin_fminimizer)
  end function fgsl_multimin_fminimizer_minimum
  function fgsl_multimin_fdfminimizer_minimum(s)
    type(fgsl_multimin_fdfminimizer), intent(in) :: s
    real(fgsl_double) :: fgsl_multimin_fdfminimizer_minimum
    fgsl_multimin_fdfminimizer_minimum = &
         gsl_multimin_fdfminimizer_minimum(s%gsl_multimin_fdfminimizer)
  end function fgsl_multimin_fdfminimizer_minimum
  function fgsl_multimin_fdfminimizer_gradient(s)
    type(fgsl_multimin_fdfminimizer), intent(in) :: s
    type(fgsl_vector) :: fgsl_multimin_fdfminimizer_gradient
    fgsl_multimin_fdfminimizer_gradient%gsl_vector = &
         gsl_multimin_fdfminimizer_gradient(s%gsl_multimin_fdfminimizer)
  end function fgsl_multimin_fdfminimizer_gradient
  function fgsl_multimin_fminimizer_size(s)
    type(fgsl_multimin_fminimizer), intent(in) :: s
    real(fgsl_double) :: fgsl_multimin_fminimizer_size
    fgsl_multimin_fminimizer_size = &
         gsl_multimin_fminimizer_size(s%gsl_multimin_fminimizer)
  end function fgsl_multimin_fminimizer_size
  function fgsl_multimin_fdfminimizer_restart(s)
    type(fgsl_multimin_fdfminimizer), intent(in) :: s
    integer(fgsl_int) :: fgsl_multimin_fdfminimizer_restart
    fgsl_multimin_fdfminimizer_restart = &
         gsl_multimin_fdfminimizer_restart(s%gsl_multimin_fdfminimizer)
  end function fgsl_multimin_fdfminimizer_restart
  function fgsl_multimin_test_gradient(g, epsabs) 
    type(fgsl_vector), intent(in) :: g
    real(fgsl_double), intent(in) :: epsabs
    integer(fgsl_int) :: fgsl_multimin_test_gradient
    fgsl_multimin_test_gradient = gsl_multimin_test_gradient(g%gsl_vector, epsabs)
  end function fgsl_multimin_test_gradient
  function fgsl_multimin_test_size(size, epsabs) 
    real(fgsl_double), intent(in) :: size, epsabs
    integer(fgsl_int) :: fgsl_multimin_test_size
    fgsl_multimin_test_size = &
         gsl_multimin_test_size(size, epsabs)
  end function fgsl_multimin_test_size
  function fgsl_multimin_fminimizer_status(s)
    type(fgsl_multimin_fminimizer), intent(in) :: s
    logical :: fgsl_multimin_fminimizer_status
    fgsl_multimin_fminimizer_status = .false.
    if (c_associated(s%gsl_multimin_fminimizer)) &
         fgsl_multimin_fminimizer_status = .true.
  end function fgsl_multimin_fminimizer_status
  function fgsl_multimin_fdfminimizer_status(s)
    type(fgsl_multimin_fdfminimizer), intent(in) :: s
    logical :: fgsl_multimin_fdfminimizer_status
    fgsl_multimin_fdfminimizer_status = .false.
    if (c_associated(s%gsl_multimin_fdfminimizer)) &
         fgsl_multimin_fdfminimizer_status = .true.
  end function fgsl_multimin_fdfminimizer_status
end module fgsl_multimin
