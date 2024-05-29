module fgsl_multiroots
  !> Multi-dimensional Root-finding
  use fgsl_base
  use fgsl_array
  use fgsl_math
  implicit none

  private :: gsl_multiroot_fsolver_alloc, gsl_multiroot_fdfsolver_alloc, &
    gsl_multiroot_fsolver_set, gsl_multiroot_fdfsolver_set, &
    gsl_multiroot_fsolver_free, gsl_multiroot_fdfsolver_free, &
    gsl_multiroot_fsolver_name, gsl_multiroot_fdfsolver_name, &
    gsl_multiroot_fsolver_iterate, gsl_multiroot_fdfsolver_iterate, &
    gsl_multiroot_fsolver_root, gsl_multiroot_fsolver_f, &
    gsl_multiroot_fsolver_dx, gsl_multiroot_fdfsolver_root, &
    gsl_multiroot_fdfsolver_f, gsl_multiroot_fdfsolver_dx, &
    gsl_multiroot_test_delta, gsl_multiroot_test_residual, &
    fgsl_multiroot_function_cinit, fgsl_multiroot_function_fdf_cinit, &
    fgsl_multiroot_function_cfree, fgsl_multiroot_function_fdf_cfree, &
    fgsl_aux_multiroot_fsolver_alloc, fgsl_aux_multiroot_fdfsolver_alloc
    
  !
  !> Types
  type, public :: fgsl_multiroot_function
     private
     type(c_ptr) :: gsl_multiroot_function = c_null_ptr
  end type fgsl_multiroot_function
  type, public :: fgsl_multiroot_function_fdf
     private
     type(c_ptr) :: gsl_multiroot_function_fdf = c_null_ptr
  end type fgsl_multiroot_function_fdf
  type, public :: fgsl_multiroot_fsolver
     private
     type(c_ptr) :: gsl_multiroot_fsolver = c_null_ptr
  end type fgsl_multiroot_fsolver
  type, public :: fgsl_multiroot_fsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multiroot_fsolver_type
  type(fgsl_multiroot_fsolver_type), public, parameter :: &
       fgsl_multiroot_fsolver_dnewton = fgsl_multiroot_fsolver_type(1), &
       fgsl_multiroot_fsolver_broyden = fgsl_multiroot_fsolver_type(2), &
       fgsl_multiroot_fsolver_hybrid = fgsl_multiroot_fsolver_type(3), &
       fgsl_multiroot_fsolver_hybrids = fgsl_multiroot_fsolver_type(4)
  type, public :: fgsl_multiroot_fdfsolver
     private
     type(c_ptr) :: gsl_multiroot_fdfsolver = c_null_ptr
  end type fgsl_multiroot_fdfsolver
  type, public :: fgsl_multiroot_fdfsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multiroot_fdfsolver_type
  type(fgsl_multiroot_fdfsolver_type), public, parameter :: &
       fgsl_multiroot_fdfsolver_newton = fgsl_multiroot_fdfsolver_type(1), &
       fgsl_multiroot_fdfsolver_gnewton = fgsl_multiroot_fdfsolver_type(2), &
       fgsl_multiroot_fdfsolver_hybridj = fgsl_multiroot_fdfsolver_type(3), &
       fgsl_multiroot_fdfsolver_hybridsj = fgsl_multiroot_fdfsolver_type(4)
      
  !
  !> Generics
  interface fgsl_well_defined
     module procedure fgsl_multiroot_fsolver_status
     module procedure fgsl_multiroot_fdfsolver_status
  end interface
  !
  !> C Interfaces
  interface
	  function gsl_multiroot_fsolver_alloc(t, n) bind(c)
	    import
	    type(c_ptr), value :: t
	    integer(c_size_t), value :: n
	    type(c_ptr) :: gsl_multiroot_fsolver_alloc
	  end function gsl_multiroot_fsolver_alloc
	  function gsl_multiroot_fdfsolver_alloc(t, n) bind(c)
	    import
	    type(c_ptr), value :: t
	    integer(c_size_t), value :: n
	    type(c_ptr) :: gsl_multiroot_fdfsolver_alloc
	  end function gsl_multiroot_fdfsolver_alloc
	  function gsl_multiroot_fsolver_set(s, f, x) bind(c)
	    import
	    type(c_ptr), value :: s, f, x
	    integer(c_int) :: gsl_multiroot_fsolver_set
	  end function gsl_multiroot_fsolver_set
	  function gsl_multiroot_fdfsolver_set(s, f, x) bind(c)
	    import
	    type(c_ptr), value :: s, f, x
	    integer(c_int) :: gsl_multiroot_fdfsolver_set
	  end function gsl_multiroot_fdfsolver_set
	  subroutine gsl_multiroot_fsolver_free(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	  end subroutine gsl_multiroot_fsolver_free
	  subroutine gsl_multiroot_fdfsolver_free(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	  end subroutine gsl_multiroot_fdfsolver_free
	  function gsl_multiroot_fsolver_name(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multiroot_fsolver_name
	  end function gsl_multiroot_fsolver_name
	  function gsl_multiroot_fdfsolver_name(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multiroot_fdfsolver_name
	  end function gsl_multiroot_fdfsolver_name
	  function gsl_multiroot_fsolver_iterate(s) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: s
	    integer(c_int) :: gsl_multiroot_fsolver_iterate
	  end function gsl_multiroot_fsolver_iterate
	  function gsl_multiroot_fdfsolver_iterate(s) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: s
	    integer(c_int) :: gsl_multiroot_fdfsolver_iterate
	  end function gsl_multiroot_fdfsolver_iterate
	  function gsl_multiroot_fsolver_root(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multiroot_fsolver_root
	  end function gsl_multiroot_fsolver_root
	  function gsl_multiroot_fsolver_f(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multiroot_fsolver_f
	  end function gsl_multiroot_fsolver_f
	  function gsl_multiroot_fsolver_dx(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multiroot_fsolver_dx
	  end function gsl_multiroot_fsolver_dx
	  function gsl_multiroot_fdfsolver_root(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multiroot_fdfsolver_root
	  end function gsl_multiroot_fdfsolver_root
	  function gsl_multiroot_fdfsolver_f(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multiroot_fdfsolver_f
	  end function gsl_multiroot_fdfsolver_f
	  function gsl_multiroot_fdfsolver_dx(s) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: s
	    type(c_ptr) :: gsl_multiroot_fdfsolver_dx
	  end function gsl_multiroot_fdfsolver_dx
	  function gsl_multiroot_test_delta(dx, x, epsabs, epsrel) bind(c)
	    import :: c_ptr, c_int, c_double
	    type(c_ptr), value :: dx, x
	    real(c_double), value :: epsabs, epsrel
	    integer(c_int) :: gsl_multiroot_test_delta
	  end function gsl_multiroot_test_delta
	  function gsl_multiroot_test_residual(f, epsabs) bind(c)
	    import :: c_ptr, c_int, c_double
	    type(c_ptr), value :: f
	    real(c_double), value :: epsabs
	    integer(c_int) :: gsl_multiroot_test_residual
	  end function gsl_multiroot_test_residual
	!
	  function fgsl_multiroot_function_cinit(fp, ndim, params) bind(c)
	    import
	    type(c_funptr), value :: fp
	    integer(c_size_t), value :: ndim
	    type(c_ptr), value :: params
	    type(c_ptr) :: fgsl_multiroot_function_cinit
	  end function fgsl_multiroot_function_cinit
	  function fgsl_multiroot_function_fdf_cinit(fp, dfp, fdfp, ndim, params) bind(c)
	    import
	    type(c_funptr), value :: fp, dfp, fdfp
	    integer(c_size_t), value :: ndim
	    type(c_ptr), value :: params
	    type(c_ptr) :: fgsl_multiroot_function_fdf_cinit
	  end function fgsl_multiroot_function_fdf_cinit
	  subroutine fgsl_multiroot_function_cfree(f) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: f
	  end subroutine fgsl_multiroot_function_cfree
	  subroutine fgsl_multiroot_function_fdf_cfree(f) bind(c)
	    import :: c_ptr
	    type(c_ptr), value :: f
	  end subroutine fgsl_multiroot_function_fdf_cfree
	  function fgsl_aux_multiroot_fsolver_alloc(it) bind(c)
	    import
	    integer(c_int), value :: it
	    type(c_ptr) :: fgsl_aux_multiroot_fsolver_alloc
	  end function fgsl_aux_multiroot_fsolver_alloc
	  function fgsl_aux_multiroot_fdfsolver_alloc(it) bind(c)
	    import
	    integer(c_int), value :: it
	    type(c_ptr) :: fgsl_aux_multiroot_fdfsolver_alloc
	  end function fgsl_aux_multiroot_fdfsolver_alloc
  end interface
contains
!-*-f90-*-
!
!  API: multi-dimensional root finding
!
!> \page "Comments on multidimensional root finding"
!> Please go to api/multiroots.finc for the API documentation.
!
  function fgsl_multiroot_function_init(func, ndim, params)
    interface
       function func(x, params, f) bind(c)
         import :: c_ptr, c_int
         type(c_ptr), value :: x, params, f
         integer(c_int) :: func
       end function func
    end interface
    integer(fgsl_size_t), intent(in) :: ndim
    type(c_ptr), intent(in) :: params
    type(fgsl_multiroot_function) :: fgsl_multiroot_function_init
!
    type(c_funptr) :: fp
    fp = c_funloc(func)
    fgsl_multiroot_function_init%gsl_multiroot_function = &
         fgsl_multiroot_function_cinit(fp, ndim, params)
  end function fgsl_multiroot_function_init
  function fgsl_multiroot_function_fdf_init(func, dfunc, fdfunc, ndim, params)
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
    integer(fgsl_size_t), intent(in) :: ndim
    type(c_ptr), intent(in) :: params
    type(fgsl_multiroot_function_fdf) :: fgsl_multiroot_function_fdf_init
!
    type(c_funptr) :: fp, dfp, fdfp
    fp = c_funloc(func)
    dfp = c_funloc(dfunc)
    fdfp = c_funloc(fdfunc)
    fgsl_multiroot_function_fdf_init%gsl_multiroot_function_fdf = &
         fgsl_multiroot_function_fdf_cinit(fp, dfp, fdfp, ndim, params)
  end function fgsl_multiroot_function_fdf_init
  subroutine fgsl_multiroot_function_free(fun) 
    type(fgsl_multiroot_function), intent(inout) :: fun
    call fgsl_multiroot_function_cfree(fun%gsl_multiroot_function)
  end subroutine fgsl_multiroot_function_free
  subroutine fgsl_multiroot_function_fdf_free(fun) 
    type(fgsl_multiroot_function_fdf), intent(inout) :: fun
    call fgsl_multiroot_function_fdf_cfree(fun%gsl_multiroot_function_fdf)
  end subroutine fgsl_multiroot_function_fdf_free
  function fgsl_multiroot_fsolver_alloc(t, n) 
    type(fgsl_multiroot_fsolver_type), intent(in) :: t
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_multiroot_fsolver) :: fgsl_multiroot_fsolver_alloc
! 
    type(c_ptr) :: ftype
    ftype = fgsl_aux_multiroot_fsolver_alloc(t%which)
    fgsl_multiroot_fsolver_alloc%gsl_multiroot_fsolver = &
         gsl_multiroot_fsolver_alloc(ftype,n)
  end function fgsl_multiroot_fsolver_alloc
  function fgsl_multiroot_fdfsolver_alloc(t, n) 
    type(fgsl_multiroot_fdfsolver_type), intent(in) :: t
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_multiroot_fdfsolver) :: fgsl_multiroot_fdfsolver_alloc
! 
    type(c_ptr) :: ftype
    ftype = fgsl_aux_multiroot_fdfsolver_alloc(t%which)
    fgsl_multiroot_fdfsolver_alloc%gsl_multiroot_fdfsolver = &
         gsl_multiroot_fdfsolver_alloc(ftype,n)
  end function fgsl_multiroot_fdfsolver_alloc
  subroutine fgsl_multiroot_fsolver_free(s) 
    type(fgsl_multiroot_fsolver), intent(inout) :: s
    call gsl_multiroot_fsolver_free(s%gsl_multiroot_fsolver)
  end subroutine fgsl_multiroot_fsolver_free
  subroutine fgsl_multiroot_fdfsolver_free(s) 
    type(fgsl_multiroot_fdfsolver), intent(inout) :: s
    call gsl_multiroot_fdfsolver_free(s%gsl_multiroot_fdfsolver)
  end subroutine fgsl_multiroot_fdfsolver_free
  function fgsl_multiroot_fsolver_set(s, f, x)
    type(fgsl_multiroot_fsolver), intent(inout) :: s
    type(fgsl_multiroot_function), intent(in)  :: f
    type(fgsl_vector), intent(in)  :: x
    integer(fgsl_int) :: fgsl_multiroot_fsolver_set
    fgsl_multiroot_fsolver_set = gsl_multiroot_fsolver_set(s%gsl_multiroot_fsolver, &
         f%gsl_multiroot_function, x%gsl_vector)
  end function fgsl_multiroot_fsolver_set
  function fgsl_multiroot_fdfsolver_set(s, fdf, x)
    type(fgsl_multiroot_fdfsolver), intent(inout) :: s
    type(fgsl_multiroot_function_fdf), intent(in)  :: fdf
    type(fgsl_vector), intent(in)  :: x
    integer(fgsl_int) :: fgsl_multiroot_fdfsolver_set
    fgsl_multiroot_fdfsolver_set = gsl_multiroot_fdfsolver_set(s%gsl_multiroot_fdfsolver, &
         fdf%gsl_multiroot_function_fdf, x%gsl_vector)
  end function fgsl_multiroot_fdfsolver_set
  function fgsl_multiroot_fsolver_name(s)
    type(fgsl_multiroot_fsolver), intent(in) :: s
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_multiroot_fsolver_name
!
    type(c_ptr) :: name
!
    name = gsl_multiroot_fsolver_name(s%gsl_multiroot_fsolver)
    fgsl_multiroot_fsolver_name = fgsl_name(name)
  end function fgsl_multiroot_fsolver_name
  function fgsl_multiroot_fdfsolver_name(s)
    type(fgsl_multiroot_fdfsolver), intent(in) :: s
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_multiroot_fdfsolver_name
!
    type(c_ptr) :: name
!
    name = gsl_multiroot_fdfsolver_name(s%gsl_multiroot_fdfsolver)
    fgsl_multiroot_fdfsolver_name = fgsl_name(name)
  end function fgsl_multiroot_fdfsolver_name
  function fgsl_multiroot_fsolver_iterate(s)
    type(fgsl_multiroot_fsolver), intent(in) :: s
    integer(fgsl_int) :: fgsl_multiroot_fsolver_iterate
    fgsl_multiroot_fsolver_iterate = &
         gsl_multiroot_fsolver_iterate(s%gsl_multiroot_fsolver)
  end function fgsl_multiroot_fsolver_iterate
  function fgsl_multiroot_fdfsolver_iterate(s)
    type(fgsl_multiroot_fdfsolver), intent(in) :: s
    integer(fgsl_int) :: fgsl_multiroot_fdfsolver_iterate
    fgsl_multiroot_fdfsolver_iterate = &
         gsl_multiroot_fdfsolver_iterate(s%gsl_multiroot_fdfsolver)
  end function fgsl_multiroot_fdfsolver_iterate
  function fgsl_multiroot_fsolver_root(s)
    type(fgsl_multiroot_fsolver), intent(in) :: s
    type(fgsl_vector) :: fgsl_multiroot_fsolver_root
    fgsl_multiroot_fsolver_root%gsl_vector = &
         gsl_multiroot_fsolver_root(s%gsl_multiroot_fsolver)
  end function fgsl_multiroot_fsolver_root
  function fgsl_multiroot_fdfsolver_root(s)
    type(fgsl_multiroot_fdfsolver), intent(in) :: s
    type(fgsl_vector) :: fgsl_multiroot_fdfsolver_root
    fgsl_multiroot_fdfsolver_root%gsl_vector = &
         gsl_multiroot_fdfsolver_root(s%gsl_multiroot_fdfsolver)
  end function fgsl_multiroot_fdfsolver_root
  function fgsl_multiroot_fsolver_f(s)
    type(fgsl_multiroot_fsolver), intent(in) :: s
    type(fgsl_vector) :: fgsl_multiroot_fsolver_f
    fgsl_multiroot_fsolver_f%gsl_vector = &
         gsl_multiroot_fsolver_f(s%gsl_multiroot_fsolver)
  end function fgsl_multiroot_fsolver_f
  function fgsl_multiroot_fdfsolver_f(s)
    type(fgsl_multiroot_fdfsolver), intent(in) :: s
    type(fgsl_vector) :: fgsl_multiroot_fdfsolver_f
    fgsl_multiroot_fdfsolver_f%gsl_vector = &
         gsl_multiroot_fdfsolver_f(s%gsl_multiroot_fdfsolver)
  end function fgsl_multiroot_fdfsolver_f
  function fgsl_multiroot_fsolver_dx(s)
    type(fgsl_multiroot_fsolver), intent(in) :: s
    type(fgsl_vector) :: fgsl_multiroot_fsolver_dx
    fgsl_multiroot_fsolver_dx%gsl_vector = &
         gsl_multiroot_fsolver_dx(s%gsl_multiroot_fsolver)
  end function fgsl_multiroot_fsolver_dx
  function fgsl_multiroot_fdfsolver_dx(s)
    type(fgsl_multiroot_fdfsolver), intent(in) :: s
    type(fgsl_vector) :: fgsl_multiroot_fdfsolver_dx
    fgsl_multiroot_fdfsolver_dx%gsl_vector = &
         gsl_multiroot_fdfsolver_dx(s%gsl_multiroot_fdfsolver)
  end function fgsl_multiroot_fdfsolver_dx
  function fgsl_multiroot_test_delta(dx, x, epsabs, epsrel) 
    type(fgsl_vector), intent(in) :: dx, x
    real(fgsl_double), intent(in) :: epsabs, epsrel
    integer(fgsl_int) :: fgsl_multiroot_test_delta
    fgsl_multiroot_test_delta = gsl_multiroot_test_delta(dx%gsl_vector, &
         x%gsl_vector, epsabs, epsrel)
  end function fgsl_multiroot_test_delta
  function fgsl_multiroot_test_residual(f, epsabs) 
    type(fgsl_vector), intent(in) :: f
    real(fgsl_double), intent(in) :: epsabs
    integer(fgsl_int) :: fgsl_multiroot_test_residual
    fgsl_multiroot_test_residual = &
         gsl_multiroot_test_residual(f%gsl_vector, epsabs)
  end function fgsl_multiroot_test_residual
  function fgsl_multiroot_fsolver_status(s)
    type(fgsl_multiroot_fsolver), intent(in) :: s
    logical :: fgsl_multiroot_fsolver_status
    fgsl_multiroot_fsolver_status = .false.
    if (c_associated(s%gsl_multiroot_fsolver)) &
         fgsl_multiroot_fsolver_status = .true.
  end function fgsl_multiroot_fsolver_status
  function fgsl_multiroot_fdfsolver_status(s)
    type(fgsl_multiroot_fdfsolver), intent(in) :: s
    logical :: fgsl_multiroot_fdfsolver_status
    fgsl_multiroot_fdfsolver_status = .false.
    if (c_associated(s%gsl_multiroot_fdfsolver)) &
         fgsl_multiroot_fdfsolver_status = .true.
  end function fgsl_multiroot_fdfsolver_status
end module fgsl_multiroots
