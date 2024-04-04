!-*-f90-*-
module fgsl_math
  !> Mathematical functions
  !> Many of these elementary functions are also available as Fortran 
  !> intrinsics.
  !> The module also contains constructors for function objects. 
  use fgsl_base
  implicit none

  private :: fgsl_function_cinit, fgsl_function_fdf_cinit, fgsl_function_cfree, &
       fgsl_function_fdf_cfree, fgsl_fn_eval_aux, fgsl_fn_fdf_eval_f_aux, &
       fgsl_fn_fdf_eval_df_aux, fgsl_fn_fdf_eval_f_df_aux
  !
  !> types
  !> components are public since other modules need access to them
  type, public :: fgsl_function
     type(c_ptr) :: gsl_function = c_null_ptr
  end type fgsl_function
  type, public :: fgsl_function_fdf
     type(c_ptr) :: gsl_function_fdf = c_null_ptr
  end type fgsl_function_fdf
  !> mathematical constants
  !
  real(fgsl_extended), parameter, public :: m_e = 2.71828182845904523536028747135_fgsl_extended
  real(fgsl_extended), parameter, public :: m_log2e = 1.44269504088896340735992468100_fgsl_extended
  real(fgsl_extended), parameter, public :: m_log10e = 0.43429448190325182765112891892_fgsl_extended
  real(fgsl_extended), parameter, public :: m_sqrt2 = 1.41421356237309504880168872421_fgsl_extended
  real(fgsl_extended), parameter, public :: m_sqrt1_2 = 0.70710678118654752440084436210_fgsl_extended
  real(fgsl_extended), parameter, public :: m_sqrt3 = 1.73205080756887729352744634151_fgsl_extended
  real(fgsl_extended), parameter, public :: m_pi = 3.14159265358979323846264338328_fgsl_extended
  real(fgsl_extended), parameter, public :: m_pi_2 = 1.57079632679489661923132169164_fgsl_extended
  real(fgsl_extended), parameter, public :: m_pi_4 = 0.78539816339744830961566084582_fgsl_extended
  real(fgsl_extended), parameter, public :: m_sqrtpi = 1.77245385090551602729816748334_fgsl_extended
  real(fgsl_extended), parameter, public :: m_2_sqrtpi = 1.12837916709551257389615890312_fgsl_extended
  real(fgsl_extended), parameter, public :: m_1_pi = 0.31830988618379067153776752675_fgsl_extended
  real(fgsl_extended), parameter, public :: m_2_pi = 0.63661977236758134307553505349_fgsl_extended
  real(fgsl_extended), parameter, public :: m_ln10 = 2.30258509299404568401799145468_fgsl_extended
  real(fgsl_extended), parameter, public :: m_ln2 = 0.69314718055994530941723212146_fgsl_extended
  real(fgsl_extended), parameter, public :: m_lnpi = 1.14472988584940017414342735135_fgsl_extended
  real(fgsl_extended), parameter, public :: m_euler = 0.57721566490153286060651209008_fgsl_extended
  ! the following provokes warnings from g95 ... may need to change if refused by other compilers
  !  real(fgsl_double), parameter, public :: fgsl_posinf = 1.0_fgsl_double / 0.0_fgsl_double
  !  real(fgsl_double), parameter, public :: fgsl_neginf = -1.0_fgsl_double / 0.0_fgsl_double
  !  real(fgsl_double), parameter, public :: fgsl_nan = 0.0_fgsl_double / 0.0_fgsl_double
  ! probably should throw this out - use IEEE_VALUE intrinsic if these are needed.
  !


  !   public interfaces
  !
  interface
     function fgsl_isnan(x) bind(c, name='gsl_isnan')
       import
       real(c_double), value :: x
       integer(c_int) :: fgsl_isnan
     end function fgsl_isnan
     function fgsl_isinf(x) bind(c, name='gsl_isinf')
       import
       real(c_double), value :: x
       integer(c_int) :: fgsl_isinf
     end function fgsl_isinf
     function fgsl_finite(x) bind(c, name='gsl_finite')
       import
       real(c_double), value :: x
       integer(c_int) :: fgsl_finite
     end function fgsl_finite
     function fgsl_log1p(x) bind(c, name='gsl_log1p')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_log1p
     end function fgsl_log1p
     function fgsl_expm1(x) bind(c, name='gsl_expm1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_expm1
     end function fgsl_expm1
     function fgsl_hypot(x,y) bind(c, name='gsl_hypot')
       import
       real(c_double), value :: x, y
       real(c_double) :: fgsl_hypot
     end function fgsl_hypot
     function fgsl_hypot3(x,y,z) bind(c, name='gsl_hypot3')
       import
       real(c_double), value :: x, y, z
       real(c_double) :: fgsl_hypot3
     end function fgsl_hypot3
     function fgsl_acosh(x) bind(c, name='gsl_acosh')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_acosh
     end function fgsl_acosh
     function fgsl_asinh(x) bind(c, name='gsl_asinh')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_asinh
     end function fgsl_asinh
     function fgsl_atanh(x) bind(c, name='gsl_atanh')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_atanh
     end function fgsl_atanh
     function fgsl_ldexp(x,e) bind(c, name='gsl_ldexp')
       import
       real(c_double), value :: x
       integer(c_int), value :: e
       real(c_double) :: fgsl_ldexp
     end function fgsl_ldexp
     function fgsl_frexp(x,e) bind(c, name='gsl_frexp')
       import
       real(c_double), value :: x
       integer(c_int), intent(out) :: e
       real(c_double) :: fgsl_frexp
     end function fgsl_frexp
     function fgsl_fcmp(x,y,eps) bind(c, name='gsl_fcmp')
       import
       real(c_double), value :: x, y, eps
       integer(c_int) :: fgsl_fcmp
     end function fgsl_fcmp
  end interface
  !> private interfaces
  interface
  !> constructors for abstract types
     function fgsl_function_cinit(func, params) bind(c)
       import
       type(c_funptr), value :: func
       type(c_ptr), value :: params
       type(c_ptr) :: fgsl_function_cinit
     end function fgsl_function_cinit
     function fgsl_function_fdf_cinit(f, df, fdf, params) bind(c)
       import
       type(c_funptr), value :: f, df, fdf
       type(c_ptr), value :: params
       type(c_ptr) :: fgsl_function_fdf_cinit
     end function fgsl_function_fdf_cinit
     subroutine fgsl_function_cfree(sfunc) bind(c)
       import
       type(c_ptr), value :: sfunc
     end subroutine fgsl_function_cfree
     subroutine fgsl_function_fdf_cfree(sfunc) bind(c)
       import
       type(c_ptr), value :: sfunc
     end subroutine fgsl_function_fdf_cfree
! auxiliary routines
     function fgsl_fn_eval_aux(f, x) bind(c)
       import
       type(c_ptr), value :: f
       real(c_double), value :: x
       real(c_double) :: fgsl_fn_eval_aux
     end function fgsl_fn_eval_aux
     function fgsl_fn_fdf_eval_f_aux(f, x) bind(c)
       import
       type(c_ptr), value :: f
       real(c_double), value :: x
       real(c_double) :: fgsl_fn_fdf_eval_f_aux
     end function fgsl_fn_fdf_eval_f_aux
     function fgsl_fn_fdf_eval_df_aux(f, x) bind(c)
       import
       type(c_ptr), value :: f
       real(c_double), value :: x
       real(c_double) :: fgsl_fn_fdf_eval_df_aux
     end function fgsl_fn_fdf_eval_df_aux
     subroutine fgsl_fn_fdf_eval_f_df_aux(f, x, y, dy) bind(c)
       import
       type(c_ptr), value :: f
       real(c_double), value :: x
       real(c_double), intent(out) :: y, dy
     end subroutine fgsl_fn_fdf_eval_f_df_aux
  end interface
contains
 

!> Constructor for an FGSL function type
!> \param func - interface for a double precision valued function with
!> a parameter of arbitrary type
!> \param params - parameter of arbitrary type
!> \result FGSL function object.
  function fgsl_function_init(func, params)
    interface
       function func(x, params) bind(c)
         use, intrinsic :: iso_c_binding
         real(c_double), value :: x
         type(c_ptr), value :: params
         real(c_double) :: func
       end function func
    end interface
    type(c_ptr), intent(in) :: params
    type(fgsl_function) :: fgsl_function_init
!
    type(c_funptr) :: fp
    fp = c_funloc(func)
    fgsl_function_init%gsl_function = fgsl_function_cinit(fp, params)
  end function fgsl_function_init
!> Constructor for an FGSL function type including a derivative
!> \param f - interface for a double precision valued function with
!> a parameter of arbitrary type
!> \param df - interface for a function evaluating the derivative of f
!> \param fdf - interface for a subroutine evaluating f as well as its
!> derivative given an argument and a parameter.
!> \param params - parameter of arbitrary type
!> \result FGSL function with derivative object.
 function fgsl_function_fdf_init(f, df, fdf, params)
    interface
       function f(x, params) bind(c)
         use, intrinsic :: iso_c_binding
         real(c_double), value :: x
         type(c_ptr), value :: params
         real(c_double) :: f
       end function f
       function df(x, params) bind(c)
         use, intrinsic :: iso_c_binding
         real(c_double), value :: x
         type(c_ptr), value :: params
         real(c_double) :: df
       end function df
       subroutine fdf(x, params, f, df) bind(c)
         use, intrinsic :: iso_c_binding
         real(c_double), value :: x
         type(c_ptr), value :: params
         real(c_double), intent(out) :: f, df
       end subroutine fdf
    end interface
    type(c_ptr), intent(in) :: params
    type(fgsl_function_fdf) :: fgsl_function_fdf_init
!
    type(c_funptr) :: fp, dfp, fdfp
    fp = c_funloc(f)
    dfp = c_funloc(df)
    fdfp = c_funloc(fdf)
    fgsl_function_fdf_init%gsl_function_fdf = fgsl_function_fdf_cinit(fp, dfp, fdfp, params)
  end function fgsl_function_fdf_init
!> Free resources associated with a FGSL function object. 
  subroutine fgsl_function_free(sfunc)
    type(fgsl_function), intent(inout) :: sfunc
    call fgsl_function_cfree(sfunc%gsl_function)
  end subroutine fgsl_function_free
!> Free resources associated with a FGSL function with derivative object. 
  subroutine fgsl_function_fdf_free(sfunc)
    type(fgsl_function_fdf), intent(inout) :: sfunc
    call fgsl_function_fdf_cfree(sfunc%gsl_function_fdf)
  end subroutine fgsl_function_fdf_free
!> Evaluate a function value for a FGSL function object.
!> \param sfunc - function object.
!> \param x - argument value
!> \result Function value 
  function fgsl_fn_eval(sfunc, x)
    type(fgsl_function), intent(inout) :: sfunc
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: fgsl_fn_eval
    fgsl_fn_eval = fgsl_fn_eval_aux(sfunc%gsl_function, x)
  end function fgsl_fn_eval
!> Evaluate a function value for a FGSL function with derivative object.
!> \param sfunc - function with derivative object.
!> \param x - argument value
!> \result Function value 
  function fgsl_fn_fdf_eval_f(sfunc, x)
    type(fgsl_function_fdf), intent(inout) :: sfunc
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: fgsl_fn_fdf_eval_f
    fgsl_fn_fdf_eval_f = fgsl_fn_fdf_eval_f_aux(sfunc%gsl_function_fdf, x)
  end function fgsl_fn_fdf_eval_f
!> Evaluate a derivative value for a FGSL function with derivative object.
!> \param sfunc - function with derivative object.
!> \param x - argument value
!> \result Derivative value
  function fgsl_fn_fdf_eval_df(sfunc, x)
    type(fgsl_function_fdf), intent(inout) :: sfunc
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: fgsl_fn_fdf_eval_df
    fgsl_fn_fdf_eval_df = fgsl_fn_fdf_eval_df_aux(sfunc%gsl_function_fdf, x)
  end function fgsl_fn_fdf_eval_df
!> Evaluate function as well as derivative value for a FGSL function with derivative object.
!> \param sfunc - function with derivative object.
!> \param x - argument value
!> \param y - function value
!> \param dy - derivative value  
 subroutine fgsl_fn_fdf_eval_f_df(sfunc, x, y, dy)
    type(fgsl_function_fdf), intent(inout) :: sfunc
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(out) :: y, dy
    call fgsl_fn_fdf_eval_f_df_aux(sfunc%gsl_function_fdf, x, y, dy)
  end subroutine fgsl_fn_fdf_eval_f_df
end module fgsl_math
