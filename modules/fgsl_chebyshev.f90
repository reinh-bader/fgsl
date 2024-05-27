module fgsl_chebyshev
  !> Chebyshev Approximations
  use fgsl_base
  use fgsl_math

  implicit none
  
  private :: gsl_cheb_alloc, gsl_cheb_free, gsl_cheb_init, &
    gsl_cheb_order, gsl_cheb_size, gsl_cheb_coeffs, gsl_cheb_eval, &
    gsl_cheb_eval_err, gsl_cheb_eval_n, gsl_cheb_eval_n_err, &
    gsl_cheb_calc_deriv, gsl_cheb_calc_integ
  !
  !> Types
  type, public :: fgsl_cheb_series
     private
     type(c_ptr) :: gsl_cheb_series = c_null_ptr
  end type fgsl_cheb_series
  
  !
  !> Generics
  interface fgsl_well_defined
     module procedure fgsl_cheb_series_status
  end interface fgsl_well_defined
  !
  !> C interfaces
  interface
	  function gsl_cheb_alloc(n) bind(c)
	    import
	    integer(c_int), value :: n
	    type(c_ptr) :: gsl_cheb_alloc
	  end function gsl_cheb_alloc
	  subroutine gsl_cheb_free(cs) bind(c)
	    import
	    type(c_ptr), value :: cs
	  end subroutine gsl_cheb_free
	  function gsl_cheb_init(cs, f, a, b) bind(c)
	    import
	    type(c_ptr), value :: cs, f
	    real(c_double), value :: a, b
	    integer(c_int) :: gsl_cheb_init
	  end function gsl_cheb_init
	  function gsl_cheb_order(cs) bind(c)
	    import :: c_ptr, c_size_t
	    integer(c_size_t) :: gsl_cheb_order
	    type(c_ptr), value :: cs
	  end function gsl_cheb_order
	  function gsl_cheb_size(cs) bind(c)
	    import :: c_ptr, c_size_t
	    integer(c_size_t) :: gsl_cheb_size
	    type(c_ptr), value :: cs
	  end function gsl_cheb_size
	  function gsl_cheb_coeffs(cs) bind(c)
	    import :: c_ptr
	    type(c_ptr) :: gsl_cheb_coeffs
	    type(c_ptr), value :: cs
	  end function gsl_cheb_coeffs
	  function gsl_cheb_eval(cs, x) bind(c)
	    import
	    type(c_ptr), value :: cs
	    real(c_double), value :: x
	    real(c_double) :: gsl_cheb_eval
	  end function gsl_cheb_eval
	  function gsl_cheb_eval_err(cs, x, result, abserr) bind(c)
	    import
	    type(c_ptr), value :: cs
	    real(c_double), value :: x
	    real(c_double), intent(out) :: result, abserr
	    integer(c_int) :: gsl_cheb_eval_err
	  end function gsl_cheb_eval_err
	  function gsl_cheb_eval_n(cs, order, x) bind(c)
	    import
	    type(c_ptr), value :: cs
	    integer(c_size_t), value :: order
	    real(c_double), value :: x
	    real(c_double) :: gsl_cheb_eval_n
	  end function gsl_cheb_eval_n
	  function gsl_cheb_eval_n_err(cs, order, x, result, abserr) bind(c)
	    import
	    type(c_ptr), value :: cs
	    integer(c_size_t), value :: order
	    real(c_double), value :: x
	    real(c_double), intent(out) :: result, abserr
	    integer(c_int) :: gsl_cheb_eval_n_err
	  end function gsl_cheb_eval_n_err
	  function gsl_cheb_calc_deriv(deriv, cs) bind(c)
	    import
	    type(c_ptr), value :: deriv, cs
	    integer(c_int) :: gsl_cheb_calc_deriv
	  end function gsl_cheb_calc_deriv
	  function gsl_cheb_calc_integ(integ, cs) bind(c)
	    import
	    type(c_ptr), value :: integ, cs
	    integer(c_int) :: gsl_cheb_calc_integ
	  end function gsl_cheb_calc_integ
  end interface
contains
!> API
  function fgsl_cheb_alloc(n)
    integer(fgsl_int), intent(in) :: n
    type(fgsl_cheb_series) :: fgsl_cheb_alloc
    fgsl_cheb_alloc%gsl_cheb_series = gsl_cheb_alloc(n)
  end function fgsl_cheb_alloc
  subroutine fgsl_cheb_free(cs)
    type(fgsl_cheb_series), intent(in) :: cs
    call gsl_cheb_free(cs%gsl_cheb_series)
  end subroutine fgsl_cheb_free
  function fgsl_cheb_init(cs, f, a, b) 
    type(fgsl_cheb_series), intent(inout) :: cs
    type(fgsl_function), intent(in) :: f
    real(fgsl_double), intent(in) :: a, b
    integer(fgsl_int) :: fgsl_cheb_init
    fgsl_cheb_init = gsl_cheb_init(cs%gsl_cheb_series, f%gsl_function, a, b) 
  end function fgsl_cheb_init
  function fgsl_cheb_order(cs)
    integer(fgsl_size_t) :: fgsl_cheb_order
    type(fgsl_cheb_series), intent(in) :: cs
    fgsl_cheb_order = gsl_cheb_order(cs%gsl_cheb_series)
  end function fgsl_cheb_order
  function fgsl_cheb_size(cs)
    integer(fgsl_size_t) :: fgsl_cheb_size
    type(fgsl_cheb_series), intent(in) :: cs
    fgsl_cheb_size = gsl_cheb_size(cs%gsl_cheb_series)
  end function fgsl_cheb_size
  function fgsl_cheb_coeffs(cs)
    real(fgsl_double), pointer :: fgsl_cheb_coeffs(:)
    type(fgsl_cheb_series), intent(in) :: cs
    integer(fgsl_size_t) :: isz
    type(c_ptr) :: coeff_ptr
    isz = gsl_cheb_size(cs%gsl_cheb_series)
    coeff_ptr = gsl_cheb_coeffs(cs%gsl_cheb_series)
    if (c_associated(coeff_ptr)) then
       call c_f_pointer(coeff_ptr, fgsl_cheb_coeffs, (/isz/))
    else
       nullify(fgsl_cheb_coeffs)
    end if
  end function fgsl_cheb_coeffs
  function fgsl_cheb_eval(cs, x) 
    type(fgsl_cheb_series), intent(in) :: cs
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: fgsl_cheb_eval
    fgsl_cheb_eval = gsl_cheb_eval(cs%gsl_cheb_series, x)
  end function fgsl_cheb_eval
  function fgsl_cheb_eval_err(cs, x, result, abserr) 
    type(fgsl_cheb_series), intent(in) :: cs
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(out) :: result, abserr
    integer(fgsl_int) :: fgsl_cheb_eval_err
    fgsl_cheb_eval_err = gsl_cheb_eval_err(cs%gsl_cheb_series, x, result, abserr) 
  end function fgsl_cheb_eval_err
  function fgsl_cheb_eval_n(cs, order, x) 
    type(fgsl_cheb_series), intent(in) :: cs
    integer(fgsl_size_t), intent(in) :: order
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: fgsl_cheb_eval_n
    fgsl_cheb_eval_n = gsl_cheb_eval_n(cs%gsl_cheb_series, order, x)
  end function fgsl_cheb_eval_n
  function fgsl_cheb_eval_n_err(cs, order, x, result, abserr) 
    type(fgsl_cheb_series), intent(in) :: cs
    integer(fgsl_size_t), intent(in) :: order
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(out) :: result, abserr
    integer(fgsl_int) :: fgsl_cheb_eval_n_err
    fgsl_cheb_eval_n_err = gsl_cheb_eval_n_err(cs%gsl_cheb_series, order, x, result, abserr) 
  end function fgsl_cheb_eval_n_err
  function fgsl_cheb_calc_deriv(deriv, cs) 
    type(fgsl_cheb_series), intent(inout) :: deriv
    type(fgsl_cheb_series), intent(in) :: cs
    integer(fgsl_int) :: fgsl_cheb_calc_deriv
    fgsl_cheb_calc_deriv = gsl_cheb_calc_deriv(deriv%gsl_cheb_series, cs%gsl_cheb_series)
  end function fgsl_cheb_calc_deriv
  function fgsl_cheb_calc_integ(integ, cs) 
    type(fgsl_cheb_series), intent(inout) :: integ
    type(fgsl_cheb_series), intent(in) :: cs
    integer(fgsl_int) :: fgsl_cheb_calc_integ
    fgsl_cheb_calc_integ = gsl_cheb_calc_integ(integ%gsl_cheb_series, cs%gsl_cheb_series)
  end function fgsl_cheb_calc_integ
  function fgsl_cheb_series_status(cheb_series)
    type(fgsl_cheb_series), intent(in) :: cheb_series
    logical :: fgsl_cheb_series_status
    fgsl_cheb_series_status = .true.
    if (.not. c_associated(cheb_series%gsl_cheb_series)) &
         fgsl_cheb_series_status = .false.
  end function fgsl_cheb_series_status
end module fgsl_chebyshev
