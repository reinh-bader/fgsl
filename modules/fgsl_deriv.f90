module fgsl_deriv
  !> \page Deriv Numerical Differentiation
  !> See \ref fgsl_deriv for details.
  use fgsl_base
  use fgsl_math

  implicit none
  
  private :: gsl_deriv_central, gsl_deriv_forward, gsl_deriv_backward

  !
  ! C interfaces
  interface
	  function gsl_deriv_central(f, x, h, result, abserr) bind(c)
	    import
	    type(c_ptr), value :: f
	    real(c_double), value :: x, h
	    real(c_double), intent(out) :: result, abserr
	    integer(c_int) :: gsl_deriv_central
	  end function gsl_deriv_central
	  function gsl_deriv_forward(f, x, h, result, abserr) bind(c)
	    import
	    type(c_ptr), value :: f
	    real(c_double), value :: x, h
	    real(c_double), intent(out) :: result, abserr
	    integer(c_int) :: gsl_deriv_forward
	  end function gsl_deriv_forward
	  function gsl_deriv_backward(f, x, h, result, abserr) bind(c)
	    import
	    type(c_ptr), value :: f
	    real(c_double), value :: x, h
	    real(c_double), intent(out) :: result, abserr
	    integer(c_int) :: gsl_deriv_backward
	  end function gsl_deriv_backward
  end interface
contains
  function fgsl_deriv_central(f, x, h, result, abserr) 
    type(fgsl_function), intent(in) :: f
    real(fgsl_double), intent(in) :: x, h
    real(fgsl_double), intent(out) :: result, abserr
    integer(fgsl_int) :: fgsl_deriv_central
    fgsl_deriv_central = gsl_deriv_central(f%gsl_function, x, h, result, abserr) 
  end function fgsl_deriv_central
  function fgsl_deriv_forward(f, x, h, result, abserr) 
    type(fgsl_function), intent(in) :: f
    real(fgsl_double), intent(in) :: x, h
    real(fgsl_double), intent(out) :: result, abserr
    integer(fgsl_int) :: fgsl_deriv_forward
    fgsl_deriv_forward = gsl_deriv_forward(f%gsl_function, x, h, result, abserr) 
  end function fgsl_deriv_forward
  function fgsl_deriv_backward(f, x, h, result, abserr) 
    type(fgsl_function), intent(in) :: f
    real(fgsl_double), intent(in) :: x, h
    real(fgsl_double), intent(out) :: result, abserr
    integer(fgsl_int) :: fgsl_deriv_backward
    fgsl_deriv_backward = gsl_deriv_backward(f%gsl_function, x, h, result, abserr) 
  end function fgsl_deriv_backward
end module fgsl_deriv
