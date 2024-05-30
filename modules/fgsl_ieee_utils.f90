module fgsl_ieee_utils
  !> IEEE floating point arithmetic
  !> Note: interaction between the Fortran run time settings and C may 
  !> lead to unreliable behaviour; for example, setting of IEEE rounding
  !> apparently does not always work correctly. 
  !> Within Fortran, usage of the facilities defined in the intrinsic 
  !> IEEE modules is the reliable and therefore appropriate method. 
  use fgsl_base
  use fgsl_io

  implicit none
  
  private :: gsl_ieee_fprintf_float, gsl_ieee_fprintf_double, &
    gsl_ieee_printf_float, gsl_ieee_printf_double, &
    gsl_ieee_env_setup
  ! 
  !> Generics
  interface fgsl_ieee_fprintf
     module procedure fgsl_ieee_fprintf_float
     module procedure fgsl_ieee_fprintf_double
  end interface
  interface fgsl_ieee_printf
     module procedure fgsl_ieee_printf_float
     module procedure fgsl_ieee_printf_double
  end interface

  !
  !> C interfaces
  interface
	  subroutine gsl_ieee_fprintf_float(stream, x) bind(c)
	    import :: c_ptr, c_float
	    type(c_ptr), value :: stream
	    real(c_float) :: x
	  end subroutine gsl_ieee_fprintf_float
	  subroutine gsl_ieee_fprintf_double(stream, x) bind(c)
	    import :: c_ptr, c_double
	    type(c_ptr), value :: stream
	    real(c_double) :: x
	  end subroutine gsl_ieee_fprintf_double
	  subroutine gsl_ieee_printf_float(x) bind(c)
	    import :: c_float
	    real(c_float) :: x
	  end subroutine gsl_ieee_printf_float
	  subroutine gsl_ieee_printf_double(x) bind(c)
	    import :: c_double
	    real(c_double) :: x
	  end subroutine gsl_ieee_printf_double
	  subroutine gsl_ieee_env_setup() bind(c)
	  end subroutine gsl_ieee_env_setup
  end interface
contains
!
!>  API
  subroutine fgsl_ieee_fprintf_float(stream, x) 
    type(fgsl_file), intent(in) :: stream
    real(fgsl_float) :: x
    call gsl_ieee_fprintf_float(stream%gsl_file, x)
  end subroutine fgsl_ieee_fprintf_float
  subroutine fgsl_ieee_fprintf_double(stream, x) 
    type(fgsl_file), intent(in) :: stream
    real(fgsl_double) :: x
    call gsl_ieee_fprintf_double(stream%gsl_file, x)
  end subroutine fgsl_ieee_fprintf_double
  subroutine fgsl_ieee_printf_float(x) 
    real(fgsl_float) :: x
    call gsl_ieee_printf_float(x)
  end subroutine fgsl_ieee_printf_float
  subroutine fgsl_ieee_printf_double(x) 
    real(fgsl_double) :: x
    call gsl_ieee_printf_double(x)
  end subroutine fgsl_ieee_printf_double
  subroutine fgsl_ieee_env_setup()
    call gsl_ieee_env_setup()
  end subroutine fgsl_ieee_env_setup
! FIXME (maybe): some routines from the include file have not been mapped. 

end module fgsl_ieee_utils
