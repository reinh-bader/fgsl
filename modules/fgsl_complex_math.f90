!-*-f90-*-
module fgsl_complex_math
  !> Complex numbers 
  !>
  !> Since the Fortran standard provides extensive support for complex
  !> numbers, ony those routines for which no Fortran intrinsic is
  !> available are mapped in FGSL. A standard Fortran
  !> <CODE>complex(fgsl_double)</CODE> is used for all mapped functions,
  !> and it is assumed that this directly maps to the corresponding C11
  !> complex type. The legacy <CODE>gsl_complex</CODE> type (which was
  !> not exposed anyway) is not supported any more.
  use fgsl_base
  implicit none

  interface
     complex(fgsl_double_complex) function fgsl_complex_polar(r, theta) &
          bind(c, name='gsl_complex_polar')
       import
       real(fgsl_double), value :: r, theta
     end function fgsl_complex_polar
     real(fgsl_double) function fgsl_complex_arg(z) &
          bind(c, name='gsl_complex_arg')
       import
       complex(fgsl_double_complex), value :: z
     end function fgsl_complex_arg
     real(fgsl_double) function fgsl_complex_logabs(z) &
          bind(c, name='gsl_complex_arg')
       import
       complex(fgsl_double_complex), value :: z
     end function fgsl_complex_logabs
     complex(fgsl_double_complex) function fgsl_complex_log10(z) &
          bind(c, name='gsl_complex_log10')
       import
       complex(fgsl_double_complex), value :: z
     end function fgsl_complex_log10
     complex(fgsl_double_complex) function fgsl_complex_log_b(z,b) &
          bind(c, name='gsl_complex_log_b')
       import
       complex(fgsl_double_complex), value :: z, b
     end function fgsl_complex_log_b
     !
     ! the following is left in, but intrinsic support is available
     ! in Fortran (asin)
     complex(fgsl_double_complex) function fgsl_complex_arcsin(z) &
          bind(c, name='gsl_complex_arcsin')
       import
       complex(fgsl_double_complex), value :: z
     end function fgsl_complex_arcsin
     complex(fgsl_double_complex) function fgsl_complex_arcsin_real(r) &
          bind(c, name='gsl_complex_arcsin_real')
       import
       real(fgsl_double), value :: r
     end function fgsl_complex_arcsin_real
     !
     ! the following is left in, but intrinsic support is available
     ! in Fortran (acos)
     complex(fgsl_double_complex) function fgsl_complex_arccos(z) &
          bind(c, name='gsl_complex_arccos')
       import
       complex(fgsl_double_complex), value :: z
     end function fgsl_complex_arccos
     complex(fgsl_double_complex) function fgsl_complex_arccos_real(r) &
          bind(c, name='gsl_complex_arccos_real')
       import
       real(fgsl_double), value :: r
     end function fgsl_complex_arccos_real
     !
     ! the following is left in, but intrinsic support is available
     ! in Fortran (atan)
     complex(fgsl_double_complex) function fgsl_complex_arctan(z) &
          bind(c, name='gsl_complex_arctan')
       import
       complex(fgsl_double_complex), value :: z
     end function fgsl_complex_arctan
     complex(fgsl_double_complex) function fgsl_complex_arcsec(z) &
          bind(c, name='gsl_complex_arcsec')
       import 
       complex(fgsl_double_complex), value :: z
     end function fgsl_complex_arcsec
     complex(fgsl_double_complex) function fgsl_complex_arcsec_real(r) &
          bind(c, name='gsl_complex_arcsec_real')
       import
       real(fgsl_double), value :: r
     end function fgsl_complex_arcsec_real
     complex(fgsl_double_complex) function fgsl_complex_arccsc(z) &
          bind(c, name='gsl_complex_arccsc')
       import
       complex(fgsl_double_complex), value :: z
     end function fgsl_complex_arccsc
     complex(fgsl_double_complex) function fgsl_complex_arccsc_real(r) &
          bind(c, name='gsl_complex_arccsc_real')
       import
       real(fgsl_double), value :: r
     end function fgsl_complex_arccsc_real
     complex(fgsl_double_complex) function fgsl_complex_arccot(z) &
          bind(c, name='gsl_complex_arccot')
       import
       complex(fgsl_double_complex), value :: z
     end function fgsl_complex_arccot
     !
     ! the following is left in, but intrinsic support is available
     ! in Fortran (asinh)
     complex(fgsl_double_complex) function fgsl_complex_arcsinh(z) &
          bind(c, name='gsl_complex_arcsinh')
       import
       complex(fgsl_double_complex), value :: z
     end function fgsl_complex_arcsinh
     complex(fgsl_double_complex) function fgsl_complex_arccosh(z) &
          bind(c, name='gsl_complex_arccosh')
       import
       complex(fgsl_double_complex), value :: z
     end function fgsl_complex_arccosh
     complex(fgsl_double_complex) function fgsl_complex_arccosh_real(r) &
          bind(c, name='gsl_complex_arccosh_real')
       import
       real(fgsl_double), value :: r
     end function fgsl_complex_arccosh_real
     complex(fgsl_double_complex) function fgsl_complex_arctanh(z) &
          bind(c, name='gsl_complex_arctanh')
       import
       complex(fgsl_double_complex), value :: z
     end function fgsl_complex_arctanh
     complex(fgsl_double_complex) function fgsl_complex_arctanh_real(r) &
          bind(c, name='gsl_complex_arctanh_real')
       import
       real(fgsl_double), value :: r
     end function fgsl_complex_arctanh_real
     complex(fgsl_double_complex) function fgsl_complex_arcsech(z) &
          bind(c, name='gsl_complex_arcsech')
       import
       complex(fgsl_double_complex), value :: z
     end function fgsl_complex_arcsech
     complex(fgsl_double_complex)  function fgsl_complex_arccsch(z) &
          bind(c, name='gsl_complex_arccsch')
       import
       complex(fgsl_double_complex), value :: z
     end function fgsl_complex_arccsch
     complex(fgsl_double_complex) function fgsl_complex_arccoth(z) &
          bind(c, name='gsl_complex_arccoth')
       import
       complex(fgsl_double_complex), value :: z
     end function fgsl_complex_arccoth
  end interface
  
end module
