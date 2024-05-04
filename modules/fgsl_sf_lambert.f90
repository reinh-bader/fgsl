!-*-f90-*-
module fgsl_sf_lambert
  !>  Special functions - Lambert W functions
  use fgsl_sf_types
  implicit none

  private ::  gsl_sf_lambert_w0_e, gsl_sf_lambert_wm1_e
  
  interface
     function fgsl_sf_lambert_w0(x) bind(c, name='gsl_sf_lambert_W0')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_lambert_w0
     end function fgsl_sf_lambert_w0
     function gsl_sf_lambert_w0_e(x, result) bind(c, name='gsl_sf_lambert_W0_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_lambert_w0_e
     end function gsl_sf_lambert_w0_e
     function fgsl_sf_lambert_wm1(x) bind(c, name='gsl_sf_lambert_Wm1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_lambert_wm1
     end function fgsl_sf_lambert_wm1
     function gsl_sf_lambert_wm1_e(x, result) bind(c, name='gsl_sf_lambert_Wm1_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_lambert_wm1_e
     end function gsl_sf_lambert_wm1_e
  end interface

contains
  !  fgsl_sf_lambert_w0 --> interface
  function fgsl_sf_lambert_w0_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_lambert_w0_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_lambert_w0_e = gsl_sf_lambert_w0_e(x, res)
    result = res
  end function fgsl_sf_lambert_w0_e
!  fgsl_sf_lambert_wm1 --> interface
  function fgsl_sf_lambert_wm1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_lambert_wm1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_lambert_wm1_e = gsl_sf_lambert_wm1_e(x, res)
    result = res
  end function fgsl_sf_lambert_wm1_e
!  fgsl_sf_legendre_p1 --> interface
end module fgsl_sf_lambert
