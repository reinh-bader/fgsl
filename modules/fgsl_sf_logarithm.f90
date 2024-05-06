!-*-f90-*-
module fgsl_sf_logarithm
  !>  Special functions - Logarithm and related functions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_log_e, gsl_sf_log_abs_e, gsl_sf_complex_log_e, &
       gsl_sf_log_1plusx_e, gsl_sf_log_1plusx_mx_e

  interface
     function fgsl_sf_log(x) bind(c, name='gsl_sf_log')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_log
     end function fgsl_sf_log
     function gsl_sf_log_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_log_e
     end function gsl_sf_log_e
     function fgsl_sf_log_abs(x) bind(c, name='gsl_sf_log_abs')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_log_abs
     end function fgsl_sf_log_abs
     function gsl_sf_log_abs_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_log_abs_e
     end function gsl_sf_log_abs_e
     function gsl_sf_complex_log_e(zr, zi, lnr, theta) bind(c)
       import
       real(c_double), value :: zr, zi
       type(gsl_sf_result) :: lnr, theta
       integer(c_int) :: gsl_sf_complex_log_e
     end function gsl_sf_complex_log_e
     function fgsl_sf_log_1plusx(x) bind(c, name='gsl_sf_log_1plusx')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_log_1plusx
     end function fgsl_sf_log_1plusx
     function gsl_sf_log_1plusx_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_log_1plusx_e
     end function gsl_sf_log_1plusx_e
     function fgsl_sf_log_1plusx_mx(x) bind(c, name='gsl_sf_log_1plusx_mx')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_log_1plusx_mx
     end function fgsl_sf_log_1plusx_mx
     function gsl_sf_log_1plusx_mx_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_log_1plusx_mx_e
     end function gsl_sf_log_1plusx_mx_e
  end interface

contains
  !  fgsl_sf_log --> interface
  function fgsl_sf_log_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_log_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_log_e = gsl_sf_log_e(x, res)
    result = res
  end function fgsl_sf_log_e
!  fgsl_sf_log_abs --> interface
  function fgsl_sf_log_abs_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_log_abs_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_log_abs_e = gsl_sf_log_abs_e(x, res)
    result = res
  end function fgsl_sf_log_abs_e
  function fgsl_sf_complex_log_e(zr, zi, lnr, theta)
    real(fgsl_double), intent(in) :: zr, zi
    type(fgsl_sf_result), intent(out) :: lnr, theta
    integer(fgsl_int) :: fgsl_sf_complex_log_e
!
    type(gsl_sf_result) :: lnr_loc, theta_loc
    fgsl_sf_complex_log_e = gsl_sf_complex_log_e(zr, zi, lnr_loc, theta_loc)
    lnr = lnr_loc
    theta = theta_loc
  end function fgsl_sf_complex_log_e
!  fgsl_sf_log_1plusx --> interface
  function fgsl_sf_log_1plusx_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_log_1plusx_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_log_1plusx_e = gsl_sf_log_1plusx_e(x, res)
    result = res
  end function fgsl_sf_log_1plusx_e
!  fgsl_sf_log_1plusx_mx --> interface
  function fgsl_sf_log_1plusx_mx_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_log_1plusx_mx_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_log_1plusx_mx_e = gsl_sf_log_1plusx_mx_e(x, res)
    result = res
  end function fgsl_sf_log_1plusx_mx_e
end module fgsl_sf_logarithm
