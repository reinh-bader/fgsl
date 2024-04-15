!-*-f90-*-
module fgsl_sf_dilogarithm
  !>  Special functions - Dilogarithm
  use fgsl_sf_types
  implicit none

  private ::  gsl_sf_dilog_e, gsl_sf_complex_dilog_e
  
  
  !> C interfaces
  interface
     function fgsl_sf_dilog(x) bind(c, name='gsl_sf_dilog')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_dilog
     end function fgsl_sf_dilog
     function gsl_sf_dilog_e(x, result) bind(c, name='gsl_sf_dilog_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_dilog_e
     end function gsl_sf_dilog_e
     function gsl_sf_complex_dilog_e(r, theta, result_re, result_im) bind(c)
       import
       real(c_double), value :: r, theta
       type(gsl_sf_result) :: result_re, result_im
       integer(c_int) :: gsl_sf_complex_dilog_e
     end function gsl_sf_complex_dilog_e
  end interface
contains
  !> API
  !  fgsl_sf_dilog --> interface
  function fgsl_sf_dilog_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_dilog_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_dilog_e = gsl_sf_dilog_e(x, res)
    result = res
  end function fgsl_sf_dilog_e
  function fgsl_sf_complex_dilog_e(r, theta, result_re, result_im)
    real(fgsl_double), intent(in) :: r, theta
    type(fgsl_sf_result), intent(out) :: result_re, result_im
    integer(fgsl_int) :: fgsl_sf_complex_dilog_e
!
    type(gsl_sf_result) :: res_re, res_im
    fgsl_sf_complex_dilog_e = gsl_sf_complex_dilog_e(r, theta, res_re, res_im)
    result_re = res_re
    result_im = res_im
  end function fgsl_sf_complex_dilog_e
end module fgsl_sf_dilogarithm
