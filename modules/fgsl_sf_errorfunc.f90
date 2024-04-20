!-*-f90-*-
module fgsl_sf_errorfunc
  !>  Special functions - Error functions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_erf_e, gsl_sf_erfc_e, gsl_sf_log_erfc_e, gsl_sf_erf_z_e, &
       gsl_sf_erf_q_e, gsl_sf_hazard_e

  interface
     function fgsl_sf_erf(x) bind(c, name='gsl_sf_erf')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_erf
     end function fgsl_sf_erf
     function gsl_sf_erf_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_erf_e
     end function gsl_sf_erf_e
     function fgsl_sf_erfc(x) bind(c, name='gsl_sf_erfc')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_erfc
     end function fgsl_sf_erfc
     function gsl_sf_erfc_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_erfc_e
     end function gsl_sf_erfc_e
     function fgsl_sf_log_erfc(x) bind(c, name='gsl_sf_log_erfc')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_log_erfc
     end function fgsl_sf_log_erfc
     function gsl_sf_log_erfc_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_log_erfc_e
     end function gsl_sf_log_erfc_e
     function fgsl_sf_erf_z(x) bind(c, name='gsl_sf_erf_Z')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_erf_Z
     end function fgsl_sf_erf_z
     function gsl_sf_erf_z_e(x, result) bind(c, name='gsl_sf_erf_Z_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_erf_z_e
     end function gsl_sf_erf_z_e
     function fgsl_sf_erf_q(x) bind(c, name='gsl_sf_erf_Q')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_erf_q
     end function fgsl_sf_erf_q
     function gsl_sf_erf_q_e(x, result) bind(c, name='gsl_sf_erf_Q_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_erf_q_e
     end function gsl_sf_erf_q_e
     function fgsl_sf_hazard(x) bind(c, name='gsl_sf_hazard')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_hazard
     end function fgsl_sf_hazard
     function gsl_sf_hazard_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_hazard_e
     end function gsl_sf_hazard_e
  end interface


contains
!  fgsl_sf_erf --> interface
  function fgsl_sf_erf_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_erf_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_erf_e = gsl_sf_erf_e(x, res)
    result = res
  end function fgsl_sf_erf_e
!  fgsl_sf_erfc --> interface
  function fgsl_sf_erfc_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_erfc_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_erfc_e = gsl_sf_erfc_e(x, res)
    result = res
  end function fgsl_sf_erfc_e
!  fgsl_sf_log_erfc --> interface
  function fgsl_sf_log_erfc_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_log_erfc_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_log_erfc_e = gsl_sf_log_erfc_e(x, res)
    result = res
  end function fgsl_sf_log_erfc_e
!  fgsl_sf_erf_z --> interface
  function fgsl_sf_erf_z_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_erf_z_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_erf_z_e = gsl_sf_erf_z_e(x, res)
    result = res
  end function fgsl_sf_erf_z_e
!  fgsl_sf_erf_q --> interface
  function fgsl_sf_erf_q_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_erf_q_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_erf_q_e = gsl_sf_erf_q_e(x, res)
    result = res
  end function fgsl_sf_erf_q_e
!  fgsl_sf_hazard --> interface
  function fgsl_sf_hazard_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hazard_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hazard_e = gsl_sf_hazard_e(x, res)
    result = res
  end function fgsl_sf_hazard_e
end module fgsl_sf_errorfunc
