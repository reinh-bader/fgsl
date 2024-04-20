!-*-f90-*-
module fgsl_sf_fermi_dirac
  !>  Special functions - Fermi-Dirac functions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_fermi_dirac_m1_e, gsl_sf_fermi_dirac_0_e, gsl_sf_fermi_dirac_1_e, &
       gsl_sf_fermi_dirac_2_e, gsl_sf_fermi_dirac_int_e, gsl_sf_fermi_dirac_mhalf_e, &
       gsl_sf_fermi_dirac_half_e, gsl_sf_fermi_dirac_3half_e, gsl_sf_fermi_dirac_inc_0_e
       

  interface
     function fgsl_sf_fermi_dirac_m1(x) bind(c, name='gsl_sf_fermi_dirac_m1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_fermi_dirac_m1
     end function fgsl_sf_fermi_dirac_m1
     function gsl_sf_fermi_dirac_m1_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_fermi_dirac_m1_e
     end function gsl_sf_fermi_dirac_m1_e
     function fgsl_sf_fermi_dirac_0(x) bind(c, name='gsl_sf_fermi_dirac_0')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_fermi_dirac_0
     end function fgsl_sf_fermi_dirac_0
     function gsl_sf_fermi_dirac_0_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_fermi_dirac_0_e
     end function gsl_sf_fermi_dirac_0_e
     function fgsl_sf_fermi_dirac_1(x) bind(c, name='gsl_sf_fermi_dirac_1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_fermi_dirac_1
     end function fgsl_sf_fermi_dirac_1
     function gsl_sf_fermi_dirac_1_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_fermi_dirac_1_e
     end function gsl_sf_fermi_dirac_1_e
     function fgsl_sf_fermi_dirac_2(x) bind(c, name='gsl_sf_fermi_dirac_2')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_fermi_dirac_2
     end function fgsl_sf_fermi_dirac_2
     function gsl_sf_fermi_dirac_2_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_fermi_dirac_2_e
     end function gsl_sf_fermi_dirac_2_e
     function fgsl_sf_fermi_dirac_int(i, x) bind(c, name='gsl_sf_fermi_dirac_int')
       import
       integer(c_int), value :: i
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_fermi_dirac_int
     end function fgsl_sf_fermi_dirac_int
     function gsl_sf_fermi_dirac_int_e(i, x, result) bind(c)
       import
       integer(c_int), value :: i
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_fermi_dirac_int_e
     end function gsl_sf_fermi_dirac_int_e
     function fgsl_sf_fermi_dirac_mhalf(x) bind(c, name='gsl_sf_fermi_dirac_mhalf')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_fermi_dirac_mhalf
     end function fgsl_sf_fermi_dirac_mhalf
     function gsl_sf_fermi_dirac_mhalf_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_fermi_dirac_mhalf_e
     end function gsl_sf_fermi_dirac_mhalf_e
     function fgsl_sf_fermi_dirac_half(x) bind(c, name='gsl_sf_fermi_dirac_half')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_fermi_dirac_half
     end function fgsl_sf_fermi_dirac_half
     function gsl_sf_fermi_dirac_half_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_fermi_dirac_half_e
     end function gsl_sf_fermi_dirac_half_e
     function fgsl_sf_fermi_dirac_3half(x) bind(c, name='gsl_sf_fermi_dirac_3half')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_fermi_dirac_3half
     end function fgsl_sf_fermi_dirac_3half
     function gsl_sf_fermi_dirac_3half_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_fermi_dirac_3half_e
     end function gsl_sf_fermi_dirac_3half_e
     function fgsl_sf_fermi_dirac_inc_0(x, b) bind(c, name='gsl_sf_fermi_dirac_inc_0')
       import
       real(c_double), value :: x, b
       real(c_double) :: fgsl_sf_fermi_dirac_inc_0
     end function fgsl_sf_fermi_dirac_inc_0
     function gsl_sf_fermi_dirac_inc_0_e(x, b, result) bind(c)
       import
       real(c_double), value :: x, b
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_fermi_dirac_inc_0_e
     end function gsl_sf_fermi_dirac_inc_0_e
  end interface

contains
!  fgsl_sf_fermi_dirac_m1 --> interface
  function fgsl_sf_fermi_dirac_m1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_fermi_dirac_m1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_fermi_dirac_m1_e = gsl_sf_fermi_dirac_m1_e(x, res)
    result = res
  end function fgsl_sf_fermi_dirac_m1_e
!  fgsl_sf_fermi_dirac_0 --> interface
  function fgsl_sf_fermi_dirac_0_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_fermi_dirac_0_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_fermi_dirac_0_e = gsl_sf_fermi_dirac_0_e(x, res)
    result = res
  end function fgsl_sf_fermi_dirac_0_e
!  fgsl_sf_fermi_dirac_1 --> interface
  function fgsl_sf_fermi_dirac_1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_fermi_dirac_1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_fermi_dirac_1_e = gsl_sf_fermi_dirac_1_e(x, res)
    result = res
  end function fgsl_sf_fermi_dirac_1_e
!  fgsl_sf_fermi_dirac_2 --> interface
  function fgsl_sf_fermi_dirac_2_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_fermi_dirac_2_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_fermi_dirac_2_e = gsl_sf_fermi_dirac_2_e(x, res)
    result = res
  end function fgsl_sf_fermi_dirac_2_e
!  fgsl_sf_fermi_dirac_int --> interface
  function fgsl_sf_fermi_dirac_int_e(i, x, result)
    integer(fgsl_int), intent(in) :: i
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_fermi_dirac_int_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_fermi_dirac_int_e = gsl_sf_fermi_dirac_int_e(i, x, res)
    result = res
  end function fgsl_sf_fermi_dirac_int_e
!  fgsl_sf_fermi_dirac_mhalf --> interface
  function fgsl_sf_fermi_dirac_mhalf_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_fermi_dirac_mhalf_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_fermi_dirac_mhalf_e = gsl_sf_fermi_dirac_mhalf_e(x, res)
    result = res
  end function fgsl_sf_fermi_dirac_mhalf_e
!  fgsl_sf_fermi_dirac_half --> interface
  function fgsl_sf_fermi_dirac_half_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_fermi_dirac_half_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_fermi_dirac_half_e = gsl_sf_fermi_dirac_half_e(x, res)
    result = res
  end function fgsl_sf_fermi_dirac_half_e
!  fgsl_sf_fermi_dirac_3half --> interface
  function fgsl_sf_fermi_dirac_3half_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_fermi_dirac_3half_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_fermi_dirac_3half_e = gsl_sf_fermi_dirac_3half_e(x, res)
    result = res
  end function fgsl_sf_fermi_dirac_3half_e
!  fgsl_sf_fermi_dirac_inc_0 --> interface
  function fgsl_sf_fermi_dirac_inc_0_e(x, b, result)
    real(fgsl_double), intent(in) :: x, b
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_fermi_dirac_inc_0_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_fermi_dirac_inc_0_e = gsl_sf_fermi_dirac_inc_0_e(x, b, res)
    result = res
  end function fgsl_sf_fermi_dirac_inc_0_e
end module fgsl_sf_fermi_dirac
