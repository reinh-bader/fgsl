!-*-f90-*-
module fgsl_sf_expint
  !>  Special functions - Exponential Integrals
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_expint_e1_e, gsl_sf_expint_e2_e, gsl_sf_expint_en_e, &
       gsl_sf_expint_ei_e, gsl_sf_shi_e, gsl_sf_chi_e, gsl_sf_expint_3_e, &
       gsl_sf_si_e, gsl_sf_ci_e, gsl_sf_atanint_e

  
  interface
     function fgsl_sf_expint_e1(x) bind(c, name='gsl_sf_expint_E1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_expint_e1
     end function fgsl_sf_expint_e1
     function gsl_sf_expint_e1_e(x, result) bind(c, name='gsl_sf_expint_E1_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_expint_e1_e
     end function gsl_sf_expint_e1_e
     function fgsl_sf_expint_e2(x) bind(c, name='gsl_sf_expint_E2')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_expint_e2
     end function fgsl_sf_expint_e2
     function gsl_sf_expint_e2_e(x, result) bind(c, name='gsl_sf_expint_E2_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_expint_e2_e
     end function gsl_sf_expint_e2_e
     function fgsl_sf_expint_en(n, x) bind(c, name='gsl_sf_expint_En')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_expint_en
     end function fgsl_sf_expint_en
     function gsl_sf_expint_en_e(n, x, result) bind(c, name='gsl_sf_expint_En_e')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_expint_en_e
     end function gsl_sf_expint_en_e
     function fgsl_sf_expint_ei(x) bind(c, name='gsl_sf_expint_Ei')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_expint_ei
     end function fgsl_sf_expint_ei
     function gsl_sf_expint_ei_e(x, result) bind(c, name='gsl_sf_expint_Ei_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_expint_ei_e
     end function gsl_sf_expint_ei_e
     function fgsl_sf_shi(x) bind(c, name='gsl_sf_Shi')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_shi
     end function fgsl_sf_shi
     function gsl_sf_shi_e(x, result) bind(c, name='gsl_sf_Shi_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_shi_e
     end function gsl_sf_shi_e
     function fgsl_sf_chi(x) bind(c, name='gsl_sf_Chi')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_chi
     end function fgsl_sf_chi
     function gsl_sf_chi_e(x, result) bind(c, name='gsl_sf_Chi_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_chi_e
     end function gsl_sf_chi_e
     function fgsl_sf_expint_3(x) bind(c, name='gsl_sf_expint_3')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_expint_3
     end function fgsl_sf_expint_3
     function gsl_sf_expint_3_e(x, result) bind(c, name='gsl_sf_expint_3_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_expint_3_e
     end function gsl_sf_expint_3_e
     function fgsl_sf_si(x) bind(c, name='gsl_sf_Si')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_si
     end function fgsl_sf_si
     function gsl_sf_si_e(x, result) bind(c, name='gsl_sf_Si_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_si_e
     end function gsl_sf_si_e
     function fgsl_sf_ci(x) bind(c, name='gsl_sf_Ci')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_ci
     end function fgsl_sf_ci
     function gsl_sf_ci_e(x, result) bind(c, name='gsl_sf_Ci_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_ci_e
     end function gsl_sf_ci_e
     function fgsl_sf_atanint(x) bind(c, name='gsl_sf_atanint')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_atanint
     end function fgsl_sf_atanint
     function gsl_sf_atanint_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_atanint_e
     end function gsl_sf_atanint_e
  end interface

contains
!  fgsl_sf_expint_e1 --> interface
  function fgsl_sf_expint_e1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_expint_e1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_expint_e1_e = gsl_sf_expint_e1_e(x, res)
    result = res
  end function fgsl_sf_expint_e1_e
!  fgsl_sf_expint_e2 --> interface
  function fgsl_sf_expint_e2_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_expint_e2_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_expint_e2_e = gsl_sf_expint_e2_e(x, res)
    result = res
  end function fgsl_sf_expint_e2_e
!  fgsl_sf_expint_en --> interface
  function fgsl_sf_expint_en_e(n, x, result)
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_expint_en_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_expint_en_e = gsl_sf_expint_en_e(n, x, res)
    result = res
  end function fgsl_sf_expint_en_e
!  fgsl_sf_expint_ei --> interface
  function fgsl_sf_expint_ei_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_expint_ei_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_expint_ei_e = gsl_sf_expint_ei_e(x, res)
    result = res
  end function fgsl_sf_expint_ei_e
!  fgsl_sf_shi --> interface
  function fgsl_sf_shi_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_shi_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_shi_e = gsl_sf_shi_e(x, res)
    result = res
  end function fgsl_sf_shi_e
!  fgsl_sf_chi --> interface
  function fgsl_sf_chi_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_chi_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_chi_e = gsl_sf_chi_e(x, res)
    result = res
  end function fgsl_sf_chi_e
!  fgsl_sf_expint_3 --> interface
  function fgsl_sf_expint_3_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_expint_3_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_expint_3_e = gsl_sf_expint_3_e(x, res)
    result = res
  end function fgsl_sf_expint_3_e
!  fgsl_sf_si --> interface
  function fgsl_sf_si_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_si_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_si_e = gsl_sf_si_e(x, res)
    result = res
  end function fgsl_sf_si_e
!  fgsl_sf_ci --> interface
  function fgsl_sf_ci_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_ci_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_ci_e = gsl_sf_ci_e(x, res)
    result = res
  end function fgsl_sf_ci_e
!  fgsl_sf_atanint --> interface
  function fgsl_sf_atanint_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_atanint_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_atanint_e = gsl_sf_atanint_e(x, res)
    result = res
  end function fgsl_sf_atanint_e
end module fgsl_sf_expint
