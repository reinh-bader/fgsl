!-*-f90-*-
module fgsl_sf_debye
  !>  Special functions - Debye function
  use fgsl_sf_types
  implicit none

  private ::  gsl_sf_debye_1_e,  gsl_sf_debye_2_e,  gsl_sf_debye_3_e, &
        gsl_sf_debye_4_e,  gsl_sf_debye_5_e,  gsl_sf_debye_6_e
  
  !> C interfaces
  interface
     function fgsl_sf_debye_1(x) bind(c, name='gsl_sf_debye_1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_debye_1
     end function fgsl_sf_debye_1
     function gsl_sf_debye_1_e(x, result) bind(c, name='gsl_sf_debye_1_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_debye_1_e
     end function gsl_sf_debye_1_e
     function fgsl_sf_debye_2(x) bind(c, name='gsl_sf_debye_2')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_debye_2
     end function fgsl_sf_debye_2
     function gsl_sf_debye_2_e(x, result) bind(c, name='gsl_sf_debye_2_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_debye_2_e
     end function gsl_sf_debye_2_e
     function fgsl_sf_debye_3(x) bind(c, name='gsl_sf_debye_3')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_debye_3
     end function fgsl_sf_debye_3
     function gsl_sf_debye_3_e(x, result) bind(c, name='gsl_sf_debye_3_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_debye_3_e
     end function gsl_sf_debye_3_e
     function fgsl_sf_debye_4(x) bind(c, name='gsl_sf_debye_4')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_debye_4
     end function fgsl_sf_debye_4
     function gsl_sf_debye_4_e(x, result) bind(c, name='gsl_sf_debye_4_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_debye_4_e
     end function gsl_sf_debye_4_e
     function fgsl_sf_debye_5(x) bind(c, name='gsl_sf_debye_5')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_debye_5
     end function fgsl_sf_debye_5
     function gsl_sf_debye_5_e(x, result) bind(c, name='gsl_sf_debye_5_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_debye_5_e
     end function gsl_sf_debye_5_e
     function fgsl_sf_debye_6(x) bind(c, name='gsl_sf_debye_6')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_debye_6
     end function fgsl_sf_debye_6
     function gsl_sf_debye_6_e(x, result) bind(c, name='gsl_sf_debye_6_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_debye_6_e
     end function gsl_sf_debye_6_e
  end interface
contains
  !> API
  !  fgsl_sf_debye_1 --> interface
  function fgsl_sf_debye_1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_debye_1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_debye_1_e = gsl_sf_debye_1_e(x, res)
    result = res
  end function fgsl_sf_debye_1_e
!  fgsl_sf_debye_2 --> interface
  function fgsl_sf_debye_2_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_debye_2_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_debye_2_e = gsl_sf_debye_2_e(x, res)
    result = res
  end function fgsl_sf_debye_2_e
!  fgsl_sf_debye_3 --> interface
  function fgsl_sf_debye_3_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_debye_3_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_debye_3_e = gsl_sf_debye_3_e(x, res)
    result = res
  end function fgsl_sf_debye_3_e
! fgsl_sf_debye_4 --> interface
  function fgsl_sf_debye_4_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_debye_4_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_debye_4_e = gsl_sf_debye_4_e(x, res)
    result = res
  end function fgsl_sf_debye_4_e
!  fgsl_sf_debye_5 --> interface
  function fgsl_sf_debye_5_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_debye_5_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_debye_5_e = gsl_sf_debye_5_e(x, res)
    result = res
  end function fgsl_sf_debye_5_e
!  fgsl_sf_debye_6 --> interface
  function fgsl_sf_debye_6_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_debye_6_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_debye_6_e = gsl_sf_debye_6_e(x, res)
    result = res
  end function fgsl_sf_debye_6_e
end module fgsl_sf_debye
