!-*-f90-*-
module fgsl_sf_transport
  !>  Special functions - Transport Functions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_transport_2_e, gsl_sf_transport_3_e, &
       gsl_sf_transport_4_e, gsl_sf_transport_5_e

  interface
     function fgsl_sf_transport_2(x) bind(c, name='gsl_sf_transport_2')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_transport_2
     end function fgsl_sf_transport_2
     function gsl_sf_transport_2_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_transport_2_e
     end function gsl_sf_transport_2_e
     function fgsl_sf_transport_3(x) bind(c, name='gsl_sf_transport_3')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_transport_3
     end function fgsl_sf_transport_3
     function gsl_sf_transport_3_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_transport_3_e
     end function gsl_sf_transport_3_e
     function fgsl_sf_transport_4(x) bind(c, name='gsl_sf_transport_4')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_transport_4
     end function fgsl_sf_transport_4
     function gsl_sf_transport_4_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_transport_4_e
     end function gsl_sf_transport_4_e
     function fgsl_sf_transport_5(x) bind(c, name='gsl_sf_transport_5')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_transport_5
     end function fgsl_sf_transport_5
     function gsl_sf_transport_5_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_transport_5_e
     end function gsl_sf_transport_5_e     
  end interface

contains
!  fgsl_sf_transport_2 --> interface
  function fgsl_sf_transport_2_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_transport_2_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_transport_2_e = gsl_sf_transport_2_e(x, res)
    result = res
  end function fgsl_sf_transport_2_e
!  fgsl_sf_transport_3 --> interface
  function fgsl_sf_transport_3_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_transport_3_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_transport_3_e = gsl_sf_transport_3_e(x, res)
    result = res
  end function fgsl_sf_transport_3_e
!  fgsl_sf_transport_4 --> interface
  function fgsl_sf_transport_4_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_transport_4_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_transport_4_e = gsl_sf_transport_4_e(x, res)
    result = res
  end function fgsl_sf_transport_4_e
!  fgsl_sf_transport_5 --> interface
  function fgsl_sf_transport_5_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_transport_5_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_transport_5_e = gsl_sf_transport_5_e(x, res)
    result = res
  end function fgsl_sf_transport_5_e  
end module fgsl_sf_transport
