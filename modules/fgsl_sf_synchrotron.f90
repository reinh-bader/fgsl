!-*-f90-*-
module fgsl_sf_synchrotron
  !>  Special functions - Synchrotron Functions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_synchrotron_1_e, gsl_sf_synchrotron_2_e

  interface
     function fgsl_sf_synchrotron_1(x) bind(c, name='gsl_sf_synchrotron_1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_synchrotron_1
     end function fgsl_sf_synchrotron_1
     function gsl_sf_synchrotron_1_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_synchrotron_1_e
     end function gsl_sf_synchrotron_1_e
     function fgsl_sf_synchrotron_2(x) bind(c, name='gsl_sf_synchrotron_2')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_synchrotron_2
     end function fgsl_sf_synchrotron_2
     function gsl_sf_synchrotron_2_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_synchrotron_2_e
     end function gsl_sf_synchrotron_2_e     
  end interface
contains
!  fgsl_sf_synchrotron_1 --> interface
  function fgsl_sf_synchrotron_1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_synchrotron_1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_synchrotron_1_e = gsl_sf_synchrotron_1_e(x, res)
    result = res
  end function fgsl_sf_synchrotron_1_e
!  fgsl_sf_synchrotron_2 --> interface
  function fgsl_sf_synchrotron_2_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_synchrotron_2_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_synchrotron_2_e = gsl_sf_synchrotron_2_e(x, res)
    result = res
  end function fgsl_sf_synchrotron_2_e  
end module fgsl_sf_synchrotron
