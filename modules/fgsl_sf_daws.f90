!-*-f90-*-
module fgsl_sf_daws
  !>  Special functions - Dawson function
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_dawson_e
  
  !> C interfaces
  interface
     function fgsl_sf_dawson(x) bind(c, name='gsl_sf_dawson')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_dawson
     end function fgsl_sf_dawson
     function gsl_sf_dawson_e(x, result) bind(c, name='gsl_sf_dawson_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_dawson_e
     end function gsl_sf_dawson_e
  end interface
contains
  !  fgsl_sf_dawson --> interface
  function fgsl_sf_dawson_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_dawson_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_dawson_e = gsl_sf_dawson_e(x, res)
    result = res
  end function fgsl_sf_dawson_e
end module fgsl_sf_daws
