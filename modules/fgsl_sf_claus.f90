!-*-f90-*-
module fgsl_sf_claus
  !>  Special functions - Clausen functions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_clausen_e

  !
  !> C Interfaces
  interface
     function fgsl_sf_clausen(x) bind(c, name='gsl_sf_clausen')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_clausen
     end function fgsl_sf_clausen
     function gsl_sf_clausen_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_clausen_e
     end function gsl_sf_clausen_e
  end interface
contains
  !> API
  function fgsl_sf_clausen_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_clausen_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_clausen_e = gsl_sf_clausen_e(x, res)
    result = res
  end function fgsl_sf_clausen_e
end module fgsl_sf_claus
