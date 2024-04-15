!-*-f90-*-
module fgsl_sf_elementary
  !>  Special functions - Elementary Operations
  use fgsl_sf_types
  implicit none

  private ::  gsl_sf_multiply_e, gsl_sf_multiply_err_e
  
  !> C interfaces
  interface
     function gsl_sf_multiply_e(x, y, result) bind(c)
       import
       real(c_double), value :: x, y
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_multiply_e
     end function gsl_sf_multiply_e
     function gsl_sf_multiply_err_e(x, dx, y, dy, result) bind(c)
       import
       real(c_double), value :: x, y, dx, dy
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_multiply_err_e
     end function gsl_sf_multiply_err_e
  end interface
contains
  function fgsl_sf_multiply_e(x, y, result)
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_multiply_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_multiply_e = gsl_sf_multiply_e(x, y, res)
    result = res
  end function fgsl_sf_multiply_e
  function fgsl_sf_multiply_err_e(x, dx, y, dy, result)
    real(fgsl_double), intent(in) :: x, y, dx, dy
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_multiply_err_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_multiply_err_e = gsl_sf_multiply_err_e(x, dx, y, dy, res)
    result = res
  end function fgsl_sf_multiply_err_e
end module fgsl_sf_elementary
