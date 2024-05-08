!-*-f90-*-
module fgsl_sf_power
  !>  Special functions - Power Function
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_pow_int_e

  interface
     real(c_double) function fgsl_sf_pow_int(x, n) bind(c, name='gsl_sf_pow_int')
       import :: c_int, c_double
       integer(c_int), value :: n
       real(c_double), value :: x
     end function fgsl_sf_pow_int
     integer(c_int) function gsl_sf_pow_int_e(x, n, result) bind(c, name='gsl_sf_pow_int_e')
       import :: c_int, c_double, gsl_sf_result
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result), intent(inout) :: result
     end function gsl_sf_pow_int_e
  end interface
contains
  integer(c_int) function fgsl_sf_pow_int_e(x, n, result) 
    integer(c_int), intent(in) :: n
    real(c_double), intent(in) :: x
    type(fgsl_sf_result), intent(inout) :: result
    type(gsl_sf_result) :: res
    fgsl_sf_pow_int_e = gsl_sf_pow_int_e(x, n, res)
    result = res
  end function fgsl_sf_pow_int_e
end module fgsl_sf_power
