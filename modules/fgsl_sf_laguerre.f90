!-*-f90-*-
module fgsl_sf_laguerre
  !>  Special functions - Laguerrecfunctions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_laguerre_1_e, gsl_sf_laguerre_2_e, gsl_sf_laguerre_3_e, &
       gsl_sf_laguerre_n_e
  
  interface
     function fgsl_sf_laguerre_1(a, x) bind(c, name='gsl_sf_laguerre_1')
       import
       real(c_double), value :: a, x
       real(c_double) :: fgsl_sf_laguerre_1
     end function fgsl_sf_laguerre_1
     function gsl_sf_laguerre_1_e(a, x, result) bind(c)
       import
       real(c_double), value :: a, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_laguerre_1_e
     end function gsl_sf_laguerre_1_e
     function fgsl_sf_laguerre_2(a, x) bind(c, name='gsl_sf_laguerre_2')
       import
       real(c_double), value :: a, x
       real(c_double) :: fgsl_sf_laguerre_2
     end function fgsl_sf_laguerre_2
     function gsl_sf_laguerre_2_e(a, x, result) bind(c)
       import
       real(c_double), value :: a, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_laguerre_2_e
     end function gsl_sf_laguerre_2_e
     function fgsl_sf_laguerre_3(a, x) bind(c, name='gsl_sf_laguerre_3')
       import
       real(c_double), value :: a, x
       real(c_double) :: fgsl_sf_laguerre_3
     end function fgsl_sf_laguerre_3
     function gsl_sf_laguerre_3_e(a, x, result) bind(c)
       import
       real(c_double), value :: a, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_laguerre_3_e
     end function gsl_sf_laguerre_3_e
     function fgsl_sf_laguerre_n(n, a, x) bind(c, name='gsl_sf_laguerre_n')
       import
       integer(c_int), value :: n
       real(c_double), value :: a, x
       real(c_double) :: fgsl_sf_laguerre_n
     end function fgsl_sf_laguerre_n
     function gsl_sf_laguerre_n_e(n, a, x, result) bind(c)
       import
       integer(c_int), value :: n
       real(c_double), value :: a, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_laguerre_n_e
     end function gsl_sf_laguerre_n_e
  end interface

contains
  !  fgsl_sf_laguerre_1 --> interface
  function fgsl_sf_laguerre_1_e(a, x, result)
    real(fgsl_double), intent(in) :: a, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_laguerre_1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_laguerre_1_e = gsl_sf_laguerre_1_e(a, x, res)
    result = res
  end function fgsl_sf_laguerre_1_e
!  fgsl_sf_laguerre_2 --> interface
  function fgsl_sf_laguerre_2_e(a, x, result)
    real(fgsl_double), intent(in) :: a, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_laguerre_2_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_laguerre_2_e = gsl_sf_laguerre_2_e(a, x, res)
    result = res
  end function fgsl_sf_laguerre_2_e
!  fgsl_sf_laguerre_3 --> interface
  function fgsl_sf_laguerre_3_e(a, x, result)
    real(fgsl_double), intent(in) :: a, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_laguerre_3_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_laguerre_3_e = gsl_sf_laguerre_3_e(a, x, res)
    result = res
  end function fgsl_sf_laguerre_3_e
!  fgsl_sf_laguerre_n --> interface
  function fgsl_sf_laguerre_n_e(n, a, x, result)
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: a, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_laguerre_n_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_laguerre_n_e = gsl_sf_laguerre_n_e(n, a, x, res)
    result = res
  end function fgsl_sf_laguerre_n_e
end module fgsl_sf_laguerre
