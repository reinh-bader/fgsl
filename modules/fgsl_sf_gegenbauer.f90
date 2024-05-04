!-*-f90-*-
module fgsl_sf_gegenbauer
  !>  Special functions - Gegenbauer functions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_gegenpoly_1_e, gsl_sf_gegenpoly_2_e, gsl_sf_gegenpoly_3_e, &
        gsl_sf_gegenpoly_n_e, gsl_sf_gegenpoly_array 
  
  interface
     function fgsl_sf_gegenpoly_1(lambda, x) bind(c, name='gsl_sf_gegenpoly_1')
       import
       real(c_double), value :: lambda, x
       real(c_double) :: fgsl_sf_gegenpoly_1
     end function fgsl_sf_gegenpoly_1
     function gsl_sf_gegenpoly_1_e(lambda, x, result) bind(c)
       import
       real(c_double), value :: lambda, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_gegenpoly_1_e
     end function gsl_sf_gegenpoly_1_e
     function fgsl_sf_gegenpoly_2(lambda, x) bind(c, name='gsl_sf_gegenpoly_2')
       import
       real(c_double), value :: lambda, x
       real(c_double) :: fgsl_sf_gegenpoly_2
     end function fgsl_sf_gegenpoly_2
     function gsl_sf_gegenpoly_2_e(lambda, x, result) bind(c)
       import
       real(c_double), value :: lambda, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_gegenpoly_2_e
     end function gsl_sf_gegenpoly_2_e
     function fgsl_sf_gegenpoly_3(lambda, x) bind(c, name='gsl_sf_gegenpoly_3')
       import
       real(c_double), value :: lambda, x
       real(c_double) :: fgsl_sf_gegenpoly_3
     end function fgsl_sf_gegenpoly_3
     function gsl_sf_gegenpoly_3_e(lambda, x, result) bind(c)
       import
       real(c_double), value :: lambda, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_gegenpoly_3_e
     end function gsl_sf_gegenpoly_3_e
     function fgsl_sf_gegenpoly_n(n, lambda, x) bind(c, name='gsl_sf_gegenpoly_n')
       import
       integer(c_int), value :: n
       real(c_double), value :: lambda, x
       real(c_double) :: fgsl_sf_gegenpoly_n
     end function fgsl_sf_gegenpoly_n
     function gsl_sf_gegenpoly_n_e(n, lambda, x, result) bind(c)
       import
       integer(c_int), value :: n
       real(c_double), value :: lambda, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_gegenpoly_n_e
     end function gsl_sf_gegenpoly_n_e
     function gsl_sf_gegenpoly_array(nmax, lambda, x, result_array) bind(c)
       import
       integer(c_int), value :: nmax
       real(c_double), value :: lambda, x
       type(c_ptr), value :: result_array
       integer(c_int) :: gsl_sf_gegenpoly_array
     end function gsl_sf_gegenpoly_array
  end interface
contains
  !  fgsl_sf_gegenpoly_1 --> interface
  function fgsl_sf_gegenpoly_1_e(lambda, x, result)
    real(fgsl_double), intent(in) :: lambda, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_gegenpoly_1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_gegenpoly_1_e = gsl_sf_gegenpoly_1_e(lambda, x, res)
    result = res
  end function fgsl_sf_gegenpoly_1_e
!  fgsl_sf_gegenpoly_2 --> interface
  function fgsl_sf_gegenpoly_2_e(lambda, x, result)
    real(fgsl_double), intent(in) :: lambda, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_gegenpoly_2_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_gegenpoly_2_e = gsl_sf_gegenpoly_2_e(lambda, x, res)
    result = res
  end function fgsl_sf_gegenpoly_2_e
!  fgsl_sf_gegenpoly_3 --> interface
  function fgsl_sf_gegenpoly_3_e(lambda, x, result)
    real(fgsl_double), intent(in) :: lambda, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_gegenpoly_3_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_gegenpoly_3_e = gsl_sf_gegenpoly_3_e(lambda, x, res)
    result = res
  end function fgsl_sf_gegenpoly_3_e
!  fgsl_sf_gegenpoly_n --> interface
  function fgsl_sf_gegenpoly_n_e(n, lambda, x, result)
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: lambda, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_gegenpoly_n_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_gegenpoly_n_e = gsl_sf_gegenpoly_n_e(n, lambda, x, res)
    result = res
  end function fgsl_sf_gegenpoly_n_e
  function fgsl_sf_gegenpoly_array(lambda, x, result_array)
    real(fgsl_double), intent(in) :: lambda, x
    real(fgsl_double), intent(inout), contiguous, target :: result_array(:)
    integer(fgsl_int) :: fgsl_sf_gegenpoly_array
    fgsl_sf_gegenpoly_array = gsl_sf_gegenpoly_array( &
         size(result_array, kind=fgsl_int)-1,&
         lambda, x, c_loc(result_array))
  end function fgsl_sf_gegenpoly_array
end module fgsl_sf_gegenbauer
