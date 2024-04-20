!-*-f90-*-
module fgsl_sf_exponential
  !>  Special functions - Exponential functions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_exp_e, gsl_sf_exp_e10_e, gsl_sf_exp_mult_e, gsl_sf_exp_mult_e10_e, &
       gsl_sf_expm1_e, gsl_sf_exprel_e, gsl_sf_exprel_2_e, gsl_sf_exprel_n_e, &
       gsl_sf_exp_err_e, gsl_sf_exp_err_e10_e, gsl_sf_exp_mult_err_e, gsl_sf_exp_mult_err_e10_e
       

  interface
     function fgsl_sf_exp(x) bind(c, name='gsl_sf_exp')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_exp
     end function fgsl_sf_exp
     function gsl_sf_exp_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_exp_e
     end function gsl_sf_exp_e
     function gsl_sf_exp_e10_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result_e10) :: result
       integer(c_int) :: gsl_sf_exp_e10_e
     end function gsl_sf_exp_e10_e
     function fgsl_sf_exp_mult(x, y) bind(c, name='gsl_sf_exp_mult')
       import
       real(c_double), value :: x, y
       real(c_double) :: fgsl_sf_exp_mult
     end function fgsl_sf_exp_mult
     function gsl_sf_exp_mult_e(x, y, result) bind(c)
       import
       real(c_double), value :: x, y
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_exp_mult_e
     end function gsl_sf_exp_mult_e
     function gsl_sf_exp_mult_e10_e(x, y, result) bind(c)
       import
       real(c_double), value :: x, y
       type(gsl_sf_result_e10) :: result
       integer(c_int) :: gsl_sf_exp_mult_e10_e
     end function gsl_sf_exp_mult_e10_e
     function fgsl_sf_expm1(x) bind(c, name='gsl_sf_expm1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_expm1
     end function fgsl_sf_expm1
     function gsl_sf_expm1_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_expm1_e
     end function gsl_sf_expm1_e
     function fgsl_sf_exprel(x) bind(c, name='gsl_sf_exprel')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_exprel
     end function fgsl_sf_exprel
     function gsl_sf_exprel_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_exprel_e
     end function gsl_sf_exprel_e
     function fgsl_sf_exprel_2(x) bind(c, name='gsl_sf_exprel_2')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_exprel_2
     end function fgsl_sf_exprel_2
     function gsl_sf_exprel_2_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_exprel_2_e
     end function gsl_sf_exprel_2_e
     function fgsl_sf_exprel_n(n, x) bind(c, name='gsl_sf_exprel_n')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_exprel_n
     end function fgsl_sf_exprel_n
     function gsl_sf_exprel_n_e(n, x, result) bind(c)
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_exprel_n_e
     end function gsl_sf_exprel_n_e
     function gsl_sf_exp_err_e(x, dx, result) bind(c)
       import
       real(c_double), value :: x, dx
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_exp_err_e
     end function gsl_sf_exp_err_e
     function gsl_sf_exp_err_e10_e(x, dx, result) bind(c)
       import
       real(c_double), value :: x, dx
       type(gsl_sf_result_e10) :: result
       integer(c_int) :: gsl_sf_exp_err_e10_e
     end function gsl_sf_exp_err_e10_e
     function gsl_sf_exp_mult_err_e(x, dx, y, dy, result) bind(c)
       import
       real(c_double), value :: x, dx, y, dy
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_exp_mult_err_e
     end function gsl_sf_exp_mult_err_e
     function gsl_sf_exp_mult_err_e10_e(x, dx, y, dy, result) bind(c)
       import
       real(c_double), value :: x, dx, y, dy
       type(gsl_sf_result_e10) :: result
       integer(c_int) :: gsl_sf_exp_mult_err_e10_e
     end function gsl_sf_exp_mult_err_e10_e
  end interface
contains
!  fgsl_sf_exp --> interface
  function fgsl_sf_exp_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_exp_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_exp_e = gsl_sf_exp_e(x, res)
    result = res
  end function fgsl_sf_exp_e
  function fgsl_sf_exp_e10_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result_e10), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_exp_e10_e
!
    type(gsl_sf_result_e10) :: res
    fgsl_sf_exp_e10_e = gsl_sf_exp_e10_e(x, res)
    result = res
  end function fgsl_sf_exp_e10_e
!  fgsl_sf_exp_mult --> interface
  function fgsl_sf_exp_mult_e(x, y, result)
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_exp_mult_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_exp_mult_e = gsl_sf_exp_mult_e(x, y, res)
    result = res
  end function fgsl_sf_exp_mult_e
  function fgsl_sf_exp_mult_e10_e(x, y, result)
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_sf_result_e10), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_exp_mult_e10_e
!
    type(gsl_sf_result_e10) :: res
    fgsl_sf_exp_mult_e10_e = gsl_sf_exp_mult_e10_e(x, y, res)
    result = res
  end function fgsl_sf_exp_mult_e10_e
!  fgsl_sf_expm1 --> interface
  function fgsl_sf_expm1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_expm1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_expm1_e = gsl_sf_expm1_e(x, res)
    result = res
  end function fgsl_sf_expm1_e
!  fgsl_sf_exprel --> interface
  function fgsl_sf_exprel_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_exprel_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_exprel_e = gsl_sf_exprel_e(x, res)
    result = res
  end function fgsl_sf_exprel_e
!  fgsl_sf_exprel_2 --> interface
  function fgsl_sf_exprel_2_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_exprel_2_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_exprel_2_e = gsl_sf_exprel_2_e(x, res)
    result = res
  end function fgsl_sf_exprel_2_e
!  fgsl_sf_exprel_n --> interface
  function fgsl_sf_exprel_n_e(n, x, result)
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_exprel_n_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_exprel_n_e = gsl_sf_exprel_n_e(n, x, res)
    result = res
  end function fgsl_sf_exprel_n_e
  function fgsl_sf_exp_err_e(x, dx, result)
    real(fgsl_double), intent(in) :: x, dx
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_exp_err_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_exp_err_e = gsl_sf_exp_err_e(x, dx, res)
    result = res
  end function fgsl_sf_exp_err_e
  function fgsl_sf_exp_err_e10_e(x, dx, result)
    real(fgsl_double), intent(in) :: x, dx
    type(fgsl_sf_result_e10), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_exp_err_e10_e
!
    type(gsl_sf_result_e10) :: res
    fgsl_sf_exp_err_e10_e = gsl_sf_exp_err_e10_e(x, dx, res)
    result = res
  end function fgsl_sf_exp_err_e10_e
  function fgsl_sf_exp_mult_err_e(x, dx, y, dy, result)
    real(fgsl_double), intent(in) :: x, dx, y, dy
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_exp_mult_err_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_exp_mult_err_e = gsl_sf_exp_mult_err_e(x, dx, y, dy, res)
    result = res
  end function fgsl_sf_exp_mult_err_e
  function fgsl_sf_exp_mult_err_e10_e(x, dx, y, dy, result)
    real(fgsl_double), intent(in) :: x, dx, y, dy
    type(fgsl_sf_result_e10), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_exp_mult_err_e10_e
!
    type(gsl_sf_result_e10) :: res
    fgsl_sf_exp_mult_err_e10_e = gsl_sf_exp_mult_err_e10_e(x, dx, y, dy, res)
    result = res
  end function fgsl_sf_exp_mult_err_e10_e
end module fgsl_sf_exponential
