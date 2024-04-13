!-*-f90-*-
module fgsl_sf_airy
  !>  Special functions - Airy functions

  use fgsl_sf_types
  implicit none

  private :: gsl_sf_airy_ai, gsl_sf_airy_ai_e, gsl_sf_airy_bi, gsl_sf_airy_bi_e, &
       gsl_sf_airy_ai_scaled,  gsl_sf_airy_ai_scaled_e, gsl_sf_airy_bi_scaled, &
       gsl_sf_airy_bi_scaled_e, gsl_sf_airy_ai_deriv, gsl_sf_airy_ai_deriv_e, &
       gsl_sf_airy_bi_deriv, gsl_sf_airy_bi_deriv_e, gsl_sf_airy_ai_deriv_scaled, &
       gsl_sf_airy_ai_deriv_scaled_e, gsl_sf_airy_bi_deriv_scaled, &
       gsl_sf_airy_bi_deriv_scaled_e, gsl_sf_airy_zero_ai, gsl_sf_airy_zero_ai_e, &
       gsl_sf_airy_zero_bi, gsl_sf_airy_zero_bi_e, gsl_sf_airy_zero_ai_deriv, &
       gsl_sf_airy_zero_ai_deriv_e, gsl_sf_airy_zero_bi_deriv, gsl_sf_airy_zero_bi_deriv_e
  
  interface
     function gsl_sf_airy_ai(x, mode) bind(c, name='gsl_sf_airy_Ai')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_airy_ai
     end function gsl_sf_airy_ai
     function gsl_sf_airy_ai_e(x, mode, result) bind(c, name='gsl_sf_airy_Ai_e')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_airy_ai_e
     end function gsl_sf_airy_ai_e
     function gsl_sf_airy_bi(x, mode) bind(c, name='gsl_sf_airy_Bi')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_airy_bi
     end function gsl_sf_airy_bi
     function gsl_sf_airy_bi_e(x, mode, result) bind(c, name='gsl_sf_airy_Bi_e')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_airy_bi_e
     end function gsl_sf_airy_bi_e
     function gsl_sf_airy_ai_scaled(x, mode) bind(c, name='gsl_sf_airy_Ai_scaled')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_airy_ai_scaled
     end function gsl_sf_airy_ai_scaled
     function gsl_sf_airy_ai_scaled_e(x, mode, result) bind(c, name='gsl_sf_airy_Ai_scaled_e')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_airy_ai_scaled_e
     end function gsl_sf_airy_ai_scaled_e
     function gsl_sf_airy_bi_scaled(x, mode) bind(c, name='gsl_sf_airy_Bi_scaled')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_airy_bi_scaled
     end function gsl_sf_airy_bi_scaled
     function gsl_sf_airy_bi_scaled_e(x, mode, result) bind(c, name='gsl_sf_airy_Bi_scaled_e')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_airy_bi_scaled_e
     end function gsl_sf_airy_bi_scaled_e
     function gsl_sf_airy_ai_deriv(x, mode) bind(c, name='gsl_sf_airy_Ai_deriv')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_airy_ai_deriv
     end function gsl_sf_airy_ai_deriv
     function gsl_sf_airy_ai_deriv_e(x, mode, result) bind(c, name='gsl_sf_airy_Ai_deriv_e')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_airy_ai_deriv_e
     end function gsl_sf_airy_ai_deriv_e
     function gsl_sf_airy_bi_deriv(x, mode) bind(c, name='gsl_sf_airy_Bi_deriv')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_airy_bi_deriv
     end function gsl_sf_airy_bi_deriv
     function gsl_sf_airy_bi_deriv_e(x, mode, result) bind(c, name='gsl_sf_airy_Bi_deriv_e')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_airy_bi_deriv_e
     end function gsl_sf_airy_bi_deriv_e
     function gsl_sf_airy_ai_deriv_scaled(x, mode) bind(c, name='gsl_sf_airy_Ai_deriv_scaled')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_airy_ai_deriv_scaled
     end function gsl_sf_airy_ai_deriv_scaled
     function gsl_sf_airy_ai_deriv_scaled_e(x, mode, result) bind(c, name='gsl_sf_airy_Ai_deriv_scaled_e')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_airy_ai_deriv_scaled_e
     end function gsl_sf_airy_ai_deriv_scaled_e
     function gsl_sf_airy_bi_deriv_scaled(x, mode) bind(c, name='gsl_sf_airy_Bi_deriv_scaled')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_airy_bi_deriv_scaled
     end function gsl_sf_airy_bi_deriv_scaled
     function gsl_sf_airy_bi_deriv_scaled_e(x, mode, result) bind(c, name='gsl_sf_airy_Bi_deriv_scaled_e')
       import
       real(c_double), value :: x
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_airy_bi_deriv_scaled_e
     end function gsl_sf_airy_bi_deriv_scaled_e
     function gsl_sf_airy_zero_ai(s)  bind(c, name='gsl_sf_airy_zero_Ai')
       import
       integer(c_int), value :: s
       real(c_double) :: gsl_sf_airy_zero_ai
     end function gsl_sf_airy_zero_ai
     function gsl_sf_airy_zero_ai_e(s, result) bind(c, name='gsl_sf_airy_zero_Ai_e')
       import
       integer(c_int), value :: s
       integer(c_int) :: gsl_sf_airy_zero_ai_e
       type(gsl_sf_result) :: result
     end function gsl_sf_airy_zero_ai_e
     function gsl_sf_airy_zero_bi(s)  bind(c, name='gsl_sf_airy_zero_Bi')
       import
       integer(c_int), value :: s
       real(c_double) :: gsl_sf_airy_zero_bi
     end function gsl_sf_airy_zero_bi
     function gsl_sf_airy_zero_bi_e (s, result) bind(c, name='gsl_sf_airy_zero_Bi_e')
       import
       integer(c_int), value :: s
       integer(c_int) :: gsl_sf_airy_zero_bi_e
       type(gsl_sf_result) :: result
     end function gsl_sf_airy_zero_bi_e
     function gsl_sf_airy_zero_ai_deriv(s)  bind(c, name='gsl_sf_airy_zero_Ai_deriv')
       import
       integer(c_int), value :: s
       real(c_double) :: gsl_sf_airy_zero_ai_deriv
     end function gsl_sf_airy_zero_ai_deriv
     function gsl_sf_airy_zero_ai_deriv_e (s, result) bind(c, name='gsl_sf_airy_zero_Ai_deriv_e')
       import
       integer(c_int), value :: s
       integer(c_int) :: gsl_sf_airy_zero_ai_deriv_e
       type(gsl_sf_result) :: result
     end function gsl_sf_airy_zero_ai_deriv_e
     function gsl_sf_airy_zero_bi_deriv(s)  bind(c, name='gsl_sf_airy_zero_Bi_deriv')
       import
       integer(c_int), value :: s
       real(c_double) :: gsl_sf_airy_zero_bi_deriv
     end function gsl_sf_airy_zero_bi_deriv
     function gsl_sf_airy_zero_bi_deriv_e (s, result) bind(c, name='gsl_sf_airy_zero_Bi_deriv_e')
       import
       integer(c_int), value :: s
       integer(c_int) :: gsl_sf_airy_zero_bi_deriv_e
       type(gsl_sf_result) :: result
     end function gsl_sf_airy_zero_bi_deriv_e
  end interface
contains
    function fgsl_sf_airy_ai(x, mode)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_airy_ai
    fgsl_sf_airy_ai = gsl_sf_airy_ai(x, mode%gsl_mode)
  end function fgsl_sf_airy_ai
  function fgsl_sf_airy_ai_e(x, mode, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_airy_ai_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_airy_ai_e = gsl_sf_airy_ai_e(x, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_airy_ai_e
  function fgsl_sf_airy_bi(x, mode)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_airy_bi
    fgsl_sf_airy_bi = gsl_sf_airy_bi(x, mode%gsl_mode)
  end function fgsl_sf_airy_bi
  function fgsl_sf_airy_bi_e(x, mode, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_airy_bi_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_airy_bi_e = gsl_sf_airy_bi_e(x, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_airy_bi_e
  function fgsl_sf_airy_ai_scaled(x, mode)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_airy_ai_scaled
    fgsl_sf_airy_ai_scaled = gsl_sf_airy_ai_scaled(x, mode%gsl_mode)
  end function fgsl_sf_airy_ai_scaled
  function fgsl_sf_airy_ai_scaled_e(x, mode, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_airy_ai_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_airy_ai_scaled_e = gsl_sf_airy_ai_scaled_e(x, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_airy_ai_scaled_e
  function fgsl_sf_airy_bi_scaled(x, mode)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_airy_bi_scaled
    fgsl_sf_airy_bi_scaled = gsl_sf_airy_bi_scaled(x, mode%gsl_mode)
  end function fgsl_sf_airy_bi_scaled
  function fgsl_sf_airy_bi_scaled_e(x, mode, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_airy_bi_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_airy_bi_scaled_e = gsl_sf_airy_bi_scaled_e(x, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_airy_bi_scaled_e
  function fgsl_sf_airy_ai_deriv(x, mode)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_airy_ai_deriv
    fgsl_sf_airy_ai_deriv = gsl_sf_airy_ai_deriv(x, mode%gsl_mode)
  end function fgsl_sf_airy_ai_deriv
  function fgsl_sf_airy_ai_deriv_e(x, mode, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_airy_ai_deriv_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_airy_ai_deriv_e = gsl_sf_airy_ai_deriv_e(x, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_airy_ai_deriv_e
  function fgsl_sf_airy_bi_deriv(x, mode)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_airy_bi_deriv
!
    fgsl_sf_airy_bi_deriv = gsl_sf_airy_bi_deriv(x, mode%gsl_mode)
  end function fgsl_sf_airy_bi_deriv
  function fgsl_sf_airy_bi_deriv_e(x, mode, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_airy_bi_deriv_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_airy_bi_deriv_e = gsl_sf_airy_bi_deriv_e(x, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_airy_bi_deriv_e
  function fgsl_sf_airy_ai_deriv_scaled(x, mode)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_airy_ai_deriv_scaled
    fgsl_sf_airy_ai_deriv_scaled = gsl_sf_airy_ai_deriv_scaled(x, mode%gsl_mode)
  end function fgsl_sf_airy_ai_deriv_scaled
  function fgsl_sf_airy_ai_deriv_scaled_e(x, mode, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_airy_ai_deriv_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_airy_ai_deriv_scaled_e = gsl_sf_airy_ai_deriv_scaled_e(x, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_airy_ai_deriv_scaled_e
  function fgsl_sf_airy_bi_deriv_scaled(x, mode)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_airy_bi_deriv_scaled
    fgsl_sf_airy_bi_deriv_scaled = gsl_sf_airy_bi_deriv_scaled(x, mode%gsl_mode)
  end function fgsl_sf_airy_bi_deriv_scaled
  function fgsl_sf_airy_bi_deriv_scaled_e(x, mode, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_airy_bi_deriv_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_airy_bi_deriv_scaled_e = gsl_sf_airy_bi_deriv_scaled_e(x, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_airy_bi_deriv_scaled_e
  function fgsl_sf_airy_zero_ai(s)
    integer(fgsl_int), intent(in) :: s
    real(fgsl_double) :: fgsl_sf_airy_zero_ai
    fgsl_sf_airy_zero_ai = gsl_sf_airy_zero_ai(s)
  end function fgsl_sf_airy_zero_ai
  function fgsl_sf_airy_zero_ai_e (s, result)
    integer(fgsl_int), intent(in) :: s
    integer(fgsl_int) :: fgsl_sf_airy_zero_ai_e
    type(fgsl_sf_result), intent(out) :: result
!
    type(gsl_sf_result) :: res
    fgsl_sf_airy_zero_ai_e = gsl_sf_airy_zero_ai_e(s, res)
    result = res
  end function fgsl_sf_airy_zero_ai_e
  function fgsl_sf_airy_zero_bi(s)
    integer(fgsl_int), intent(in) :: s
    real(fgsl_double) :: fgsl_sf_airy_zero_bi
    fgsl_sf_airy_zero_bi = gsl_sf_airy_zero_bi(s)
  end function fgsl_sf_airy_zero_bi
  function fgsl_sf_airy_zero_bi_e (s, result)
    integer(fgsl_int), intent(in) :: s
    integer(fgsl_int) :: fgsl_sf_airy_zero_bi_e
    type(fgsl_sf_result), intent(out) :: result
!
    type(gsl_sf_result) :: res
    fgsl_sf_airy_zero_bi_e = gsl_sf_airy_zero_bi_e(s, res)
    result = res
  end function fgsl_sf_airy_zero_bi_e
  function fgsl_sf_airy_zero_ai_deriv(s)
    integer(fgsl_int), intent(in) :: s
    real(fgsl_double) :: fgsl_sf_airy_zero_ai_deriv
    fgsl_sf_airy_zero_ai_deriv = gsl_sf_airy_zero_ai_deriv(s)
  end function fgsl_sf_airy_zero_ai_deriv
  function fgsl_sf_airy_zero_ai_deriv_e (s, result)
    integer(fgsl_int), intent(in) :: s
    integer(fgsl_int) :: fgsl_sf_airy_zero_ai_deriv_e
    type(fgsl_sf_result), intent(out) :: result
!
    type(gsl_sf_result) :: res
    fgsl_sf_airy_zero_ai_deriv_e = gsl_sf_airy_zero_ai_deriv_e(s, res)
    result = res
  end function fgsl_sf_airy_zero_ai_deriv_e
  function fgsl_sf_airy_zero_bi_deriv(s)
    integer(fgsl_int), intent(in) :: s
    real(fgsl_double) :: fgsl_sf_airy_zero_bi_deriv
    fgsl_sf_airy_zero_bi_deriv = gsl_sf_airy_zero_bi_deriv(s)
  end function fgsl_sf_airy_zero_bi_deriv
  function fgsl_sf_airy_zero_bi_deriv_e (s, result)
    integer(fgsl_int), intent(in) :: s
    integer(fgsl_int) :: fgsl_sf_airy_zero_bi_deriv_e
    type(fgsl_sf_result), intent(out) :: result
!
    type(gsl_sf_result) :: res
    fgsl_sf_airy_zero_bi_deriv_e = gsl_sf_airy_zero_bi_deriv_e(s, res)
    result = res
  end function fgsl_sf_airy_zero_bi_deriv_e
end module fgsl_sf_airy
