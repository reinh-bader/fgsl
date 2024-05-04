!-*-f90-*-
module fgsl_sf_hyperg
  !>  Special functions - Hypergeometric functions
  use fgsl_sf_types
  implicit none

  private ::  gsl_sf_hyperg_0f1_e, gsl_sf_hyperg_1f1_int_e, gsl_sf_hyperg_1f1_e, &
       gsl_sf_hyperg_u_int_e, gsl_sf_hyperg_u_int_e10_e, gsl_sf_hyperg_u_e, &
       gsl_sf_hyperg_u_e10_e, gsl_sf_hyperg_2f1_e, gsl_sf_hyperg_2f1_conj_e, &
       gsl_sf_hyperg_2f1_renorm_e, gsl_sf_hyperg_2f1_conj_renorm_e, gsl_sf_hyperg_2f0_e
  
  interface
     function fgsl_sf_hyperg_0f1(c, x) bind(c, name='gsl_sf_hyperg_0F1')
       import
       real(c_double), value :: c, x
       real(c_double) :: fgsl_sf_hyperg_0F1
     end function fgsl_sf_hyperg_0f1
     function gsl_sf_hyperg_0f1_e(c, x, result) bind(c, name='gsl_sf_hyperg_0F1_e')
       import
       real(c_double), value :: c, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_hyperg_0F1_e
     end function gsl_sf_hyperg_0f1_e
     function fgsl_sf_hyperg_1f1_int(m, n, x) bind(c, name='gsl_sf_hyperg_1F1_int')
       import
       integer(c_int), value :: m, n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_hyperg_1F1_int
     end function fgsl_sf_hyperg_1f1_int
     function gsl_sf_hyperg_1f1_int_e(m, n, x, result) bind(c, name='gsl_sf_hyperg_1F1_int_e')
       import
       integer(c_int), value :: m, n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_hyperg_1F1_int_e
     end function gsl_sf_hyperg_1f1_int_e
     function fgsl_sf_hyperg_1f1(a, b, x) bind(c, name='gsl_sf_hyperg_1F1')
       import
       real(c_double), value :: a, b, x
       real(c_double) :: fgsl_sf_hyperg_1f1
     end function fgsl_sf_hyperg_1f1
     function gsl_sf_hyperg_1f1_e(a, b, x, result) bind(c, name='gsl_sf_hyperg_1F1_e')
       import
       real(c_double), value :: a, b, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_hyperg_1f1_e
     end function gsl_sf_hyperg_1f1_e
     function fgsl_sf_hyperg_u_int(m, n, x) bind(c, name='gsl_sf_hyperg_U_int')
       import
       integer(c_int), value :: m, n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_hyperg_u_int
     end function fgsl_sf_hyperg_u_int
     function gsl_sf_hyperg_u_int_e(m, n, x, result) bind(c, name='gsl_sf_hyperg_U_int_e')
       import
       integer(c_int), value :: m, n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_hyperg_u_int_e
     end function gsl_sf_hyperg_u_int_e
     function gsl_sf_hyperg_u_int_e10_e(m, n, x, result) bind(c, name='gsl_sf_hyperg_U_int_e10_e')
       import
       integer(c_int), value :: m, n
       real(c_double), value :: x
       type(gsl_sf_result_e10) :: result
       integer(c_int) :: gsl_sf_hyperg_u_int_e10_e
     end function gsl_sf_hyperg_u_int_e10_e
     function fgsl_sf_hyperg_u(a, b, x) bind(c, name='gsl_sf_hyperg_U')
       import
       real(c_double), value :: a, b, x
       real(c_double) :: fgsl_sf_hyperg_u
     end function fgsl_sf_hyperg_u
     function gsl_sf_hyperg_u_e(a, b, x, result) bind(c, name='gsl_sf_hyperg_U_e')
       import
       real(c_double), value :: a, b, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_hyperg_u_e
     end function gsl_sf_hyperg_u_e
     function gsl_sf_hyperg_u_e10_e(a, b, x, result) bind(c, name='gsl_sf_hyperg_U_e10_e')
       import
       real(c_double), value :: a, b, x
       type(gsl_sf_result_e10) :: result
       integer(c_int) :: gsl_sf_hyperg_u_e10_e
     end function gsl_sf_hyperg_u_e10_e
     function fgsl_sf_hyperg_2f1(a, b, c, x) bind(c, name='gsl_sf_hyperg_2F1')
       import
       real(c_double), value :: a, b, c, x
       real(c_double) :: fgsl_sf_hyperg_2f1
     end function fgsl_sf_hyperg_2f1
     function gsl_sf_hyperg_2f1_e(a, b, c, x, result) bind(c, name='gsl_sf_hyperg_2F1_e')
       import
       real(c_double), value :: a, b, c, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_hyperg_2f1_e
     end function gsl_sf_hyperg_2f1_e
     function fgsl_sf_hyperg_2f1_conj(ar, ai, c, x) bind(c, name='gsl_sf_hyperg_2F1_conj')
       import
       real(c_double), value :: ar, ai, c, x
       real(c_double) :: fgsl_sf_hyperg_2f1_conj
     end function fgsl_sf_hyperg_2f1_conj
     function gsl_sf_hyperg_2f1_conj_e(ar, ai, c, x, result) &
          bind(c, name='gsl_sf_hyperg_2F1_conj_e')
       import
       real(c_double), value :: ar, ai, c, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_hyperg_2f1_conj_e
     end function gsl_sf_hyperg_2f1_conj_e
     function fgsl_sf_hyperg_2f1_renorm(a, b, c, x) &
          bind(c, name='gsl_sf_hyperg_2F1_renorm')
       import
       real(c_double), value :: a, b, c, x
       real(c_double) :: fgsl_sf_hyperg_2f1_renorm
     end function fgsl_sf_hyperg_2f1_renorm
     function gsl_sf_hyperg_2f1_renorm_e(a, b, c, x, result) &
          bind(c, name='gsl_sf_hyperg_2F1_renorm_e')
       import
       real(c_double), value :: a, b, c, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_hyperg_2f1_renorm_e
     end function gsl_sf_hyperg_2f1_renorm_e
     function fgsl_sf_hyperg_2f1_conj_renorm(ar, ai, c, x) &
          bind(c, name='gsl_sf_hyperg_2F1_conj_renorm')
       import
       real(c_double), value :: ar, ai, c, x
       real(c_double) :: fgsl_sf_hyperg_2f1_conj_renorm
     end function fgsl_sf_hyperg_2f1_conj_renorm
     function gsl_sf_hyperg_2f1_conj_renorm_e(ar, ai, c, x, result) &
          bind(c, name='gsl_sf_hyperg_2F1_conj_renorm_e')
       import
       real(c_double), value :: ar, ai, c, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_hyperg_2f1_conj_renorm_e
     end function gsl_sf_hyperg_2f1_conj_renorm_e
     function fgsl_sf_hyperg_2f0(a, b, x) bind(c, name='gsl_sf_hyperg_2F0')
       import
       real(c_double), value :: a, b, x
       real(c_double) :: fgsl_sf_hyperg_2f0
     end function fgsl_sf_hyperg_2f0
     function gsl_sf_hyperg_2f0_e(a, b, x, result) bind(c, name='gsl_sf_hyperg_2F0_e')
       import
       real(c_double), value :: a, b, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_hyperg_2f0_e
     end function gsl_sf_hyperg_2f0_e
  end interface

contains
  !  fgsl_sf_hyperg_0f1 --> interface
  function fgsl_sf_hyperg_0f1_e(c, x, result)
    real(fgsl_double), intent(in) :: c, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hyperg_0f1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hyperg_0f1_e = gsl_sf_hyperg_0f1_e(c, x, res)
    result = res
  end function fgsl_sf_hyperg_0f1_e
!  fgsl_sf_hyperg_1f1_int --> interface
  function fgsl_sf_hyperg_1f1_int_e(m, n, x, result)
    integer(fgsl_int), intent(in) :: m, n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hyperg_1f1_int_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hyperg_1f1_int_e = gsl_sf_hyperg_1f1_int_e(m, n, x, res)
    result = res
  end function fgsl_sf_hyperg_1f1_int_e
!  fgsl_sf_hyperg_1f1 --> interface
  function fgsl_sf_hyperg_1f1_e(a, b, x, result)
    real(fgsl_double), intent(in) :: a, b, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hyperg_1f1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hyperg_1f1_e = gsl_sf_hyperg_1f1_e(a, b, x, res)
    result = res
  end function fgsl_sf_hyperg_1f1_e
!  fgsl_sf_hyperg_u_int --> interface
  function fgsl_sf_hyperg_u_int_e(m, n, x, result)
    integer(fgsl_int), intent(in) :: m, n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hyperg_u_int_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hyperg_u_int_e = gsl_sf_hyperg_u_int_e(m, n, x, res)
    result = res
  end function fgsl_sf_hyperg_u_int_e
  function fgsl_sf_hyperg_u_int_e10_e(m, n, x, result)
    integer(fgsl_int), intent(in) :: m, n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result_e10), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hyperg_u_int_e10_e
!
    type(gsl_sf_result_e10) :: res
    fgsl_sf_hyperg_u_int_e10_e = gsl_sf_hyperg_u_int_e10_e(m, n, x, res)
    result = res
  end function fgsl_sf_hyperg_u_int_e10_e
!  fgsl_sf_hyperg_u --> interface
  function fgsl_sf_hyperg_u_e(a, b, x, result)
    real(fgsl_double), intent(in) :: a, b, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hyperg_u_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hyperg_u_e = gsl_sf_hyperg_u_e(a, b, x, res)
    result = res
  end function fgsl_sf_hyperg_u_e
  function fgsl_sf_hyperg_u_e10_e(a, b, x, result)
    real(fgsl_double), intent(in) :: a, b, x
    type(fgsl_sf_result_e10), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hyperg_u_e10_e
!
    type(gsl_sf_result_e10) :: res
    fgsl_sf_hyperg_u_e10_e = gsl_sf_hyperg_u_e10_e(a, b, x, res)
    result = res
  end function fgsl_sf_hyperg_u_e10_e
!  fgsl_sf_hyperg_2f1 --> interface
  function fgsl_sf_hyperg_2f1_e(a, b, c, x, result)
    real(fgsl_double), intent(in) :: a, b, c, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hyperg_2f1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hyperg_2f1_e = gsl_sf_hyperg_2f1_e(a, b, c, x, res)
    result = res
  end function fgsl_sf_hyperg_2f1_e
!  fgsl_sf_hyperg_2f1_conj --> interface
  function fgsl_sf_hyperg_2f1_conj_e(ar, ai, c, x, result)
    real(fgsl_double), intent(in) :: ar, ai, c, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hyperg_2f1_conj_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hyperg_2f1_conj_e = gsl_sf_hyperg_2f1_conj_e(ar, ai, c, x, res)
    result = res
  end function fgsl_sf_hyperg_2f1_conj_e
!  fgsl_sf_hyperg_2f1_renorm --> interface
  function fgsl_sf_hyperg_2f1_renorm_e(a, b, c, x, result)
    real(fgsl_double), intent(in) :: a, b, c, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hyperg_2f1_renorm_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hyperg_2f1_renorm_e = gsl_sf_hyperg_2f1_renorm_e(a, b, c, x, res)
    result = res
  end function fgsl_sf_hyperg_2f1_renorm_e
!  fgsl_sf_hyperg_2f1_conj_renorm --> interface
  function fgsl_sf_hyperg_2f1_conj_renorm_e(ar, ai, c, x, result)
    real(fgsl_double), intent(in) :: ar, ai, c, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hyperg_2f1_conj_renorm_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hyperg_2f1_conj_renorm_e = gsl_sf_hyperg_2f1_conj_renorm_e(ar, ai, c, x, res)
    result = res
  end function fgsl_sf_hyperg_2f1_conj_renorm_e
!  fgsl_sf_hyperg_2f0 --> interface
  function fgsl_sf_hyperg_2f0_e(a, b, x, result)
    real(fgsl_double), intent(in) :: a, b, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hyperg_2f0_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hyperg_2f0_e = gsl_sf_hyperg_2f0_e(a, b, x, res)
    result = res
  end function fgsl_sf_hyperg_2f0_e
end module fgsl_sf_hyperg
