module fgsl_sf_zetafun
  !>  Special functions - Zeta Functions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_zeta_int_e, gsl_sf_zeta_e, gsl_sf_zetam1_int_e, &
       gsl_sf_zetam1_e, gsl_sf_hzeta_e, gsl_sf_eta_int_e, gsl_sf_eta_e 

  interface
     function fgsl_sf_zeta_int(n) bind(c, name='gsl_sf_zeta_int')
       import
       integer(c_int), value :: n
       real(c_double) :: fgsl_sf_zeta_int
     end function fgsl_sf_zeta_int
     function gsl_sf_zeta_int_e(n, result) bind(c)
       import
       integer(c_int), value :: n
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_zeta_int_e
     end function gsl_sf_zeta_int_e
     function fgsl_sf_zeta(x) bind(c, name='gsl_sf_zeta')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_zeta
     end function fgsl_sf_zeta
     function gsl_sf_zeta_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_zeta_e
     end function gsl_sf_zeta_e
     function fgsl_sf_zetam1_int(n) bind(c, name='gsl_sf_zetam1_int')
       import
       integer(c_int), value :: n
       real(c_double) :: fgsl_sf_zetam1_int
     end function fgsl_sf_zetam1_int
     function gsl_sf_zetam1_int_e(n, result) bind(c)
       import
       integer(c_int), value :: n
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_zetam1_int_e
     end function gsl_sf_zetam1_int_e
     function fgsl_sf_zetam1(x) bind(c, name='gsl_sf_zetam1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_zetam1
     end function fgsl_sf_zetam1
     function gsl_sf_zetam1_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_zetam1_e
     end function gsl_sf_zetam1_e
     function fgsl_sf_hzeta(s, q) bind(c, name='gsl_sf_hzeta')
       import
       real(c_double), value :: s, q
       real(c_double) :: fgsl_sf_hzeta
     end function fgsl_sf_hzeta
     function gsl_sf_hzeta_e(s, q, result) bind(c)
       import
       real(c_double), value :: s, q
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_hzeta_e
     end function gsl_sf_hzeta_e
     function fgsl_sf_eta_int(n) bind(c, name='gsl_sf_eta_int')
       import
       integer(c_int), value :: n
       real(c_double) :: fgsl_sf_eta_int
     end function fgsl_sf_eta_int
     function gsl_sf_eta_int_e(n, result) bind(c)
       import
       integer(c_int), value :: n
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_eta_int_e
     end function gsl_sf_eta_int_e
     function fgsl_sf_eta(x) bind(c, name='gsl_sf_eta')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_eta
     end function fgsl_sf_eta
     function gsl_sf_eta_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_eta_e
     end function gsl_sf_eta_e
  end interface
contains
!  fgsl_sf_zeta_int --> interface
  function fgsl_sf_zeta_int_e(n, result)
    integer(c_int), intent(in) :: n
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_zeta_int_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_zeta_int_e = gsl_sf_zeta_int_e(n, res)
    result = res
  end function fgsl_sf_zeta_int_e
!  fgsl_sf_zeta --> interface
  function fgsl_sf_zeta_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_zeta_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_zeta_e = gsl_sf_zeta_e(x, res)
    result = res
  end function fgsl_sf_zeta_e
!  fgsl_sf_zetam1_int --> interface
  function fgsl_sf_zetam1_int_e(n, result)
    integer(c_int), intent(in) :: n
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_zetam1_int_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_zetam1_int_e = gsl_sf_zetam1_int_e(n, res)
    result = res
  end function fgsl_sf_zetam1_int_e
!  fgsl_sf_zetam1 --> interface
  function fgsl_sf_zetam1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_zetam1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_zetam1_e = gsl_sf_zetam1_e(x, res)
    result = res
  end function fgsl_sf_zetam1_e
!  fgsl_sf_hzeta --> interface
  function fgsl_sf_hzeta_e(s, q, result)
    real(fgsl_double), intent(in) :: s, q
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hzeta_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hzeta_e = gsl_sf_hzeta_e(s, q, res)
    result = res
  end function fgsl_sf_hzeta_e
!  fgsl_sf_eta_int --> interface
  function fgsl_sf_eta_int_e(n, result)
    integer(c_int), intent(in) :: n
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_eta_int_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_eta_int_e = gsl_sf_eta_int_e(n, res)
    result = res
  end function fgsl_sf_eta_int_e
!  fgsl_sf_eta --> interface
  function fgsl_sf_eta_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_eta_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_eta_e = gsl_sf_eta_e(x, res)
    result = res
  end function fgsl_sf_eta_e  
end module fgsl_sf_zetafun
