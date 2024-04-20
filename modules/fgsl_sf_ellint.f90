!-*-f90-*-
module fgsl_sf_ellint
  !>  Special functions - Elliptic integrals
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_ellint_kcomp,  gsl_sf_ellint_kcomp_e, gsl_sf_ellint_ecomp, gsl_sf_ellint_ecomp_e, &
       gsl_sf_ellint_pcomp, gsl_sf_ellint_pcomp_e, gsl_sf_ellint_f, gsl_sf_ellint_f_e, &
       gsl_sf_ellint_e,  gsl_sf_ellint_e_e, gsl_sf_ellint_p, gsl_sf_ellint_p_e, gsl_sf_ellint_d, &
       gsl_sf_ellint_d_e, gsl_sf_ellint_rc, gsl_sf_ellint_rc_e, gsl_sf_ellint_rd, gsl_sf_ellint_rd_e, &
       gsl_sf_ellint_rf, gsl_sf_ellint_rf_e, gsl_sf_ellint_rj, gsl_sf_ellint_rj_e

  interface
     function gsl_sf_ellint_kcomp(k, mode) bind(c, name='gsl_sf_ellint_Kcomp')
       import
       real(c_double), value :: k
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_ellint_kcomp
     end function gsl_sf_ellint_kcomp
     function gsl_sf_ellint_kcomp_e(k, mode, result) bind(c, name='gsl_sf_ellint_Kcomp_e')
       import
       real(c_double), value :: k
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_ellint_kcomp_e
     end function gsl_sf_ellint_kcomp_e
     function gsl_sf_ellint_ecomp(k, mode) bind(c, name='gsl_sf_ellint_Ecomp')
       import
       real(c_double), value :: k
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_ellint_ecomp
     end function gsl_sf_ellint_ecomp
     function gsl_sf_ellint_ecomp_e(k, mode, result) bind(c, name='gsl_sf_ellint_Ecomp_e')
       import
       real(c_double), value :: k
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_ellint_ecomp_e
     end function gsl_sf_ellint_ecomp_e
     function gsl_sf_ellint_pcomp(k, n, mode) bind(c, name='gsl_sf_ellint_Pcomp')
       import
       real(c_double), value :: k, n
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_ellint_pcomp
     end function gsl_sf_ellint_pcomp
     function gsl_sf_ellint_pcomp_e(k, n, mode, result) bind(c, name='gsl_sf_ellint_Pcomp_e')
       import
       real(c_double), value :: k, n
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_ellint_pcomp_e
     end function gsl_sf_ellint_pcomp_e
     function gsl_sf_ellint_f(phi, k, mode) bind(c, name='gsl_sf_ellint_F')
       import
       real(c_double), value :: phi, k
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_ellint_f
     end function gsl_sf_ellint_f
     function gsl_sf_ellint_f_e(phi, k, mode, result) bind(c, name='gsl_sf_ellint_F_e')
       import
       real(c_double), value :: phi, k
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_ellint_f_e
     end function gsl_sf_ellint_f_e
     function gsl_sf_ellint_e(phi, k, mode) bind(c, name='gsl_sf_ellint_E')
       import
       real(c_double), value :: phi, k
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_ellint_e
     end function gsl_sf_ellint_e
     function gsl_sf_ellint_e_e(phi, k, mode, result) bind(c, name='gsl_sf_ellint_E_e')
       import
       real(c_double), value :: phi, k
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_ellint_e_e
     end function gsl_sf_ellint_e_e
     function gsl_sf_ellint_p(phi, k, n, mode) bind(c, name='gsl_sf_ellint_P')
       import
       real(c_double), value :: phi, k, n
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_ellint_p
     end function gsl_sf_ellint_p
     function gsl_sf_ellint_p_e(phi, k, n, mode, result) bind(c, name='gsl_sf_ellint_P_e')
       import
       real(c_double), value :: phi, k, n
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_ellint_p_e
     end function gsl_sf_ellint_p_e
     function gsl_sf_ellint_d(phi, k, mode) bind(c, name='gsl_sf_ellint_D')
       import
       real(c_double), value :: phi, k
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_ellint_d
     end function gsl_sf_ellint_d
     function gsl_sf_ellint_d_e(phi, k, mode, result) bind(c, name='gsl_sf_ellint_D_e')
       import
       real(c_double), value :: phi, k
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_ellint_d_e
     end function gsl_sf_ellint_d_e
     function gsl_sf_ellint_rc(x, y, mode) bind(c, name='gsl_sf_ellint_RC')
       import
       real(c_double), value :: x, y
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_ellint_rc
     end function gsl_sf_ellint_rc
     function gsl_sf_ellint_rc_e(x, y, mode, result) bind(c, name='gsl_sf_ellint_RC_e')
       import
       real(c_double), value :: x, y
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_ellint_rc_e
     end function gsl_sf_ellint_rc_e
     function gsl_sf_ellint_rd(x, y, z, mode) bind(c, name='gsl_sf_ellint_RD')
       import
       real(c_double), value :: x, y, z
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_ellint_rd
     end function gsl_sf_ellint_rd
     function gsl_sf_ellint_rd_e(x, y, z, mode, result) bind(c, name='gsl_sf_ellint_RD_e')
       import
       real(c_double), value :: x, y, z
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_ellint_rd_e
     end function gsl_sf_ellint_rd_e
     function gsl_sf_ellint_rf(x, y, z, mode) bind(c, name='gsl_sf_ellint_RF')
       import
       real(c_double), value :: x, y, z
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_ellint_rf
     end function gsl_sf_ellint_rf
     function gsl_sf_ellint_rf_e(x, y, z, mode, result) bind(c, name='gsl_sf_ellint_RF_e')
       import
       real(c_double), value :: x, y, z
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_ellint_rf_e
     end function gsl_sf_ellint_rf_e
     function gsl_sf_ellint_rj(x, y, z, p, mode) bind(c, name='gsl_sf_ellint_RJ')
       import
       real(c_double), value :: x, y, z, p
       integer(c_int), value :: mode
       real(c_double) :: gsl_sf_ellint_rj
     end function gsl_sf_ellint_rj
     function gsl_sf_ellint_rj_e(x, y, z, p, mode, result) bind(c, name='gsl_sf_ellint_RJ_e')
       import
       real(c_double), value :: x, y, z, p
       integer(c_int), value :: mode
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_ellint_rj_e
     end function gsl_sf_ellint_rj_e
  end interface

contains
   function fgsl_sf_ellint_kcomp(k, mode)
    real(fgsl_double), intent(in) :: k
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_ellint_kcomp
    fgsl_sf_ellint_kcomp = gsl_sf_ellint_kcomp(k, mode%gsl_mode)
  end function fgsl_sf_ellint_kcomp
  function fgsl_sf_ellint_kcomp_e(k, mode, result)
    real(fgsl_double), intent(in) :: k
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_ellint_kcomp_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_ellint_kcomp_e = gsl_sf_ellint_kcomp_e(k, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_ellint_kcomp_e
  function fgsl_sf_ellint_ecomp(k, mode)
    real(fgsl_double), intent(in) :: k
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_ellint_ecomp
    fgsl_sf_ellint_ecomp = gsl_sf_ellint_ecomp(k, mode%gsl_mode)
  end function fgsl_sf_ellint_ecomp
  function fgsl_sf_ellint_ecomp_e(k, mode, result)
    real(fgsl_double), intent(in) :: k
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_ellint_ecomp_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_ellint_ecomp_e = gsl_sf_ellint_ecomp_e(k, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_ellint_ecomp_e
  function fgsl_sf_ellint_pcomp(k, n, mode)
    real(fgsl_double), intent(in) :: k, n
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_ellint_pcomp
    fgsl_sf_ellint_pcomp = gsl_sf_ellint_pcomp(k, n, mode%gsl_mode)
  end function fgsl_sf_ellint_pcomp
  function fgsl_sf_ellint_pcomp_e(k, n, mode, result)
    real(fgsl_double), intent(in) :: k, n
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_ellint_pcomp_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_ellint_pcomp_e = gsl_sf_ellint_pcomp_e(k, n, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_ellint_pcomp_e
  function fgsl_sf_ellint_f(phi, k, mode)
    real(fgsl_double), intent(in) :: phi, k
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_ellint_f
    fgsl_sf_ellint_f = gsl_sf_ellint_f(phi, k, mode%gsl_mode)
  end function fgsl_sf_ellint_f
  function fgsl_sf_ellint_f_e(phi, k, mode, result)
    real(fgsl_double), intent(in) :: phi, k
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_ellint_f_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_ellint_f_e = gsl_sf_ellint_f_e(phi, k, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_ellint_f_e
  function fgsl_sf_ellint_e(phi, k, mode)
    real(fgsl_double), intent(in) :: phi, k
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_ellint_e
    fgsl_sf_ellint_e = gsl_sf_ellint_e(phi, k, mode%gsl_mode)
  end function fgsl_sf_ellint_e
  function fgsl_sf_ellint_e_e(phi, k, mode, result)
    real(fgsl_double), intent(in) :: phi, k
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_ellint_e_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_ellint_e_e = gsl_sf_ellint_e_e(phi, k, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_ellint_e_e
  function fgsl_sf_ellint_p(phi, k, n, mode)
    real(fgsl_double), intent(in) :: phi, k, n
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_ellint_p
    fgsl_sf_ellint_p = gsl_sf_ellint_p(phi, k, n, mode%gsl_mode)
  end function fgsl_sf_ellint_p
  function fgsl_sf_ellint_p_e(phi, k, n, mode, result)
    real(fgsl_double), intent(in) :: phi, k, n
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_ellint_p_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_ellint_p_e = gsl_sf_ellint_p_e(phi, k, n, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_ellint_p_e
  function fgsl_sf_ellint_d(phi, k, mode)
    real(fgsl_double), intent(in) :: phi, k
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_ellint_d
    fgsl_sf_ellint_d = gsl_sf_ellint_d(phi, k, mode%gsl_mode)
  end function fgsl_sf_ellint_d
  function fgsl_sf_ellint_d_e(phi, k, mode, result)
    real(fgsl_double), intent(in) :: phi, k
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_ellint_d_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_ellint_d_e = gsl_sf_ellint_d_e(phi, k, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_ellint_d_e
  function fgsl_sf_ellint_rc(x, y, mode)
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_ellint_rc
    fgsl_sf_ellint_rc = gsl_sf_ellint_rc(x, y, mode%gsl_mode)
  end function fgsl_sf_ellint_rc
  function fgsl_sf_ellint_rc_e(x, y, mode, result)
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_ellint_rc_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_ellint_rc_e = gsl_sf_ellint_rc_e(x, y, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_ellint_rc_e
  function fgsl_sf_ellint_rd(x, y, z, mode)
    real(fgsl_double), intent(in) :: x, y, z
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_ellint_rd
    fgsl_sf_ellint_rd = gsl_sf_ellint_rd(x, y, z, mode%gsl_mode)
  end function fgsl_sf_ellint_rd
  function fgsl_sf_ellint_rd_e(x, y, z, mode, result)
    real(fgsl_double), intent(in) :: x, y, z
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_ellint_rd_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_ellint_rd_e = gsl_sf_ellint_rd_e(x, y, z, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_ellint_rd_e
  function fgsl_sf_ellint_rf(x, y, z, mode)
    real(fgsl_double), intent(in) :: x, y, z
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_ellint_rf
    fgsl_sf_ellint_rf = gsl_sf_ellint_rf(x, y, z, mode%gsl_mode)
  end function fgsl_sf_ellint_rf
  function fgsl_sf_ellint_rf_e(x, y, z, mode, result)
    real(fgsl_double), intent(in) :: x, y, z
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_ellint_rf_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_ellint_rf_e = gsl_sf_ellint_rf_e(x, y, z, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_ellint_rf_e
  function fgsl_sf_ellint_rj(x, y, z, p, mode)
    real(fgsl_double), intent(in) :: x, y, z, p
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double) :: fgsl_sf_ellint_rj
    fgsl_sf_ellint_rj = gsl_sf_ellint_rj(x, y, z, p, mode%gsl_mode)
  end function fgsl_sf_ellint_rj
  function fgsl_sf_ellint_rj_e(x, y, z, p, mode, result)
    real(fgsl_double), intent(in) :: x, y, z, p
    type(fgsl_mode_t), intent(in) :: mode
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_ellint_rj_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_ellint_rj_e = gsl_sf_ellint_rj_e(x, y, z, p, mode%gsl_mode, res)
    result = res
  end function fgsl_sf_ellint_rj_e
end module fgsl_sf_ellint
