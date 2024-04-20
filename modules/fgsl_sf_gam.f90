!-*-f90-*-
module fgsl_sf_gam
  !>  Special functions - Gamma and Beta functions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_gamma_e, gsl_sf_lngamma_e, gsl_sf_lngamma_sgn_e, gsl_sf_gammastar_e, &
       gsl_sf_gammainv_e, gsl_sf_lngamma_complex_e, gsl_sf_fact_e, gsl_sf_doublefact_e, &
       gsl_sf_lnfact_e, gsl_sf_lndoublefact_e, gsl_sf_choose_e, gsl_sf_lnchoose_e, &
       gsl_sf_taylorcoeff_e, gsl_sf_poch_e, gsl_sf_lnpoch_sgn_e, gsl_sf_pochrel_e, &
       gsl_sf_gamma_inc_e, gsl_sf_gamma_inc_q_e, gsl_sf_gamma_inc_p_e, &
       gsl_sf_beta_e, gsl_sf_lnbeta_e, gsl_sf_beta_inc_e

  interface
     function fgsl_sf_gamma(x) bind(c, name='gsl_sf_gamma')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_gamma
     end function fgsl_sf_gamma
     function gsl_sf_gamma_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_gamma_e
     end function gsl_sf_gamma_e
     function fgsl_sf_lngamma(x) bind(c, name='gsl_sf_lngamma')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_lngamma
     end function fgsl_sf_lngamma
     function gsl_sf_lngamma_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_lngamma_e
     end function gsl_sf_lngamma_e
     function gsl_sf_lngamma_sgn_e(x, result_lg, sgn) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result_lg
       real(c_double) :: sgn
       integer(c_int) :: gsl_sf_lngamma_sgn_e
     end function gsl_sf_lngamma_sgn_e
     function fgsl_sf_gammastar(x) bind(c, name='gsl_sf_gammastar')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_gammastar
     end function fgsl_sf_gammastar
     function gsl_sf_gammastar_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_gammastar_e
     end function gsl_sf_gammastar_e
     function fgsl_sf_gammainv(x) bind(c, name='gsl_sf_gammainv')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_gammainv
     end function fgsl_sf_gammainv
     function gsl_sf_gammainv_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_gammainv_e
     end function gsl_sf_gammainv_e
     function gsl_sf_lngamma_complex_e(zr, zi, lnr, arg) bind(c)
       import
       real(c_double), value :: zr, zi
       type(gsl_sf_result) :: lnr, arg
       integer(c_int) :: gsl_sf_lngamma_complex_e
     end function gsl_sf_lngamma_complex_e
     function fgsl_sf_fact(n) bind(c, name='gsl_sf_fact')
       import
       integer(c_int), value :: n
       real(c_double) :: fgsl_sf_fact
     end function fgsl_sf_fact
     function gsl_sf_fact_e(n, result) bind(c)
       import
       integer(c_int), value :: n
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_fact_e
     end function gsl_sf_fact_e
     function fgsl_sf_doublefact(n) bind(c, name='gsl_sf_doublefact')
       import
       integer(c_int), value :: n
       real(c_double) :: fgsl_sf_doublefact
     end function fgsl_sf_doublefact
     function gsl_sf_doublefact_e(n, result) bind(c)
       import
       integer(c_int), value :: n
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_doublefact_e
     end function gsl_sf_doublefact_e
     function fgsl_sf_lnfact(n) bind(c, name='gsl_sf_lnfact')
       import
       integer(c_int), value :: n
       real(c_double) :: fgsl_sf_lnfact
     end function fgsl_sf_lnfact
     function gsl_sf_lnfact_e(n, result) bind(c)
       import
       integer(c_int), value :: n
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_lnfact_e
     end function gsl_sf_lnfact_e
     function fgsl_sf_lndoublefact(n) bind(c, name='gsl_sf_lndoublefact')
       import
       integer(c_int), value :: n
       real(c_double) :: fgsl_sf_lndoublefact
     end function fgsl_sf_lndoublefact
     function gsl_sf_lndoublefact_e(n, result) bind(c)
       import
       integer(c_int), value :: n
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_lndoublefact_e
     end function gsl_sf_lndoublefact_e
     function fgsl_sf_choose(n, m) bind(c, name='gsl_sf_choose')
       import
       integer(c_int), value :: n, m
       real(c_double) :: fgsl_sf_choose
     end function fgsl_sf_choose
     function gsl_sf_choose_e(n, m, result) bind(c)
       import
       integer(c_int), value :: n, m
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_choose_e
     end function gsl_sf_choose_e
     function fgsl_sf_lnchoose(n, m) bind(c, name='gsl_sf_lnchoose')
       import
       integer(c_int), value :: n, m
       real(c_double) :: fgsl_sf_lnchoose
     end function fgsl_sf_lnchoose
     function gsl_sf_lnchoose_e(n, m, result) bind(c)
       import
       integer(c_int), value :: n, m
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_lnchoose_e
     end function gsl_sf_lnchoose_e
     function fgsl_sf_taylorcoeff(n, x) bind(c, name='gsl_sf_taylorcoeff')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_taylorcoeff
     end function fgsl_sf_taylorcoeff
     function gsl_sf_taylorcoeff_e(n, x, result) bind(c)
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_taylorcoeff_e
     end function gsl_sf_taylorcoeff_e
     function fgsl_sf_poch(a, x) bind(c, name='gsl_sf_poch')
       import
       real(c_double), value :: a, x
       real(c_double) :: fgsl_sf_poch
     end function fgsl_sf_poch
     function gsl_sf_poch_e(a, x, result) bind(c)
       import
       real(c_double), value :: a, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_poch_e
     end function gsl_sf_poch_e
     function fgsl_sf_lnpoch(a, x) bind(c, name='gsl_sf_lnpoch')
       import
       real(c_double), value :: a, x
       real(c_double) :: fgsl_sf_lnpoch
     end function fgsl_sf_lnpoch
     function gsl_sf_lnpoch_e(a, x, result) bind(c)
       import
       real(c_double), value :: a, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_lnpoch_e
     end function gsl_sf_lnpoch_e
     function gsl_sf_lnpoch_sgn_e(a, x, result_lg, sgn) bind(c)
       import
       real(c_double), value :: a, x
       type(gsl_sf_result) :: result_lg
       real(c_double) :: sgn
       integer(c_int) :: gsl_sf_lnpoch_sgn_e
     end function gsl_sf_lnpoch_sgn_e
     function fgsl_sf_pochrel(a, x) bind(c, name='gsl_sf_pochrel')
       import
       real(c_double), value :: a, x
       real(c_double) :: fgsl_sf_pochrel
     end function fgsl_sf_pochrel
     function gsl_sf_pochrel_e(a, x, result) bind(c)
       import
       real(c_double), value :: a, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_pochrel_e
     end function gsl_sf_pochrel_e
     function fgsl_sf_gamma_inc(a, x) bind(c, name='gsl_sf_gamma_inc')
       import
       real(c_double), value :: a, x
       real(c_double) :: fgsl_sf_gamma_inc
     end function fgsl_sf_gamma_inc
     function gsl_sf_gamma_inc_e(a, x, result) bind(c)
       import
       real(c_double), value :: a, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_gamma_inc_e
     end function gsl_sf_gamma_inc_e
     function fgsl_sf_gamma_inc_q(a, x) bind(c, name='gsl_sf_gamma_inc_Q')
       import
       real(c_double), value :: a, x
       real(c_double) :: fgsl_sf_gamma_inc_q
     end function fgsl_sf_gamma_inc_q
     function gsl_sf_gamma_inc_q_e(a, x, result) bind(c, name='gsl_sf_gamma_inc_Q_e')
       import
       real(c_double), value :: a, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_gamma_inc_q_e
     end function gsl_sf_gamma_inc_q_e
     function fgsl_sf_gamma_inc_p(a, x) bind(c, name='gsl_sf_gamma_inc_P')
       import
       real(c_double), value :: a, x
       real(c_double) :: fgsl_sf_gamma_inc_p
     end function fgsl_sf_gamma_inc_p
     function gsl_sf_gamma_inc_p_e(a, x, result) bind(c, name='gsl_sf_gamma_inc_P_e')
       import
       real(c_double), value :: a, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_gamma_inc_p_e
     end function gsl_sf_gamma_inc_p_e
     function fgsl_sf_beta(a, b) bind(c, name='gsl_sf_beta')
       import
       real(c_double), value :: a, b
       real(c_double) :: fgsl_sf_beta
     end function fgsl_sf_beta
     function gsl_sf_beta_e(a, b, result) bind(c)
       import
       real(c_double), value :: a, b
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_beta_e
     end function gsl_sf_beta_e
     function fgsl_sf_lnbeta(a, b) bind(c, name='gsl_sf_lnbeta')
       import
       real(c_double), value :: a, b
       real(c_double) :: fgsl_sf_lnbeta
     end function fgsl_sf_lnbeta
     function gsl_sf_lnbeta_e(a, b, result) bind(c)
       import
       real(c_double), value :: a, b
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_lnbeta_e
     end function gsl_sf_lnbeta_e
     function fgsl_sf_beta_inc(a, b, x) bind(c, name='gsl_sf_beta_inc')
       import
       real(c_double), value :: a, b, x
       real(c_double) :: fgsl_sf_beta_inc
     end function fgsl_sf_beta_inc
     function gsl_sf_beta_inc_e(a, b, x, result) bind(c)
       import
       real(c_double), value :: a, b, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_beta_inc_e
     end function gsl_sf_beta_inc_e
  end interface

contains
!  fgsl_sf_gamma --> interface
  function fgsl_sf_gamma_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_gamma_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_gamma_e = gsl_sf_gamma_e(x, res)
    result = res
  end function fgsl_sf_gamma_e
!  fgsl_sf_lngamma --> interface
  function fgsl_sf_lngamma_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_lngamma_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_lngamma_e = gsl_sf_lngamma_e(x, res)
    result = res
  end function fgsl_sf_lngamma_e
  function fgsl_sf_lngamma_sgn_e(x, result_lg, sgn)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result_lg
    real(fgsl_double), intent(out) :: sgn
    integer(fgsl_int) :: fgsl_sf_lngamma_sgn_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_lngamma_sgn_e = gsl_sf_lngamma_sgn_e(x, res, sgn)
    result_lg = res
  end function fgsl_sf_lngamma_sgn_e
!  fgsl_sf_gammastar --> interface
  function fgsl_sf_gammastar_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_gammastar_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_gammastar_e = gsl_sf_gammastar_e(x, res)
    result = res
  end function fgsl_sf_gammastar_e
!  fgsl_sf_gammainv --> interface
  function fgsl_sf_gammainv_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_gammainv_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_gammainv_e = gsl_sf_gammainv_e(x, res)
    result = res
  end function fgsl_sf_gammainv_e
  function fgsl_sf_lngamma_complex_e(zr, zi, lnr, arg)
    real(fgsl_double), intent(in) :: zr, zi
    type(fgsl_sf_result), intent(out) :: lnr, arg
    integer(fgsl_int) :: fgsl_sf_lngamma_complex_e
!
    type(gsl_sf_result) :: lnr_loc, arg_loc
    fgsl_sf_lngamma_complex_e = gsl_sf_lngamma_complex_e(zr, zi, lnr_loc, arg_loc)
    lnr = lnr_loc
    arg = arg_loc
  end function fgsl_sf_lngamma_complex_e
!  fgsl_sf_fact --> interface
  function fgsl_sf_fact_e(n, result)
    integer(c_int), intent(in) :: n
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_fact_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_fact_e = gsl_sf_fact_e(n, res)
    result = res
  end function fgsl_sf_fact_e
!  fgsl_sf_doublefact --> interface
  function fgsl_sf_doublefact_e(n, result)
    integer(c_int), intent(in) :: n
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_doublefact_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_doublefact_e = gsl_sf_doublefact_e(n, res)
    result = res
  end function fgsl_sf_doublefact_e
!  fgsl_sf_lnfact --> interface
  function fgsl_sf_lnfact_e(n, result)
    integer(c_int), intent(in) :: n
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_lnfact_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_lnfact_e = gsl_sf_lnfact_e(n, res)
    result = res
  end function fgsl_sf_lnfact_e
!  fgsl_sf_lndoublefact --> interface
  function fgsl_sf_lndoublefact_e(n, result)
    integer(c_int), intent(in) :: n
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_lndoublefact_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_lndoublefact_e = gsl_sf_lndoublefact_e(n, res)
    result = res
  end function fgsl_sf_lndoublefact_e
!  fgsl_sf_choose --> interface
  function fgsl_sf_choose_e(n, m, result)
    integer(c_int), intent(in) :: n, m
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_choose_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_choose_e = gsl_sf_choose_e(n, m, res)
    result = res
  end function fgsl_sf_choose_e
!  fgsl_sf_lnchoose --> interface
  function fgsl_sf_lnchoose_e(n, m, result)
    integer(c_int), intent(in) :: n, m
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_lnchoose_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_lnchoose_e = gsl_sf_lnchoose_e(n, m, res)
    result = res
  end function fgsl_sf_lnchoose_e
!  fgsl_sf_taylorcoeff --> interface
  function fgsl_sf_taylorcoeff_e(n, x, result)
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_taylorcoeff_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_taylorcoeff_e = gsl_sf_taylorcoeff_e(n, x, res)
    result = res
  end function fgsl_sf_taylorcoeff_e
!  fgsl_sf_poch --> interface
  function fgsl_sf_poch_e(a, x, result)
    real(fgsl_double), intent(in) :: a, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_poch_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_poch_e = gsl_sf_poch_e(a, x, res)
    result = res
  end function fgsl_sf_poch_e
!  fgsl_sf_lnpoch --> interface
  function fgsl_sf_lnpoch_e(a, x, result)
    real(fgsl_double), intent(in) :: a, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_lnpoch_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_lnpoch_e = gsl_sf_lnpoch_e(a, x, res)
    result = res
  end function fgsl_sf_lnpoch_e
  function fgsl_sf_lnpoch_sgn_e(a, x, result_lg, sgn)
    real(fgsl_double), intent(in) :: a, x
    type(fgsl_sf_result), intent(out) :: result_lg
    real(fgsl_double), intent(out) :: sgn
    integer(fgsl_int) :: fgsl_sf_lnpoch_sgn_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_lnpoch_sgn_e = gsl_sf_lnpoch_sgn_e(a, x, res, sgn)
    result_lg = res
  end function fgsl_sf_lnpoch_sgn_e
!  fgsl_sf_pochrel --> interface
  function fgsl_sf_pochrel_e(a, x, result)
    real(fgsl_double), intent(in) :: a, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_pochrel_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_pochrel_e = gsl_sf_pochrel_e(a, x, res)
    result = res
  end function fgsl_sf_pochrel_e
!  fgsl_sf_gamma_inc --> interface
  function fgsl_sf_gamma_inc_e(a, x, result)
    real(fgsl_double), intent(in) :: a, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_gamma_inc_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_gamma_inc_e = gsl_sf_gamma_inc_e(a, x, res)
    result = res
  end function fgsl_sf_gamma_inc_e
!  fgsl_sf_gamma_inc_q --> interface
  function fgsl_sf_gamma_inc_q_e(a, x, result)
    real(fgsl_double), intent(in) :: a, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_gamma_inc_q_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_gamma_inc_q_e = gsl_sf_gamma_inc_q_e(a, x, res)
    result = res
  end function fgsl_sf_gamma_inc_q_e
!  fgsl_sf_gamma_inc_p --> interface
  function fgsl_sf_gamma_inc_p_e(a, x, result)
    real(fgsl_double), intent(in) :: a, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_gamma_inc_p_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_gamma_inc_p_e = gsl_sf_gamma_inc_p_e(a, x, res)
    result = res
  end function fgsl_sf_gamma_inc_p_e
!  fgsl_sf_beta --> interface
  function fgsl_sf_beta_e(a, b, result)
    real(fgsl_double), intent(in) :: a, b
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_beta_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_beta_e = gsl_sf_beta_e(a, b, res)
    result = res
  end function fgsl_sf_beta_e
!  fgsl_sf_lnbeta --> interface
  function fgsl_sf_lnbeta_e(a, b, result)
    real(fgsl_double), intent(in) :: a, b
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_lnbeta_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_lnbeta_e = gsl_sf_lnbeta_e(a, b, res)
    result = res
  end function fgsl_sf_lnbeta_e
!  fgsl_sf_beta_inc --> interface
  function fgsl_sf_beta_inc_e(a, b, x, result)
    real(fgsl_double), intent(in) :: a, b, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_beta_inc_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_beta_inc_e = gsl_sf_beta_inc_e(a, b, x, res)
    result = res
  end function fgsl_sf_beta_inc_e
end module fgsl_sf_gam
