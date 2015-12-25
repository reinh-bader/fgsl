module fgsl
#include "config.h"
!-------------------------------------------------------------------------------
!>  \mainpage
!>  \brief Interface module for use of GSL from Fortran
!>  \author R. Bader
!>  \author Leibniz Supercomputing Centre, Garching, Germany
!>  \details
!>  Please see the <a href="pages.html">Related Pages</a> section
!>  for the information about the conventions used in the interface.
!>  Examples on how to use the interface are available in the
!>  <p><b>doc/examples</b><p> subdirectory of the source package.
!>  \page "Introduction"
!>  <OL>
!>  <LI> Introductory notes:
!>      <UL>
!>      <LI> In Fortran code, GSL_* must be replaced by FGSL_* for each API
!>           call, abstract data type, module variables and parameters (with
!>           exception of the M_* mathematical constants)
!>      <LI> Some names were changed due to UC/LC aliasing. See the documentation
!>           chapter on special functions for details.
!>      <LI> Intrinsic type matching:
!>        -# real(fgsl_double) is used for double precision values
!>        -# real(fgsl_float) is used for single precision values
!>        -# integer(fgsl_int) for integer
!>        -# integer(fgsl_long) for long integer
!>        -# integer(fgsl_size_t) for size_t integer
!>        -# complex(fgsl_double_complex) for gsl_complex
!>        -# character(fgsl_char) for characters
!>        -# no value attributes and mostly no pointers in Fortran calls
!>        -# unsigned int must be converted to integer(fgsl_long).
!>        -# char * results are converted to fixed length strings. Use TRIM.
!>       </UL>
!>  <LI> Additional routines:
!>      <UL>
!>      <LI> Generic interface fgsl_well_defined for checking status of FGSL
!>           objects (which are typically opaque).
!>      <LI> See api/array.finc for array alignment routines.
!>      <LI> See api/math.finc for function object constructors.
!>      <LI> See api/io.finc for I/O related add-ons.
!>      </UL>
!>  <LI> Structure of the documentation:
!>      <UL>
!>      <LI> type definitions are in the fgsl section of the Modules menu item
!>      <LI> all API routines are available via the Files menu item
!>      <LI> additional remarks on the various files are available via the Related
!>           Pages menu item
!>      </UL>
!>  <LI> Only interfaces from the GSL manual are implemented. The
!>       C include files may contain more stuff which may only be meant
!>       for internal use, or is not officially documented.
!>  <LI> Inlining of GSL routines is not possible.
!>  <LI> Macros are not supported:
!>      <UL>
!>      <LI> macro values are replicated as parameters
!>      <LI> Inf/Nan need to use IEEE_VALUE (if available)
!>      </UL>
!>  </OL>
!>
!-------------------------------------------------------------------------------
  use, intrinsic :: iso_c_binding
  implicit none
  private
!
! Exported entities
!
  public :: fgsl_set_error_handler_off, fgsl_set_error_handler, fgsl_strerror, &
       fgsl_error_handler_init, fgsl_name, fgsl_error
  public :: fgsl_well_defined, fgsl_obj_c_ptr, fgsl_sizeof
  public :: fgsl_open, fgsl_close, fgsl_stdin, fgsl_stdout, fgsl_stderr, fgsl_flush
  public :: assignment(=)
  public :: fgsl_isnan, fgsl_isinf, fgsl_finite, fgsl_log1p, fgsl_expm1, &
       fgsl_hypot, fgsl_hypot3, fgsl_acosh, fgsl_asinh, fgsl_atanh, fgsl_ldexp, fgsl_frexp, &
       fgsl_fcmp, fgsl_function_init, fgsl_function_fdf_init, fgsl_function_free, &
       fgsl_function_fdf_free, fgsl_fn_eval, fgsl_fn_fdf_eval_f, fgsl_fn_fdf_eval_df, &
       fgsl_fn_fdf_eval_f_df
! complex functions
  public :: fgsl_complex_arg, fgsl_complex_logabs, fgsl_complex_log10, &
       fgsl_complex_log_b, fgsl_complex_arcsin, fgsl_complex_arcsin_real, &
       fgsl_complex_arccos, fgsl_complex_arccos_real,  fgsl_complex_arctan, &
       fgsl_complex_arcsec, fgsl_complex_arcsec_real, fgsl_complex_arccsc, &
       fgsl_complex_arccsc_real, fgsl_complex_arccot, fgsl_complex_arcsinh, &
       fgsl_complex_arccosh, fgsl_complex_arccosh_real, fgsl_complex_arctanh, &
       fgsl_complex_arctanh_real, fgsl_complex_arcsech, fgsl_complex_arccsch, &
       fgsl_complex_arccoth
! polynomials
  public :: fgsl_poly_eval, fgsl_poly_complex_eval, fgsl_complex_poly_complex_eval, &
       fgsl_poly_eval_derivs, fgsl_poly_dd_init, fgsl_poly_dd_eval, fgsl_poly_dd_taylor, &
       fgsl_poly_solve_quadratic, fgsl_poly_complex_solve_quadratic, &
       fgsl_poly_solve_cubic, fgsl_poly_complex_solve_cubic, &
       fgsl_poly_complex_workspace_alloc, fgsl_poly_complex_workspace_free, &
       fgsl_poly_complex_solve
  public :: fgsl_poly_dd_hermite_init
! special functions
  public :: fgsl_sf_airy_ai, fgsl_sf_airy_ai_e, fgsl_sf_airy_bi, fgsl_sf_airy_bi_e, &
       fgsl_sf_airy_ai_scaled, fgsl_sf_airy_ai_scaled_e, fgsl_sf_airy_bi_scaled, &
       fgsl_sf_airy_bi_scaled_e, fgsl_sf_airy_ai_deriv, fgsl_sf_airy_bi_deriv, &
       fgsl_sf_airy_ai_deriv_scaled, fgsl_sf_airy_bi_deriv_scaled, &
       fgsl_sf_airy_ai_deriv_e, fgsl_sf_airy_bi_deriv_e, &
       fgsl_sf_airy_ai_deriv_scaled_e, fgsl_sf_airy_bi_deriv_scaled_e, &
       fgsl_sf_airy_zero_ai, fgsl_sf_airy_zero_ai_e, &
       fgsl_sf_airy_zero_bi, fgsl_sf_airy_zero_bi_e, &
       fgsl_sf_airy_zero_ai_deriv, fgsl_sf_airy_zero_ai_deriv_e, &
       fgsl_sf_airy_zero_bi_deriv, fgsl_sf_airy_zero_bi_deriv_e
  public :: fgsl_sf_bessel_jc0, fgsl_sf_bessel_jc0_e, &
       fgsl_sf_bessel_jc1, fgsl_sf_bessel_jc1_e, fgsl_sf_bessel_jcn, &
       fgsl_sf_bessel_jcn_e, fgsl_sf_bessel_jcn_array, &
       fgsl_sf_bessel_yc0, fgsl_sf_bessel_yc0_e, &
       fgsl_sf_bessel_yc1, fgsl_sf_bessel_yc1_e, fgsl_sf_bessel_ycn, &
       fgsl_sf_bessel_ycn_e, fgsl_sf_bessel_ycn_array, &
       fgsl_sf_bessel_ic0, fgsl_sf_bessel_ic0_e, &
       fgsl_sf_bessel_ic1, fgsl_sf_bessel_ic1_e, fgsl_sf_bessel_icn, &
       fgsl_sf_bessel_icn_e, fgsl_sf_bessel_icn_array, &
       fgsl_sf_bessel_ic0_scaled, fgsl_sf_bessel_ic0_scaled_e, &
       fgsl_sf_bessel_ic1_scaled, fgsl_sf_bessel_ic1_scaled_e, fgsl_sf_bessel_icn_scaled, &
       fgsl_sf_bessel_icn_scaled_e, fgsl_sf_bessel_icn_scaled_array, &
       fgsl_sf_bessel_kc0, fgsl_sf_bessel_kc0_e, &
       fgsl_sf_bessel_kc1, fgsl_sf_bessel_kc1_e, fgsl_sf_bessel_kcn, &
       fgsl_sf_bessel_kcn_e, fgsl_sf_bessel_kcn_array, &
       fgsl_sf_bessel_kc0_scaled, fgsl_sf_bessel_kc0_scaled_e, &
       fgsl_sf_bessel_kc1_scaled, fgsl_sf_bessel_kc1_scaled_e, fgsl_sf_bessel_kcn_scaled, &
       fgsl_sf_bessel_kcn_scaled_e, fgsl_sf_bessel_kcn_scaled_array
  public :: fgsl_sf_bessel_js0, fgsl_sf_bessel_js0_e, fgsl_sf_bessel_js1, fgsl_sf_bessel_js1_e, &
       fgsl_sf_bessel_js2, fgsl_sf_bessel_js2_e, fgsl_sf_bessel_jsl, fgsl_sf_bessel_jsl_e, &
       fgsl_sf_bessel_jsl_array, fgsl_sf_bessel_jsl_steed_array, &
       fgsl_sf_bessel_ys0, fgsl_sf_bessel_ys0_e, fgsl_sf_bessel_ys1, fgsl_sf_bessel_ys1_e, &
       fgsl_sf_bessel_ys2, fgsl_sf_bessel_ys2_e, fgsl_sf_bessel_ysl, fgsl_sf_bessel_ysl_e, &
       fgsl_sf_bessel_ysl_array, fgsl_sf_bessel_is0_scaled, fgsl_sf_bessel_is0_scaled_e, &
       fgsl_sf_bessel_is1_scaled, fgsl_sf_bessel_is1_scaled_e, &
       fgsl_sf_bessel_is2_scaled, fgsl_sf_bessel_is2_scaled_e, fgsl_sf_bessel_isl_scaled, &
       fgsl_sf_bessel_isl_scaled_e, fgsl_sf_bessel_isl_scaled_array, &
       fgsl_sf_bessel_ks0_scaled, fgsl_sf_bessel_ks0_scaled_e, &
       fgsl_sf_bessel_ks1_scaled, fgsl_sf_bessel_ks1_scaled_e, &
       fgsl_sf_bessel_ks2_scaled, fgsl_sf_bessel_ks2_scaled_e, fgsl_sf_bessel_ksl_scaled, &
       fgsl_sf_bessel_ksl_scaled_e, fgsl_sf_bessel_ksl_scaled_array
  public :: fgsl_sf_bessel_jnu, fgsl_sf_bessel_jnu_e, fgsl_sf_bessel_sequence_jnu_e, &
       fgsl_sf_bessel_ynu, fgsl_sf_bessel_ynu_e, fgsl_sf_bessel_inu, fgsl_sf_bessel_inu_e, &
       fgsl_sf_bessel_inu_scaled, fgsl_sf_bessel_inu_scaled_e, &
       fgsl_sf_bessel_knu, fgsl_sf_bessel_knu_e, &
       fgsl_sf_bessel_lnknu, fgsl_sf_bessel_lnknu_e, &
       fgsl_sf_bessel_knu_scaled, fgsl_sf_bessel_knu_scaled_e, &
       fgsl_sf_bessel_zero_jc0, fgsl_sf_bessel_zero_jc0_e, &
       fgsl_sf_bessel_zero_jc1, fgsl_sf_bessel_zero_jc1_e, &
       fgsl_sf_bessel_zero_jnu, fgsl_sf_bessel_zero_jnu_e
  public :: fgsl_sf_clausen, fgsl_sf_clausen_e, fgsl_sf_hydrogenicr_1, &
       fgsl_sf_hydrogenicr_1_e, fgsl_sf_hydrogenicr, fgsl_sf_hydrogenicr_e, &
       fgsl_sf_coulomb_wave_fg_e, fgsl_sf_coulomb_wave_f_array, fgsl_sf_coulomb_wave_fg_array, &
       fgsl_sf_coulomb_wave_fgp_array, fgsl_sf_coulomb_wave_sphf_array, &
       fgsl_sf_coulomb_cl_e, fgsl_sf_coulomb_cl_array
  public :: fgsl_sf_coupling_3j, fgsl_sf_coupling_3j_e, fgsl_sf_coupling_6j, &
       fgsl_sf_coupling_6j_e, fgsl_sf_coupling_9j, fgsl_sf_coupling_9j_e, &
       fgsl_sf_dawson, fgsl_sf_dawson_e, fgsl_sf_debye_1, fgsl_sf_debye_1_e, &
       fgsl_sf_debye_2, fgsl_sf_debye_2_e, fgsl_sf_debye_3, fgsl_sf_debye_3_e, &
       fgsl_sf_debye_4, fgsl_sf_debye_4_e, fgsl_sf_debye_5, fgsl_sf_debye_5_e, &
       fgsl_sf_debye_6, fgsl_sf_debye_6_e, fgsl_sf_dilog, fgsl_sf_dilog_e, &
       fgsl_sf_complex_dilog_e, fgsl_sf_multiply_e, fgsl_sf_multiply_err_e
  public :: fgsl_sf_ellint_kcomp, fgsl_sf_ellint_kcomp_e, fgsl_sf_ellint_ecomp, &
       fgsl_sf_ellint_ecomp_e, fgsl_sf_ellint_f, fgsl_sf_ellint_pcomp, &
       fgsl_sf_ellint_pcomp_e, fgsl_sf_ellint_f_e, &
       fgsl_sf_ellint_e, fgsl_sf_ellint_e_e, fgsl_sf_ellint_p, fgsl_sf_ellint_p_e, &
       fgsl_sf_ellint_d, fgsl_sf_ellint_d_e, fgsl_sf_ellint_rc, fgsl_sf_ellint_rc_e, &
       fgsl_sf_ellint_rd, fgsl_sf_ellint_rd_e, fgsl_sf_ellint_rf, fgsl_sf_ellint_rf_e, &
       fgsl_sf_ellint_rj, fgsl_sf_ellint_rj_e, fgsl_sf_elljac_e
  public :: fgsl_sf_erf, fgsl_sf_erf_e, fgsl_sf_erfc, fgsl_sf_erfc_e, fgsl_sf_log_erfc, &
       fgsl_sf_log_erfc_e, fgsl_sf_erf_z, fgsl_sf_erf_z_e, fgsl_sf_erf_q, fgsl_sf_erf_q_e, &
       fgsl_sf_hazard, fgsl_sf_hazard_e, fgsl_sf_exp, fgsl_sf_exp_e, fgsl_sf_exp_e10_e, &
       fgsl_sf_exp_mult, fgsl_sf_exp_mult_e, fgsl_sf_exp_mult_e10_e, fgsl_sf_expm1, &
       fgsl_sf_expm1_e, fgsl_sf_exprel, fgsl_sf_exprel_e, fgsl_sf_exprel_2, fgsl_sf_exprel_2_e, &
       fgsl_sf_exprel_n, fgsl_sf_exprel_n_e, fgsl_sf_exp_err_e, fgsl_sf_exp_err_e10_e, &
       fgsl_sf_exp_mult_err_e, fgsl_sf_exp_mult_err_e10_e
  public :: fgsl_sf_expint_e1, fgsl_sf_expint_e1_e, fgsl_sf_expint_e2, fgsl_sf_expint_e2_e, &
       fgsl_sf_expint_en, fgsl_sf_expint_en_e, &
       fgsl_sf_expint_ei, fgsl_sf_expint_ei_e, fgsl_sf_shi, fgsl_sf_shi_e, fgsl_sf_chi, &
       fgsl_sf_chi_e, fgsl_sf_expint_3, fgsl_sf_expint_3_e, fgsl_sf_si, fgsl_sf_si_e, &
       fgsl_sf_ci, fgsl_sf_ci_e, fgsl_sf_atanint, fgsl_sf_atanint_e
  public :: fgsl_sf_fermi_dirac_m1, fgsl_sf_fermi_dirac_m1_e, fgsl_sf_fermi_dirac_0, &
       fgsl_sf_fermi_dirac_0_e, fgsl_sf_fermi_dirac_1, fgsl_sf_fermi_dirac_1_e, &
       fgsl_sf_fermi_dirac_2, fgsl_sf_fermi_dirac_2_e, fgsl_sf_fermi_dirac_int, &
       fgsl_sf_fermi_dirac_int_e, fgsl_sf_fermi_dirac_mhalf, fgsl_sf_fermi_dirac_mhalf_e, &
       fgsl_sf_fermi_dirac_half, fgsl_sf_fermi_dirac_half_e, fgsl_sf_fermi_dirac_3half, &
       fgsl_sf_fermi_dirac_3half_e, fgsl_sf_fermi_dirac_inc_0, &
       fgsl_sf_fermi_dirac_inc_0_e
  public :: fgsl_sf_gamma, fgsl_sf_gamma_e, fgsl_sf_lngamma, fgsl_sf_lngamma_e, &
       fgsl_sf_lngamma_sgn_e, fgsl_sf_gammastar, fgsl_sf_gammastar_e, fgsl_sf_gammainv, &
       fgsl_sf_gammainv_e, fgsl_sf_lngamma_complex_e, fgsl_sf_fact, fgsl_sf_fact_e, &
       fgsl_sf_doublefact, fgsl_sf_doublefact_e, fgsl_sf_lnfact, fgsl_sf_lnfact_e, &
       fgsl_sf_lndoublefact, fgsl_sf_lndoublefact_e, fgsl_sf_choose, fgsl_sf_choose_e, &
       fgsl_sf_lnchoose, fgsl_sf_lnchoose_e, fgsl_sf_taylorcoeff, fgsl_sf_taylorcoeff_e, &
       fgsl_sf_poch, fgsl_sf_poch_e, fgsl_sf_lnpoch, fgsl_sf_lnpoch_e, &
       fgsl_sf_lnpoch_sgn_e, fgsl_sf_pochrel, fgsl_sf_pochrel_e, &
       fgsl_sf_gamma_inc, fgsl_sf_gamma_inc_e, &
       fgsl_sf_gamma_inc_q, fgsl_sf_gamma_inc_q_e, fgsl_sf_gamma_inc_p, &
       fgsl_sf_gamma_inc_p_e, fgsl_sf_beta, fgsl_sf_beta_e, fgsl_sf_lnbeta, &
       fgsl_sf_lnbeta_e, fgsl_sf_beta_inc, fgsl_sf_beta_inc_e
  public :: fgsl_sf_gegenpoly_1, fgsl_sf_gegenpoly_1_e, fgsl_sf_gegenpoly_2, &
       fgsl_sf_gegenpoly_2_e, fgsl_sf_gegenpoly_3, fgsl_sf_gegenpoly_3_e, &
       fgsl_sf_gegenpoly_n, fgsl_sf_gegenpoly_n_e, fgsl_sf_gegenpoly_array, &
       fgsl_sf_hyperg_0f1, fgsl_sf_hyperg_0f1_e, fgsl_sf_hyperg_1f1_int, &
       fgsl_sf_hyperg_1f1_int_e, fgsl_sf_hyperg_1f1, fgsl_sf_hyperg_1f1_e, &
       fgsl_sf_hyperg_u_int, fgsl_sf_hyperg_u_int_e, fgsl_sf_hyperg_u_int_e10_e, &
       fgsl_sf_hyperg_u, fgsl_sf_hyperg_u_e, fgsl_sf_hyperg_u_e10_e, &
       fgsl_sf_hyperg_2f1, fgsl_sf_hyperg_2f1_e, fgsl_sf_hyperg_2f1_conj, &
       fgsl_sf_hyperg_2f1_conj_e, fgsl_sf_hyperg_2f1_renorm, fgsl_sf_hyperg_2f1_renorm_e, &
       fgsl_sf_hyperg_2f1_conj_renorm, fgsl_sf_hyperg_2f1_conj_renorm_e, &
       fgsl_sf_hyperg_2f0, fgsl_sf_hyperg_2f0_e
  public :: fgsl_sf_laguerre_1, fgsl_sf_laguerre_1_e, fgsl_sf_laguerre_2, &
       fgsl_sf_laguerre_2_e, fgsl_sf_laguerre_3, fgsl_sf_laguerre_3_e, &
       fgsl_sf_laguerre_n, fgsl_sf_laguerre_n_e
  public :: fgsl_sf_lambert_w0, fgsl_sf_lambert_w0_e, fgsl_sf_lambert_wm1, &
       fgsl_sf_lambert_wm1_e, fgsl_sf_legendre_p1, fgsl_sf_legendre_p1_e, &
       fgsl_sf_legendre_p2, fgsl_sf_legendre_p2_e, fgsl_sf_legendre_p3, &
       fgsl_sf_legendre_p3_e, fgsl_sf_legendre_pl, fgsl_sf_legendre_pl_e, &
       fgsl_sf_legendre_pl_array, fgsl_sf_legendre_pl_deriv_array, &
       fgsl_sf_legendre_q0, fgsl_sf_legendre_q0_e, &
       fgsl_sf_legendre_q1, fgsl_sf_legendre_q1_e, fgsl_sf_legendre_ql, &
       fgsl_sf_legendre_ql_e, fgsl_sf_legendre_plm, fgsl_sf_legendre_plm_e, &
       fgsl_sf_legendre_sphplm, fgsl_sf_legendre_sphplm_e
  public :: fgsl_sf_conicalp_half, fgsl_sf_conicalp_half_e, fgsl_sf_conicalp_mhalf, &
       fgsl_sf_conicalp_mhalf_e, fgsl_sf_conicalp_0, fgsl_sf_conicalp_0_e, &
       fgsl_sf_conicalp_1, fgsl_sf_conicalp_1_e, fgsl_sf_conicalp_sph_reg, &
       fgsl_sf_conicalp_sph_reg_e, fgsl_sf_conicalp_cyl_reg, fgsl_sf_conicalp_cyl_reg_e, &
       fgsl_sf_legendre_h3d_0, fgsl_sf_legendre_h3d_0_e, fgsl_sf_legendre_h3d_1, &
       fgsl_sf_legendre_h3d_1_e, fgsl_sf_legendre_h3d, fgsl_sf_legendre_h3d_e, &
       fgsl_sf_legendre_h3d_array
  public :: fgsl_sf_log, fgsl_sf_log_e, fgsl_sf_log_abs, fgsl_sf_log_abs_e, &
       fgsl_sf_complex_log_e, fgsl_sf_log_1plusx, fgsl_sf_log_1plusx_e, &
       fgsl_sf_log_1plusx_mx, fgsl_sf_log_1plusx_mx_e, fgsl_sf_psi_int, &
       fgsl_sf_psi_1_int, fgsl_sf_psi_1_int_e, fgsl_sf_psi_1, fgsl_sf_psi_1_e,&
       fgsl_sf_psi_n, fgsl_sf_psi_n_e,&
       fgsl_sf_psi_int_e, fgsl_sf_psi, fgsl_sf_psi_e, fgsl_sf_psi_1piy, &
       fgsl_sf_psi_1piy_e, fgsl_sf_synchrotron_1, fgsl_sf_synchrotron_1_e, &
       fgsl_sf_synchrotron_2, fgsl_sf_synchrotron_2_e, fgsl_sf_transport_2, &
       fgsl_sf_transport_2_e, fgsl_sf_transport_3, fgsl_sf_transport_3_e, &
       fgsl_sf_transport_4, fgsl_sf_transport_4_e, fgsl_sf_transport_5, &
       fgsl_sf_transport_5_e, fgsl_sf_hypot, fgsl_sf_hypot_e, fgsl_sf_sinc, &
       fgsl_sf_sinc_e, fgsl_sf_complex_sin_e, fgsl_sf_complex_cos_e, &
       fgsl_sf_complex_logsin_e, fgsl_sf_lnsinh, fgsl_sf_lnsinh_e, &
       fgsl_sf_lncosh, fgsl_sf_lncosh_e, fgsl_sf_polar_to_rect, &
       fgsl_sf_rect_to_polar, fgsl_sf_angle_restrict_symm, fgsl_sf_angle_restrict_symm_e, &
       fgsl_sf_angle_restrict_pos, fgsl_sf_angle_restrict_pos_e, fgsl_sf_sin_err_e, &
       fgsl_sf_cos_err_e, fgsl_sf_zeta_int, fgsl_sf_zeta_int_e, &
       fgsl_sf_eta_int, fgsl_sf_eta_int_e, fgsl_sf_zeta, &
       fgsl_sf_zeta_e, fgsl_sf_zetam1_int, fgsl_sf_zetam1_int_e, fgsl_sf_zetam1, &
       fgsl_sf_zetam1_e, fgsl_sf_hzeta, fgsl_sf_hzeta_e, fgsl_sf_eta, fgsl_sf_eta_e
! array processing
  public :: fgsl_vector_init, fgsl_vector_align, fgsl_vector_free,&
  fgsl_vector_get_size, fgsl_vector_get_stride
  public :: fgsl_matrix_init, fgsl_matrix_align, fgsl_matrix_free,&
  fgsl_matrix_get_size1, fgsl_matrix_get_size2, fgsl_matrix_get_tda
! interpolation
  public :: fgsl_interp_alloc, fgsl_interp_init, &
       fgsl_interp_free, fgsl_interp_eval, fgsl_interp_eval_e, &
       fgsl_interp_eval_deriv, fgsl_interp_eval_deriv_e, &
       fgsl_interp_eval_deriv2, fgsl_interp_eval_deriv2_e, &
       fgsl_interp_eval_integ, fgsl_interp_eval_integ_e, &
       fgsl_interp_min_size, &
       fgsl_interp_type_min_size,&
       fgsl_interp_name
  public :: fgsl_spline_alloc, fgsl_spline_init, fgsl_spline_free, &
       fgsl_spline_name, fgsl_spline_min_size, fgsl_spline_eval, &
       fgsl_spline_eval_e, fgsl_spline_eval_deriv, fgsl_spline_eval_deriv_e, &
       fgsl_spline_eval_deriv2, fgsl_spline_eval_deriv2_e, &
       fgsl_spline_eval_integ, fgsl_spline_eval_integ_e
  public :: fgsl_interp_accel_alloc, fgsl_interp_accel_free, &
       fgsl_interp_accel_find, fgsl_interp_bsearch
! permutations, combinations and multisets
  public :: fgsl_permutation_alloc, fgsl_permutation_calloc, fgsl_permutation_init, &
       fgsl_permutation_free, fgsl_permutation_memcpy, fgsl_permutation_get, &
       fgsl_permutation_swap, fgsl_permutation_size, fgsl_permutation_data, &
       fgsl_permutation_valid, fgsl_permutation_reverse, fgsl_permutation_inverse, &
       fgsl_permutation_next, fgsl_permutation_prev, &
       fgsl_permute, fgsl_permute_inverse, fgsl_permute_vector, &
       fgsl_permute_vector_inverse, fgsl_permutation_mul, &
       fgsl_permutation_linear_to_canonical, fgsl_permutation_canonical_to_linear, &
       fgsl_permutation_inversions, fgsl_permutation_linear_cycles, &
       fgsl_permutation_canonical_cycles, fgsl_permutation_fwrite, &
       fgsl_permutation_fread, fgsl_permutation_fprintf, &
       fgsl_permutation_fscanf, &
       fgsl_multiset_alloc, &
       fgsl_multiset_calloc, fgsl_multiset_init_first, fgsl_multiset_init_last, &
       fgsl_multiset_free, fgsl_multiset_memcpy, fgsl_multiset_get, &
       fgsl_multiset_n, fgsl_multiset_k, fgsl_multiset_data, &
       fgsl_multiset_valid, fgsl_multiset_next, &
       fgsl_multiset_prev, fgsl_multiset_fwrite, fgsl_multiset_fread, &
       fgsl_multiset_fprintf, fgsl_multiset_fscanf, &
       fgsl_combination_alloc, &
       fgsl_combination_calloc, fgsl_combination_init_first, fgsl_combination_init_last, &
       fgsl_combination_free, fgsl_combination_memcpy, fgsl_combination_get, &
       fgsl_combination_n, fgsl_combination_k, fgsl_combination_data, &
       fgsl_combination_valid, fgsl_combination_next, &
       fgsl_combination_prev, fgsl_combination_fwrite, fgsl_combination_fread, &
       fgsl_combination_fprintf, fgsl_combination_fscanf
! sorting
  public :: fgsl_heapsort, fgsl_heapsort_index, &
       fgsl_sort_smallest, fgsl_sort_smallest_index, &
       fgsl_sort_largest, fgsl_sort_largest_index, &
       fgsl_sort, fgsl_sort_index
  public :: fgsl_sort_vector2
! linear algebra
  public :: fgsl_linalg_lu_decomp, fgsl_linalg_complex_lu_decomp, &
       fgsl_linalg_lu_solve, fgsl_linalg_complex_lu_solve, &
       fgsl_linalg_lu_svx, fgsl_linalg_complex_lu_svx, &
       fgsl_linalg_lu_refine, fgsl_linalg_complex_lu_refine, &
       fgsl_linalg_lu_invert, fgsl_linalg_complex_lu_invert, &
       fgsl_linalg_lu_det, fgsl_linalg_complex_lu_det, &
       fgsl_linalg_lu_lndet, fgsl_linalg_complex_lu_lndet, &
       fgsl_linalg_lu_sgndet, fgsl_linalg_complex_lu_sgndet, &
       fgsl_linalg_qr_decomp, fgsl_linalg_qr_solve, fgsl_linalg_qr_svx, &
       fgsl_linalg_qr_lssolve, fgsl_linalg_qr_qtvec, fgsl_linalg_qr_qvec, &
       fgsl_linalg_qr_qtmat, fgsl_linalg_qr_rsolve, fgsl_linalg_qr_rsvx, &
       fgsl_linalg_qr_unpack, fgsl_linalg_qr_qrsolve, fgsl_linalg_qr_update, &
       fgsl_linalg_r_solve, fgsl_linalg_r_svx, fgsl_linalg_qrpt_decomp, &
       fgsl_linalg_qrpt_decomp2, fgsl_linalg_qrpt_solve, fgsl_linalg_qrpt_svx, &
       fgsl_linalg_qrpt_qrsolve, fgsl_linalg_qrpt_update, &
       fgsl_linalg_qrpt_rsolve, fgsl_linalg_qrpt_rsvx, &
       fgsl_linalg_sv_decomp, fgsl_linalg_sv_decomp_mod, &
       fgsl_linalg_sv_decomp_jacobi, fgsl_linalg_sv_solve, &
       fgsl_linalg_cholesky_decomp, fgsl_linalg_complex_cholesky_decomp, &
       fgsl_linalg_cholesky_solve, fgsl_linalg_complex_cholesky_solve, &
       fgsl_linalg_cholesky_svx, fgsl_linalg_complex_cholesky_svx, &
       fgsl_linalg_cholesky_invert, &
       fgsl_linalg_complex_cholesky_invert, &
       fgsl_linalg_symmtd_decomp, fgsl_linalg_symmtd_unpack, &
       fgsl_linalg_symmtd_unpack_t, fgsl_linalg_hermtd_decomp, &
       fgsl_linalg_hermtd_unpack, fgsl_linalg_hermtd_unpack_t, &
       fgsl_linalg_hessenberg_decomp, fgsl_linalg_hessenberg_unpack, &
       fgsl_linalg_hessenberg_unpack_accum, fgsl_linalg_hessenberg_set_zero, &
       fgsl_linalg_hesstri_decomp, &
       fgsl_linalg_bidiag_decomp, fgsl_linalg_bidiag_unpack, &
       fgsl_linalg_bidiag_unpack2, fgsl_linalg_bidiag_unpack_b, &
       fgsl_linalg_householder_transform, &
       fgsl_linalg_complex_householder_transform, &
       fgsl_linalg_householder_hm, fgsl_linalg_complex_householder_hm, &
       fgsl_linalg_householder_mh, fgsl_linalg_complex_householder_mh, &
       fgsl_linalg_householder_hv, fgsl_linalg_complex_householder_hv, &
       fgsl_linalg_hh_solve, fgsl_linalg_hh_svx, fgsl_linalg_solve_tridiag, &
       fgsl_linalg_solve_symm_tridiag, fgsl_linalg_solve_cyc_tridiag, &
       fgsl_linalg_solve_symm_cyc_tridiag, fgsl_linalg_balance_matrix
  public :: fgsl_linalg_sv_leverage
! eigensystems
  public :: fgsl_eigen_symm_alloc, fgsl_eigen_symm_free, fgsl_eigen_symm, &
       fgsl_eigen_symmv_alloc, fgsl_eigen_symmv_free, fgsl_eigen_symmv, &
       fgsl_eigen_herm_alloc, fgsl_eigen_herm_free, fgsl_eigen_herm, &
       fgsl_eigen_hermv_alloc, fgsl_eigen_hermv_free, fgsl_eigen_hermv, &
       fgsl_eigen_nonsymm_alloc, fgsl_eigen_nonsymm_free, fgsl_eigen_nonsymm, &
       fgsl_eigen_nonsymm_params, &
        fgsl_eigen_nonsymmv_params, &
       fgsl_eigen_nonsymmv_alloc, fgsl_eigen_nonsymmv_free, fgsl_eigen_nonsymmv, &
       fgsl_eigen_nonsymm_z, fgsl_eigen_nonsymmv_z, &
       fgsl_eigen_gensymm_alloc, fgsl_eigen_gensymm_free, fgsl_eigen_gensymm, &
       fgsl_eigen_gensymmv_alloc, fgsl_eigen_gensymmv_free, fgsl_eigen_gensymmv, &
       fgsl_eigen_genherm_alloc, fgsl_eigen_genherm_free, fgsl_eigen_genherm, &
       fgsl_eigen_genhermv_alloc, fgsl_eigen_genhermv_free, fgsl_eigen_genhermv, &
       fgsl_eigen_gen_alloc, fgsl_eigen_gen_free, fgsl_eigen_gen, &
       fgsl_eigen_genv_alloc, fgsl_eigen_genv_free, fgsl_eigen_genv, &
       fgsl_eigen_gen_params, fgsl_eigen_gen_qz, fgsl_eigen_genv_qz, &
       fgsl_eigen_symmv_sort, fgsl_eigen_hermv_sort, &
       fgsl_eigen_nonsymmv_sort, fgsl_eigen_gensymmv_sort, &
       fgsl_eigen_genhermv_sort, fgsl_eigen_genv_sort
! FFT
  public :: fgsl_fft_complex_radix2_forward, fgsl_fft_complex_radix2_transform, &
       fgsl_fft_complex_radix2_backward, fgsl_fft_complex_radix2_inverse, &
       fgsl_fft_complex_radix2_dif_forward, fgsl_fft_complex_radix2_dif_transform, &
       fgsl_fft_complex_radix2_dif_backward, fgsl_fft_complex_radix2_dif_inverse, &
       fgsl_fft_complex_wavetable_alloc, fgsl_fft_complex_wavetable_free, &
       fgsl_fft_complex_workspace_alloc, fgsl_fft_complex_workspace_free, &
       fgsl_fft_complex_forward, fgsl_fft_complex_transform, &
       fgsl_fft_complex_backward, fgsl_fft_complex_inverse, &
       fgsl_fft_real_radix2_transform, fgsl_fft_halfcomplex_radix2_inverse, &
       fgsl_fft_halfcomplex_radix2_backward, fgsl_fft_real_wavetable_alloc, &
       fgsl_fft_real_wavetable_free, fgsl_fft_halfcomplex_wavetable_alloc, &
       fgsl_fft_halfcomplex_wavetable_free, fgsl_fft_real_transform, &
       fgsl_fft_halfcomplex_transform, fgsl_fft_real_unpack, &
       fgsl_fft_halfcomplex_unpack, fgsl_fft_real_workspace_alloc, &
       fgsl_fft_real_workspace_free
! numerical integration
  public :: fgsl_integration_qng, fgsl_integration_workspace_alloc, &
       fgsl_integration_workspace_free, &
       fgsl_integration_qag, fgsl_integration_qagi, fgsl_integration_qags, &
       fgsl_integration_qagp, fgsl_integration_qagiu, fgsl_integration_qagil, &
       fgsl_integration_qawc, fgsl_integration_qaws_table_alloc, &
       fgsl_integration_qaws_table_set, fgsl_integration_qaws_table_free, &
       fgsl_integration_qaws, &
       fgsl_integration_qawo_table_alloc, fgsl_integration_qawo_table_set, &
       fgsl_integration_qawo_table_set_length, fgsl_integration_qawo, &
       fgsl_integration_cquad_workspace_alloc, fgsl_integration_cquad_workspace_free, &
       fgsl_integration_cquad, &
       fgsl_integration_glfixed_point,&
       fgsl_integration_glfixed, &
       fgsl_integration_glfixed_table_alloc, fgsl_integration_glfixed_table_free, &
       fgsl_integration_qawo_table_free, fgsl_integration_qawf

! random numbers, quasi-random numbers, distribution functions
  public :: fgsl_rng_alloc, fgsl_rng_set, fgsl_rng_free, fgsl_rng_get, fgsl_rng_uniform, &
       fgsl_rng_uniform_pos, fgsl_rng_uniform_int, fgsl_rng_name, fgsl_rng_max, &
       fgsl_rng_min, fgsl_rng_env_setup, fgsl_rng_memcpy, fgsl_rng_clone, &
       fgsl_rng_fwrite, fgsl_rng_fread, fgsl_qrng_alloc, &
       fgsl_qrng_free, fgsl_qrng_init, fgsl_qrng_get, fgsl_qrng_name, fgsl_qrng_memcpy, &
       fgsl_qrng_clone
  public :: fgsl_ran_gaussian, fgsl_ran_gaussian_pdf, fgsl_ran_gaussian_ziggurat, &
       fgsl_ran_gaussian_ratio_method, fgsl_ran_ugaussian, fgsl_ran_ugaussian_pdf, &
       fgsl_ran_ugaussian_ratio_method, fgsl_cdf_gaussian_p, fgsl_cdf_gaussian_q, &
       fgsl_cdf_gaussian_pinv, fgsl_cdf_gaussian_qinv, fgsl_cdf_ugaussian_p, &
       fgsl_cdf_ugaussian_q, fgsl_cdf_ugaussian_pinv, fgsl_cdf_ugaussian_qinv, &
       fgsl_ran_gaussian_tail, fgsl_ran_gaussian_tail_pdf, fgsl_ran_ugaussian_tail, &
       fgsl_ran_ugaussian_tail_pdf, fgsl_ran_bivariate_gaussian, fgsl_ran_exponential, &
       fgsl_ran_exponential_pdf, fgsl_cdf_exponential_p, fgsl_cdf_exponential_q, &
       fgsl_cdf_exponential_pinv, fgsl_cdf_exponential_qinv, fgsl_ran_laplace, &
       fgsl_ran_laplace_pdf, fgsl_cdf_laplace_p, fgsl_cdf_laplace_q, &
       fgsl_cdf_laplace_pinv, fgsl_cdf_laplace_qinv, fgsl_ran_exppow, &
       fgsl_ran_exppow_pdf, fgsl_cdf_exppow_p, fgsl_cdf_exppow_q, fgsl_ran_cauchy, &
       fgsl_ran_cauchy_pdf, fgsl_cdf_cauchy_p, fgsl_cdf_cauchy_q, fgsl_cdf_cauchy_pinv, &
       fgsl_cdf_cauchy_qinv, fgsl_ran_rayleigh, fgsl_ran_rayleigh_pdf, &
       fgsl_cdf_rayleigh_p, fgsl_cdf_rayleigh_q, fgsl_cdf_rayleigh_pinv, &
       fgsl_cdf_rayleigh_qinv, fgsl_ran_rayleigh_tail, fgsl_ran_rayleigh_tail_pdf, &
       fgsl_ran_landau, fgsl_ran_landau_pdf, fgsl_ran_levy, fgsl_ran_levy_skew
  public :: fgsl_ran_gamma, fgsl_ran_gamma_mt, fgsl_ran_gamma_pdf, fgsl_cdf_gamma_p, &
       fgsl_cdf_gamma_q, fgsl_cdf_gamma_pinv, fgsl_cdf_gamma_qinv, fgsl_ran_flat, &
       fgsl_ran_flat_pdf, fgsl_cdf_flat_p, fgsl_cdf_flat_q, fgsl_cdf_flat_pinv, &
       fgsl_cdf_flat_qinv, fgsl_ran_lognormal, fgsl_ran_lognormal_pdf, &
       fgsl_cdf_lognormal_p, fgsl_cdf_lognormal_q, fgsl_cdf_lognormal_pinv, &
       fgsl_cdf_lognormal_qinv, fgsl_ran_chisq, fgsl_ran_chisq_pdf, fgsl_cdf_chisq_p, &
       fgsl_cdf_chisq_q, fgsl_cdf_chisq_pinv, fgsl_cdf_chisq_qinv, fgsl_ran_fdist, &
       fgsl_ran_fdist_pdf, fgsl_cdf_fdist_p, fgsl_cdf_fdist_q, fgsl_cdf_fdist_pinv, &
       fgsl_cdf_fdist_qinv, fgsl_ran_tdist, fgsl_ran_tdist_pdf, fgsl_cdf_tdist_p, &
       fgsl_cdf_tdist_q, fgsl_cdf_tdist_pinv, fgsl_cdf_tdist_qinv, fgsl_ran_beta, &
       fgsl_ran_beta_pdf, fgsl_cdf_beta_p, fgsl_cdf_beta_q, fgsl_cdf_beta_pinv, &
       fgsl_cdf_beta_qinv, fgsl_ran_logistic, fgsl_ran_logistic_pdf, fgsl_cdf_logistic_p, &
       fgsl_cdf_logistic_q, fgsl_cdf_logistic_pinv, fgsl_cdf_logistic_qinv, &
       fgsl_ran_pareto, fgsl_ran_pareto_pdf, fgsl_cdf_pareto_p, fgsl_cdf_pareto_q, &
       fgsl_cdf_pareto_pinv, fgsl_cdf_pareto_qinv, fgsl_ran_dir_2d, &
       fgsl_ran_dir_2d_trig_method, fgsl_ran_dir_3d, fgsl_ran_dir_nd, &
       fgsl_ran_weibull, fgsl_ran_weibull_pdf, fgsl_cdf_weibull_p, fgsl_cdf_weibull_q, &
       fgsl_cdf_weibull_pinv, fgsl_cdf_weibull_qinv, fgsl_ran_gumbel1, &
       fgsl_ran_gumbel1_pdf, fgsl_cdf_gumbel1_p, fgsl_cdf_gumbel1_q, &
       fgsl_cdf_gumbel1_pinv, fgsl_cdf_gumbel1_qinv, fgsl_ran_gumbel2, &
       fgsl_ran_gumbel2_pdf, fgsl_cdf_gumbel2_p, fgsl_cdf_gumbel2_q, &
       fgsl_cdf_gumbel2_pinv, fgsl_cdf_gumbel2_qinv, fgsl_ran_dirichlet, &
       fgsl_ran_dirichlet_pdf, fgsl_ran_dirichlet_lnpdf, fgsl_ran_discrete_preproc, &
       fgsl_ran_discrete, fgsl_ran_discrete_pdf, fgsl_ran_poisson, fgsl_ran_poisson_pdf, &
       fgsl_cdf_poisson_p, fgsl_cdf_poisson_q, fgsl_ran_bernoulli, &
       fgsl_ran_bernoulli_pdf, fgsl_ran_binomial, fgsl_ran_binomial_pdf, &
       fgsl_cdf_binomial_p, fgsl_cdf_binomial_q, fgsl_ran_multinomial, &
       fgsl_ran_multinomial_pdf, fgsl_ran_multinomial_lnpdf, fgsl_ran_negative_binomial, &
       fgsl_ran_negative_binomial_pdf, fgsl_cdf_negative_binomial_p, &
       fgsl_cdf_negative_binomial_q, fgsl_ran_pascal, fgsl_ran_pascal_pdf, &
       fgsl_cdf_pascal_p, fgsl_cdf_pascal_q, fgsl_ran_geometric, fgsl_ran_geometric_pdf, &
       fgsl_cdf_geometric_p, fgsl_cdf_geometric_q, fgsl_ran_hypergeometric, &
       fgsl_ran_hypergeometric_pdf, fgsl_cdf_hypergeometric_p, fgsl_cdf_hypergeometric_q, &
       fgsl_ran_logarithmic, fgsl_ran_logarithmic_pdf, &
       fgsl_ran_shuffle, fgsl_ran_choose, fgsl_ran_sample, &
       fgsl_ran_discrete_free
! simulated annealing
  public :: fgsl_siman_params_init, fgsl_siman_params_free, fgsl_siman_solve
! ordinary differential equations
  public :: fgsl_odeiv2_system_init, fgsl_odeiv2_system_free, &
       fgsl_odeiv2_step_alloc, fgsl_odeiv2_step_status, fgsl_odeiv2_system_status, &
       fgsl_odeiv2_step_reset, fgsl_odeiv2_step_free, fgsl_odeiv2_step_name, &
       fgsl_odeiv2_step_order, fgsl_odeiv2_step_set_driver, fgsl_odeiv2_step_apply, &
       fgsl_odeiv2_control_standard_new, fgsl_odeiv2_control_y_new, fgsl_odeiv2_control_yp_new, &
       fgsl_odeiv2_control_scaled_new, fgsl_odeiv2_control_alloc, fgsl_odeiv2_control_init, &
       fgsl_odeiv2_control_free, fgsl_odeiv2_control_hadjust, &
       fgsl_odeiv2_control_name, fgsl_odeiv2_control_errlevel, fgsl_odeiv2_control_set_driver, &
       fgsl_odeiv2_evolve_alloc, fgsl_odeiv2_evolve_apply, fgsl_odeiv2_evolve_apply_fixed_step, &
       fgsl_odeiv2_evolve_reset, fgsl_odeiv2_evolve_free, fgsl_odeiv2_evolve_set_driver, &
       fgsl_odeiv2_driver_alloc_y_new, fgsl_odeiv2_driver_alloc_yp_new, &
       fgsl_odeiv2_driver_alloc_standard_new, fgsl_odeiv2_driver_alloc_scaled_new, &
       fgsl_odeiv2_driver_set_hmin, fgsl_odeiv2_driver_set_hmax, fgsl_odeiv2_driver_set_nmax, &
       fgsl_odeiv2_driver_apply, fgsl_odeiv2_driver_apply_fixed_step, &
       fgsl_odeiv2_driver_reset, fgsl_odeiv2_driver_free
  public :: gsl_odeiv2_driver_reset_hstart
!      legacy calls
  public :: fgsl_odeiv_system_init, fgsl_odeiv_system_free, &
       fgsl_odeiv_step_alloc, fgsl_odeiv_step_status, fgsl_odeiv_system_status, &
       fgsl_odeiv_step_reset, fgsl_odeiv_step_free, fgsl_odeiv_step_name, &
       fgsl_odeiv_step_order, fgsl_odeiv_step_apply, fgsl_odeiv_control_standard_new, &
       fgsl_odeiv_control_y_new, fgsl_odeiv_control_yp_new, &
       fgsl_odeiv_control_scaled_new, fgsl_odeiv_control_alloc, fgsl_odeiv_control_init, &
       fgsl_odeiv_control_free, fgsl_odeiv_control_hadjust, &
       fgsl_odeiv_control_name, fgsl_odeiv_evolve_alloc, fgsl_odeiv_evolve_apply, &
       fgsl_odeiv_evolve_reset, fgsl_odeiv_evolve_free
! Monte Carlo
  public :: fgsl_monte_function_init, fgsl_monte_function_free, &
       fgsl_monte_plain_alloc, fgsl_monte_plain_init, &
       fgsl_monte_plain_integrate, fgsl_monte_plain_free, &
       fgsl_monte_miser_alloc, fgsl_monte_miser_init, &
       fgsl_monte_miser_integrate, fgsl_monte_miser_free, &
       fgsl_monte_vegas_alloc, fgsl_monte_vegas_init, &
       fgsl_monte_vegas_integrate, fgsl_monte_vegas_free, &
       fgsl_monte_vegas_chisq, fgsl_monte_vegas_runval, &
       fgsl_monte_miser_setparams, fgsl_monte_miser_getparams, &
       fgsl_monte_vegas_setparams, fgsl_monte_vegas_getparams
! Histograms
  public :: fgsl_histogram_alloc, fgsl_histogram_set_ranges, &
       fgsl_histogram_set_ranges_uniform, fgsl_histogram_free, &
       fgsl_histogram_memcpy, fgsl_histogram_clone, fgsl_histogram_increment, &
       fgsl_histogram_accumulate, fgsl_histogram_get, fgsl_histogram_get_range, &
       fgsl_histogram_max, fgsl_histogram_min, fgsl_histogram_bins, &
       fgsl_histogram_reset, fgsl_histogram_find, fgsl_histogram_max_val, &
       fgsl_histogram_min_val, fgsl_histogram_max_bin, &
       fgsl_histogram_min_bin, fgsl_histogram_mean, &
       fgsl_histogram_sigma, fgsl_histogram_sum, fgsl_histogram_equal_bins_p, &
       fgsl_histogram_add, fgsl_histogram_sub, fgsl_histogram_mul, &
       fgsl_histogram_div, fgsl_histogram_scale, fgsl_histogram_shift, &
       fgsl_histogram_fwrite, fgsl_histogram_fread, fgsl_histogram_fprintf, &
       fgsl_histogram_fscanf, fgsl_histogram_pdf_alloc, fgsl_histogram_pdf_init, &
       fgsl_histogram_pdf_free, fgsl_histogram_pdf_sample, &
       fgsl_histogram2d_alloc, fgsl_histogram2d_set_ranges, &
       fgsl_histogram2d_set_ranges_uniform, fgsl_histogram2d_free, &
       fgsl_histogram2d_memcpy, fgsl_histogram2d_clone, fgsl_histogram2d_increment, &
       fgsl_histogram2d_accumulate, fgsl_histogram2d_get, fgsl_histogram2d_get_xrange, &
       fgsl_histogram2d_get_yrange, fgsl_histogram2d_xmax, &
       fgsl_histogram2d_xmin, fgsl_histogram2d_ymax, &
       fgsl_histogram2d_ymin, fgsl_histogram2d_nx, fgsl_histogram2d_ny, &
       fgsl_histogram2d_reset, fgsl_histogram2d_find, fgsl_histogram2d_max_val, &
       fgsl_histogram2d_min_val, fgsl_histogram2d_max_bin, &
       fgsl_histogram2d_min_bin, fgsl_histogram2d_xmean, fgsl_histogram2d_ymean, &
       fgsl_histogram2d_xsigma, fgsl_histogram2d_ysigma, fgsl_histogram2d_cov, &
       fgsl_histogram2d_sum, fgsl_histogram2d_equal_bins_p, &
       fgsl_histogram2d_add, fgsl_histogram2d_sub, fgsl_histogram2d_mul, &
       fgsl_histogram2d_div, fgsl_histogram2d_scale, fgsl_histogram2d_shift, &
       fgsl_histogram2d_fwrite, fgsl_histogram2d_fread, fgsl_histogram2d_fprintf, &
       fgsl_histogram2d_fscanf, fgsl_histogram2d_pdf_alloc, fgsl_histogram2d_pdf_init, &
       fgsl_histogram2d_pdf_free, fgsl_histogram2d_pdf_sample
! Ntuples
  public :: fgsl_ntuple_create, fgsl_ntuple_open, fgsl_ntuple_write, &
       fgsl_ntuple_bookdata, fgsl_ntuple_read, fgsl_ntuple_close, &
       fgsl_ntuple_select_fn_init, fgsl_ntuple_value_fn_init, &
       fgsl_ntuple_select_fn_free, fgsl_ntuple_value_fn_free, &
       fgsl_ntuple_project, fgsl_ntuple_data, fgsl_ntuple_size
! Numerical derivatives
  public :: fgsl_deriv_central, fgsl_deriv_forward, fgsl_deriv_backward
! Chebyshev approximations
  public :: fgsl_cheb_alloc, fgsl_cheb_free, fgsl_cheb_init, fgsl_cheb_order, &
       fgsl_cheb_size, fgsl_cheb_coeffs, fgsl_cheb_eval, &
       fgsl_cheb_eval_err, fgsl_cheb_eval_n, fgsl_cheb_eval_n_err, fgsl_cheb_calc_deriv, &
       fgsl_cheb_calc_integ
! Series acceleration
  public :: fgsl_sum_levin_u_alloc, fgsl_sum_levin_u_free, fgsl_sum_levin_u_accel, &
       fgsl_sum_levin_utrunc_alloc, fgsl_sum_levin_utrunc_free, fgsl_sum_levin_utrunc_accel
! Wavelet transforms
  public :: fgsl_wavelet_alloc, fgsl_wavelet_name, fgsl_wavelet_free, &
       fgsl_wavelet_workspace_alloc, fgsl_wavelet_workspace_free, fgsl_wavelet_transform, &
       fgsl_wavelet_transform_forward, fgsl_wavelet_transform_inverse, &
       fgsl_wavelet2d_transform, fgsl_wavelet2d_transform_forward, &
       fgsl_wavelet2d_transform_inverse, fgsl_wavelet2d_nstransform, &
       fgsl_wavelet2d_nstransform_forward, fgsl_wavelet2d_nstransform_inverse
! Hankel Transform
  public :: fgsl_dht_alloc, fgsl_dht_init, fgsl_dht_new, fgsl_dht_free, &
       fgsl_dht_apply, fgsl_dht_x_sample, fgsl_dht_k_sample
! One-dimensional root finding
  public :: fgsl_root_fsolver_alloc, fgsl_root_fdfsolver_alloc, fgsl_root_fsolver_set, &
       fgsl_root_fdfsolver_set, fgsl_root_fsolver_free, fgsl_root_fdfsolver_free, &
       fgsl_root_fsolver_name, fgsl_root_fdfsolver_name, fgsl_root_fsolver_iterate, &
       fgsl_root_fdfsolver_iterate, fgsl_root_fsolver_x_lower, fgsl_root_fsolver_x_upper, &
       fgsl_root_test_interval, fgsl_root_test_delta, fgsl_root_test_residual, &
       fgsl_root_fsolver_root, fgsl_root_fdfsolver_root
! One-dimensional minimization
  public :: fgsl_min_fminimizer_alloc, fgsl_min_fminimizer_free, fgsl_min_fminimizer_set, &
       fgsl_min_fminimizer_set_with_values, fgsl_min_fminimizer_iterate, fgsl_min_fminimizer_name, &
       fgsl_min_fminimizer_x_minimum, fgsl_min_fminimizer_x_lower, fgsl_min_fminimizer_x_upper, &
       fgsl_min_fminimizer_f_minimum, fgsl_min_fminimizer_f_lower, fgsl_min_fminimizer_f_upper, &
       fgsl_min_test_interval
! Multi-root
  public :: fgsl_multiroot_function_init, fgsl_multiroot_function_free, &
       fgsl_multiroot_fsolver_alloc, fgsl_multiroot_fsolver_free, fgsl_multiroot_fsolver_name, &
       fgsl_multiroot_fsolver_iterate, fgsl_multiroot_fsolver_root, fgsl_multiroot_fsolver_f, &
       fgsl_multiroot_fsolver_dx, fgsl_multiroot_test_delta, fgsl_multiroot_test_residual, &
       fgsl_multiroot_fsolver_set, fgsl_multiroot_fdfsolver_alloc, fgsl_multiroot_fdfsolver_free, &
       fgsl_multiroot_fdfsolver_name, fgsl_multiroot_function_fdf_init, fgsl_multiroot_function_fdf_free, &
       fgsl_multiroot_fdfsolver_iterate, fgsl_multiroot_fdfsolver_root, fgsl_multiroot_fdfsolver_f, &
       fgsl_multiroot_fdfsolver_dx, fgsl_multiroot_fdfsolver_set
! Multi-minimization
  public :: fgsl_multimin_function_init, fgsl_multimin_function_fdf_init, &
       fgsl_multimin_function_free, fgsl_multimin_function_fdf_free, fgsl_multimin_fminimizer_alloc, &
       fgsl_multimin_fdfminimizer_alloc, fgsl_multimin_fminimizer_free, fgsl_multimin_fdfminimizer_free, &
       fgsl_multimin_fminimizer_set, fgsl_multimin_fdfminimizer_set, fgsl_multimin_fminimizer_name, &
       fgsl_multimin_fdfminimizer_name, fgsl_multimin_fminimizer_iterate, &
       fgsl_multimin_fdfminimizer_iterate, &
       fgsl_multimin_fminimizer_minimum, fgsl_multimin_fdfminimizer_minimum, &
       fgsl_multimin_fdfminimizer_gradient, fgsl_multimin_fminimizer_size, &
       fgsl_multimin_fdfminimizer_restart, fgsl_multimin_fminimizer_x, &
       fgsl_multimin_test_size, fgsl_multimin_fdfminimizer_x, fgsl_multimin_test_gradient
! Linear and nonlinear fitting
  public :: fgsl_fit_linear, fgsl_fit_wlinear, fgsl_fit_linear_est, fgsl_fit_mul, &
       fgsl_fit_wmul, fgsl_fit_mul_est

  public :: fgsl_multifit_linear_alloc, fgsl_multifit_linear_free, &
       fgsl_multifit_linear, fgsl_multifit_linear_svd, &
       fgsl_multifit_wlinear, fgsl_multifit_wlinear_svd, &
       fgsl_multifit_wlinear_usvd, fgsl_multifit_linear_bsvd, &
       fgsl_multifit_linear_est, fgsl_multifit_linear_solve, &
       fgsl_multifit_linear_residuals, fgsl_multifit_linear_applyw, &
       fgsl_multifit_linear_stdform1, fgsl_multifit_linear_wstdform1, &
       fgsl_multifit_linear_l_decomp, fgsl_multifit_linear_stdform2, &
       fgsl_multifit_linear_wstdform2, fgsl_multifit_linear_genform1, &
       fgsl_multifit_linear_genform2, fgsl_multifit_linear_wgenform2, &
       fgsl_multifit_linear_lreg, fgsl_multifit_linear_lcurve, &
       fgsl_multifit_linear_lcorner, fgsl_multifit_linear_lcorner2, &
       fgsl_multifit_linear_lk, fgsl_multifit_linear_lsobolev, &
       fgsl_multifit_linear_rcond, fgsl_multifit_robust_maxiter, &
       fgsl_multifit_robust_weights, fgsl_multifit_robust_residuals



!TODO: new gsl_multifit_linear functions

  public :: fgsl_multifit_function_init, fgsl_multifit_function_fdf_init, &
       fgsl_multifit_function_free, fgsl_multifit_function_fdf_free, fgsl_multifit_fsolver_alloc, &
       fgsl_multifit_fdfsolver_alloc, fgsl_multifit_fsolver_free, fgsl_multifit_fdfsolver_free, &
       fgsl_multifit_fsolver_set, fgsl_multifit_fdfsolver_set, fgsl_multifit_fsolver_name, &
       fgsl_multifit_fdfsolver_name, fgsl_multifit_fsolver_iterate, fgsl_multifit_fdfsolver_iterate, &
       fgsl_multifit_fsolver_position, fgsl_multifit_fdfsolver_position, &
       fgsl_multifit_fdfsolver_dx, fgsl_multifit_fdfsolver_f, fgsl_multifit_fdfsolver_jac, &
       fgsl_multifit_test_delta, fgsl_multifit_test_gradient, fgsl_multifit_gradient, &
       fgsl_multifit_covar, fgsl_multifit_covar_qrpt, fgsl_multifit_fdfsolver_wset, &
       fgsl_multifit_fdfsolver_residual, fgsl_multifit_fdfsolver_niter, fgsl_multifit_eval_wf, &
       fgsl_multifit_eval_wdf, fgsl_multifit_fdfsolver_test
  public :: fgsl_multifit_fsolver_driver, fgsl_multifit_fdfsolver_driver, &
       fgsl_multifit_fdfsolver_dif_df, fgsl_multifit_fdfsolver_dif_fdf
  public :: fgsl_multifit_robust_alloc, fgsl_multifit_robust_free, &
       fgsl_multifit_robust_tune, fgsl_multifit_robust_name, &
       fgsl_multifit_robust_statistics, fgsl_multifit_robust, &
       fgsl_multifit_robust_est
! statistics
  public :: fgsl_stats_mean, fgsl_stats_variance, fgsl_stats_variance_m, &
       fgsl_stats_sd, fgsl_stats_sd_m, fgsl_stats_variance_with_fixed_mean, &
       fgsl_stats_sd_with_fixed_mean, fgsl_stats_absdev, fgsl_stats_absdev_m, &
       fgsl_stats_skew, fgsl_stats_skew_m_sd, fgsl_stats_kurtosis, &
       fgsl_stats_kurtosis_m_sd, fgsl_stats_lag1_autocorrelation, fgsl_stats_lag1_autocorrelation_m, &
       fgsl_stats_covariance, fgsl_stats_correlation, fgsl_stats_covariance_m, &
       fgsl_stats_wmean, fgsl_stats_wvariance, fgsl_stats_wvariance_m, &
       fgsl_stats_wsd, fgsl_stats_wsd_m, &
       fgsl_stats_wvariance_with_fixed_mean, fgsl_stats_wsd_with_fixed_mean, &
       fgsl_stats_wabsdev, fgsl_stats_wabsdev_m, fgsl_stats_wskew, &
       fgsl_stats_wskew_m_sd, fgsl_stats_wkurtosis, fgsl_stats_wkurtosis_m_sd, fgsl_stats_max, &
       fgsl_stats_min, fgsl_stats_minmax, fgsl_stats_max_index, &
       fgsl_stats_min_index, fgsl_stats_minmax_index, fgsl_stats_median_from_sorted_data, &
       fgsl_stats_quantile_from_sorted_data
  public :: fgsl_stats_spearman
! B-splines
  public :: fgsl_bspline_alloc, fgsl_bspline_free, fgsl_bspline_knots, &
       fgsl_bspline_knots_uniform, fgsl_bspline_eval, &
       fgsl_bspline_eval_nonzero,  fgsl_bspline_ncoeffs, &
       fgsl_bspline_deriv_eval, fgsl_bspline_deriv_eval_nonzero, &
       fgsl_bspline_greville_abscissa
  public :: fgsl_bspline_knots_greville

! IEEE
  public :: fgsl_ieee_fprintf, fgsl_ieee_printf, fgsl_ieee_env_setup
!
!
! Kind and length parameters are default integer
!
  integer, parameter, public :: fgsl_double = c_double
  integer, parameter, public :: fgsl_double_complex = c_double_complex
!  integer, parameter, public :: fgsl_extended = selected_real_kind(18)
  integer, parameter, public :: fgsl_extended = selected_real_kind(13)
! FIXME - c_long_double unsupported, selected_real_kind(30) unsupported in g95
  integer, parameter, public :: fgsl_float = c_float
  integer, parameter, public :: fgsl_int = c_int
  integer, parameter, public :: fgsl_long = c_long
  integer, parameter, public :: fgsl_size_t = c_size_t
  integer, parameter, public :: fgsl_char = c_char
  integer, parameter, public :: fgsl_strmax = 128
  integer, parameter, public :: fgsl_pathmax = 2048
!
! Version strings
!
  character(kind=fgsl_char, len=*), public, parameter ::&
  fgsl_version=PACKAGE_VERSION
  character(kind=fgsl_char, len=*), public, parameter ::&
  fgsl_gslbase=GSL_VERSION
!
! Error codes
!
  integer(fgsl_int), parameter, public :: fgsl_success = 0
  integer(fgsl_int), parameter, public :: fgsl_failure = -1
  integer(fgsl_int), parameter, public :: fgsl_continue = -2 ! iteration has not converged
  integer(fgsl_int), parameter, public :: fgsl_edom = 1      ! input domain error, e.g. sqrt(-1)
  integer(fgsl_int), parameter, public :: fgsl_erange = 2    ! output range error, e.g. exp(1e100)
  integer(fgsl_int), parameter, public :: fgsl_efault = 3    ! invalid pointer
  integer(fgsl_int), parameter, public :: fgsl_einval = 4    ! invalid argument supplied by user
  integer(fgsl_int), parameter, public :: fgsl_efactor = 6   ! generic failure
  integer(fgsl_int), parameter, public :: fgsl_esanity = 7   ! sanity check failed - shouldn't happen
  integer(fgsl_int), parameter, public :: fgsl_enomem = 8    ! malloc failed
  integer(fgsl_int), parameter, public :: fgsl_ebadfunc = 9  ! problem with user-supplied function
  integer(fgsl_int), parameter, public :: fgsl_erunaway = 10 ! iterative process is out of control
  integer(fgsl_int), parameter, public :: fgsl_emaxiter = 11 ! exceeded max number of iterations
  integer(fgsl_int), parameter, public :: fgsl_ezerodiv = 12 ! tried to divide by zero
  integer(fgsl_int), parameter, public :: fgsl_ebadtol = 13  ! user specified an invalid tolerance
  integer(fgsl_int), parameter, public :: fgsl_etol = 14     ! failed to reach the specified tolerance
  integer(fgsl_int), parameter, public :: fgsl_eundrflw = 15 ! underflow
  integer(fgsl_int), parameter, public :: fgsl_eovrflw = 16  ! overflow
  integer(fgsl_int), parameter, public :: fgsl_eloss = 17    ! loss of accuracy
  integer(fgsl_int), parameter, public :: fgsl_eround = 18   ! failed because of roundoff error
  integer(fgsl_int), parameter, public :: fgsl_ebadlen = 19  ! matrix, vector lengths are not conformant
  integer(fgsl_int), parameter, public :: fgsl_enotsqr = 20  ! matrix not square
  integer(fgsl_int), parameter, public :: fgsl_esing = 21    ! apparent singularity detected
  integer(fgsl_int), parameter, public :: fgsl_ediverge = 22 ! integral or series is divergent
  integer(fgsl_int), parameter, public :: fgsl_eunsup = 23   ! no hw support for requested feature
  integer(fgsl_int), parameter, public :: fgsl_eunimpl = 24  ! requested feature not (yet) implemented
  integer(fgsl_int), parameter, public :: fgsl_ecache = 25   ! cache limit exceeded
  integer(fgsl_int), parameter, public :: fgsl_etable = 26   ! table limit exceeded
  integer(fgsl_int), parameter, public :: fgsl_enoprog = 27  ! iteration: no progress towards solution
  integer(fgsl_int), parameter, public :: fgsl_enoprogj = 28 ! jacobian evals not improving the solution
  integer(fgsl_int), parameter, public :: fgsl_etolf = 29    ! can't reach specified tolerance in F
  integer(fgsl_int), parameter, public :: fgsl_etolx = 30    ! can't reach specified tolerance in X
  integer(fgsl_int), parameter, public :: fgsl_etolg = 31    ! can't reach specified tolerance in gradient
  integer(fgsl_int), parameter, public :: fgsl_eof = 32      ! end of file
!
! mathematical constants from gsl_math.h
!
  real(fgsl_extended), parameter, public :: m_e = 2.71828182845904523536028747135_fgsl_extended
  real(fgsl_extended), parameter, public :: m_log2e = 1.44269504088896340735992468100_fgsl_extended
  real(fgsl_extended), parameter, public :: m_log10e = 0.43429448190325182765112891892_fgsl_extended
  real(fgsl_extended), parameter, public :: m_sqrt2 = 1.41421356237309504880168872421_fgsl_extended
  real(fgsl_extended), parameter, public :: m_sqrt1_2 = 0.70710678118654752440084436210_fgsl_extended
  real(fgsl_extended), parameter, public :: m_sqrt3 = 1.73205080756887729352744634151_fgsl_extended
  real(fgsl_extended), parameter, public :: m_pi = 3.14159265358979323846264338328_fgsl_extended
  real(fgsl_extended), parameter, public :: m_pi_2 = 1.57079632679489661923132169164_fgsl_extended
  real(fgsl_extended), parameter, public :: m_pi_4 = 0.78539816339744830961566084582_fgsl_extended
  real(fgsl_extended), parameter, public :: m_sqrtpi = 1.77245385090551602729816748334_fgsl_extended
  real(fgsl_extended), parameter, public :: m_2_sqrtpi = 1.12837916709551257389615890312_fgsl_extended
  real(fgsl_extended), parameter, public :: m_1_pi = 0.31830988618379067153776752675_fgsl_extended
  real(fgsl_extended), parameter, public :: m_2_pi = 0.63661977236758134307553505349_fgsl_extended
  real(fgsl_extended), parameter, public :: m_ln10 = 2.30258509299404568401799145468_fgsl_extended
  real(fgsl_extended), parameter, public :: m_ln2 = 0.69314718055994530941723212146_fgsl_extended
  real(fgsl_extended), parameter, public :: m_lnpi = 1.14472988584940017414342735135_fgsl_extended
  real(fgsl_extended), parameter, public :: m_euler = 0.57721566490153286060651209008_fgsl_extended
! the following provokes warnings from g95 ... may need to change if refused by other compilers
!  real(fgsl_double), parameter, public :: fgsl_posinf = 1.0_fgsl_double / 0.0_fgsl_double
!  real(fgsl_double), parameter, public :: fgsl_neginf = -1.0_fgsl_double / 0.0_fgsl_double
!  real(fgsl_double), parameter, public :: fgsl_nan = 0.0_fgsl_double / 0.0_fgsl_double
! probably should throw this out - use IEEE_VALUE intrinsic if these are needed.
!
! Numerical constants
!
  real(fgsl_double), parameter, public :: fgsl_const_num_fine_structure = 7.297352533E-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_avogadro = 6.02214199E23_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_yotta = 1e24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_zetta = 1e21_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_exa = 1e18_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_peta = 1e15_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_tera = 1e12_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_giga = 1e9_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_mega = 1e6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_kilo = 1e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_milli = 1e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_micro = 1e-6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_nano = 1e-9_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_pico = 1e-12_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_femto = 1e-15_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_atto = 1e-18_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_zepto = 1e-21_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_yocto = 1e-24_fgsl_double
!
! MKSA physical units
!
  real(fgsl_double), parameter, public :: fgsl_const_mksa_speed_of_light = 2.99792458e8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_gravitational_constant = 6.673e-11_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_plancks_constant_h = 6.62606896e-34_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_plancks_constant_hbar = 1.05457162825e-34_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_astronomical_unit = 1.49597870691e11_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_light_year = 9.46053620707e15_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_parsec = 3.08567758135e16_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_grav_accel = 9.80665e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_electron_volt = 1.602176487e-19_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_mass_electron = 9.10938188e-31_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_mass_muon = 1.88353109e-28_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_mass_proton = 1.67262158e-27_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_mass_neutron = 1.67492716e-27_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_rydberg = 2.17987196968e-18_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_boltzmann = 1.3806504e-23_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_bohr_magneton = 9.27400899e-24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_nuclear_magneton = 5.05078317e-27_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_electron_magnetic_moment = 9.28476362e-24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_proton_magnetic_moment = 1.410606633e-26_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_molar_gas = 8.314472e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_standard_gas_volume = 2.2710981e-2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_minute = 6e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_hour = 3.6e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_day = 8.64e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_week = 6.048e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_inch = 2.54e-2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_foot = 3.048e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_yard = 9.144e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_mile = 1.609344e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_nautical_mile = 1.852e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_fathom = 1.8288e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_mil = 2.54e-5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_point = 3.52777777778e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_texpoint = 3.51459803515e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_micron = 1e-6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_angstrom = 1e-10_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_hectare = 1e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_acre = 4.04685642241e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_barn = 1e-28_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_liter = 1e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_us_gallon = 3.78541178402e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_quart = 9.46352946004e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_pint = 4.73176473002e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_cup = 2.36588236501e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_fluid_ounce = 2.95735295626e-5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_tablespoon = 1.47867647813e-5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_teaspoon = 4.92892159375e-6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_canadian_gallon = 4.54609e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_uk_gallon = 4.546092e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_miles_per_hour = 4.4704e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_kilometers_per_hour = 2.77777777778e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_knot = 5.14444444444e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_pound_mass = 4.5359237e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_ounce_mass = 2.8349523125e-2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_ton = 9.0718474e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_metric_ton = 1e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_uk_ton = 1.0160469088e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_troy_ounce = 3.1103475e-2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_carat = 2e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_unified_atomic_mass = 1.660538782e-27_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_gram_force = 9.80665e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_pound_force = 4.44822161526e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_kilopound_force = 4.44822161526e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_poundal = 1.38255e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_calorie = 4.1868e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_btu = 1.05505585262e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_therm = 1.05506e8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_horsepower = 7.457e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_bar = 1e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_std_atmosphere = 1.01325e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_torr = 1.33322368421e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_meter_of_mercury = 1.33322368421e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_inch_of_mercury = 3.38638815789e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_inch_of_water = 2.490889e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_psi = 6.89475729317e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_poise = 1e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_stokes = 1e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_faraday = 9.64853429775e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_electron_charge = 1.602176487e-19_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_gauss = 1e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_stilb = 1e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_lumen = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_lux = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_phot = 1e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_footcandle = 1.076e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_lambert = 1e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_footlambert = 1.07639104e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_curie = 3.7e10_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_roentgen = 2.58e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_rad = 1e-2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_solar_mass = 1.98892e30_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_bohr_radius = 5.291772083e-11_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_newton = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_dyne = 1e-5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_joule = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_erg = 1e-7_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_stefan_boltzmann_constant = 5.67040047374e-8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_thomson_cross_section = 6.65245893699e-29_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_vacuum_permittivity = 8.854187817e-12_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_vacuum_permeability = 1.25663706144e-6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_debye = 3.33564095198e-30_fgsl_double
!
! CGSM physical constants
!
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_speed_of_light = 2.99792458e10_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_gravitational_constant = 6.673e-8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_plancks_constant_h = 6.62606896e-27_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_plancks_constant_hbar = 1.05457162825e-27_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_astronomical_unit = 1.49597870691e13_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_light_year = 9.46053620707e17_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_parsec = 3.08567758135e18_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_grav_accel = 9.80665e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_electron_volt = 1.602176487e-12_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_mass_electron = 9.10938188e-28_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_mass_muon = 1.88353109e-25_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_mass_proton = 1.67262158e-24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_mass_neutron = 1.67492716e-24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_rydberg = 2.17987196968e-11_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_boltzmann = 1.3806504e-16_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_bohr_magneton = 9.27400899e-21_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_nuclear_magneton = 5.05078317e-24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_electron_magnetic_moment = 9.28476362e-21_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_proton_magnetic_moment = 1.410606633e-23_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_molar_gas = 8.314472e7_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_standard_gas_volume = 2.2710981e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_minute = 6e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_hour = 3.6e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_day = 8.64e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_week = 6.048e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_inch = 2.54e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_foot = 3.048e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_yard = 9.144e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_mile = 1.609344e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_nautical_mile = 1.852e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_fathom = 1.8288e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_mil = 2.54e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_point = 3.52777777778e-2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_texpoint = 3.51459803515e-2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_micron = 1e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_angstrom = 1e-8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_hectare = 1e8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_acre = 4.04685642241e7_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_barn = 1e-24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_liter = 1e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_us_gallon = 3.78541178402e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_quart = 9.46352946004e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_pint = 4.73176473002e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_cup = 2.36588236501e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_fluid_ounce = 2.95735295626e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_tablespoon = 1.47867647813e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_teaspoon = 4.92892159375e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_canadian_gallon = 4.54609e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_uk_gallon = 4.546092e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_miles_per_hour = 4.4704e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_kilometers_per_hour = 2.77777777778e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_knot = 5.14444444444e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_pound_mass = 4.5359237e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_ounce_mass = 2.8349523125e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_ton = 9.0718474e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_metric_ton = 1e6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_uk_ton = 1.0160469088e6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_troy_ounce = 3.1103475e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_carat = 2e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_unified_atomic_mass = 1.660538782e-24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_gram_force = 9.80665e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_pound_force = 4.44822161526e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_kilopound_force = 4.44822161526e8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_poundal = 1.38255e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_calorie = 4.1868e7_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_btu = 1.05505585262e10_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_therm = 1.05506e15_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_horsepower = 7.457e9_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_bar = 1e6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_std_atmosphere = 1.01325e6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_torr = 1.33322368421e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_meter_of_mercury = 1.33322368421e6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_inch_of_mercury = 3.38638815789e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_inch_of_water = 2.490889e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_psi = 6.89475729317e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_poise = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_stokes = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_faraday = 9.64853429775e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_electron_charge = 1.602176487e-20_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_gauss = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_stilb = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_lumen = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_lux = 1e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_phot = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_footcandle = 1.076e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_lambert = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_footlambert = 1.07639104e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_curie = 3.7e10_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_roentgen = 2.58e-8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_rad = 1e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_solar_mass = 1.98892e33_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_bohr_radius = 5.291772083e-9_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_newton = 1e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_dyne = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_joule = 1e7_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_erg = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_stefan_boltzmann_constant = 5.67040047374e-5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_thomson_cross_section = 6.65245893699e-25_fgsl_double
!
! Types : Error treatment
!
  type, public :: fgsl_error_handler_t
     private
     type(c_funptr) :: gsl_error_handler_t = c_null_funptr
  end type fgsl_error_handler_t
!
! Types: I/O Add-ons
!
  type, public :: fgsl_file
     private
     type(c_ptr) :: gsl_file = c_null_ptr
  end type fgsl_file
!
! Types: Mathematical Functions
!
  type, public :: fgsl_function
     private
     type(c_ptr) :: gsl_function = c_null_ptr
  end type fgsl_function
  type, public :: fgsl_function_fdf
     private
     type(c_ptr) :: gsl_function_fdf = c_null_ptr
  end type fgsl_function_fdf
!
! Types: Polynomial
!
! FIXME ifort refuses = overload if not public
  type, public, bind(c) :: gsl_complex
     real(c_double) :: dat(2)
  end type
  type, public :: fgsl_poly_complex_workspace
     private
     type(c_ptr) :: gsl_poly_complex_workspace
  end type fgsl_poly_complex_workspace
!
! Types: Special Functions
!
  type, public :: fgsl_sf_result
     real(fgsl_double) :: val, err
  end type fgsl_sf_result
! FIXME ifort refuses = overload if not public
  type, public, bind(c) :: gsl_sf_result
     real(c_double) :: val, err
  end type
  type, public :: fgsl_sf_result_e10
     real(fgsl_double) :: val, err
     integer(fgsl_int) :: e10
  end type fgsl_sf_result_e10
! FIXME ifort refuses = overload if not public
  type, public, bind(c) :: gsl_sf_result_e10
     real(c_double) :: val, err
     integer(c_int) :: e10
  end type
  type, public :: fgsl_mode_t
     private
     integer(c_int) :: gsl_mode = 0
  end type fgsl_mode_t
  type(fgsl_mode_t), parameter, public :: &
       fgsl_prec_double = fgsl_mode_t(0), &
       fgsl_prec_single = fgsl_mode_t(1), &
       fgsl_prec_approx = fgsl_mode_t(2)
!
! Types : Array support
!
  type, public :: fgsl_vector
     private
     type(c_ptr) :: gsl_vector = c_null_ptr
  end type fgsl_vector
  type, public :: fgsl_matrix
     private
     type(c_ptr) :: gsl_matrix = c_null_ptr
  end type fgsl_matrix
  type, public :: fgsl_vector_complex
     private
     type(c_ptr) :: gsl_vector_complex = c_null_ptr
  end type fgsl_vector_complex
  type, public :: fgsl_matrix_complex
     private
     type(c_ptr) :: gsl_matrix_complex = c_null_ptr
  end type fgsl_matrix_complex
!
! Types : Interpolation
!
!  integer, parameter :: interp_maxnum = 6
  type, public :: fgsl_interp_type
     private
     integer(fgsl_int) :: which = 0
  end type fgsl_interp_type
  type(fgsl_interp_type), parameter, public :: &
       fgsl_interp_linear = fgsl_interp_type(1), &
       fgsl_interp_polynomial = fgsl_interp_type(2), &
       fgsl_interp_cspline = fgsl_interp_type(3), &
       fgsl_interp_cspline_periodic = fgsl_interp_type(4), &
       fgsl_interp_akima = fgsl_interp_type(5), &
       fgsl_interp_akima_periodic = fgsl_interp_type(6)
  type, public :: fgsl_interp
     private
     type(c_ptr) :: gsl_interp = c_null_ptr
  end type fgsl_interp
  type, public :: fgsl_interp_accel
     private
     type(c_ptr) :: gsl_interp_accel = c_null_ptr
  end type fgsl_interp_accel
  type, public :: fgsl_spline
     private
     type(c_ptr) :: gsl_spline = c_null_ptr
  end type fgsl_spline
!
! Types: Permutations, Combinations and Multisets
!
  type, public :: fgsl_permutation
     private
     type(c_ptr) :: gsl_permutation = c_null_ptr
  end type fgsl_permutation
  type, public :: fgsl_combination
     private
     type(c_ptr) :: gsl_combination = c_null_ptr
  end type fgsl_combination
  type, public :: fgsl_multiset
     private
     type(c_ptr) :: gsl_multiset = c_null_ptr
  end type fgsl_multiset
!
! Types: Robust multifit
!
  type, public :: fgsl_multifit_robust_type
     private
     integer(fgsl_int) :: which = 0
  end type fgsl_multifit_robust_type
  type(fgsl_multifit_robust_type), parameter, public :: &
       fgsl_multifit_robust_default = fgsl_multifit_robust_type(1), &
       fgsl_multifit_robust_bisquare = fgsl_multifit_robust_type(2), &
       fgsl_multifit_robust_cauchy = fgsl_multifit_robust_type(3), &
       fgsl_multifit_robust_fair = fgsl_multifit_robust_type(4), &
       fgsl_multifit_robust_huber = fgsl_multifit_robust_type(5), &
       fgsl_multifit_robust_ols = fgsl_multifit_robust_type(6), &
       fgsl_multifit_robust_welsch = fgsl_multifit_robust_type(7)
  type, public :: fgsl_multifit_robust_workspace
       private
       type(c_ptr) :: gsl_multifit_robust_workspace
  end type fgsl_multifit_robust_workspace
  type, public :: fgsl_multifit_robust_stats
       real(fgsl_double) :: sigma_ols
       real(fgsl_double) :: sigma_mad
       real(fgsl_double) :: sigma_rob
       real(fgsl_double) :: sigma
       real(fgsl_double) :: Rsq
       real(fgsl_double) :: adj_Rsq
       real(fgsl_double) :: rmse
       real(fgsl_double) :: sse
       real(fgsl_double) :: dof
       real(fgsl_double) :: numit
       type(fgsl_vector) :: weights
       type(fgsl_vector) :: r
  end type fgsl_multifit_robust_stats
  type, bind(c) :: gsl_multifit_robust_stats
       real(c_double) :: sigma_ols
       real(c_double) :: sigma_mad
       real(c_double) :: sigma_rob
       real(c_double) :: sigma
       real(c_double) :: Rsq
       real(c_double) :: adj_Rsq
       real(c_double) :: rmse
       real(c_double) :: sse
       real(c_double) :: dof
       real(c_double) :: numit
       type(c_ptr) :: weights
       type(c_ptr) :: r
  end type gsl_multifit_robust_stats
!
! Types: Eigensystems
!
  type, public :: fgsl_eigen_symm_workspace
     private
     type(c_ptr) :: gsl_eigen_symm_workspace = c_null_ptr
  end type fgsl_eigen_symm_workspace
  type, public :: fgsl_eigen_symmv_workspace
     private
     type(c_ptr) :: gsl_eigen_symmv_workspace = c_null_ptr
  end type fgsl_eigen_symmv_workspace
  type, public :: fgsl_eigen_herm_workspace
     private
     type(c_ptr) :: gsl_eigen_herm_workspace = c_null_ptr
  end type fgsl_eigen_herm_workspace
  type, public :: fgsl_eigen_hermv_workspace
     private
     type(c_ptr) :: gsl_eigen_hermv_workspace = c_null_ptr
  end type fgsl_eigen_hermv_workspace
  type, public :: fgsl_eigen_nonsymm_workspace
     private
     type(c_ptr) :: gsl_eigen_nonsymm_workspace = c_null_ptr
  end type fgsl_eigen_nonsymm_workspace
  type, public :: fgsl_eigen_nonsymmv_workspace
     private
     type(c_ptr) :: gsl_eigen_nonsymmv_workspace = c_null_ptr
  end type fgsl_eigen_nonsymmv_workspace
  type, public :: fgsl_eigen_gensymm_workspace
     private
     type(c_ptr) :: gsl_eigen_gensymm_workspace = c_null_ptr
  end type fgsl_eigen_gensymm_workspace
  type, public :: fgsl_eigen_gensymmv_workspace
     private
     type(c_ptr) :: gsl_eigen_gensymmv_workspace = c_null_ptr
  end type fgsl_eigen_gensymmv_workspace
  type, public :: fgsl_eigen_genherm_workspace
     private
     type(c_ptr) :: gsl_eigen_genherm_workspace = c_null_ptr
  end type fgsl_eigen_genherm_workspace
  type, public :: fgsl_eigen_genhermv_workspace
     private
     type(c_ptr) :: gsl_eigen_genhermv_workspace = c_null_ptr
  end type fgsl_eigen_genhermv_workspace
  type, public :: fgsl_eigen_gen_workspace
     private
     type(c_ptr) :: gsl_eigen_gen_workspace = c_null_ptr
  end type fgsl_eigen_gen_workspace
  type, public :: fgsl_eigen_genv_workspace
     private
     type(c_ptr) :: gsl_eigen_genv_workspace = c_null_ptr
  end type fgsl_eigen_genv_workspace
  integer(c_int), parameter, public :: fgsl_eigen_sort_val_asc = 0
  integer(c_int), parameter, public :: fgsl_eigen_sort_val_desc = 1
  integer(c_int), parameter, public :: fgsl_eigen_sort_abs_asc = 2
  integer(c_int), parameter, public :: fgsl_eigen_sort_abs_desc = 3
!
! Types: FFT
!
  type, public :: fgsl_fft_complex_wavetable
     private
     type(c_ptr) :: gsl_fft_complex_wavetable = c_null_ptr
  end type fgsl_fft_complex_wavetable
  type, public :: fgsl_fft_real_wavetable
     private
     type(c_ptr) :: gsl_fft_real_wavetable = c_null_ptr
  end type fgsl_fft_real_wavetable
  type, public :: fgsl_fft_halfcomplex_wavetable
     private
     type(c_ptr) :: gsl_fft_halfcomplex_wavetable = c_null_ptr
  end type fgsl_fft_halfcomplex_wavetable
  type, public :: fgsl_fft_complex_workspace
     private
     type(c_ptr) :: gsl_fft_complex_workspace = c_null_ptr
  end type fgsl_fft_complex_workspace
  type, public :: fgsl_fft_real_workspace
     private
     type(c_ptr) :: gsl_fft_real_workspace = c_null_ptr
  end type fgsl_fft_real_workspace
!
! Types: Numerical Integration
!
  type, public :: fgsl_integration_workspace
     private
     type(c_ptr) :: gsl_integration_workspace = c_null_ptr
  end type fgsl_integration_workspace
  integer(fgsl_int), parameter, public :: fgsl_integ_gauss15 = 1
  integer(fgsl_int), parameter, public :: fgsl_integ_gauss21 = 2
  integer(fgsl_int), parameter, public :: fgsl_integ_gauss31 = 3
  integer(fgsl_int), parameter, public :: fgsl_integ_gauss41 = 4
  integer(fgsl_int), parameter, public :: fgsl_integ_gauss51 = 5
  integer(fgsl_int), parameter, public :: fgsl_integ_gauss61 = 6
  type, public :: fgsl_integration_qaws_table
     private
     type(c_ptr) :: gsl_integration_qaws_table = c_null_ptr
  end type fgsl_integration_qaws_table
  type, public :: fgsl_integration_qawo_table
     private
     type(c_ptr) :: gsl_integration_qawo_table = c_null_ptr
  end type fgsl_integration_qawo_table
  integer(fgsl_int), parameter, public :: fgsl_integ_cosine = 0
  integer(fgsl_int), parameter, public :: fgsl_integ_sine = 1
  type, public :: fgsl_integration_cquad_workspace
     private
     type(c_ptr) :: gsl_integration_cquad_workspace = c_null_ptr
  end type fgsl_integration_cquad_workspace
  type, public :: fgsl_integration_glfixed_table
     private
     type(c_ptr) :: gsl_integration_glfixed_table = c_null_ptr
  end type fgsl_integration_glfixed_table
!
! Types: Random and Quasi-random numbers
!
  type, public :: fgsl_rng
     private
     type(c_ptr) :: gsl_rng
  end type fgsl_rng
  type, public :: fgsl_rng_type
     private
     type(c_ptr) :: gsl_rng_type
     integer(fgsl_int) :: type = 0
  end type fgsl_rng_type
! Note: we need a dynamic component here, since
! fgsl_rng_default needs to change at run time.
! fgsl_rng_default will be set by fgsl_rng_env_setup,
! and the static objects will be all set at the
! first call of fgsl_rng_alloc
!  integer, parameter :: rngmax = 61
! check new g95  type(fgsl_rng_type) :: fgsl_rng_allgen(rngmax) = (/(fgsl_rng_type(c_null_ptr, i)), i=1, rngmax/)
! cannot have protected attribute since modified by fgsl_rng_alloc
  type(fgsl_rng_type), public :: &
       fgsl_rng_default = fgsl_rng_type(c_null_ptr, -1), &
       fgsl_rng_borosh13 = fgsl_rng_type(c_null_ptr, 1), &
       fgsl_rng_coveyou = fgsl_rng_type(c_null_ptr, 2), &
       fgsl_rng_cmrg = fgsl_rng_type(c_null_ptr, 3), &
       fgsl_rng_fishman18 = fgsl_rng_type(c_null_ptr, 4), &
       fgsl_rng_fishman20 = fgsl_rng_type(c_null_ptr, 5), &
       fgsl_rng_fishman2x = fgsl_rng_type(c_null_ptr, 6), &
       fgsl_rng_gfsr4 = fgsl_rng_type(c_null_ptr, 7), &
       fgsl_rng_knuthran = fgsl_rng_type(c_null_ptr, 8), &
       fgsl_rng_knuthran2 = fgsl_rng_type(c_null_ptr, 9), &
       fgsl_rng_lecuyer21 = fgsl_rng_type(c_null_ptr, 10), &
       fgsl_rng_minstd = fgsl_rng_type(c_null_ptr, 11), &
       fgsl_rng_mrg = fgsl_rng_type(c_null_ptr, 12), &
       fgsl_rng_mt19937 = fgsl_rng_type(c_null_ptr, 13), &
       fgsl_rng_mt19937_1999 = fgsl_rng_type(c_null_ptr, 14), &
       fgsl_rng_mt19937_1998 = fgsl_rng_type(c_null_ptr, 15), &
       fgsl_rng_r250 = fgsl_rng_type(c_null_ptr, 16), &
       fgsl_rng_ran0 = fgsl_rng_type(c_null_ptr, 17), &
       fgsl_rng_ran1 = fgsl_rng_type(c_null_ptr, 18), &
       fgsl_rng_ran2 = fgsl_rng_type(c_null_ptr, 19), &
       fgsl_rng_ran3 = fgsl_rng_type(c_null_ptr, 20), &
       fgsl_rng_rand = fgsl_rng_type(c_null_ptr, 21), &
       fgsl_rng_rand48 = fgsl_rng_type(c_null_ptr, 22), &
       fgsl_rng_random128_bsd = fgsl_rng_type(c_null_ptr, 23), &
       fgsl_rng_random128_glibc2 = fgsl_rng_type(c_null_ptr, 24), &
       fgsl_rng_random128_libc5 = fgsl_rng_type(c_null_ptr, 25), &
       fgsl_rng_random256_bsd = fgsl_rng_type(c_null_ptr, 26), &
       fgsl_rng_random256_glibc2 = fgsl_rng_type(c_null_ptr, 27), &
       fgsl_rng_random256_libc5 = fgsl_rng_type(c_null_ptr, 28), &
       fgsl_rng_random32_bsd = fgsl_rng_type(c_null_ptr, 29), &
       fgsl_rng_random32_glibc2 = fgsl_rng_type(c_null_ptr, 30), &
       fgsl_rng_random32_libc5 = fgsl_rng_type(c_null_ptr, 31), &
       fgsl_rng_random64_bsd = fgsl_rng_type(c_null_ptr, 32), &
       fgsl_rng_random64_glibc2 = fgsl_rng_type(c_null_ptr, 33), &
       fgsl_rng_random64_libc5 = fgsl_rng_type(c_null_ptr, 34), &
       fgsl_rng_random8_bsd = fgsl_rng_type(c_null_ptr, 35)
  type(fgsl_rng_type), public ::  &
       fgsl_rng_random8_glibc2 = fgsl_rng_type(c_null_ptr, 36), &
       fgsl_rng_random8_libc5 = fgsl_rng_type(c_null_ptr, 37), &
       fgsl_rng_random_bsd = fgsl_rng_type(c_null_ptr, 38), &
       fgsl_rng_random_glibc2 = fgsl_rng_type(c_null_ptr, 39), &
       fgsl_rng_random_libc5 = fgsl_rng_type(c_null_ptr, 40), &
       fgsl_rng_randu = fgsl_rng_type(c_null_ptr, 41), &
       fgsl_rng_ranf = fgsl_rng_type(c_null_ptr, 42), &
       fgsl_rng_ranlux = fgsl_rng_type(c_null_ptr, 43), &
       fgsl_rng_ranlux389 = fgsl_rng_type(c_null_ptr, 44), &
       fgsl_rng_ranlxd1 = fgsl_rng_type(c_null_ptr, 45), &
       fgsl_rng_ranlxd2 = fgsl_rng_type(c_null_ptr, 46), &
       fgsl_rng_ranlxs0 = fgsl_rng_type(c_null_ptr, 47), &
       fgsl_rng_ranlxs1 = fgsl_rng_type(c_null_ptr, 48), &
       fgsl_rng_ranlxs2 = fgsl_rng_type(c_null_ptr, 49), &
       fgsl_rng_ranmar = fgsl_rng_type(c_null_ptr, 50), &
       fgsl_rng_slatec = fgsl_rng_type(c_null_ptr, 51), &
       fgsl_rng_taus = fgsl_rng_type(c_null_ptr, 52), &
       fgsl_rng_taus2 = fgsl_rng_type(c_null_ptr, 53), &
       fgsl_rng_taus113 = fgsl_rng_type(c_null_ptr, 54), &
       fgsl_rng_transputer = fgsl_rng_type(c_null_ptr, 55), &
       fgsl_rng_tt800 = fgsl_rng_type(c_null_ptr, 56), &
       fgsl_rng_uni = fgsl_rng_type(c_null_ptr, 57), &
       fgsl_rng_uni32 = fgsl_rng_type(c_null_ptr, 58), &
       fgsl_rng_vax = fgsl_rng_type(c_null_ptr, 59), &
       fgsl_rng_waterman14 = fgsl_rng_type(c_null_ptr, 60), &
       fgsl_rng_zuf = fgsl_rng_type(c_null_ptr, 61), &
       fgsl_rng_knuthran2002 = fgsl_rng_type(c_null_ptr, 62)
  integer(fgsl_long), public, bind(c, name='gsl_rng_default_seed') :: fgsl_rng_default_seed
  type, public :: fgsl_qrng
     private
     type(c_ptr) :: gsl_qrng
  end type fgsl_qrng
  type, public :: fgsl_qrng_type
     private
     integer(fgsl_int) :: type = 0
  end type fgsl_qrng_type
  type(fgsl_qrng_type), parameter, public :: &
       fgsl_qrng_niederreiter_2 = fgsl_qrng_type(1), &
       fgsl_qrng_sobol = fgsl_qrng_type(2), &
       fgsl_qrng_halton =  fgsl_qrng_type(3), &
       fgsl_qrng_reversehalton =  fgsl_qrng_type(4)
  type, public :: fgsl_ran_discrete_t
     private
     type(c_ptr) :: gsl_ran_discrete_t
  end type fgsl_ran_discrete_t
!
! Types: Histograms
!
  type, public :: fgsl_histogram
     private
     type(c_ptr) :: gsl_histogram = c_null_ptr
  end type fgsl_histogram
  type, public :: fgsl_histogram_pdf
     private
     type(c_ptr) :: gsl_histogram_pdf = c_null_ptr
  end type fgsl_histogram_pdf
  type, public :: fgsl_histogram2d
     private
     type(c_ptr) :: gsl_histogram2d = c_null_ptr
  end type fgsl_histogram2d
  type, public :: fgsl_histogram2d_pdf
     private
     type(c_ptr) :: gsl_histogram2d_pdf = c_null_ptr
  end type fgsl_histogram2d_pdf
!
! Types: Ntuples
!
  type, public :: fgsl_ntuple
     private
     type(c_ptr) :: gsl_ntuple = c_null_ptr
  end type fgsl_ntuple
  type, public :: fgsl_ntuple_select_fn
     private
     type(c_ptr) :: gsl_ntuple_select_fn = c_null_ptr
  end type fgsl_ntuple_select_fn
  type, public :: fgsl_ntuple_value_fn
     private
     type(c_ptr) :: gsl_ntuple_value_fn = c_null_ptr
  end type fgsl_ntuple_value_fn
!
! Types: Monte Carlo integration
!
  type, public :: fgsl_monte_function
     private
     type(c_ptr) :: gsl_monte_function = c_null_ptr
  end type fgsl_monte_function
  type, public :: fgsl_monte_plain_state
     private
     type(c_ptr) :: gsl_monte_plain_state = c_null_ptr
  end type fgsl_monte_plain_state
  type, public :: fgsl_monte_miser_state
     private
     type(c_ptr) :: gsl_monte_miser_state = c_null_ptr
  end type fgsl_monte_miser_state
  type, public :: fgsl_monte_vegas_state
     private
     type(c_ptr) :: gsl_monte_vegas_state = c_null_ptr
  end type fgsl_monte_vegas_state
! NOTE: not all compilers support enum yet
  integer(c_int), parameter, public :: fgsl_vegas_mode_importance = 1
  integer(c_int), parameter, public :: fgsl_vegas_mode_importance_only = 0
  integer(c_int), parameter, public :: fgsl_vegas_mode_stratified = -1
!
! Types: Simulated Annealing
!
  type, bind(c) :: gsl_siman_params_t
     integer(c_int) :: n_tries, iters_fixed_t
     real(c_double) :: step_size, k, t_initial, mu_t, t_min
  end type gsl_siman_params_t
  type, public :: fgsl_siman_params_t
     private
     type(gsl_siman_params_t), pointer :: gsl_siman_params_t => null()
  end type fgsl_siman_params_t
!
! Types: Ordinary Differential Equations
!
  type, public :: fgsl_odeiv2_system
     private
     type(c_ptr) :: gsl_odeiv2_system = c_null_ptr
  end type fgsl_odeiv2_system
  type, public :: fgsl_odeiv2_step_type
     private
     integer(c_int) :: which = 0
  end type fgsl_odeiv2_step_type
  type(fgsl_odeiv2_step_type), parameter, public :: &
       fgsl_odeiv2_step_rk2 = fgsl_odeiv2_step_type(1), &
       fgsl_odeiv2_step_rk4 = fgsl_odeiv2_step_type(2), &
       fgsl_odeiv2_step_rkf45 = fgsl_odeiv2_step_type(3), &
       fgsl_odeiv2_step_rkck = fgsl_odeiv2_step_type(4), &
       fgsl_odeiv2_step_rk8pd = fgsl_odeiv2_step_type(5), &
       fgsl_odeiv2_step_rk1imp = fgsl_odeiv2_step_type(6), &
       fgsl_odeiv2_step_rk2imp = fgsl_odeiv2_step_type(7), &
       fgsl_odeiv2_step_rk4imp = fgsl_odeiv2_step_type(8), &
       fgsl_odeiv2_step_bsimp = fgsl_odeiv2_step_type(9), &
       fgsl_odeiv2_step_msadams = fgsl_odeiv2_step_type(10), &
       fgsl_odeiv2_step_msbdf = fgsl_odeiv2_step_type(11)
  type, public :: fgsl_odeiv2_step
     type(c_ptr) :: gsl_odeiv2_step = c_null_ptr
  end type fgsl_odeiv2_step
  type, public :: fgsl_odeiv2_driver
     private
     type(c_ptr) :: gsl_odeiv2_driver = c_null_ptr
  end type fgsl_odeiv2_driver
  type, public :: fgsl_odeiv2_control_type
     type(c_ptr) :: gsl_odeiv2_control_type = c_null_ptr
  end type fgsl_odeiv2_control_type
  type, public :: fgsl_odeiv2_control
     type(c_ptr) :: gsl_odeiv2_control = c_null_ptr
  end type fgsl_odeiv2_control
  type, public :: fgsl_odeiv2_evolve
     type(c_ptr) :: gsl_odeiv2_evolve
  end type fgsl_odeiv2_evolve

! obsolescent legacy interface
  type, public :: fgsl_odeiv_system
     private
     type(c_ptr) :: gsl_odeiv_system = c_null_ptr
  end type fgsl_odeiv_system
  type, public :: fgsl_odeiv_step_type
     private
     integer(c_int) :: which = 0
  end type fgsl_odeiv_step_type
  type(fgsl_odeiv_step_type), parameter, public :: &
       fgsl_odeiv_step_rk2 = fgsl_odeiv_step_type(1), &
       fgsl_odeiv_step_rk4 = fgsl_odeiv_step_type(2), &
       fgsl_odeiv_step_rkf45 = fgsl_odeiv_step_type(3), &
       fgsl_odeiv_step_rkck = fgsl_odeiv_step_type(4), &
       fgsl_odeiv_step_rk8pd = fgsl_odeiv_step_type(5), &
       fgsl_odeiv_step_rk2imp = fgsl_odeiv_step_type(6), &
       fgsl_odeiv_step_rk2simp = fgsl_odeiv_step_type(7), &
       fgsl_odeiv_step_rk4imp = fgsl_odeiv_step_type(8), &
       fgsl_odeiv_step_bsimp = fgsl_odeiv_step_type(9), &
       fgsl_odeiv_step_gear1 = fgsl_odeiv_step_type(10), &
       fgsl_odeiv_step_gear2 = fgsl_odeiv_step_type(11)
  type, public :: fgsl_odeiv_step
     type(c_ptr) :: gsl_odeiv_step = c_null_ptr
  end type fgsl_odeiv_step
  type, public :: fgsl_odeiv_control
     type(c_ptr) :: gsl_odeiv_control = c_null_ptr
  end type fgsl_odeiv_control
  type, public :: fgsl_odeiv_control_type
     type(c_ptr) :: gsl_odeiv_control_type = c_null_ptr
  end type fgsl_odeiv_control_type
  integer(fgsl_int), parameter, public :: fgsl_odeiv_hadj_inc = 1
  integer(fgsl_int), parameter, public :: fgsl_odeiv_hadj_nil = 0
  integer(fgsl_int), parameter, public :: fgsl_odeiv_hadj_dec = -1
  type, public :: fgsl_odeiv_evolve
     type(c_ptr) :: gsl_odeiv_evolve
  end type fgsl_odeiv_evolve
!
! Types: Chebyshev approximation
!
  type, public :: fgsl_cheb_series
     private
     type(c_ptr) :: gsl_cheb_series = c_null_ptr
  end type fgsl_cheb_series
!
! Types: Series acceleration
!
  type, public :: fgsl_sum_levin_u_workspace
     private
     type(c_ptr) :: gsl_sum_levin_u_workspace = c_null_ptr
  end type fgsl_sum_levin_u_workspace
  type, public :: fgsl_sum_levin_utrunc_workspace
     private
     type(c_ptr) :: gsl_sum_levin_utrunc_workspace = c_null_ptr
  end type fgsl_sum_levin_utrunc_workspace
!
! Types: Wavelet transforms
!
  type, public :: fgsl_wavelet
     private
     type(c_ptr) :: gsl_wavelet = c_null_ptr
  end type fgsl_wavelet
  type, public :: fgsl_wavelet_type
     private
     integer(c_int) :: which = 0
  end type fgsl_wavelet_type
  type(fgsl_wavelet_type), public, parameter :: &
       fgsl_wavelet_daubechies = fgsl_wavelet_type(1), &
       fgsl_wavelet_daubechies_centered = fgsl_wavelet_type(2), &
       fgsl_wavelet_haar = fgsl_wavelet_type(3), &
       fgsl_wavelet_haar_centered = fgsl_wavelet_type(4), &
       fgsl_wavelet_bspline = fgsl_wavelet_type(5), &
       fgsl_wavelet_bspline_centered = fgsl_wavelet_type(6)
  type, public :: fgsl_wavelet_workspace
     private
     type(c_ptr) :: gsl_wavelet_workspace
  end type fgsl_wavelet_workspace
!
! Types: Hankel transforms
!
  type, public :: fgsl_dht
     private
     type(c_ptr) :: gsl_dht = c_null_ptr
  end type fgsl_dht
!
! Types: Root finding
!
  type, public :: fgsl_root_fsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_root_fsolver_type
  type(fgsl_root_fsolver_type), public, parameter :: &
       fgsl_root_fsolver_bisection = fgsl_root_fsolver_type(1), &
       fgsl_root_fsolver_brent = fgsl_root_fsolver_type(2), &
       fgsl_root_fsolver_falsepos = fgsl_root_fsolver_type(3)
  type, public :: fgsl_root_fdfsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_root_fdfsolver_type
  type(fgsl_root_fdfsolver_type), public, parameter :: &
       fgsl_root_fdfsolver_newton = fgsl_root_fdfsolver_type(1), &
       fgsl_root_fdfsolver_secant = fgsl_root_fdfsolver_type(2), &
       fgsl_root_fdfsolver_steffenson = fgsl_root_fdfsolver_type(3)
  type, public :: fgsl_root_fsolver
     private
     type(c_ptr) :: gsl_root_fsolver = c_null_ptr
  end type fgsl_root_fsolver
  type, public :: fgsl_root_fdfsolver
     private
     type(c_ptr) :: gsl_root_fdfsolver = c_null_ptr
  end type fgsl_root_fdfsolver
!
! Types: Minimization
!
  type, public :: fgsl_min_fminimizer_type
     private
     integer(c_int) :: which = 0
  end type fgsl_min_fminimizer_type
  type(fgsl_min_fminimizer_type), public, parameter :: &
       fgsl_min_fminimizer_goldensection = fgsl_min_fminimizer_type(1), &
       fgsl_min_fminimizer_brent = fgsl_min_fminimizer_type(2), &
       fgsl_min_fminimizer_quad_golden = fgsl_min_fminimizer_type(3)
  type, public :: fgsl_min_fminimizer
     private
     type(c_ptr) :: gsl_min_fminimizer = c_null_ptr
  end type fgsl_min_fminimizer
!
! Types: Multi-Root
!
  type, public :: fgsl_multiroot_function
     private
     type(c_ptr) :: gsl_multiroot_function = c_null_ptr
  end type fgsl_multiroot_function
  type, public :: fgsl_multiroot_function_fdf
     private
     type(c_ptr) :: gsl_multiroot_function_fdf = c_null_ptr
  end type fgsl_multiroot_function_fdf
  type, public :: fgsl_multiroot_fsolver
     private
     type(c_ptr) :: gsl_multiroot_fsolver = c_null_ptr
  end type fgsl_multiroot_fsolver
  type, public :: fgsl_multiroot_fsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multiroot_fsolver_type
  type(fgsl_multiroot_fsolver_type), public, parameter :: &
       fgsl_multiroot_fsolver_dnewton = fgsl_multiroot_fsolver_type(1), &
       fgsl_multiroot_fsolver_broyden = fgsl_multiroot_fsolver_type(2), &
       fgsl_multiroot_fsolver_hybrid = fgsl_multiroot_fsolver_type(3), &
       fgsl_multiroot_fsolver_hybrids = fgsl_multiroot_fsolver_type(4)
  type, public :: fgsl_multiroot_fdfsolver
     private
     type(c_ptr) :: gsl_multiroot_fdfsolver = c_null_ptr
  end type fgsl_multiroot_fdfsolver
  type, public :: fgsl_multiroot_fdfsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multiroot_fdfsolver_type
  type(fgsl_multiroot_fdfsolver_type), public, parameter :: &
       fgsl_multiroot_fdfsolver_newton = fgsl_multiroot_fdfsolver_type(1), &
       fgsl_multiroot_fdfsolver_gnewton = fgsl_multiroot_fdfsolver_type(2), &
       fgsl_multiroot_fdfsolver_hybridj = fgsl_multiroot_fdfsolver_type(3), &
       fgsl_multiroot_fdfsolver_hybridsj = fgsl_multiroot_fdfsolver_type(4)
!
! Types: Multi-Min
!
  type, public :: fgsl_multimin_function
     private
     type(c_ptr) :: gsl_multimin_function = c_null_ptr
  end type fgsl_multimin_function
  type, public :: fgsl_multimin_function_fdf
     private
     type(c_ptr) :: gsl_multimin_function_fdf = c_null_ptr
  end type fgsl_multimin_function_fdf
  type, public :: fgsl_multimin_fminimizer
     private
     type(c_ptr) :: gsl_multimin_fminimizer = c_null_ptr
  end type fgsl_multimin_fminimizer
  type, public :: fgsl_multimin_fminimizer_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multimin_fminimizer_type
  type(fgsl_multimin_fminimizer_type), public, parameter :: &
       fgsl_multimin_fminimizer_nmsimplex = fgsl_multimin_fminimizer_type(1), &
       fgsl_multimin_fminimizer_nmsimplex2 = fgsl_multimin_fminimizer_type(2), &
       fgsl_multimin_fminimizer_nmsimplex2rand = fgsl_multimin_fminimizer_type(3)
  type, public :: fgsl_multimin_fdfminimizer
     private
     type(c_ptr) :: gsl_multimin_fdfminimizer = c_null_ptr
  end type fgsl_multimin_fdfminimizer
  type, public :: fgsl_multimin_fdfminimizer_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multimin_fdfminimizer_type
  type(fgsl_multimin_fdfminimizer_type), public, parameter :: &
       fgsl_multimin_fdfminimizer_steepest_descent = fgsl_multimin_fdfminimizer_type(1), &
       fgsl_multimin_fdfminimizer_conjugate_pr = fgsl_multimin_fdfminimizer_type(2), &
       fgsl_multimin_fdfminimizer_conjugate_fr = fgsl_multimin_fdfminimizer_type(3), &
       fgsl_multimin_fdfminimizer_vector_bfgs = fgsl_multimin_fdfminimizer_type(4), &
       fgsl_multimin_fdfminimizer_vector_bfgs2 = fgsl_multimin_fdfminimizer_type(5)
!
! Types: Fitting
!
  type, public :: fgsl_multifit_linear_workspace
     private
     type(c_ptr) :: gsl_multifit_linear_workspace = c_null_ptr
  end type fgsl_multifit_linear_workspace
  type, public :: fgsl_multifit_function
     private
     type(c_ptr) :: gsl_multifit_function = c_null_ptr
  end type fgsl_multifit_function
  type, public :: fgsl_multifit_function_fdf
     private
     type(c_ptr) :: gsl_multifit_function_fdf = c_null_ptr
  end type fgsl_multifit_function_fdf
  type, public :: fgsl_multifit_fsolver
     private
     type(c_ptr) :: gsl_multifit_fsolver = c_null_ptr
  end type fgsl_multifit_fsolver
  type, public :: fgsl_multifit_fsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multifit_fsolver_type
  type, public :: fgsl_multifit_fdfsolver
     private
     type(c_ptr) :: gsl_multifit_fdfsolver = c_null_ptr
  end type fgsl_multifit_fdfsolver
  type, public :: fgsl_multifit_fdfsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multifit_fdfsolver_type
  type(fgsl_multifit_fdfsolver_type), public, parameter :: &
       fgsl_multifit_fdfsolver_lmder = fgsl_multifit_fdfsolver_type(1), &
       fgsl_multifit_fdfsolver_lmsder = fgsl_multifit_fdfsolver_type(2), &
       fgsl_multifit_fdfsolver_lmniel = fgsl_multifit_fdfsolver_type(3)
!
! Types: B-Splines
!
  type, public :: fgsl_bspline_workspace
     private
     type(c_ptr) :: gsl_bspline_workspace
  end type fgsl_bspline_workspace
!
! required C interfaces
! FGSL names occurring here are auxiliary routines
! needed to transfer static C information to the Fortran subsystem
  interface
#include "interface/error.finc"
#include "interface/misc.finc"
#include "interface/io.finc"
#include "interface/math.finc"
#include "interface/complex.finc"
#include "interface/poly.finc"
#include "interface/specfunc.finc"
#include "interface/array.finc"
#include "interface/interp.finc"
#include "interface/permutation.finc"
#include "interface/sort.finc"
#include "interface/linalg.finc"
#include "interface/eigen.finc"
#include "interface/fft.finc"
#include "interface/integration.finc"
#include "interface/rng.finc"
#include "interface/statistics.finc"
#include "interface/histogram.finc"
#include "interface/ntuple.finc"
#include "interface/montecarlo.finc"
#include "interface/siman.finc"
#include "interface/ode.finc"
#include "interface/deriv.finc"
#include "interface/chebyshev.finc"
#include "interface/sum_levin.finc"
#include "interface/wavelet.finc"
#include "interface/dht.finc"
#include "interface/roots.finc"
#include "interface/min.finc"
#include "interface/multiroots.finc"
#include "interface/multimin.finc"
#include "interface/fit.finc"
#include "interface/multifit.finc"
#include "interface/bspline.finc"
#include "interface/ieee.finc"
  end interface
#include "interface/generics.finc"
contains
#include "api/error.finc"
#include "api/misc.finc"
#include "api/io.finc"
#include "api/math.finc"
#include "api/complex.finc"
#include "api/poly.finc"
#include "api/specfunc.finc"
#include "api/array.finc"
#include "api/interp.finc"
#include "api/permutation.finc"
#include "api/sort.finc"
#include "api/linalg.finc"
#include "api/eigen.finc"
#include "api/fft.finc"
#include "api/integration.finc"
#include "api/rng.finc"
#include "api/statistics.finc"
#include "api/histogram.finc"
#include "api/ntuple.finc"
#include "api/montecarlo.finc"
#include "api/siman.finc"
#include "api/ode.finc"
#include "api/deriv.finc"
#include "api/chebyshev.finc"
#include "api/sum_levin.finc"
#include "api/wavelet.finc"
#include "api/dht.finc"
#include "api/roots.finc"
#include "api/min.finc"
#include "api/multiroots.finc"
#include "api/multimin.finc"
#include "api/fit.finc"
#include "api/multifit.finc"
#include "api/bspline.finc"
#include "api/ieee.finc"
end module fgsl
