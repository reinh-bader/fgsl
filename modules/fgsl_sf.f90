!-*-f90-*-
module fgsl_sf
  !>  Special Functions
  !>
  !> The following modules are accessed:  
  !> - fgsl_sf_types (name pattern diverges due to naming collision): derived type definitions
  !> - fgsl_sf_airy:     Airy functions
  !> - fgsl_sf_bessel:   Bessel functions
  !> - fgsl_sf_claus (name pattern diverges due to naming collision): Clausen functions
  !> - fgsl_sf_coulomb:  Coulomb functions
  !> - fgsl_sf_daws (name pattern diverges due to naming collision): Dawson functions
  !> - fgsl_sf_debye:    Debye functions
  !> - fgsl_sf_dilogarithm: Dilogarithm
  !> - fgsl_sf_elementary: Elementary Operations
  !> - fgsl_sf_ellint:   Elliptic Integrals
  !> - fgsl_sf_elljac:   Elliptic Functions
  !> - fgsl_sf_errorfunc (name pattern diverges due to naming collision): Error functions
  !> - fgsl_sf_exponential: Exponential functions
  !> - fgsl_sf_expint:   Exponential Integrals
  !> - fgsl_sf_fermi_dirac: Fermi-Dirac Functions
  !> - fgsl_sf_gam (name pattern diverges due to naming collision): Gamma and Beta Functions
  !> - fgsl_sf_gegenbauer: Gegenbauer Functions
  !> - fgsl_sf_herm (name pattern diverges due to naming collision): Hermite polynomials and functions
  !> - fgsl_sf_hyperg:  Hypergeomatric Functions
  !> - fgsl_sf_laguerre: Laguerre Functions
  !> - fgsl_sf_lambert:  Lambert W Functions
  !>
  !> Functions for which two identical names would result due to LC/UC aliasing have been assigned
  !> new names. The name mappings are given in the following table. The additional letters
  !> <STRONG>c</STRONG> viz
  !> <STRONG>s</STRONG> are used to denote cylindrical and spherical Bessel functions, respectively.
  !> <TABLE>
  !> <TR><TH>      C name           <TH>              Fortran name
  !> <TR><TD>    gsl_sf_bessel_J0   <TD>             fgsl_sf_bessel_jc0
  !> <TR><TD>    gsl_sf_bessel_J0_e <TD>             fgsl_sf_bessel_jc0_e
  !> <TR><TD>    gsl_sf_bessel_J1   <TD>             fgsl_sf_bessel_jc1
  !> <TR><TD>    gsl_sf_bessel_J1_e  <TD>            fgsl_sf_bessel_jc1_e
  !> <TR><TD>    gsl_sf_bessel_Jn    <TD>            fgsl_sf_bessel_jcn
  !> <TR><TD>    gsl_sf_bessel_Jn_e  <TD>            fgsl_sf_bessel_jcn_e
  !> <TR><TD>    gsl_sf_bessel_Jn_array <TD>         fgsl_sf_bessel_jcn_array
  !> <TR><TD>    gsl_sf_bessel_Y0    <TD>            fgsl_sf_bessel_yc0
  !> <TR><TD>    gsl_sf_bessel_Y0_e  <TD>            fgsl_sf_bessel_yc0_e
  !> <TR><TD>    gsl_sf_bessel_Y1    <TD>            fgsl_sf_bessel_yc1
  !> <TR><TD>    gsl_sf_bessel_Y1_e  <TD>            fgsl_sf_bessel_yc1_e
  !> <TR><TD>    gsl_sf_bessel_Yn    <TD>            fgsl_sf_bessel_ycn
  !> <TR><TD>    gsl_sf_bessel_Yn_e  <TD>            fgsl_sf_bessel_ycn_e
  !> <TR><TD>    gsl_sf_bessel_Yn_array <TD>         fgsl_sf_bessel_ycn_array
  !> <TR><TD>    gsl_sf_bessel_I0    <TD>            fgsl_sf_bessel_ic0
  !> <TR><TD>    gsl_sf_bessel_I0_e  <TD>            fgsl_sf_bessel_ic0_e
  !> <TR><TD>    gsl_sf_bessel_I1    <TD>            fgsl_sf_bessel_ic1
  !> <TR><TD>    gsl_sf_bessel_I1_e  <TD>            fgsl_sf_bessel_ic1_e
  !> <TR><TD>    gsl_sf_bessel_In    <TD>            fgsl_sf_bessel_icn
  !> <TR><TD>    gsl_sf_bessel_In_e  <TD>            fgsl_sf_bessel_icn_e
  !> <TR><TD>    gsl_sf_bessel_In_array  <TD>        fgsl_sf_bessel_icn_array
  !> <TR><TD>    gsl_sf_bessel_I0_scaled <TD>        fgsl_sf_bessel_ic0_scaled
  !> <TR><TD>    gsl_sf_bessel_I0_scaled_e <TD>      fgsl_sf_bessel_ic0_scaled_e
  !> <TR><TD>    gsl_sf_bessel_I1_scaled   <TD>      fgsl_sf_bessel_ic1_scaled
  !> <TR><TD>    gsl_sf_bessel_I1_scaled_e <TD>      fgsl_sf_bessel_ic1_scaled_e
  !> <TR><TD>    gsl_sf_bessel_In_scaled   <TD>      fgsl_sf_bessel_icn_scaled
  !> <TR><TD>    gsl_sf_bessel_In_scaled_e <TD>      fgsl_sf_bessel_icn_scaled_e
  !> <TR><TD>    gsl_sf_bessel_In_scaled_array <TD>  fgsl_sf_bessel_icn_scaled_array
  !> <TR><TD>    gsl_sf_bessel_K0     <TD>           fgsl_sf_bessel_kc0
  !> <TR><TD>    gsl_sf_bessel_K0_e   <TD>           fgsl_sf_bessel_kc0_e
  !> <TR><TD>    gsl_sf_bessel_K1     <TD>           fgsl_sf_bessel_kc1
  !> <TR><TD>    gsl_sf_bessel_K1_e   <TD>           fgsl_sf_bessel_kc1_e
  !> <TR><TD>    gsl_sf_bessel_Kn     <TD>           fgsl_sf_bessel_kcn
  !> <TR><TD>    gsl_sf_bessel_Kn_e   <TD>           fgsl_sf_bessel_kcn_e
  !> <TR><TD>    gsl_sf_bessel_Kn_array   <TD>       fgsl_sf_bessel_kcn_array
  !> <TR><TD>    gsl_sf_bessel_K0_scaled  <TD>       fgsl_sf_bessel_kc0_scaled
  !> <TR><TD>    gsl_sf_bessel_K0_scaled_e  <TD>     fgsl_sf_bessel_kc0_scaled_e
  !> <TR><TD>    gsl_sf_bessel_K1_scaled    <TD>     fgsl_sf_bessel_kc1_scaled
  !> <TR><TD>    gsl_sf_bessel_K1_scaled_e  <TD>     fgsl_sf_bessel_kc1_scaled_e
  !> <TR><TD>    gsl_sf_bessel_Kn_scaled    <TD>     fgsl_sf_bessel_kcn_scaled
  !> <TR><TD>    gsl_sf_bessel_Kn_scaled_e  <TD>     fgsl_sf_bessel_kcn_scaled_e
  !> <TR><TD>    gsl_sf_bessel_Kn_scaled_array  <TD> fgsl_sf_bessel_kcn_scaled_array
  !> <TR><TD>    gsl_sf_bessel_j0      <TD>          fgsl_sf_bessel_js0
  !> <TR><TD>    gsl_sf_bessel_j0_e    <TD>          fgsl_sf_bessel_js0_e
  !> <TR><TD>    gsl_sf_bessel_j1      <TD>          fgsl_sf_bessel_js1
  !> <TR><TD>    gsl_sf_bessel_j1_e    <TD>          fgsl_sf_bessel_js1_e
  !> <TR><TD>    gsl_sf_bessel_j2      <TD>          fgsl_sf_bessel_js2
  !> <TR><TD>    gsl_sf_bessel_j2_e     <TD>         fgsl_sf_bessel_js2_e
  !> <TR><TD>    gsl_sf_bessel_jl      <TD>          fgsl_sf_bessel_jsl
  !> <TR><TD>    gsl_sf_bessel_jl_e    <TD>          fgsl_sf_bessel_jsl_e
  !> <TR><TD>    gsl_sf_bessel_jl_array  <TD>        fgsl_sf_bessel_jsl_array
  !> <TR><TD>    gsl_sf_bessel_jl_steed_array <TD>   fgsl_sf_bessel_jsl_steed_array
  !> <TR><TD>    gsl_sf_bessel_y0      <TD>          fgsl_sf_bessel_ys0
  !> <TR><TD>    gsl_sf_bessel_y0_e    <TD>          fgsl_sf_bessel_ys0_e
  !> <TR><TD>    gsl_sf_bessel_y1      <TD>          fgsl_sf_bessel_ys1
  !> <TR><TD>    gsl_sf_bessel_y1_e    <TD>          fgsl_sf_bessel_ys1_e
  !> <TR><TD>    gsl_sf_bessel_y2      <TD>          fgsl_sf_bessel_ys2
  !> <TR><TD>    gsl_sf_bessel_y2_e     <TD>         fgsl_sf_bessel_ys2_e
  !> <TR><TD>    gsl_sf_bessel_yl      <TD>          fgsl_sf_bessel_ysl
  !> <TR><TD>    gsl_sf_bessel_yl_e      <TD>        fgsl_sf_bessel_ysl_e
  !> <TR><TD>    gsl_sf_bessel_yl_array   <TD>       fgsl_sf_bessel_ysl_array
  !> <TR><TD>    gsl_sf_bessel_i0_scaled  <TD>       fgsl_sf_bessel_is0_scaled
  !> <TR><TD>    gsl_sf_bessel_i0_scaled_e <TD>      fgsl_sf_bessel_is0_scaled_e
  !> <TR><TD>    gsl_sf_bessel_i1_scaled   <TD>      fgsl_sf_bessel_is1_scaled
  !> <TR><TD>    gsl_sf_bessel_i1_scaled_e <TD>      fgsl_sf_bessel_is1_scaled_e
  !> <TR><TD>    gsl_sf_bessel_i2_scaled   <TD>      fgsl_sf_bessel_is2_scaled
  !> <TR><TD>    gsl_sf_bessel_i2_scaled_e <TD>      fgsl_sf_bessel_is2_scaled_e
  !> <TR><TD>    gsl_sf_bessel_il_scaled   <TD>      fgsl_sf_bessel_isl_scaled
  !> <TR><TD>    gsl_sf_bessel_il_scaled_e <TD>      fgsl_sf_bessel_isl_scaled_e
  !> <TR><TD>    gsl_sf_bessel_il_scaled_array <TD>  fgsl_sf_bessel_isl_scaled_array
  !> <TR><TD>    gsl_sf_bessel_k0_scaled   <TD>      fgsl_sf_bessel_ks0_scaled
  !> <TR><TD>    gsl_sf_bessel_k0_scaled_e <TD>      fgsl_sf_bessel_ks0_scaled_e
  !> <TR><TD>    gsl_sf_bessel_k1_scaled   <TD>      fgsl_sf_bessel_ks1_scaled
  !> <TR><TD>    gsl_sf_bessel_k1_scaled_e <TD>      fgsl_sf_bessel_ks1_scaled_e
  !> <TR><TD>    gsl_sf_bessel_k2_scaled   <TD>      fgsl_sf_bessel_ks2_scaled
  !> <TR><TD>    gsl_sf_bessel_k2_scaled_e  <TD>     fgsl_sf_bessel_ks2_scaled_e
  !> <TR><TD>    gsl_sf_bessel_kl_scaled    <TD>     fgsl_sf_bessel_ksl_scaled
  !> <TR><TD>    gsl_sf_bessel_kl_scaled_e  <TD>     fgsl_sf_bessel_ksl_scaled_e
  !> <TR><TD>    gsl_sf_bessel_kl_scaled_array <TD>  fgsl_sf_bessel_ksl_scaled_array
  !> <TR><TD>    gsl_sf_bessel_zero_J0      <TD>     fgsl_sf_bessel_zero_jc0
  !> <TR><TD>    gsl_sf_bessel_zero_J0_e    <TD>     fgsl_sf_bessel_zero_jc0_e
  !> <TR><TD>    gsl_sf_bessel_zero_J1      <TD>     fgsl_sf_bessel_zero_jc1
  !> <TR><TD>    gsl_sf_bessel_zero_J1_e    <TD>     fgsl_sf_bessel_zero_jc1_e
  !> <TR><TD>    gsl_sf_bessel_zero_Jnu     <TD>     fgsl_sf_bessel_zero_jcnu
  !> <TR><TD>   gsl_sf_bessel_zero_Jnu_e   <TD>     fgsl_sf_bessel_zero_jcnu_e
  !> </TABLE>

  use fgsl_sf_types
  use fgsl_sf_airy
  use fgsl_sf_bessel
  use fgsl_sf_claus
  use fgsl_sf_coulomb
  use fgsl_sf_coupling
  use fgsl_sf_daws
  use fgsl_sf_debye
  use fgsl_sf_dilogarithm
  use fgsl_sf_elementary
  use fgsl_sf_ellint
  use fgsl_sf_elljac
  use fgsl_sf_errorfunc
  use fgsl_sf_exponential
  use fgsl_sf_expint
  use fgsl_sf_fermi_dirac
  use fgsl_sf_gam
  use fgsl_sf_gegenbauer
  use fgsl_sf_herm
  use fgsl_sf_hyperg
  use fgsl_sf_laguerre
  use fgsl_sf_lambert
  implicit none
end module fgsl_sf
