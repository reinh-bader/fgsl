!-*-f90-*-
module fgsl_sf_bessel
  !>  Special functions - Bessel functions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_bessel_jc0_e, gsl_sf_bessel_jc1_e, gsl_sf_bessel_jcn_e, &
       gsl_sf_bessel_yc0_e, gsl_sf_bessel_yc1_e, &
       gsl_sf_bessel_ycn_e, gsl_sf_bessel_ic0_e, gsl_sf_bessel_ic1_e, &
       gsl_sf_bessel_icn_e, gsl_sf_bessel_ic0_scaled_e, gsl_sf_bessel_ic1_scaled_e, &
       gsl_sf_bessel_icn_scaled_e, gsl_sf_bessel_kc0_e, gsl_sf_bessel_kc1_e, &
       gsl_sf_bessel_kcn_e, gsl_sf_bessel_kc0_scaled_e,  gsl_sf_bessel_kc1_scaled_e, &
       gsl_sf_bessel_kcn_scaled_e, gsl_sf_bessel_js0_e, gsl_sf_bessel_js1_e, &
       gsl_sf_bessel_js2_e, gsl_sf_bessel_jsl_e, gsl_sf_bessel_ys0_e, &
       gsl_sf_bessel_ys1_e, gsl_sf_bessel_ys2_e, gsl_sf_bessel_ysl_e, &
       gsl_sf_bessel_is0_scaled_e, gsl_sf_bessel_is1_scaled_e, &
       gsl_sf_bessel_is2_scaled_e, gsl_sf_bessel_isl_scaled_e, &
       gsl_sf_bessel_ks0_scaled_e, gsl_sf_bessel_ks1_scaled_e, &
       gsl_sf_bessel_ks2_scaled_e, gsl_sf_bessel_ksl_scaled_e, &
       gsl_sf_bessel_jnu_e, gsl_sf_bessel_sequence_jnu_e, gsl_sf_bessel_ynu_e, &
       gsl_sf_bessel_inu_e, gsl_sf_bessel_inu_scaled_e, gsl_sf_bessel_knu_e, &
       gsl_sf_bessel_lnknu_e, gsl_sf_bessel_knu_scaled_e, gsl_sf_bessel_zero_jc0_e, &
       gsl_sf_bessel_zero_jc1_e, gsl_sf_bessel_zero_jnu_e

  !>
  !> C interfaces
  interface
     function fgsl_sf_bessel_jc0(x) bind(c, name='gsl_sf_bessel_J0')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_jc0
     end function fgsl_sf_bessel_jc0
     function gsl_sf_bessel_jc0_e(x, result) bind(c, name='gsl_sf_bessel_J0_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_jc0_e
     end function gsl_sf_bessel_jc0_e
     function fgsl_sf_bessel_jc1(x) bind(c, name='gsl_sf_bessel_J1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_jc1
     end function fgsl_sf_bessel_jc1
     function gsl_sf_bessel_jc1_e(x, result) bind(c, name='gsl_sf_bessel_J1_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_jc1_e
     end function gsl_sf_bessel_jc1_e
     function fgsl_sf_bessel_jcn(n,x) bind(c, name='gsl_sf_bessel_Jn')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_jcn
     end function fgsl_sf_bessel_jcn
     function gsl_sf_bessel_jcn_e(n, x, result) bind(c, name='gsl_sf_bessel_Jn_e')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_jcn_e
     end function gsl_sf_bessel_jcn_e
     function fgsl_sf_bessel_jcn_array(nmin, nmax, x, result) bind(c, name='gsl_sf_bessel_Jn_array')
       import
       integer(c_int), value :: nmin, nmax
       real(c_double), value :: x
       real(c_double), dimension(*), intent(inout) :: result
       integer(c_int) :: fgsl_sf_bessel_jcn_array
     end function fgsl_sf_bessel_jcn_array
     function fgsl_sf_bessel_yc0(x) bind(c, name='gsl_sf_bessel_Y0')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_yc0
     end function fgsl_sf_bessel_yc0
     function gsl_sf_bessel_yc0_e(x, result) bind(c, name='gsl_sf_bessel_Y0_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_yc0_e
     end function gsl_sf_bessel_yc0_e
     function fgsl_sf_bessel_yc1(x) bind(c, name='gsl_sf_bessel_Y1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_yc1
     end function fgsl_sf_bessel_yc1
     function gsl_sf_bessel_yc1_e(x, result) bind(c, name='gsl_sf_bessel_Y1_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_yc1_e
     end function gsl_sf_bessel_yc1_e
     function fgsl_sf_bessel_ycn(n,x) bind(c, name='gsl_sf_bessel_Yn')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_ycn
     end function fgsl_sf_bessel_ycn
     function gsl_sf_bessel_ycn_e(n, x, result) bind(c, name='gsl_sf_bessel_Yn_e')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_ycn_e
     end function gsl_sf_bessel_ycn_e
     function fgsl_sf_bessel_ycn_array(nmin, nmax, x, result) bind(c, name='gsl_sf_bessel_Yn_array')
       import
       integer(c_int), value :: nmin, nmax
       real(c_double), value :: x
       real(c_double), dimension(*), intent(inout) :: result
       integer(c_int) :: fgsl_sf_bessel_ycn_array
     end function fgsl_sf_bessel_ycn_array
     function fgsl_sf_bessel_ic0(x) bind(c, name='gsl_sf_bessel_I0')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_ic0
     end function fgsl_sf_bessel_ic0
     function gsl_sf_bessel_ic0_e(x, result) bind(c, name='gsl_sf_bessel_I0_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_ic0_e
     end function gsl_sf_bessel_ic0_e
     function fgsl_sf_bessel_ic1(x) bind(c, name='gsl_sf_bessel_I1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_ic1
     end function fgsl_sf_bessel_ic1
     function gsl_sf_bessel_ic1_e(x, result) bind(c, name='gsl_sf_bessel_I1_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_ic1_e
     end function gsl_sf_bessel_ic1_e
     function fgsl_sf_bessel_icn(n,x) bind(c, name='gsl_sf_bessel_In')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_icn
     end function fgsl_sf_bessel_icn
     function gsl_sf_bessel_icn_e(n, x, result) bind(c, name='gsl_sf_bessel_In_e')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_icn_e
     end function gsl_sf_bessel_icn_e
     function fgsl_sf_bessel_icn_array(nmin, nmax, x, result) bind(c, name='gsl_sf_bessel_In_array')
       import
       integer(c_int), value :: nmin, nmax
       real(c_double), value :: x
       real(c_double), dimension(*), intent(inout) :: result
       integer(c_int) :: fgsl_sf_bessel_icn_array
     end function fgsl_sf_bessel_icn_array
     function fgsl_sf_bessel_ic0_scaled(x) bind(c, name='gsl_sf_bessel_I0_scaled')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_ic0_scaled
     end function fgsl_sf_bessel_ic0_scaled
     function gsl_sf_bessel_ic0_scaled_e(x, result) bind(c, name='gsl_sf_bessel_I0_scaled_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_ic0_scaled_e
     end function gsl_sf_bessel_ic0_scaled_e
     function fgsl_sf_bessel_ic1_scaled(x) bind(c, name='gsl_sf_bessel_I1_scaled')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_ic1_scaled
     end function fgsl_sf_bessel_ic1_scaled
     function gsl_sf_bessel_ic1_scaled_e(x, result) bind(c, name='gsl_sf_bessel_I1_scaled_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_ic1_scaled_e
     end function gsl_sf_bessel_ic1_scaled_e
     function fgsl_sf_bessel_icn_scaled(n,x) bind(c, name='gsl_sf_bessel_In_scaled')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_icn_scaled
     end function fgsl_sf_bessel_icn_scaled
     function gsl_sf_bessel_icn_scaled_e(n, x, result) bind(c, name='gsl_sf_bessel_In_scaled_e')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_icn_scaled_e
     end function gsl_sf_bessel_icn_scaled_e
     function fgsl_sf_bessel_icn_scaled_array(nmin, nmax, x, result) bind(c, name='gsl_sf_bessel_In_scaled_array')
       import
       integer(c_int), value :: nmin, nmax
       real(c_double), value :: x
       real(c_double), dimension(*), intent(inout) :: result
       integer(c_int) :: fgsl_sf_bessel_icn_scaled_array
     end function fgsl_sf_bessel_icn_scaled_array
     function fgsl_sf_bessel_kc0(x) bind(c, name='gsl_sf_bessel_K0')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_kc0
     end function fgsl_sf_bessel_kc0
     function gsl_sf_bessel_kc0_e(x, result) bind(c, name='gsl_sf_bessel_K0_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_kc0_e
     end function gsl_sf_bessel_kc0_e
     function fgsl_sf_bessel_kc1(x) bind(c, name='gsl_sf_bessel_K1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_kc1
     end function fgsl_sf_bessel_kc1
     function gsl_sf_bessel_kc1_e(x, result) bind(c, name='gsl_sf_bessel_K1_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_kc1_e
     end function gsl_sf_bessel_kc1_e
     function fgsl_sf_bessel_kcn(n,x) bind(c, name='gsl_sf_bessel_Kn')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_kcn
     end function fgsl_sf_bessel_kcn
     function gsl_sf_bessel_kcn_e(n, x, result) bind(c, name='gsl_sf_bessel_Kn_e')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_kcn_e
     end function gsl_sf_bessel_kcn_e
     function fgsl_sf_bessel_kcn_array(nmin, nmax, x, result) bind(c, name='gsl_sf_bessel_Kn_array')
       import
       integer(c_int), value :: nmin, nmax
       real(c_double), value :: x
       real(c_double), dimension(*), intent(inout) :: result
       integer(c_int) :: fgsl_sf_bessel_kcn_array
     end function fgsl_sf_bessel_kcn_array
     function fgsl_sf_bessel_kc0_scaled(x) bind(c, name='gsl_sf_bessel_K0_scaled')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_kc0_scaled
     end function fgsl_sf_bessel_kc0_scaled
     function gsl_sf_bessel_kc0_scaled_e(x, result) bind(c, name='gsl_sf_bessel_K0_scaled_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_kc0_scaled_e
     end function gsl_sf_bessel_kc0_scaled_e
     function fgsl_sf_bessel_kc1_scaled(x) bind(c, name='gsl_sf_bessel_K1_scaled')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_kc1_scaled
     end function fgsl_sf_bessel_kc1_scaled
     function gsl_sf_bessel_kc1_scaled_e(x, result) bind(c, name='gsl_sf_bessel_K1_scaled_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_kc1_scaled_e
     end function gsl_sf_bessel_kc1_scaled_e
     function fgsl_sf_bessel_kcn_scaled(n,x) bind(c, name='gsl_sf_bessel_Kn_scaled')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_kcn_scaled
     end function fgsl_sf_bessel_kcn_scaled
     function gsl_sf_bessel_kcn_scaled_e(n, x, result) bind(c, name='gsl_sf_bessel_Kn_scaled_e')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_kcn_scaled_e
     end function gsl_sf_bessel_kcn_scaled_e
     function fgsl_sf_bessel_kcn_scaled_array(nmin, nmax, x, result) bind(c, name='gsl_sf_bessel_Kn_scaled_array')
       import
       integer(c_int), value :: nmin, nmax
       real(c_double), value :: x
       real(c_double), dimension(*), intent(inout) :: result
       integer(c_int) :: fgsl_sf_bessel_kcn_scaled_array
     end function fgsl_sf_bessel_kcn_scaled_array
!    spherical bessel functions
     function fgsl_sf_bessel_js0(x) bind(c, name='gsl_sf_bessel_j0')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_js0
     end function fgsl_sf_bessel_js0
     function gsl_sf_bessel_js0_e(x, result) bind(c, name='gsl_sf_bessel_j0_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_js0_e
     end function gsl_sf_bessel_js0_e
     function fgsl_sf_bessel_js1(x) bind(c, name='gsl_sf_bessel_j1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_js1
     end function fgsl_sf_bessel_js1
     function gsl_sf_bessel_js1_e(x, result) bind(c, name='gsl_sf_bessel_j1_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_js1_e
     end function gsl_sf_bessel_js1_e
     function fgsl_sf_bessel_js2(x) bind(c, name='gsl_sf_bessel_j2')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_js2
     end function fgsl_sf_bessel_js2
     function gsl_sf_bessel_js2_e(x, result) bind(c, name='gsl_sf_bessel_j2_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_js2_e
     end function gsl_sf_bessel_js2_e
     function fgsl_sf_bessel_jsl(n,x) bind(c, name='gsl_sf_bessel_jl')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_jsl
     end function fgsl_sf_bessel_jsl
     function gsl_sf_bessel_jsl_e(n, x, result) bind(c, name='gsl_sf_bessel_jl_e')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_jsl_e
     end function gsl_sf_bessel_jsl_e
     function fgsl_sf_bessel_jsl_array(lmax, x, result) bind(c, name='gsl_sf_bessel_jl_array')
       import
       integer(c_int), value :: lmax
       real(c_double), value :: x
       real(c_double), dimension(*), intent(inout) :: result
       integer(c_int) :: fgsl_sf_bessel_jsl_array
     end function fgsl_sf_bessel_jsl_array
     function fgsl_sf_bessel_jsl_steed_array(lmax, x, result) bind(c, name='gsl_sf_bessel_jl_steed_array')
       import
       integer(c_int), value :: lmax
       real(c_double), value :: x
       real(c_double), dimension(*), intent(inout) :: result
       integer(c_int) :: fgsl_sf_bessel_jsl_steed_array
     end function fgsl_sf_bessel_jsl_steed_array
     function fgsl_sf_bessel_ys0(x) bind(c, name='gsl_sf_bessel_y0')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_ys0
     end function fgsl_sf_bessel_ys0
     function gsl_sf_bessel_ys0_e(x, result) bind(c, name='gsl_sf_bessel_y0_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_ys0_e
     end function gsl_sf_bessel_ys0_e
     function fgsl_sf_bessel_ys1(x) bind(c, name='gsl_sf_bessel_y1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_ys1
     end function fgsl_sf_bessel_ys1
     function gsl_sf_bessel_ys1_e(x, result) bind(c, name='gsl_sf_bessel_y1_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_ys1_e
     end function gsl_sf_bessel_ys1_e
     function fgsl_sf_bessel_ys2(x) bind(c, name='gsl_sf_bessel_y2')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_ys2
     end function fgsl_sf_bessel_ys2
     function gsl_sf_bessel_ys2_e(x, result) bind(c, name='gsl_sf_bessel_y2_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_ys2_e
     end function gsl_sf_bessel_ys2_e
     function fgsl_sf_bessel_ysl(n,x) bind(c, name='gsl_sf_bessel_yl')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_ysl
     end function fgsl_sf_bessel_ysl
     function gsl_sf_bessel_ysl_e(n, x, result) bind(c, name='gsl_sf_bessel_yl_e')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_ysl_e
     end function gsl_sf_bessel_ysl_e
     function fgsl_sf_bessel_ysl_array(lmax, x, result) bind(c, name='gsl_sf_bessel_yl_array')
       import
       integer(c_int), value :: lmax
       real(c_double), value :: x
       real(c_double), dimension(*), intent(inout) :: result
       integer(c_int) :: fgsl_sf_bessel_ysl_array
     end function fgsl_sf_bessel_ysl_array
     function fgsl_sf_bessel_is0_scaled(x) bind(c, name='gsl_sf_bessel_i0_scaled')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_is0_scaled
     end function fgsl_sf_bessel_is0_scaled
     function gsl_sf_bessel_is0_scaled_e(x, result) bind(c, name='gsl_sf_bessel_i0_scaled_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_is0_scaled_e
     end function gsl_sf_bessel_is0_scaled_e
     function fgsl_sf_bessel_is1_scaled(x) bind(c, name='gsl_sf_bessel_i1_scaled')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_is1_scaled
     end function fgsl_sf_bessel_is1_scaled
     function gsl_sf_bessel_is1_scaled_e(x, result) bind(c, name='gsl_sf_bessel_i1_scaled_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_is1_scaled_e
     end function gsl_sf_bessel_is1_scaled_e
     function fgsl_sf_bessel_is2_scaled(x) bind(c, name='gsl_sf_bessel_i2_scaled')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_is2_scaled
     end function fgsl_sf_bessel_is2_scaled
     function gsl_sf_bessel_is2_scaled_e(x, result) bind(c, name='gsl_sf_bessel_i2_scaled_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_is2_scaled_e
     end function gsl_sf_bessel_is2_scaled_e
     function fgsl_sf_bessel_isl_scaled(n,x) bind(c, name='gsl_sf_bessel_il_scaled')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_isl_scaled
     end function fgsl_sf_bessel_isl_scaled
     function gsl_sf_bessel_isl_scaled_e(n, x, result) bind(c, name='gsl_sf_bessel_il_scaled_e')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_isl_scaled_e
     end function gsl_sf_bessel_isl_scaled_e
     function fgsl_sf_bessel_isl_scaled_array(lmax, x, result) bind(c, name='gsl_sf_bessel_il_scaled_array')
       import
       integer(c_int), value :: lmax
       real(c_double), value :: x
       real(c_double), dimension(*), intent(in) :: result
       integer(c_int) :: fgsl_sf_bessel_isl_scaled_array
     end function fgsl_sf_bessel_isl_scaled_array
     function fgsl_sf_bessel_ks0_scaled(x) bind(c, name='gsl_sf_bessel_k0_scaled')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_ks0_scaled
     end function fgsl_sf_bessel_ks0_scaled
     function gsl_sf_bessel_ks0_scaled_e(x, result) bind(c, name='gsl_sf_bessel_k0_scaled_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_ks0_scaled_e
     end function gsl_sf_bessel_ks0_scaled_e
     function fgsl_sf_bessel_ks1_scaled(x) bind(c, name='gsl_sf_bessel_k1_scaled')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_ks1_scaled
     end function fgsl_sf_bessel_ks1_scaled
     function gsl_sf_bessel_ks1_scaled_e(x, result) bind(c, name='gsl_sf_bessel_k1_scaled_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_ks1_scaled_e
     end function gsl_sf_bessel_ks1_scaled_e
     function fgsl_sf_bessel_ks2_scaled(x) bind(c, name='gsl_sf_bessel_k2_scaled')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_ks2_scaled
     end function fgsl_sf_bessel_ks2_scaled
     function gsl_sf_bessel_ks2_scaled_e(x, result) bind(c, name='gsl_sf_bessel_k2_scaled_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_ks2_scaled_e
     end function gsl_sf_bessel_ks2_scaled_e
     function fgsl_sf_bessel_ksl_scaled(n,x) bind(c, name='gsl_sf_bessel_kl_scaled')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_ksl_scaled
     end function fgsl_sf_bessel_ksl_scaled
     function gsl_sf_bessel_ksl_scaled_e(n, x, result) bind(c, name='gsl_sf_bessel_kl_scaled_e')
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_ksl_scaled_e
     end function gsl_sf_bessel_ksl_scaled_e
     function fgsl_sf_bessel_ksl_scaled_array(lmax, x, result) bind(c, name='gsl_sf_bessel_kl_scaled_array')
       import
       integer(c_int), value :: lmax
       real(c_double), value :: x
       real(c_double), dimension(*), intent(inout) :: result
       integer(c_int) :: fgsl_sf_bessel_ksl_scaled_array
     end function fgsl_sf_bessel_ksl_scaled_array
!  fractional order bessel functions
     function fgsl_sf_bessel_jnu(n,x) bind(c, name='gsl_sf_bessel_Jnu')
       import
       real(c_double), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_jnu
     end function fgsl_sf_bessel_jnu
     function gsl_sf_bessel_jnu_e(n, x, result) bind(c, name='gsl_sf_bessel_Jnu_e')
       import
       real(c_double), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_jnu_e
     end function gsl_sf_bessel_jnu_e
     function gsl_sf_bessel_sequence_jnu_e(nu, mode, size, v) bind(c, name='gsl_sf_bessel_sequence_Jnu_e')
       import
       real(c_double), value :: nu
       integer(c_int), value :: mode
       integer(c_size_t), value :: size
       type(c_ptr), value :: v
       integer(c_int) :: gsl_sf_bessel_sequence_jnu_e
     end function gsl_sf_bessel_sequence_jnu_e
     function fgsl_sf_bessel_ynu(n,x) bind(c, name='gsl_sf_bessel_Ynu')
       import
       real(c_double), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_ynu
     end function fgsl_sf_bessel_ynu
     function gsl_sf_bessel_ynu_e(n, x, result) bind(c, name='gsl_sf_bessel_Ynu_e')
       import
       real(c_double), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_ynu_e
     end function gsl_sf_bessel_ynu_e
     function fgsl_sf_bessel_inu(n,x) bind(c, name='gsl_sf_bessel_Inu')
       import
       real(c_double), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_inu
     end function fgsl_sf_bessel_inu
     function gsl_sf_bessel_inu_e(n, x, result) bind(c, name='gsl_sf_bessel_Inu_e')
       import
       real(c_double), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_inu_e
     end function gsl_sf_bessel_inu_e
     function fgsl_sf_bessel_inu_scaled(n,x) bind(c, name='gsl_sf_bessel_Inu_scaled')
       import
       real(c_double), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_inu_scaled
     end function fgsl_sf_bessel_inu_scaled
     function gsl_sf_bessel_inu_scaled_e(n, x, result) bind(c, name='gsl_sf_bessel_Inu_scaled_e')
       import
       real(c_double), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_inu_scaled_e
     end function gsl_sf_bessel_inu_scaled_e
     function fgsl_sf_bessel_knu(n,x) bind(c, name='gsl_sf_bessel_Knu')
       import
       real(c_double), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_knu
     end function fgsl_sf_bessel_knu
     function gsl_sf_bessel_knu_e(n, x, result) bind(c, name='gsl_sf_bessel_Knu_e')
       import
       real(c_double), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_knu_e
     end function gsl_sf_bessel_knu_e
     function fgsl_sf_bessel_lnknu(n,x) bind(c, name='gsl_sf_bessel_lnKnu')
       import
       real(c_double), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_lnknu
     end function fgsl_sf_bessel_lnknu
     function gsl_sf_bessel_lnknu_e(n, x, result) bind(c, name='gsl_sf_bessel_lnKnu_e')
       import
       real(c_double), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_lnknu_e
     end function gsl_sf_bessel_lnknu_e
     function fgsl_sf_bessel_knu_scaled(n,x) bind(c, name='gsl_sf_bessel_Knu_scaled')
       import
       real(c_double), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_bessel_knu_scaled
     end function fgsl_sf_bessel_knu_scaled
     function gsl_sf_bessel_knu_scaled_e(n, x, result) bind(c, name='gsl_sf_bessel_Knu_scaled_e')
       import
       real(c_double), value :: n
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_knu_scaled_e
     end function gsl_sf_bessel_knu_scaled_e
     function fgsl_sf_bessel_zero_jc0 (s) bind(c, name='gsl_sf_bessel_zero_J0')
       import
       integer(c_int), value :: s
       real(c_double) :: fgsl_sf_bessel_zero_jc0
     end function fgsl_sf_bessel_zero_jc0
     function gsl_sf_bessel_zero_jc0_e (s, result) bind(c, name='gsl_sf_bessel_zero_J0_e')
       import
       integer(c_int), value :: s
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_zero_jc0_e
     end function gsl_sf_bessel_zero_jc0_e
     function fgsl_sf_bessel_zero_jc1 (s) bind(c, name='gsl_sf_bessel_zero_J1')
       import
       integer(c_int), value :: s
       real(c_double) :: fgsl_sf_bessel_zero_jc1
     end function fgsl_sf_bessel_zero_jc1
     function gsl_sf_bessel_zero_jc1_e (s, result) bind(c, name='gsl_sf_bessel_zero_J1_e')
       import
       integer(c_int), value :: s
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_zero_jc1_e
     end function gsl_sf_bessel_zero_jc1_e
     function fgsl_sf_bessel_zero_jnu(nu, s) bind(c, name='gsl_sf_bessel_zero_Jnu')
       import
       real(c_double), value :: nu
       integer(c_int), value :: s
       real(c_double) :: fgsl_sf_bessel_zero_jnu
     end function fgsl_sf_bessel_zero_jnu
     function gsl_sf_bessel_zero_jnu_e (nu, s, result) bind(c, name='gsl_sf_bessel_zero_Jnu_e')
       import
       real(c_double), value :: nu
       integer(c_int), value :: s
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_bessel_zero_jnu_e
     end function gsl_sf_bessel_zero_jnu_e
  end interface
contains
  !> API
  ! fgsl_sf_bessel_jc0 --> interface
  function fgsl_sf_bessel_jc0_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_jc0_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_jc0_e = gsl_sf_bessel_jc0_e(x, res)
    result = res
  end function fgsl_sf_bessel_jc0_e
! fgsl_sf_bessel_jc1 --> interface
  function fgsl_sf_bessel_jc1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_jc1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_jc1_e = gsl_sf_bessel_jc1_e(x, res)
    result = res
  end function fgsl_sf_bessel_jc1_e
! fgsl_sf_bessel_jc1 --> interface
  function fgsl_sf_bessel_jcn_e(n, x, result)
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_jcn_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_jcn_e = gsl_sf_bessel_jcn_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_jcn_e
! fgsl_sf_bessel_jcn_array --> interface
! fgsl_sf_bessel_yc0 --> interface
  function fgsl_sf_bessel_yc0_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_yc0_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_yc0_e = gsl_sf_bessel_yc0_e(x, res)
    result = res
  end function fgsl_sf_bessel_yc0_e
! fgsl_sf_bessel_yc1 --> interface
  function fgsl_sf_bessel_yc1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_yc1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_yc1_e = gsl_sf_bessel_yc1_e(x, res)
    result = res
  end function fgsl_sf_bessel_yc1_e
!  fgsl_sf_bessel_ycn --> interface
  function fgsl_sf_bessel_ycn_e(n, x, result)
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_ycn_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_ycn_e = gsl_sf_bessel_ycn_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_ycn_e
!  fgsl_sf_bessel_ycn_array --> interface
!  fgsl_sf_bessel_ic0 --> interface
  function fgsl_sf_bessel_ic0_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_ic0_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_ic0_e = gsl_sf_bessel_ic0_e(x, res)
    result = res
  end function fgsl_sf_bessel_ic0_e
!  fgsl_sf_bessel_ic1 --> interface
  function fgsl_sf_bessel_ic1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_ic1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_ic1_e = gsl_sf_bessel_ic1_e(x, res)
    result = res
  end function fgsl_sf_bessel_ic1_e
!  fgsl_sf_bessel_icn --> interface
  function fgsl_sf_bessel_icn_e(n, x, result)
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_icn_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_icn_e = gsl_sf_bessel_icn_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_icn_e
!  fgsl_sf_bessel_icn_array --> interface
!  fgsl_sf_bessel_ic0_scaled --> interface
  function fgsl_sf_bessel_ic0_scaled_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_ic0_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_ic0_scaled_e = gsl_sf_bessel_ic0_scaled_e(x, res)
    result = res
  end function fgsl_sf_bessel_ic0_scaled_e
!  fgsl_sf_bessel_ic1_scaled --> interface
  function fgsl_sf_bessel_ic1_scaled_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_ic1_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_ic1_scaled_e = gsl_sf_bessel_ic1_scaled_e(x, res)
    result = res
  end function fgsl_sf_bessel_ic1_scaled_e
!  fgsl_sf_bessel_icn_scaled --> interface
  function fgsl_sf_bessel_icn_scaled_e(n, x, result)
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_icn_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_icn_scaled_e = gsl_sf_bessel_icn_scaled_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_icn_scaled_e
!  fgsl_sf_bessel_icn_scaled_array --> interface
!  fgsl_sf_bessel_kc0 --> interface
  function fgsl_sf_bessel_kc0_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_kc0_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_kc0_e = gsl_sf_bessel_kc0_e(x, res)
    result = res
  end function fgsl_sf_bessel_kc0_e
!  fgsl_sf_bessel_kc1 --> interface
  function fgsl_sf_bessel_kc1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_kc1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_kc1_e = gsl_sf_bessel_kc1_e(x, res)
    result = res
  end function fgsl_sf_bessel_kc1_e
!  fgsl_sf_bessel_kcn --> interface
  function fgsl_sf_bessel_kcn_e(n, x, result)
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_kcn_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_kcn_e = gsl_sf_bessel_kcn_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_kcn_e
!  fgsl_sf_bessel_kcn_array --> interface
!  fgsl_sf_bessel_kc0_scaled --> interface
  function fgsl_sf_bessel_kc0_scaled_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_kc0_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_kc0_scaled_e = gsl_sf_bessel_kc0_scaled_e(x, res)
    result = res
  end function fgsl_sf_bessel_kc0_scaled_e
!  fgsl_sf_bessel_kc1_scaled --> interface
  function fgsl_sf_bessel_kc1_scaled_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_kc1_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_kc1_scaled_e = gsl_sf_bessel_kc1_scaled_e(x, res)
    result = res
  end function fgsl_sf_bessel_kc1_scaled_e
!  fgsl_sf_bessel_kcn_scaled --> interface
  function fgsl_sf_bessel_kcn_scaled_e(n, x, result)
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_kcn_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_kcn_scaled_e = gsl_sf_bessel_kcn_scaled_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_kcn_scaled_e
!  fgsl_sf_bessel_kcn_scaled_array --> interface
!
! spherical bessel functions
!
!  fgsl_sf_bessel_js0 --> interface
  function fgsl_sf_bessel_js0_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_js0_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_js0_e = gsl_sf_bessel_js0_e(x, res)
    result = res
  end function fgsl_sf_bessel_js0_e
!  fgsl_sf_bessel_js1 --> interface
  function fgsl_sf_bessel_js1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_js1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_js1_e = gsl_sf_bessel_js1_e(x, res)
    result = res
  end function fgsl_sf_bessel_js1_e
!  fgsl_sf_bessel_js2 --> interface
  function fgsl_sf_bessel_js2_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_js2_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_js2_e = gsl_sf_bessel_js2_e(x, res)
    result = res
  end function fgsl_sf_bessel_js2_e
!   fgsl_sf_bessel_jsl --> interface
  function fgsl_sf_bessel_jsl_e(n, x, result)
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_jsl_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_jsl_e = gsl_sf_bessel_jsl_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_jsl_e
!  fgsl_sf_bessel_jsl_array --> interface
!  fgsl_sf_bessel_jsl_steed_array --> interface
!  fgsl_sf_bessel_ys0 --> interface
  function fgsl_sf_bessel_ys0_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_ys0_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_ys0_e = gsl_sf_bessel_ys0_e(x, res)
    result = res
  end function fgsl_sf_bessel_ys0_e
!  fgsl_sf_bessel_ys1 --> interface
  function fgsl_sf_bessel_ys1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_ys1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_ys1_e = gsl_sf_bessel_ys1_e(x, res)
    result = res
  end function fgsl_sf_bessel_ys1_e
!  fgsl_sf_bessel_ys2 --> interface
  function fgsl_sf_bessel_ys2_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_ys2_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_ys2_e = gsl_sf_bessel_ys2_e(x, res)
    result = res
  end function fgsl_sf_bessel_ys2_e
!  fgsl_sf_bessel_ysl --> interface
  function fgsl_sf_bessel_ysl_e(n, x, result)
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_ysl_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_ysl_e = gsl_sf_bessel_ysl_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_ysl_e
!  fgsl_sf_bessel_ysl_array --> interface
!  fgsl_sf_bessel_is0_scaled --> interface
  function fgsl_sf_bessel_is0_scaled_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_is0_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_is0_scaled_e = gsl_sf_bessel_is0_scaled_e(x, res)
    result = res
  end function fgsl_sf_bessel_is0_scaled_e
!  fgsl_sf_bessel_is1_scaled --> interface
  function fgsl_sf_bessel_is1_scaled_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_is1_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_is1_scaled_e = gsl_sf_bessel_is1_scaled_e(x, res)
    result = res
  end function fgsl_sf_bessel_is1_scaled_e
!  fgsl_sf_bessel_is2_scaled --> interface
  function fgsl_sf_bessel_is2_scaled_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_is2_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_is2_scaled_e = gsl_sf_bessel_is2_scaled_e(x, res)
    result = res
  end function fgsl_sf_bessel_is2_scaled_e
!  fgsl_sf_bessel_isl_scaled --> interface
  function fgsl_sf_bessel_isl_scaled_e(n, x, result)
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_isl_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_isl_scaled_e = gsl_sf_bessel_isl_scaled_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_isl_scaled_e
!  fgsl_sf_bessel_isl_scaled_array --> interface
!  fgsl_sf_bessel_ks0_scaled(x) --> interface
  function fgsl_sf_bessel_ks0_scaled_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_ks0_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_ks0_scaled_e = gsl_sf_bessel_ks0_scaled_e(x, res)
    result = res
  end function fgsl_sf_bessel_ks0_scaled_e
!  fgsl_sf_bessel_ks1_scaled --> interface
  function fgsl_sf_bessel_ks1_scaled_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_ks1_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_ks1_scaled_e = gsl_sf_bessel_ks1_scaled_e(x, res)
    result = res
  end function fgsl_sf_bessel_ks1_scaled_e
! fgsl_sf_bessel_ks2_scaled --> interface
  function fgsl_sf_bessel_ks2_scaled_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_ks2_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_ks2_scaled_e = gsl_sf_bessel_ks2_scaled_e(x, res)
    result = res
  end function fgsl_sf_bessel_ks2_scaled_e
!  fgsl_sf_bessel_ksl_scaled --> interface
  function fgsl_sf_bessel_ksl_scaled_e(n, x, result)
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_ksl_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_ksl_scaled_e = gsl_sf_bessel_ksl_scaled_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_ksl_scaled_e
!  fgsl_sf_bessel_ksl_scaled_array --> interface
!
! fractional order bessel functions
!
! fgsl_sf_bessel_jnu --> interface
  function fgsl_sf_bessel_jnu_e(n, x, result)
    real(fgsl_double), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_jnu_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_jnu_e = gsl_sf_bessel_jnu_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_jnu_e
  function fgsl_sf_bessel_sequence_jnu_e(nu, mode, v)
    real(fgsl_double), intent(in) :: nu
    type(fgsl_mode_t), intent(in) :: mode
    real(fgsl_double), intent(inout), contiguous, target :: v(:)
    integer(fgsl_int) :: fgsl_sf_bessel_sequence_jnu_e
    fgsl_sf_bessel_sequence_jnu_e = gsl_sf_bessel_sequence_jnu_e(nu, &
         mode%gsl_mode, size(v, kind=fgsl_size_t), c_loc(v))
  end function fgsl_sf_bessel_sequence_jnu_e
!  fgsl_sf_bessel_ynu --> interface
  function fgsl_sf_bessel_ynu_e(n, x, result)
    real(fgsl_double), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_ynu_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_ynu_e = gsl_sf_bessel_ynu_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_ynu_e
!  fgsl_sf_bessel_inu --> interface
  function fgsl_sf_bessel_inu_e(n, x, result)
    real(fgsl_double), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_inu_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_inu_e = gsl_sf_bessel_inu_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_inu_e
!  fgsl_sf_bessel_inu_scaled --> interface
  function fgsl_sf_bessel_inu_scaled_e(n, x, result)
    real(fgsl_double), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_inu_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_inu_scaled_e = gsl_sf_bessel_inu_scaled_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_inu_scaled_e
!  fgsl_sf_bessel_knu --> interface
  function fgsl_sf_bessel_knu_e(n, x, result)
    real(fgsl_double), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_knu_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_knu_e = gsl_sf_bessel_knu_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_knu_e
!  fgsl_sf_bessel_lnknu --> interface
  function fgsl_sf_bessel_lnknu_e(n, x, result)
    real(fgsl_double), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_lnknu_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_lnknu_e = gsl_sf_bessel_lnknu_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_lnknu_e
!  fgsl_sf_bessel_knu_scaled --> interface
  function fgsl_sf_bessel_knu_scaled_e(n, x, result)
    real(fgsl_double), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_knu_scaled_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_knu_scaled_e = gsl_sf_bessel_knu_scaled_e(n, x, res)
    result = res
  end function fgsl_sf_bessel_knu_scaled_e
! fgsl_sf_bessel_zero_jc0 --> interface
  function fgsl_sf_bessel_zero_jc0_e(s, result)
    integer(fgsl_int), intent(in) :: s
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_zero_jc0_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_zero_jc0_e = gsl_sf_bessel_zero_jc0_e(s, res)
    result = res
  end function fgsl_sf_bessel_zero_jc0_e
!  fgsl_sf_bessel_zero_jc1 --> interface
  function fgsl_sf_bessel_zero_jc1_e(s, result)
    integer(fgsl_int), intent(in) :: s
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_zero_jc1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_zero_jc1_e = gsl_sf_bessel_zero_jc1_e(s, res)
    result = res
  end function fgsl_sf_bessel_zero_jc1_e
!  fgsl_sf_bessel_zero_jnu --> interface
  function fgsl_sf_bessel_zero_jnu_e(nu, s, result)
    real(fgsl_double), intent(in) :: nu
    integer(fgsl_int), intent(in) :: s
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_bessel_zero_jnu_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_bessel_zero_jnu_e = gsl_sf_bessel_zero_jnu_e(nu, s, res)
    result = res
  end function fgsl_sf_bessel_zero_jnu_e
end module fgsl_sf_bessel
