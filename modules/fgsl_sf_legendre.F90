!-*-f90-*-
module fgsl_sf_legendre
  !>  Special functions - Legendre Functions and Spherical Harmonics
  use fgsl_sf_types
  use fgsl_errno
  implicit none

  private :: gsl_sf_legendre_p1_e, gsl_sf_legendre_p2_e, gsl_sf_legendre_p3_e, &
       gsl_sf_legendre_pl_e, gsl_sf_legendre_pl_array, &
       gsl_sf_legendre_pl_deriv_array, gsl_sf_legendre_q0_e, &
       gsl_sf_legendre_q1_e, gsl_sf_legendre_ql_e, gsl_sf_legendre_plm_e, &
       gsl_sf_legendre_sphplm_e, gsl_sf_conicalp_half_e, &
       gsl_sf_conicalp_mhalf_e, gsl_sf_conicalp_0_e, gsl_sf_conicalp_1_e, &
       gsl_sf_conicalp_sph_reg_e, gsl_sf_conicalp_cyl_reg_e, &
       gsl_sf_legendre_h3d_0_e, gsl_sf_legendre_h3d_1_e, &
       gsl_sf_legendre_h3d_e, gsl_sf_legendre_h3d_array, &
       gsl_sf_legendre_array, gsl_sf_legendre_array_e, &
       gsl_sf_legendre_deriv_array, gsl_sf_legendre_deriv_array_e, &
       gsl_sf_legendre_deriv_alt_array, gsl_sf_legendre_deriv_alt_array_e, &
       gsl_sf_legendre_deriv2_alt_array, gsl_sf_legendre_deriv2_alt_array_e
       

  !
  !> Types and Control Parameters
  type, public :: fgsl_sf_legendre_t
    private
    integer(c_int) :: gsl_sf_legendre_t = 0
 end type fgsl_sf_legendre_t
 
  type(fgsl_sf_legendre_t), parameter, public :: &
    fgsl_sf_legendre_schmidt = fgsl_sf_legendre_t(0), &
    fgsl_sf_legendre_spharm = fgsl_sf_legendre_t(1), &
    fgsl_sf_legendre_full = fgsl_sf_legendre_t(2), &
    fgsl_sf_legendre_none = fgsl_sf_legendre_t(3)

  !
  !> C interfaces
  interface
     function fgsl_sf_legendre_p1(x) bind(c, name='gsl_sf_legendre_P1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_legendre_p1
     end function fgsl_sf_legendre_p1
     function gsl_sf_legendre_p1_e(x, result) bind(c, name='gsl_sf_legendre_P1_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_legendre_p1_e
     end function gsl_sf_legendre_p1_e
     function fgsl_sf_legendre_p2(x) bind(c, name='gsl_sf_legendre_P2')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_legendre_p2
     end function fgsl_sf_legendre_p2
     function gsl_sf_legendre_p2_e(x, result) bind(c, name='gsl_sf_legendre_P2_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_legendre_p2_e
     end function gsl_sf_legendre_p2_e
     function fgsl_sf_legendre_p3(x) bind(c, name='gsl_sf_legendre_P3')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_legendre_p3
     end function fgsl_sf_legendre_p3
     function gsl_sf_legendre_p3_e(x, result) bind(c, name='gsl_sf_legendre_P3_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_legendre_p3_e
     end function gsl_sf_legendre_p3_e
     function fgsl_sf_legendre_pl(l, x) bind(c, name='gsl_sf_legendre_Pl')
       import
       integer(c_int), value :: l
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_legendre_pl
     end function fgsl_sf_legendre_pl
     function gsl_sf_legendre_pl_e(l, x, result) bind(c, name='gsl_sf_legendre_Pl_e')
       import
       integer(c_int), value :: l
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_legendre_pl_e
     end function gsl_sf_legendre_pl_e
     function gsl_sf_legendre_pl_array(lmax, x, res_arr) bind(c, name='gsl_sf_legendre_Pl_array')
       import
       integer(c_int), value :: lmax
       real(c_double), value :: x
       type(c_ptr), value :: res_arr
       integer(c_int) :: gsl_sf_legendre_pl_array
     end function gsl_sf_legendre_pl_array
     function gsl_sf_legendre_pl_deriv_array(lmax, x, res_arr, der_arr) &
          bind(c, name='gsl_sf_legendre_Pl_deriv_array')
       import
       integer(c_int), value :: lmax
       real(c_double), value :: x
       type(c_ptr), value :: res_arr, der_arr
       integer(c_int) :: gsl_sf_legendre_pl_deriv_array
     end function gsl_sf_legendre_pl_deriv_array
     function fgsl_sf_legendre_q0(x) bind(c, name='gsl_sf_legendre_Q0')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_legendre_q0
     end function fgsl_sf_legendre_q0
     function gsl_sf_legendre_q0_e(x, result) bind(c, name='gsl_sf_legendre_Q0_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_legendre_q0_e
     end function gsl_sf_legendre_q0_e
     function fgsl_sf_legendre_q1(x) bind(c, name='gsl_sf_legendre_Q1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_legendre_q1
     end function fgsl_sf_legendre_q1
     function gsl_sf_legendre_q1_e(x, result) bind(c, name='gsl_sf_legendre_Q1_e')
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_legendre_q1_e
     end function gsl_sf_legendre_q1_e
     function fgsl_sf_legendre_ql(l, x) bind(c, name='gsl_sf_legendre_Ql')
       import
       integer(c_int), value :: l
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_legendre_ql
     end function fgsl_sf_legendre_ql
     function gsl_sf_legendre_ql_e(l, x, result) bind(c, name='gsl_sf_legendre_Ql_e')
       import
       integer(c_int), value :: l
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_legendre_ql_e
     end function gsl_sf_legendre_ql_e
     function fgsl_sf_legendre_plm(l, m, x) bind(c, name='gsl_sf_legendre_Plm')
       import
       integer(c_int), value :: l, m
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_legendre_plm
     end function fgsl_sf_legendre_plm
     function gsl_sf_legendre_plm_e(l, m, x, result) bind(c, name='gsl_sf_legendre_Plm_e')
       import
       integer(c_int), value :: l, m
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_legendre_plm_e
     end function gsl_sf_legendre_plm_e
     function fgsl_sf_legendre_sphplm(l, m, x) bind(c, name='gsl_sf_legendre_sphPlm')
       import
       integer(c_int), value :: l, m
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_legendre_sphplm
     end function fgsl_sf_legendre_sphplm
     function gsl_sf_legendre_sphplm_e(l, m, x, result) bind(c, name='gsl_sf_legendre_sphPlm_e')
       import
       integer(c_int), value :: l, m
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_legendre_sphplm_e
     end function gsl_sf_legendre_sphplm_e
     function fgsl_sf_conicalp_half(lambda, x) bind(c, name='gsl_sf_conicalP_half')
       import
       real(c_double), value :: lambda, x
       real(c_double) :: fgsl_sf_conicalp_half
     end function fgsl_sf_conicalp_half
     function gsl_sf_conicalp_half_e(lambda, x, result) bind(c, name='gsl_sf_conicalP_half_e')
       import
       real(c_double), value :: lambda, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_conicalp_half_e
     end function gsl_sf_conicalp_half_e
     function fgsl_sf_conicalp_mhalf(lambda, x) bind(c, name='gsl_sf_conicalP_mhalf')
       import
       real(c_double), value :: lambda, x
       real(c_double) :: fgsl_sf_conicalp_mhalf
     end function fgsl_sf_conicalp_mhalf
     function gsl_sf_conicalp_mhalf_e(lambda, x, result) bind(c, name='gsl_sf_conicalP_mhalf_e')
       import
       real(c_double), value :: lambda, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_conicalp_mhalf_e
     end function gsl_sf_conicalp_mhalf_e
     function fgsl_sf_conicalp_0(lambda, x) bind(c, name='gsl_sf_conicalP_0')
       import
       real(c_double), value :: lambda, x
       real(c_double) :: fgsl_sf_conicalp_0
     end function fgsl_sf_conicalp_0
     function gsl_sf_conicalp_0_e(lambda, x, result) bind(c, name='gsl_sf_conicalP_0_e')
       import
       real(c_double), value :: lambda, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_conicalp_0_e
     end function gsl_sf_conicalp_0_e
     function fgsl_sf_conicalp_1(lambda, x) bind(c, name='gsl_sf_conicalP_1')
       import
       real(c_double), value :: lambda, x
       real(c_double) :: fgsl_sf_conicalp_1
     end function fgsl_sf_conicalp_1
     function gsl_sf_conicalp_1_e(lambda, x, result) bind(c, name='gsl_sf_conicalP_1_e')
       import
       real(c_double), value :: lambda, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_conicalp_1_e
     end function gsl_sf_conicalp_1_e
     function fgsl_sf_conicalp_sph_reg(l, lambda, x) &
          bind(c, name='gsl_sf_conicalP_sph_reg')
       import
       integer(c_int), value :: l
       real(c_double), value :: lambda, x
       real(c_double) :: fgsl_sf_conicalp_sph_reg
     end function fgsl_sf_conicalp_sph_reg
     function gsl_sf_conicalp_sph_reg_e(l, lambda, x, result) &
          bind(c, name='gsl_sf_conicalP_sph_reg_e')
       import
       integer(c_int), value :: l
       real(c_double), value :: lambda, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_conicalp_sph_reg_e
     end function gsl_sf_conicalp_sph_reg_e
     function fgsl_sf_conicalp_cyl_reg(l, lambda, x) &
          bind(c, name='gsl_sf_conicalP_cyl_reg')
       import
       integer(c_int), value :: l
       real(c_double), value :: lambda, x
       real(c_double) :: fgsl_sf_conicalp_cyl_reg
     end function fgsl_sf_conicalp_cyl_reg
     function gsl_sf_conicalp_cyl_reg_e(l, lambda, x, result) &
          bind(c, name='gsl_sf_conicalP_cyl_reg_e')
       import
       integer(c_int), value :: l
       real(c_double), value :: lambda, x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_conicalp_cyl_reg_e
     end function gsl_sf_conicalp_cyl_reg_e
     function fgsl_sf_legendre_h3d_0(lambda, eta) bind(c, name='gsl_sf_legendre_H3d_0')
       import
       real(c_double), value :: lambda, eta
       real(c_double) :: fgsl_sf_legendre_h3d_0
     end function fgsl_sf_legendre_h3d_0
     function gsl_sf_legendre_h3d_0_e(lambda, eta, result) bind(c, name='gsl_sf_legendre_H3d_0_e')
       import
       real(c_double), value :: lambda, eta
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_legendre_h3d_0_e
     end function gsl_sf_legendre_h3d_0_e
     function fgsl_sf_legendre_h3d_1(lambda, eta) bind(c, name='gsl_sf_legendre_H3d_1')
       import
       real(c_double), value :: lambda, eta
       real(c_double) :: fgsl_sf_legendre_h3d_1
     end function fgsl_sf_legendre_h3d_1
     function gsl_sf_legendre_h3d_1_e(lambda, eta, result) bind(c, name='gsl_sf_legendre_H3d_1_e')
       import
       real(c_double), value :: lambda, eta
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_legendre_h3d_1_e
     end function gsl_sf_legendre_h3d_1_e
     function fgsl_sf_legendre_h3d(l, lambda, eta) bind(c, name='gsl_sf_legendre_H3d')
       import
       integer(c_int), value :: l
       real(c_double), value :: lambda, eta
       real(c_double) :: fgsl_sf_legendre_h3d
     end function fgsl_sf_legendre_h3d
     function gsl_sf_legendre_h3d_e(l, lambda, eta, result) bind(c, name='gsl_sf_legendre_H3d_e')
       import
       integer(c_int), value :: l
       real(c_double), value :: lambda, eta
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_legendre_h3d_e
     end function gsl_sf_legendre_h3d_e
     function gsl_sf_legendre_h3d_array(lmax, lambda, eta, res_arr) &
          bind(c, name='gsl_sf_legendre_H3d_array')
       import
       integer(c_int), value :: lmax
       real(c_double), value :: lambda, eta
       type(c_ptr), value :: res_arr
       integer(c_int) :: gsl_sf_legendre_h3d_array
     end function gsl_sf_legendre_h3d_array
     function gsl_sf_legendre_array(norm, lmax, x, result_array) bind(c)
       import :: c_int, c_size_t, c_double, c_ptr
       integer(c_int), value :: norm
       integer(c_size_t), value :: lmax
       real(c_double), value :: x
       type(c_ptr), value :: result_array
       integer(c_int) :: gsl_sf_legendre_array
     end function gsl_sf_legendre_array
     function gsl_sf_legendre_array_e(norm, lmax, x, csphase, result_array) bind(c)
       import :: c_int, c_size_t, c_double, c_ptr
       integer(c_int), value :: norm
       integer(c_size_t), value :: lmax
       real(c_double), value :: x, csphase
       type(c_ptr), value :: result_array
       integer(c_int) :: gsl_sf_legendre_array_e
     end function gsl_sf_legendre_array_e
     function gsl_sf_legendre_deriv_array(norm, lmax, x, result_array, &
       result_deriv_array) bind(c)
       import :: c_int, c_size_t, c_double, c_ptr
       integer(c_int), value :: norm
       integer(c_size_t), value :: lmax
       real(c_double), value :: x
       type(c_ptr), value :: result_array, result_deriv_array
       integer(c_int) :: gsl_sf_legendre_deriv_array
     end function gsl_sf_legendre_deriv_array
     function gsl_sf_legendre_deriv_array_e(norm, lmax, x, csphase, &
       result_array, result_deriv_array) bind(c)
       import :: c_int, c_size_t, c_double, c_ptr
       integer(c_int), value :: norm
       integer(c_size_t), value :: lmax
       real(c_double), value :: x, csphase
       type(c_ptr), value :: result_array, result_deriv_array
       integer(c_int) :: gsl_sf_legendre_deriv_array_e
     end function gsl_sf_legendre_deriv_array_e
     function gsl_sf_legendre_deriv_alt_array(norm, lmax, x, result_array, &
       result_deriv_array) bind(c)
       import :: c_int, c_size_t, c_double, c_ptr
       integer(c_int), value :: norm
       integer(c_size_t), value :: lmax
       real(c_double), value :: x
       type(c_ptr), value :: result_array, result_deriv_array
       integer(c_int) :: gsl_sf_legendre_deriv_alt_array
     end function gsl_sf_legendre_deriv_alt_array
     function gsl_sf_legendre_deriv_alt_array_e(norm, lmax, x, csphase, &
       result_array, result_deriv_array) bind(c)
       import :: c_int, c_size_t, c_double, c_ptr
       integer(c_int), value :: norm
       integer(c_size_t), value :: lmax
       real(c_double), value :: x, csphase
       type(c_ptr), value :: result_array, result_deriv_array
       integer(c_int) :: gsl_sf_legendre_deriv_alt_array_e
     end function gsl_sf_legendre_deriv_alt_array_e
     function gsl_sf_legendre_deriv2_array(norm, lmax, x, result_array, &
       result_deriv_array, result_deriv2_array) bind(c)
       import :: c_int, c_size_t, c_double, c_ptr
       integer(c_int), value :: norm
       integer(c_size_t), value :: lmax
       real(c_double), value :: x
       type(c_ptr), value :: result_array, result_deriv_array, result_deriv2_array
       integer(c_int) :: gsl_sf_legendre_deriv2_array
     end function gsl_sf_legendre_deriv2_array
     function gsl_sf_legendre_deriv2_array_e(norm, lmax, x, csphase, &
       result_array, result_deriv_array, result_deriv2_array) bind(c)
       import :: c_int, c_size_t, c_double, c_ptr
       integer(c_int), value :: norm
       integer(c_size_t), value :: lmax
       real(c_double), value :: x, csphase
       type(c_ptr), value :: result_array, result_deriv_array, result_deriv2_array
       integer(c_int) :: gsl_sf_legendre_deriv2_array_e
     end function gsl_sf_legendre_deriv2_array_e
     function gsl_sf_legendre_deriv2_alt_array(norm, lmax, x, result_array,&
        result_deriv_array, result_deriv2_array) bind(c)
       import :: c_int, c_size_t, c_double, c_ptr
       integer(c_int), value :: norm
       integer(c_size_t), value :: lmax
       real(c_double), value :: x
       type(c_ptr), value :: result_array, result_deriv_array, result_deriv2_array
       integer(c_int) :: gsl_sf_legendre_deriv2_alt_array
     end function gsl_sf_legendre_deriv2_alt_array
     function gsl_sf_legendre_deriv2_alt_array_e(norm, lmax, x, csphase, &
       result_array, result_deriv_array, result_deriv2_array) bind(c)
       import :: c_int, c_size_t, c_double, c_ptr
       integer(c_int), value :: norm
       integer(c_size_t), value :: lmax
       real(c_double), value :: x, csphase
       type(c_ptr), value :: result_array, result_deriv_array, result_deriv2_array
       integer(c_int) :: gsl_sf_legendre_deriv2_alt_array_e
     end function gsl_sf_legendre_deriv2_alt_array_e
     function fgsl_sf_legendre_array_n(lmax) bind(c, name='gsl_sf_legendre_array_n')
       import :: c_size_t
       integer(c_size_t), value :: lmax
       integer(c_size_t) :: fgsl_sf_legendre_array_n
     end function fgsl_sf_legendre_array_n
     function fgsl_sf_legendre_array_index(l, m) &
          bind(c, name='gsl_sf_legendre_array_index')
       import :: c_size_t
       integer(c_size_t), value :: l, m
       integer(c_size_t) :: fgsl_sf_legendre_array_index
     end function fgsl_sf_legendre_array_index
     function fgsl_sf_legendre_nlm(lmax) bind(c, name='gsl_sf_legendre_nlm')
       import :: c_size_t
       integer(c_size_t), value :: lmax
       integer(c_size_t) :: fgsl_sf_legendre_nlm
     end function fgsl_sf_legendre_nlm
  end interface

contains
    function fgsl_sf_legendre_p1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_legendre_p1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_legendre_p1_e = gsl_sf_legendre_p1_e(x, res)
    result = res
  end function fgsl_sf_legendre_p1_e
!  fgsl_sf_legendre_p2 --> interface
  function fgsl_sf_legendre_p2_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_legendre_p2_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_legendre_p2_e = gsl_sf_legendre_p2_e(x, res)
    result = res
  end function fgsl_sf_legendre_p2_e
!  fgsl_sf_legendre_p3 --> interface
  function fgsl_sf_legendre_p3_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_legendre_p3_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_legendre_p3_e = gsl_sf_legendre_p3_e(x, res)
    result = res
  end function fgsl_sf_legendre_p3_e
!  fgsl_sf_legendre_pl --> interface
  function fgsl_sf_legendre_pl_e(l, x, result)
    integer(fgsl_int), intent(in) :: l
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_legendre_pl_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_legendre_pl_e = gsl_sf_legendre_pl_e(l, x, res)
    result = res
  end function fgsl_sf_legendre_pl_e
  function fgsl_sf_legendre_pl_array(x, result_array)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(inout), contiguous, target :: result_array(:)
    real(fgsl_double) :: fgsl_sf_legendre_pl_array
    fgsl_sf_legendre_pl_array = gsl_sf_legendre_pl_array( &
         size(result_array, kind=fgsl_int)-1,&
         x, c_loc(result_array))
  end function fgsl_sf_legendre_pl_array
  function fgsl_sf_legendre_pl_deriv_array(x, result_array, deriv_array)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(inout), contiguous, target :: &
         result_array(:), deriv_array(:)
    real(fgsl_double) :: fgsl_sf_legendre_pl_deriv_array
    if (size(result_array) /= size(deriv_array)) then
      call fgsl_error('result_array and deriv_array dimensions do not match',&
       'fgsl_specfunc', __LINE__, fgsl_ebadlen)
      fgsl_sf_legendre_pl_deriv_array = 0.0_fgsl_double
      return
    end if
    fgsl_sf_legendre_pl_deriv_array = &
         gsl_sf_legendre_pl_deriv_array(size(result_array, kind=fgsl_int)-1, &
         x, c_loc(result_array), c_loc(deriv_array))
  end function fgsl_sf_legendre_pl_deriv_array
!  fgsl_sf_legendre_q0 --> interface
  function fgsl_sf_legendre_q0_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_legendre_q0_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_legendre_q0_e = gsl_sf_legendre_q0_e(x, res)
    result = res
  end function fgsl_sf_legendre_q0_e
!  fgsl_sf_legendre_q1 --> interface
  function fgsl_sf_legendre_q1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_legendre_q1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_legendre_q1_e = gsl_sf_legendre_q1_e(x, res)
    result = res
  end function fgsl_sf_legendre_q1_e
!  fgsl_sf_legendre_ql --> interface
  function fgsl_sf_legendre_ql_e(l, x, result)
    integer(fgsl_int), intent(in) :: l
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_legendre_ql_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_legendre_ql_e = gsl_sf_legendre_ql_e(l, x, res)
    result = res
  end function fgsl_sf_legendre_ql_e
!  fgsl_sf_legendre_plm --> interface
  function fgsl_sf_legendre_plm_e(l, m, x, result)
    integer(fgsl_int), intent(in) :: l, m
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_legendre_plm_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_legendre_plm_e = gsl_sf_legendre_plm_e(l, m, x, res)
    result = res
  end function fgsl_sf_legendre_plm_e
!  fgsl_sf_legendre_sphplm --> interface
  function fgsl_sf_legendre_sphplm_e(l, m, x, result)
    integer(fgsl_int), intent(in) :: l, m
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_legendre_sphplm_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_legendre_sphplm_e = gsl_sf_legendre_sphplm_e(l, m, x, res)
    result = res
  end function fgsl_sf_legendre_sphplm_e
  !  fgsl_sf_conicalp_half --> interface
  function fgsl_sf_conicalp_half_e(lambda, x, result)
    real(fgsl_double), intent(in) :: lambda, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_conicalp_half_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_conicalp_half_e = gsl_sf_conicalp_half_e(lambda, x, res)
    result = res
  end function fgsl_sf_conicalp_half_e
!  fgsl_sf_conicalp_mhalf --> interface
  function fgsl_sf_conicalp_mhalf_e(lambda, x, result)
    real(fgsl_double), intent(in) :: lambda, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_conicalp_mhalf_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_conicalp_mhalf_e = gsl_sf_conicalp_mhalf_e(lambda, x, res)
    result = res
  end function fgsl_sf_conicalp_mhalf_e
!  fgsl_sf_conicalp_0 --> interface
  function fgsl_sf_conicalp_0_e(lambda, x, result)
    real(fgsl_double), intent(in) :: lambda, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_conicalp_0_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_conicalp_0_e = gsl_sf_conicalp_0_e(lambda, x, res)
    result = res
  end function fgsl_sf_conicalp_0_e
!  fgsl_sf_conicalp_1 --> interface
  function fgsl_sf_conicalp_1_e(lambda, x, result)
    real(fgsl_double), intent(in) :: lambda, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_conicalp_1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_conicalp_1_e = gsl_sf_conicalp_1_e(lambda, x, res)
    result = res
  end function fgsl_sf_conicalp_1_e
!  fgsl_sf_conicalp_sph_reg --> interface
  function fgsl_sf_conicalp_sph_reg_e(l, lambda, x, result)
    integer(fgsl_int), intent(in) :: l
    real(fgsl_double), intent(in) :: lambda, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_conicalp_sph_reg_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_conicalp_sph_reg_e = gsl_sf_conicalp_sph_reg_e(l, lambda, x, res)
    result = res
  end function fgsl_sf_conicalp_sph_reg_e
!  fgsl_sf_conicalp_cyl_reg --> interface
  function fgsl_sf_conicalp_cyl_reg_e(l, lambda, x, result)
    integer(fgsl_int), intent(in) :: l
    real(fgsl_double), intent(in) :: lambda, x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_conicalp_cyl_reg_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_conicalp_cyl_reg_e = gsl_sf_conicalp_cyl_reg_e(l, lambda, x, res)
    result = res
  end function fgsl_sf_conicalp_cyl_reg_e
!  fgsl_sf_legendre_h3d_0 --> interface
  function fgsl_sf_legendre_h3d_0_e(lambda, eta, result)
    real(fgsl_double), intent(in) :: lambda, eta
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_legendre_h3d_0_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_legendre_h3d_0_e = gsl_sf_legendre_h3d_0_e(lambda, eta, res)
    result = res
  end function fgsl_sf_legendre_h3d_0_e
!  fgsl_sf_legendre_h3d_1 --> interface
  function fgsl_sf_legendre_h3d_1_e(lambda, eta, result)
    real(fgsl_double), intent(in) :: lambda, eta
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_legendre_h3d_1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_legendre_h3d_1_e = gsl_sf_legendre_h3d_1_e(lambda, eta, res)
    result = res
  end function fgsl_sf_legendre_h3d_1_e
!  fgsl_sf_legendre_h3d --> interface
  function fgsl_sf_legendre_h3d_e(l, lambda, eta, result)
    integer(fgsl_int), intent(in) :: l
    real(fgsl_double), intent(in) :: lambda, eta
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_legendre_h3d_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_legendre_h3d_e = gsl_sf_legendre_h3d_e(l, lambda, eta, res)
    result = res
  end function fgsl_sf_legendre_h3d_e
  function fgsl_sf_legendre_h3d_array(lambda, eta, result_array)
    real(fgsl_double), intent(in) :: lambda, eta
    real(fgsl_double), intent(inout), contiguous, target :: result_array(:)
    integer(fgsl_int) :: fgsl_sf_legendre_h3d_array
    fgsl_sf_legendre_h3d_array = gsl_sf_legendre_h3d_array( &
         size(result_array, kind=fgsl_int)-1,&
         lambda, eta, c_loc(result_array))
  end function fgsl_sf_legendre_h3d_array
  function fgsl_sf_legendre_array(norm, lmax, x, result_array)
    type(fgsl_sf_legendre_t), intent(in) :: norm
    integer(fgsl_size_t), intent(in) :: lmax
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), dimension(:), intent(inout), contiguous, &
         target :: result_array
    integer(fgsl_int) :: fgsl_sf_legendre_array
    fgsl_sf_legendre_array = gsl_sf_legendre_array(&
    norm%gsl_sf_legendre_t, lmax, x, c_loc(result_array))
  end function fgsl_sf_legendre_array
  function fgsl_sf_legendre_array_e(norm, lmax, x, csphase, result_array)
    type(fgsl_sf_legendre_t), intent(in) :: norm
    integer(fgsl_size_t), intent(in) :: lmax
    real(fgsl_double), intent(in) :: x, csphase
    real(fgsl_double), dimension(:), intent(inout), contiguous, target :: &
         result_array
    integer(fgsl_int) :: fgsl_sf_legendre_array_e
    fgsl_sf_legendre_array_e = gsl_sf_legendre_array_e(&
    norm%gsl_sf_legendre_t, lmax, x, csphase, c_loc(result_array))
  end function fgsl_sf_legendre_array_e
  function fgsl_sf_legendre_deriv_array(norm, lmax, x, result_array, result_deriv_array)
    type(fgsl_sf_legendre_t), intent(in) :: norm
    integer(fgsl_size_t), intent(in) :: lmax
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), dimension(:), intent(inout), contiguous, &
         target :: result_array, result_deriv_array
    integer(fgsl_int) :: fgsl_sf_legendre_deriv_array
    fgsl_sf_legendre_deriv_array = gsl_sf_legendre_deriv_array(&
    norm%gsl_sf_legendre_t, lmax, x, &
    c_loc(result_array), c_loc(result_deriv_array))
  end function fgsl_sf_legendre_deriv_array
  function fgsl_sf_legendre_deriv_array_e(norm, lmax, x, csphase, result_array, &
    result_deriv_array)
    type(fgsl_sf_legendre_t), intent(in) :: norm
    integer(fgsl_size_t), intent(in) :: lmax
    real(fgsl_double), intent(in) :: x, csphase
    real(fgsl_double), dimension(:), intent(inout), contiguous, target :: &
         result_array, result_deriv_array
    integer(fgsl_int) :: fgsl_sf_legendre_deriv_array_e
    fgsl_sf_legendre_deriv_array_e = gsl_sf_legendre_deriv_array_e(&
    norm%gsl_sf_legendre_t, lmax, x, csphase, &
    c_loc(result_array), c_loc(result_deriv_array))
  end function fgsl_sf_legendre_deriv_array_e
  function fgsl_sf_legendre_deriv_alt_array(norm, lmax, x, result_array, result_deriv_array)
    type(fgsl_sf_legendre_t), intent(in) :: norm
    integer(fgsl_size_t), intent(in) :: lmax
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), dimension(:), intent(inout), contiguous, target :: &
         result_array, result_deriv_array
    integer(fgsl_int) :: fgsl_sf_legendre_deriv_alt_array
    fgsl_sf_legendre_deriv_alt_array = gsl_sf_legendre_deriv_alt_array(&
    norm%gsl_sf_legendre_t, lmax, x, c_loc(result_array), &
    c_loc(result_deriv_array))
  end function fgsl_sf_legendre_deriv_alt_array
  function fgsl_sf_legendre_deriv_alt_array_e(norm, lmax, x, csphase, result_array, &
    result_deriv_array)
    type(fgsl_sf_legendre_t), intent(in) :: norm
    integer(fgsl_size_t), intent(in) :: lmax
    real(fgsl_double), intent(in) :: x, csphase
    real(fgsl_double), dimension(:), intent(inout), contiguous, target :: &
         result_array, result_deriv_array
    integer(fgsl_int) :: fgsl_sf_legendre_deriv_alt_array_e
    fgsl_sf_legendre_deriv_alt_array_e = gsl_sf_legendre_deriv_alt_array_e(&
    norm%gsl_sf_legendre_t, lmax, x, csphase, &
    c_loc(result_array), c_loc(result_deriv_array))
  end function fgsl_sf_legendre_deriv_alt_array_e
  function fgsl_sf_legendre_deriv2_array(norm, lmax, x, result_array, &
    result_deriv_array, result_deriv2_array)
    type(fgsl_sf_legendre_t), intent(in) :: norm
    integer(fgsl_size_t), intent(in) :: lmax
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), dimension(:), intent(inout), contiguous, target :: &
         result_array, result_deriv_array, result_deriv2_array
    integer(fgsl_int) :: fgsl_sf_legendre_deriv2_array
    fgsl_sf_legendre_deriv2_array = gsl_sf_legendre_deriv2_array(&
    norm%gsl_sf_legendre_t, lmax, x, c_loc(result_array), &
    c_loc(result_deriv_array), c_loc(result_deriv2_array))
  end function fgsl_sf_legendre_deriv2_array
  function fgsl_sf_legendre_deriv2_array_e(norm, lmax, x, csphase, result_array, &
    result_deriv_array, result_deriv2_array)
    type(fgsl_sf_legendre_t), intent(in) :: norm
    integer(fgsl_size_t), intent(in) :: lmax
    real(fgsl_double), intent(in) :: x, csphase
    real(fgsl_double), dimension(:), intent(inout), contiguous, target :: &
         result_array, &
    result_deriv_array, result_deriv2_array
    integer(fgsl_int) :: fgsl_sf_legendre_deriv2_array_e
    fgsl_sf_legendre_deriv2_array_e = gsl_sf_legendre_deriv2_array_e(&
    norm%gsl_sf_legendre_t, lmax, x, csphase, c_loc(result_array), &
    c_loc(result_deriv_array), c_loc(result_deriv2_array))
  end function fgsl_sf_legendre_deriv2_array_e
  function fgsl_sf_legendre_deriv2_alt_array(norm, lmax, x, result_array, &
    result_deriv_array, result_deriv2_array)
    type(fgsl_sf_legendre_t), intent(in) :: norm
    integer(fgsl_size_t), intent(in) :: lmax
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), dimension(:), intent(inout), contiguous, target :: &
         result_array, result_deriv_array, result_deriv2_array
    integer(fgsl_int) :: fgsl_sf_legendre_deriv2_alt_array
    fgsl_sf_legendre_deriv2_alt_array = gsl_sf_legendre_deriv2_alt_array(&
    norm%gsl_sf_legendre_t, lmax, x, c_loc(result_array), &
    c_loc(result_deriv_array), c_loc(result_deriv2_array))
  end function fgsl_sf_legendre_deriv2_alt_array
  function fgsl_sf_legendre_deriv2_alt_array_e(norm, lmax, x, csphase, result_array, &
    result_deriv_array, result_deriv2_array)
    type(fgsl_sf_legendre_t), intent(in) :: norm
    integer(fgsl_size_t), intent(in) :: lmax
    real(fgsl_double), intent(in) :: x, csphase
    real(fgsl_double), dimension(:), intent(inout), contiguous, target :: &
         result_array, result_deriv_array, result_deriv2_array
    integer(fgsl_int) :: fgsl_sf_legendre_deriv2_alt_array_e
    fgsl_sf_legendre_deriv2_alt_array_e = gsl_sf_legendre_deriv2_alt_array_e(&
    norm%gsl_sf_legendre_t, lmax, x, csphase, c_loc(result_array), &
    c_loc(result_deriv_array), c_loc(result_deriv2_array))
  end function fgsl_sf_legendre_deriv2_alt_array_e
!  fgsl_sf_legendre_array_n --> interface
!  fgsl_sf_legendre_array_index --> interface
!  fgsl_sf_legendre_nlm --> interface
end module fgsl_sf_legendre
