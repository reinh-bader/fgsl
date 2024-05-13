module fgsl_linalg
  !> Linear Algebra
  !> Since GSL follows the C convention for ordering of elements,
  !> all matrices must be set up and read out transposed.
  use fgsl_array
  use fgsl_permutations
  use fgsl_io
  implicit none

  private :: gsl_linalg_lu_decomp, gsl_linalg_complex_lu_decomp, &
       gsl_linalg_lu_solve, gsl_linalg_complex_lu_solve, &
       gsl_linalg_lu_svx, gsl_linalg_complex_lu_svx, &
       gsl_linalg_lu_refine, gsl_linalg_complex_lu_refine, &
       gsl_linalg_lu_invert, gsl_linalg_complex_lu_invert, &
       gsl_linalg_lu_invx, gsl_linalg_complex_lu_invx, &
       gsl_linalg_lu_det, gsl_linalg_complex_lu_det, &
       gsl_linalg_lu_lndet, gsl_linalg_complex_lu_lndet, &
       gsl_linalg_lu_sgndet, gsl_linalg_complex_lu_sgndet
  private :: gsl_linalg_qr_decomp, gsl_linalg_complex_qr_decomp, &
       gsl_linalg_complex_qr_decomp_r, gsl_linalg_qr_solve, &
       gsl_linalg_complex_qr_solve, gsl_linalg_qr_solve_r, &
       gsl_linalg_complex_qr_solve_r, gsl_linalg_qr_svx, &
       gsl_linalg_complex_qr_svx, gsl_linalg_qr_lssolve, &
       gsl_linalg_complex_qr_lssolve, gsl_linalg_qr_lssolve_r, &
       gsl_linalg_complex_qr_lssolve_r, gsl_linalg_qr_qtvec, &
       gsl_linalg_complex_qr_qhvec, gsl_linalg_qr_qtvec_r, &
       gsl_linalg_complex_qr_qhvec_r, gsl_linalg_qr_qvec, &
       gsl_linalg_complex_qr_qvec, gsl_linalg_qr_qtmat, &
       gsl_linalg_qr_qtmat_r, gsl_linalg_qr_rsolve, gsl_linalg_qr_rsvx, &
       gsl_linalg_qr_unpack, gsl_linalg_qr_unpack_r, &
       gsl_linalg_complex_qr_unpack_r, gsl_linalg_qr_qrsolve, &
       gsl_linalg_qr_update, gsl_linalg_r_solve, gsl_linalg_r_svx, &
       gsl_linalg_qr_ur_decomp, gsl_linalg_qr_uu_decomp, &
       gsl_linalg_qr_uu_lssolve,  gsl_linalg_qr_uu_qtvec, &
       gsl_linalg_qr_uz_decomp, gsl_linalg_qr_ud_decomp, &
       gsl_linalg_qr_ud_lssolve, gsl_linalg_qrpt_decomp, &
       gsl_linalg_qrpt_decomp2, gsl_linalg_qrpt_solve, gsl_linalg_qrpt_svx, &
       gsl_linalg_qrpt_lssolve, gsl_linalg_qrpt_lssolve2, &
       gsl_linalg_qrpt_qrsolve, gsl_linalg_qrpt_update, gsl_linalg_qrpt_rsolve, &
       gsl_linalg_qrpt_rsvx, gsl_linalg_qrpt_rank, gsl_linalg_qrpt_rcond
  private :: gsl_linalg_lq_decomp, gsl_linalg_lq_lssolve, gsl_linalg_lq_unpack, &
       gsl_linalg_lq_qtvec, gsl_linalg_ql_decomp, gsl_linalg_ql_unpack
  private :: gsl_linalg_cod_decomp, gsl_linalg_cod_decomp_e, &
       gsl_linalg_cod_lssolve, gsl_linalg_cod_lssolve2, &
       gsl_linalg_cod_unpack, gsl_linalg_cod_matz
  private :: gsl_linalg_sv_decomp, gsl_linalg_sv_decomp_mod, &
       gsl_linalg_sv_decomp_jacobi, gsl_linalg_sv_solve, &
       gsl_linalg_sv_leverage
  private :: gsl_linalg_cholesky_decomp1, gsl_linalg_cholesky_decomp, &
       gsl_linalg_complex_cholesky_decomp, gsl_linalg_cholesky_solve, &
       gsl_linalg_complex_cholesky_solve, gsl_linalg_cholesky_svx, &
       gsl_linalg_complex_cholesky_svx, gsl_linalg_cholesky_invert, &
       gsl_linalg_complex_cholesky_invert, gsl_linalg_cholesky_decomp2, &
       gsl_linalg_cholesky_svx2, gsl_linalg_cholesky_scale, &
       gsl_linalg_cholesky_scale_apply, gsl_linalg_cholesky_rcond, &
       gsl_linalg_pcholesky_decomp, gsl_linalg_pcholesky_solve, &
       gsl_linalg_pcholesky_svx, gsl_linalg_pcholesky_invert, &
       gsl_linalg_pcholesky_decomp2, gsl_linalg_pcholesky_solve2, &
       gsl_linalg_pcholesky_svx2, gsl_linalg_pcholesky_rcond, &
       gsl_linalg_mcholesky_decomp, gsl_linalg_mcholesky_solve, &
       gsl_linalg_mcholesky_svx, gsl_linalg_mcholesky_invert, &
       gsl_linalg_mcholesky_rcond
  private :: gsl_linalg_ldlt_decomp, gsl_linalg_ldlt_solve, &
       gsl_linalg_ldlt_svx, gsl_linalg_ldlt_rcond
  private :: gsl_linalg_symmtd_decomp, gsl_linalg_symmtd_unpack, &
       gsl_linalg_symmtd_unpack_t
  private :: gsl_linalg_hermtd_decomp, gsl_linalg_hermtd_unpack, &
       gsl_linalg_hermtd_unpack_t
  private :: gsl_linalg_hessenberg_decomp, gsl_linalg_hessenberg_unpack, &
       gsl_linalg_hessenberg_unpack_accum, gsl_linalg_hessenberg_set_zero, &
       gsl_linalg_hesstri_decomp
  private :: gsl_linalg_bidiag_decomp, gsl_linalg_bidiag_unpack, &
       gsl_linalg_bidiag_unpack2, gsl_linalg_bidiag_unpack_b
  private :: gsl_linalg_householder_transform, &
       gsl_linalg_complex_householder_transform, &
       gsl_linalg_householder_hm, gsl_linalg_complex_householder_hm, &
       gsl_linalg_householder_mh, gsl_linalg_complex_householder_mh, &
       gsl_linalg_householder_hv, gsl_linalg_complex_householder_hv, &
       gsl_linalg_hh_solve, gsl_linalg_hh_svx
  private :: gsl_linalg_solve_tridiag, gsl_linalg_solve_symm_tridiag, &
       gsl_linalg_solve_cyc_tridiag, gsl_linalg_solve_symm_cyc_tridiag, &
       gsl_linalg_qr_matq, gsl_linalg_givens, gsl_linalg_givens_gv
  private :: gsl_linalg_tri_invert, gsl_linalg_complex_tri_invert, &
       gsl_linalg_tri_ltl, gsl_linalg_complex_tri_lhl, &
       gsl_linalg_tri_ul, gsl_linalg_complex_tri_ul, gsl_linalg_tri_rcond, &
       gsl_linalg_tri_upper_invert, gsl_linalg_tri_lower_invert, &
       gsl_linalg_tri_upper_unit_invert, gsl_linalg_tri_lower_unit_invert, &
       gsl_linalg_tri_upper_rcond,  gsl_linalg_tri_lower_rcond
  private :: gsl_linalg_cholesky_band_decomp, gsl_linalg_cholesky_band_solve, &
       gsl_linalg_cholesky_band_solvem, gsl_linalg_cholesky_band_svx, &
       gsl_linalg_cholesky_band_svxm, gsl_linalg_cholesky_band_invert, &
       gsl_linalg_cholesky_band_unpack, gsl_linalg_cholesky_band_scale, &
       gsl_linalg_cholesky_band_scale_apply, gsl_linalg_cholesky_band_rcond, &
       gsl_linalg_ldlt_band_decomp, gsl_linalg_ldlt_band_solve, &
       gsl_linalg_ldlt_band_svx, gsl_linalg_ldlt_band_unpack, &
       gsl_linalg_ldlt_band_rcond
  private ::  gsl_linalg_balance_matrix
  
  !
  ! Constants
  integer(fgsl_int), parameter, public :: &
       cblasrowmajor = 101, &
       cblascolmajor = 102, &
       cblasnotrans = 111, &
       cblastrans = 112, &
       cblasconjtrans = 113, &
       cblasupper = 121, &
       cblaslower = 122, &
       cblasnonunit = 131, &
       cblasunit = 132, &
       cblasleft = 141, &
       cblasright = 142
  !
  ! C interfaces
  interface
     !
     ! LU
     !
     function gsl_linalg_lu_decomp (a, p, signum) &
          bind(c, name='gsl_linalg_LU_decomp')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, p
       integer(c_int) :: signum
       integer(c_int) :: gsl_linalg_lu_decomp
     end function gsl_linalg_lu_decomp
     function gsl_linalg_complex_lu_decomp (a, p, signum) &
          bind(c, name='gsl_linalg_complex_LU_decomp')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, p
       integer(c_int) :: signum
       integer(c_int) :: gsl_linalg_complex_lu_decomp
     end function gsl_linalg_complex_lu_decomp
     function gsl_linalg_lu_solve (lu, p, b, x) &
          bind(c, name='gsl_linalg_LU_solve')
       import :: c_ptr, c_int
       type(c_ptr), value :: lu, p, b, x
       integer(c_int) :: gsl_linalg_lu_solve
     end function gsl_linalg_lu_solve
     function gsl_linalg_complex_lu_solve (lu, p, b, x) &
          bind(c, name='gsl_linalg_complex_LU_solve')
       import :: c_ptr, c_int
       type(c_ptr), value :: lu, p, b, x
       integer(c_int) :: gsl_linalg_complex_lu_solve
     end function gsl_linalg_complex_lu_solve
     function gsl_linalg_lu_svx (lu, p, x) &
          bind(c, name='gsl_linalg_LU_svx')
       import :: c_ptr, c_int
       type(c_ptr), value :: lu, p, x
       integer(c_int) :: gsl_linalg_lu_svx
     end function gsl_linalg_lu_svx
     function gsl_linalg_complex_lu_svx (lu, p, x) &
          bind(c, name='gsl_linalg_complex_LU_svx')
       import :: c_ptr, c_int
       type(c_ptr), value :: lu, p, x
       integer(c_int) :: gsl_linalg_complex_lu_svx
     end function gsl_linalg_complex_lu_svx
     function gsl_linalg_lu_refine (a, lu, p, b, x, residual) &
          bind(c, name='gsl_linalg_LU_refine')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, lu, p, b, x, residual
       integer(c_int) :: gsl_linalg_lu_refine
     end function gsl_linalg_lu_refine
     function gsl_linalg_complex_lu_refine (a, lu, p, b, x, residual) &
          bind(c, name='gsl_linalg_complex_LU_refine')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, lu, p, b, x, residual
       integer(c_int) :: gsl_linalg_complex_lu_refine
     end function gsl_linalg_complex_lu_refine
     function gsl_linalg_lu_invert (lu, p, inv) &
          bind(c, name='gsl_linalg_LU_invert')
       import :: c_ptr, c_int
       type(c_ptr), value :: lu, p, inv
       integer(c_int) :: gsl_linalg_lu_invert
     end function gsl_linalg_lu_invert
     function gsl_linalg_complex_lu_invert (lu, p, inv) &
          bind(c, name='gsl_linalg_complex_LU_invert')
       import :: c_ptr, c_int
       type(c_ptr), value :: lu, p, inv
       integer(c_int) :: gsl_linalg_complex_lu_invert
     end function gsl_linalg_complex_lu_invert
     function gsl_linalg_lu_invx (lu, p) &
          bind(c, name='gsl_linalg_LU_invx')
       import :: c_ptr, c_int
       type(c_ptr), value :: lu, p
       integer(c_int) :: gsl_linalg_lu_invx
     end function gsl_linalg_lu_invx
     function gsl_linalg_complex_lu_invx (lu, p) &
          bind(c, name='gsl_linalg_complex_LU_invx')
       import :: c_ptr, c_int
       type(c_ptr), value :: lu, p
       integer(c_int) :: gsl_linalg_complex_lu_invx
     end function gsl_linalg_complex_lu_invx
     function gsl_linalg_lu_det(lu, signum) bind(c, name='gsl_linalg_LU_det')
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: lu
       integer(c_int), value :: signum
       real(c_double) :: gsl_linalg_lu_det
     end function gsl_linalg_lu_det
     function gsl_linalg_complex_lu_det(lu, signum) &
          bind(c, name='gsl_linalg_complex_LU_det')
       import :: c_ptr, c_int, c_double_complex
       type(c_ptr), value :: lu
       integer(c_int), value :: signum
       complex(c_double_complex) :: gsl_linalg_complex_lu_det
     end function gsl_linalg_complex_lu_det
     function gsl_linalg_lu_lndet(lu) bind(c, name='gsl_linalg_LU_lndet')
       import :: c_ptr, c_double
       type(c_ptr), value :: lu
       real(c_double) :: gsl_linalg_lu_lndet
     end function gsl_linalg_lu_lndet
     function gsl_linalg_complex_lu_lndet(lu) &
          bind(c, name='gsl_linalg_complex_LU_lndet')
       import :: c_ptr, c_double
       type(c_ptr), value :: lu
       real(c_double) :: gsl_linalg_complex_lu_lndet
     end function gsl_linalg_complex_lu_lndet
     function gsl_linalg_lu_sgndet(lu, signum) bind(c, name='gsl_linalg_LU_sgndet')
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: lu
       integer(c_int), value :: signum
       integer(c_int) :: gsl_linalg_lu_sgndet
     end function gsl_linalg_lu_sgndet
     function gsl_linalg_complex_lu_sgndet(lu, signum) &
          bind(c, name='gsl_linalg_complex_LU_sgndet')
       import :: c_ptr, c_int, c_double_complex
       type(c_ptr), value :: lu
       integer(c_int), value :: signum
       complex(c_double_complex) :: gsl_linalg_complex_lu_sgndet
     end function gsl_linalg_complex_lu_sgndet
     !
     ! QR
     !
     function gsl_linalg_qr_decomp (a, tau) bind(c, name='gsl_linalg_QR_decomp')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, tau
       integer(c_int) :: gsl_linalg_qr_decomp
     end function gsl_linalg_qr_decomp
     function gsl_linalg_complex_qr_decomp (a, tau) bind(c, name='gsl_linalg_complex_QR_decomp')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, tau
       integer(c_int) :: gsl_linalg_complex_qr_decomp
     end function gsl_linalg_complex_qr_decomp
     function gsl_linalg_qr_decomp_r (a, t) bind(c, name='gsl_linalg_QR_decomp_r')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, t
       integer(c_int) :: gsl_linalg_qr_decomp_r
     end function gsl_linalg_qr_decomp_r
     function gsl_linalg_complex_qr_decomp_r (a, t) bind(c, name='gsl_linalg_complex_QR_decomp_r')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, t
       integer(c_int) :: gsl_linalg_complex_qr_decomp_r
     end function gsl_linalg_complex_qr_decomp_r
     function gsl_linalg_qr_solve (qr, tau, b, x) &
          bind(c, name='gsl_linalg_QR_solve')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, tau, b, x
       integer(c_int) :: gsl_linalg_qr_solve
     end function gsl_linalg_qr_solve
     function gsl_linalg_complex_qr_solve (qr, tau, b, x) &
          bind(c, name='gsl_linalg_complex_QR_solve')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, tau, b, x
       integer(c_int) :: gsl_linalg_complex_qr_solve
     end function gsl_linalg_complex_qr_solve
     function gsl_linalg_qr_solve_r (qr, t, b, x) &
          bind(c, name='gsl_linalg_QR_solve_r')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, t, b, x
       integer(c_int) :: gsl_linalg_qr_solve_r
     end function gsl_linalg_qr_solve_r
     function gsl_linalg_complex_qr_solve_r (qr, t, b, x) &
          bind(c, name='gsl_linalg_complex_QR_solve_r')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, t, b, x
       integer(c_int) :: gsl_linalg_complex_qr_solve_r
     end function gsl_linalg_complex_qr_solve_r
     function gsl_linalg_qr_svx (qr, tau, x) &
          bind(c, name='gsl_linalg_QR_svx')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, tau, x
       integer(c_int) :: gsl_linalg_qr_svx
     end function gsl_linalg_qr_svx
     function gsl_linalg_complex_qr_svx (qr, tau, x) &
          bind(c, name='gsl_linalg_complex_QR_svx')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, tau, x
       integer(c_int) :: gsl_linalg_complex_qr_svx
     end function gsl_linalg_complex_qr_svx
     function gsl_linalg_qr_lssolve (qr, tau, b, x, residual) &
          bind(c, name='gsl_linalg_QR_lssolve')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, tau, b, x, residual
       integer(c_int) :: gsl_linalg_qr_lssolve
     end function gsl_linalg_qr_lssolve
     function gsl_linalg_complex_qr_lssolve (qr, tau, b, x, residual) &
          bind(c, name='gsl_linalg_complex_QR_lssolve')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, tau, b, x, residual
       integer(c_int) :: gsl_linalg_complex_qr_lssolve
     end function gsl_linalg_complex_qr_lssolve
     function gsl_linalg_qr_lssolve_r (qr, t, b, x, work) &
          bind(c, name='gsl_linalg_QR_lssolve_r')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, t, b, x, work
       integer(c_int) :: gsl_linalg_qr_lssolve_r
     end function gsl_linalg_qr_lssolve_r
     function gsl_linalg_complex_qr_lssolve_r (qr, t, b, x, work) &
          bind(c, name='gsl_linalg_complex_QR_lssolve_r')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, t, b, x, work
       integer(c_int) :: gsl_linalg_complex_qr_lssolve_r
     end function gsl_linalg_complex_qr_lssolve_r
     function gsl_linalg_qr_qtvec (qr, tau, v) &
          bind(c, name='gsl_linalg_QR_QTvec')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, tau, v
       integer(c_int) :: gsl_linalg_qr_qtvec
     end function gsl_linalg_qr_qtvec
     function gsl_linalg_complex_qr_qhvec (qr, tau, v) &
          bind(c, name='gsl_linalg_complex_QR_QHvec')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, tau, v
       integer(c_int) :: gsl_linalg_complex_qr_qhvec
     end function gsl_linalg_complex_qr_qhvec
     function gsl_linalg_qr_qtvec_r (qr, t, v, work) &
          bind(c, name='gsl_linalg_QR_QTvec_r')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, t, v,work
       integer(c_int) :: gsl_linalg_qr_qtvec_r
     end function gsl_linalg_qr_qtvec_r
     function gsl_linalg_complex_qr_qhvec_r (qr, t, v, work) &
          bind(c, name='gsl_linalg_complex_QR_QHvec_r')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, t, v,work
       integer(c_int) :: gsl_linalg_complex_qr_qhvec_r
     end function gsl_linalg_complex_qr_qhvec_r
     function gsl_linalg_qr_qvec (qr, tau, v) &
          bind(c, name='gsl_linalg_QR_Qvec')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, tau, v
       integer(c_int) :: gsl_linalg_qr_qvec
     end function gsl_linalg_qr_qvec
     function gsl_linalg_complex_qr_qvec (qr, tau, v) &
          bind(c, name='gsl_linalg_complex_QR_Qvec')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, tau, v
       integer(c_int) :: gsl_linalg_complex_qr_qvec
     end function gsl_linalg_complex_qr_qvec
     function gsl_linalg_qr_qtmat (qr, tau, a) &
          bind(c, name='gsl_linalg_QR_QTmat')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, tau, a
       integer(c_int) :: gsl_linalg_qr_qtmat
     end function gsl_linalg_qr_qtmat
     function gsl_linalg_qr_qtmat_r (qr, t, a, work) &
          bind(c, name='gsl_linalg_QR_QTmat_r')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, t, a, work
       integer(c_int) :: gsl_linalg_qr_qtmat_r
     end function gsl_linalg_qr_qtmat_r
     function gsl_linalg_qr_rsolve (qr, b, x) &
          bind(c, name='gsl_linalg_QR_Rsolve')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, b, x
       integer(c_int) :: gsl_linalg_qr_rsolve
     end function gsl_linalg_qr_rsolve
     function gsl_linalg_qr_rsvx (qr, x) &
          bind(c, name='gsl_linalg_QR_Rsvx')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, x
       integer(c_int) :: gsl_linalg_qr_rsvx
     end function gsl_linalg_qr_rsvx
     function gsl_linalg_qr_unpack (qr, tau, q, r) &
          bind(c, name='gsl_linalg_QR_unpack')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, tau, q, r
       integer(c_int) :: gsl_linalg_qr_unpack
     end function gsl_linalg_qr_unpack
     function gsl_linalg_qr_unpack_r (qr, t, q, r) &
          bind(c, name='gsl_linalg_QR_unpack_r')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, t, q, r
       integer(c_int) :: gsl_linalg_qr_unpack_r
     end function gsl_linalg_qr_unpack_r
     function gsl_linalg_complex_qr_unpack_r (qr, t, q, r) &
          bind(c, name='gsl_linalg_complex_QR_unpack_r')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, t, q, r
       integer(c_int) :: gsl_linalg_complex_qr_unpack_r
     end function gsl_linalg_complex_qr_unpack_r
     function gsl_linalg_qr_qrsolve (q, r, b, x) &
          bind(c, name='gsl_linalg_QR_QRsolve')
       import :: c_ptr, c_int
       type(c_ptr), value :: q, r, b, x
       integer(c_int) :: gsl_linalg_qr_qrsolve
     end function gsl_linalg_qr_qrsolve
     function gsl_linalg_qr_update (q, r, w, v) &
          bind(c, name='gsl_linalg_QR_update')
       import :: c_ptr, c_int
       type(c_ptr), value :: q, r, w, v
       integer(c_int) :: gsl_linalg_qr_update
     end function gsl_linalg_qr_update
     function gsl_linalg_r_solve (r, b, x) &
          bind(c, name='gsl_linalg_R_solve')
       import :: c_ptr, c_int
       type(c_ptr), value :: r, b, x
       integer(c_int) :: gsl_linalg_r_solve
     end function gsl_linalg_r_solve
     function gsl_linalg_r_svx (r, x) &
          bind(c, name='gsl_linalg_R_svx')
       import :: c_ptr, c_int
       type(c_ptr), value :: r, x
       integer(c_int) :: gsl_linalg_r_svx
     end function gsl_linalg_r_svx
     function gsl_linalg_qr_ur_decomp(u, a, t) &
          bind(c, name='gsl_linalg_QR_UR_decomp')
       import :: c_ptr, c_int
       type(c_ptr), value :: u, a, t
       integer(c_int) :: gsl_linalg_qr_ur_decomp
     end function gsl_linalg_qr_ur_decomp
     function gsl_linalg_qr_uu_decomp(u1, u2, t) &
          bind(c, name='gsl_linalg_QR_UU_decomp')
       import :: c_ptr, c_int
       type(c_ptr), value :: u1, u2, t
       integer(c_int) :: gsl_linalg_qr_uu_decomp
     end function gsl_linalg_qr_uu_decomp
     function gsl_linalg_qr_uu_lssolve(r, y, t, b, x, work) &
          bind(c, name='gsl_linalg_QR_UU_lssolve')
       import :: c_ptr, c_int
       type(c_ptr), value :: r, y, t, b, x, work
       integer(c_int) :: gsl_linalg_qr_uu_lssolve
     end function gsl_linalg_qr_uu_lssolve
     function gsl_linalg_qr_uu_qtvec(y, t, b, work) &
          bind(c, name='gsl_linalg_QR_UU_QTvec')
       import :: c_ptr, c_int
       type(c_ptr), value :: y, t, b, work
       integer(c_int) :: gsl_linalg_qr_uu_qtvec
     end function gsl_linalg_qr_uu_qtvec
     function gsl_linalg_qr_uz_decomp(u, a, t) &
          bind(c, name='gsl_linalg_QR_UZ_decomp')
       import :: c_ptr, c_int
       type(c_ptr), value :: u, a, t
       integer(c_int) :: gsl_linalg_qr_uz_decomp
     end function gsl_linalg_qr_uz_decomp
     function gsl_linalg_qr_ud_decomp(u, d, y, t) &
          bind(c, name='gsl_linalg_QR_UD_decomp')
       import :: c_ptr, c_int
       type(c_ptr), value :: u, d, y, t
       integer(c_int) :: gsl_linalg_qr_ud_decomp
     end function gsl_linalg_qr_ud_decomp
     function gsl_linalg_qr_ud_lssolve(r, y, t, b, x, work) &
          bind(c, name='gsl_linalg_QR_UD_lssolve')
       import :: c_ptr, c_int
       type(c_ptr), value :: r, y, t, b, x, work
       integer(c_int) :: gsl_linalg_qr_ud_lssolve
     end function gsl_linalg_qr_ud_lssolve
     function gsl_linalg_qrpt_decomp (a, tau, p, signum, norm) &
          bind(c, name='gsl_linalg_QRPT_decomp')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, tau, p, norm
       integer(c_int), intent(out) :: signum
       integer(c_int) :: gsl_linalg_qrpt_decomp
     end function gsl_linalg_qrpt_decomp
     function gsl_linalg_qrpt_decomp2 (a, q, r, tau, p, signum, norm) &
          bind(c, name='gsl_linalg_QRPT_decomp2')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, q, r, tau, p, norm
       integer(c_int), intent(out) :: signum
       integer(c_int) :: gsl_linalg_qrpt_decomp2
     end function gsl_linalg_qrpt_decomp2
     function gsl_linalg_qrpt_solve (qr, tau, p, b, x) &
          bind(c, name='gsl_linalg_QRPT_solve')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, tau, p, b, x
       integer(c_int) :: gsl_linalg_qrpt_solve
     end function gsl_linalg_qrpt_solve
     function gsl_linalg_qrpt_svx (qr, tau, p, x) &
          bind(c, name='gsl_linalg_QRPT_svx')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, tau, p, x
       integer(c_int) :: gsl_linalg_qrpt_svx
     end function gsl_linalg_qrpt_svx
     function gsl_linalg_qrpt_lssolve (qr, tau, p, b, x, r) &
          bind(c, name='gsl_linalg_QRPT_lssolve')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, tau, p, b, x, r
       integer(c_int) :: gsl_linalg_qrpt_lssolve
     end function gsl_linalg_qrpt_lssolve
     function gsl_linalg_qrpt_lssolve2 (qr, tau, p, b, rank, x, r) &
          bind(c, name='gsl_linalg_QRPT_lssolve2')
       import :: c_ptr, c_int, c_size_t
       type(c_ptr), value :: qr, tau, p, b, x, r
       integer(c_size_t), value :: rank
       integer(c_int) :: gsl_linalg_qrpt_lssolve2
     end function gsl_linalg_qrpt_lssolve2
     function gsl_linalg_qrpt_qrsolve (q, r, p, b, x) &
          bind(c, name='gsl_linalg_QRPT_QRsolve')
       import :: c_ptr, c_int
       type(c_ptr), value :: q, r, p, b, x
       integer(c_int) :: gsl_linalg_qrpt_qrsolve
     end function gsl_linalg_qrpt_qrsolve
     function gsl_linalg_qrpt_update (q, r, p, w, v) &
          bind(c, name='gsl_linalg_QRPT_update')
       import :: c_ptr, c_int
       type(c_ptr), value :: q, r, p, w, v
       integer(c_int) :: gsl_linalg_qrpt_update
     end function gsl_linalg_qrpt_update
     function gsl_linalg_qrpt_rsolve (qr, p, b, x) &
          bind(c, name='gsl_linalg_QRPT_Rsolve')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, p, b, x
       integer(c_int) :: gsl_linalg_qrpt_rsolve
     end function gsl_linalg_qrpt_rsolve
     function gsl_linalg_qrpt_rsvx (qr, p, x) &
          bind(c, name='gsl_linalg_QRPT_Rsvx')
       import :: c_ptr, c_int
       type(c_ptr), value :: qr, p, x
       integer(c_int) :: gsl_linalg_qrpt_rsvx
     end function gsl_linalg_qrpt_rsvx
     function gsl_linalg_qrpt_rank (qr, tol) &
          bind(c, name='gsl_linalg_QRPT_rank')
       import :: c_ptr, c_size_t, c_double
       type(c_ptr), value :: qr
       real(c_double), value :: tol
       integer(c_size_t) :: gsl_linalg_qrpt_rank
     end function gsl_linalg_qrpt_rank
     function gsl_linalg_qrpt_rcond (qr, rcond, wk) &
          bind(c, name='gsl_linalg_QRPT_rcond')
       import :: c_ptr, c_int, c_double
       type(c_ptr), value :: qr, wk
       real(c_double) :: rcond
       integer(c_int) :: gsl_linalg_qrpt_rcond
     end function gsl_linalg_qrpt_rcond
     !
     ! LQ and QL decompositions
     !
     integer(c_int) function gsl_linalg_lq_decomp(a, tau) &
          bind(c, name='gsl_linalg_LQ_decomp')
       import :: c_int, c_ptr
       type(c_ptr), value :: a, tau
     end function gsl_linalg_lq_decomp
     integer(c_int) function gsl_linalg_lq_lssolve(lq, tau, b, x, residual) &
          bind(c, name='gsl_linalg_LQ_lssolve')
       import :: c_int, c_ptr
       type(c_ptr), value :: lq, tau, b, x, residual
     end function gsl_linalg_lq_lssolve
     integer(c_int) function gsl_linalg_lq_unpack(lq, tau, q, l) &
          bind(c, name='gsl_linalg_LQ_unpack')
       import :: c_int, c_ptr
       type(c_ptr), value :: lq, tau, q, l
     end function gsl_linalg_lq_unpack
     integer(c_int) function gsl_linalg_lq_qtvec(lq, tau, v) &
          bind(c, name='gsl_linalg_LQ_QTvec')
       import :: c_int, c_ptr
       type(c_ptr), value :: lq, tau, v
     end function gsl_linalg_lq_qtvec
     integer(c_int) function gsl_linalg_ql_decomp(a, tau) &
          bind(c, name='gsl_linalg_QL_decomp')
       import :: c_int, c_ptr
       type(c_ptr), value :: a, tau
     end function gsl_linalg_ql_decomp
     integer(c_int) function gsl_linalg_ql_unpack(ql, tau, q, l) &
          bind(c, name='gsl_linalg_QL_unpack')
       import :: c_int, c_ptr
       type(c_ptr), value :: ql, tau, q, l
     end function gsl_linalg_ql_unpack
     !  
     ! Complete orthogonal decomposition (COD)
     !
     function gsl_linalg_cod_decomp (a, tau_q, tau_z, p, rank, work) &
          bind(c, name='gsl_linalg_COD_decomp')
       import :: c_ptr, c_int, c_size_t
       type(c_ptr), value :: a, tau_q, tau_z, p, work
       integer(c_size_t) :: rank
       integer(c_int) :: gsl_linalg_cod_decomp
     end function gsl_linalg_cod_decomp
     function gsl_linalg_cod_decomp_e (a, tau_q, tau_z, p, tol, rank, work) &
          bind(c, name='gsl_linalg_COD_decomp_e')
       import :: c_ptr, c_int, c_size_t, c_double
       type(c_ptr), value :: a, tau_q, tau_z, p, work
       real(c_double), value :: tol
       integer(c_size_t) :: rank
       integer(c_int) :: gsl_linalg_cod_decomp_e
     end function gsl_linalg_cod_decomp_e
     function gsl_linalg_cod_lssolve (qrzt, tau_q, tau_z, p, rank, b, x, residual) &
          bind(c, name='gsl_linalg_COD_lssolve')
       import :: c_ptr, c_int, c_size_t
       type(c_ptr), value :: qrzt, tau_q, tau_z, p, b, x, residual
       integer(c_size_t), value :: rank
       integer(c_int) :: gsl_linalg_cod_lssolve
     end function gsl_linalg_cod_lssolve
     function gsl_linalg_cod_lssolve2 (lambda, qrzt, tau_q, tau_z, p, rank, b, &
          x, residual, s, work) bind(c, name='gsl_linalg_COD_lssolve2')
       import :: c_ptr, c_int, c_size_t, c_double
       real(c_double), value :: lambda
       type(c_ptr), value :: qrzt, tau_q, tau_z, p, b, x, residual, s, work
       integer(c_size_t), value :: rank
       integer(c_int) :: gsl_linalg_cod_lssolve2
     end function gsl_linalg_cod_lssolve2
     function gsl_linalg_cod_unpack (qrzt, tau_q, tau_z, p, rank, q, r, z) &
          bind(c, name='gsl_linalg_COD_unpack')
       import :: c_ptr, c_int, c_size_t
       type(c_ptr), value :: qrzt, tau_q, tau_z, p, q, r, z
       integer(c_size_t), value :: rank
       integer(c_int) :: gsl_linalg_cod_unpack
     end function gsl_linalg_cod_unpack
     function gsl_linalg_cod_matz (qrzt, tau_z, rank, a, work) &
          bind(c, name='gsl_linalg_COD_matZ')
       import :: c_ptr, c_int, c_size_t
       type(c_ptr), value :: qrzt, tau_z, a, work
       integer(c_size_t), value :: rank
       integer(c_int) :: gsl_linalg_cod_matz
     end function gsl_linalg_cod_matz
     !
     ! SVD
     !
     function gsl_linalg_sv_decomp (a, v, s, work) &
          bind(c, name='gsl_linalg_SV_decomp')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, v, s, work
       integer(c_int) :: gsl_linalg_sv_decomp
     end function gsl_linalg_sv_decomp
     function gsl_linalg_sv_decomp_mod (a, x, v, s, work) &
          bind(c, name='gsl_linalg_SV_decomp_mod')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, x, v, s, work
       integer(c_int) :: gsl_linalg_sv_decomp_mod
     end function gsl_linalg_sv_decomp_mod
     function gsl_linalg_sv_decomp_jacobi (a, v, s) &
          bind(c, name='gsl_linalg_SV_decomp_jacobi')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, v, s
       integer(c_int) :: gsl_linalg_sv_decomp_jacobi
     end function gsl_linalg_sv_decomp_jacobi
     function gsl_linalg_sv_solve (u, v, s, b, x) &
          bind(c, name='gsl_linalg_SV_solve')
       import :: c_ptr, c_int
       type(c_ptr), value :: u, v, s, b, x
       integer(c_int) :: gsl_linalg_sv_solve
     end function gsl_linalg_sv_solve
     function gsl_linalg_sv_leverage(u, h) &
          bind(c, name='gsl_linalg_SV_leverage')
       import :: c_ptr, c_int
       type(c_ptr), value :: u, h
       integer(c_int) :: gsl_linalg_sv_leverage
     end function gsl_linalg_sv_leverage
     !
     ! Cholesky
     !
     function gsl_linalg_cholesky_decomp1 (a) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a
       integer(c_int) :: gsl_linalg_cholesky_decomp1
     end function gsl_linalg_cholesky_decomp1
     function gsl_linalg_cholesky_decomp (a) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a
       integer(c_int) :: gsl_linalg_cholesky_decomp
     end function gsl_linalg_cholesky_decomp
     function gsl_linalg_complex_cholesky_decomp (a) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a
       integer(c_int) :: gsl_linalg_complex_cholesky_decomp
     end function gsl_linalg_complex_cholesky_decomp
     function gsl_linalg_cholesky_solve (chol, b, x) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: chol, b, x
       integer(c_int) :: gsl_linalg_cholesky_solve
     end function gsl_linalg_cholesky_solve
     function gsl_linalg_complex_cholesky_solve (chol, b, x) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: chol, b, x
       integer(c_int) :: gsl_linalg_complex_cholesky_solve
     end function gsl_linalg_complex_cholesky_solve
     function gsl_linalg_cholesky_svx (chol, x) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: chol, x
       integer(c_int) :: gsl_linalg_cholesky_svx
     end function gsl_linalg_cholesky_svx
     function gsl_linalg_complex_cholesky_svx (chol, x) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: chol, x
       integer(c_int) :: gsl_linalg_complex_cholesky_svx
     end function gsl_linalg_complex_cholesky_svx
     function gsl_linalg_cholesky_invert (chol) bind(c)
       import :: c_ptr, c_int
       integer(c_int) :: gsl_linalg_cholesky_invert
       type(c_ptr), value :: chol
     end function gsl_linalg_cholesky_invert
     function gsl_linalg_complex_cholesky_invert (chol) bind(c)
       import :: c_ptr, c_int
       integer(c_int) :: gsl_linalg_complex_cholesky_invert
       type(c_ptr), value :: chol
     end function gsl_linalg_complex_cholesky_invert
     function gsl_linalg_cholesky_decomp2 (a,s) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a,s
       integer(c_int) :: gsl_linalg_cholesky_decomp2
     end function gsl_linalg_cholesky_decomp2
     function gsl_linalg_cholesky_solve2 (chol, s, b, x) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: chol, s, b, x
       integer(c_int) :: gsl_linalg_cholesky_solve2
     end function gsl_linalg_cholesky_solve2
     function gsl_linalg_cholesky_svx2 (chol, s, x) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: chol, s, x
       integer(c_int) :: gsl_linalg_cholesky_svx2
     end function gsl_linalg_cholesky_svx2
     function gsl_linalg_cholesky_scale (a,s) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a,s
       integer(c_int) :: gsl_linalg_cholesky_scale
     end function gsl_linalg_cholesky_scale
     function gsl_linalg_cholesky_scale_apply (a,s) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a,s
       integer(c_int) :: gsl_linalg_cholesky_scale_apply
     end function gsl_linalg_cholesky_scale_apply
     function gsl_linalg_cholesky_rcond (chol, rcond, work) &
          bind(c)
       import :: c_ptr, c_int, c_double
       type(c_ptr), value :: chol, work
       real(c_double), intent(inout) :: rcond
       integer(c_int) :: gsl_linalg_cholesky_rcond
     end function gsl_linalg_cholesky_rcond
     !
     ! pivoted Cholesky
     !
     function gsl_linalg_pcholesky_decomp (a, p) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a, p
       integer(c_int) :: gsl_linalg_pcholesky_decomp
     end function gsl_linalg_pcholesky_decomp
     function gsl_linalg_pcholesky_solve (ldlt, p, b, x) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: ldlt, p, b, x
       integer(c_int) :: gsl_linalg_pcholesky_solve
     end function gsl_linalg_pcholesky_solve
     function gsl_linalg_pcholesky_svx (ldlt, p, x) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: ldlt, p, x
       integer(c_int) :: gsl_linalg_pcholesky_svx
     end function gsl_linalg_pcholesky_svx
     function gsl_linalg_pcholesky_invert (ldlt, p, ainv) bind(c)
       import :: c_ptr, c_int
       integer(c_int) :: gsl_linalg_pcholesky_invert
       type(c_ptr), value :: ldlt, p, ainv
     end function gsl_linalg_pcholesky_invert
     function gsl_linalg_pcholesky_decomp2 (a, p,s) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a, p,s
       integer(c_int) :: gsl_linalg_pcholesky_decomp2
     end function gsl_linalg_pcholesky_decomp2
     function gsl_linalg_pcholesky_solve2 (ldlt, p, s, b, x) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: ldlt, p, s, b, x
       integer(c_int) :: gsl_linalg_pcholesky_solve2
     end function gsl_linalg_pcholesky_solve2
     function gsl_linalg_pcholesky_svx2 (ldlt, p, s, x) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: ldlt, p, s, x
       integer(c_int) :: gsl_linalg_pcholesky_svx2
     end function gsl_linalg_pcholesky_svx2
     function gsl_linalg_pcholesky_rcond (ldlt, p, rcond, work) &
          bind(c)
       import :: c_ptr, c_int, c_double
       type(c_ptr), value :: ldlt, p, work
       real(c_double), intent(inout) :: rcond
       integer(c_int) :: gsl_linalg_pcholesky_rcond
     end function gsl_linalg_pcholesky_rcond
     !
     ! modified Cholesky
     !
     function gsl_linalg_mcholesky_decomp (a, p, e) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a, p, e
       integer(c_int) :: gsl_linalg_mcholesky_decomp
     end function gsl_linalg_mcholesky_decomp
     function gsl_linalg_mcholesky_solve (ldlt, p, b, x) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: ldlt, p, b, x
       integer(c_int) :: gsl_linalg_mcholesky_solve
     end function gsl_linalg_mcholesky_solve
     function gsl_linalg_mcholesky_svx (ldlt, p, x) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: ldlt, p, x
       integer(c_int) :: gsl_linalg_mcholesky_svx
     end function gsl_linalg_mcholesky_svx
     function gsl_linalg_mcholesky_invert (ldlt, p, ainv) bind(c)
       import :: c_ptr, c_int
       integer(c_int) :: gsl_linalg_mcholesky_invert
       type(c_ptr), value :: ldlt, p, ainv
     end function gsl_linalg_mcholesky_invert
     function gsl_linalg_mcholesky_rcond (ldlt, p, rcond, work) &
          bind(c)
       import :: c_ptr, c_int, c_double
       type(c_ptr), value :: ldlt, p, work
       real(c_double), intent(inout) :: rcond
       integer(c_int) :: gsl_linalg_mcholesky_rcond
     end function gsl_linalg_mcholesky_rcond
     !
     ! LDLT
     !
     function gsl_linalg_ldlt_decomp (a) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a
       integer(c_int) :: gsl_linalg_ldlt_decomp
     end function gsl_linalg_ldlt_decomp
     integer(c_int) function gsl_linalg_ldlt_solve(ldlt, b, x) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: ldlt, b, x
     end function gsl_linalg_ldlt_solve
     integer(c_int) function gsl_linalg_ldlt_svx(ldlt, x) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: ldlt, x
     end function gsl_linalg_ldlt_svx
     integer(c_int) function gsl_linalg_ldlt_rcond(ldlt, rcond, w) bind(c)
       import :: c_int, c_ptr, c_double
       type(c_ptr), value :: ldlt, w
       real(c_double) :: rcond
     end function gsl_linalg_ldlt_rcond

     !
     ! Tridiagonal Decomposition of Real Symmetric Matrices
     !
     function gsl_linalg_symmtd_decomp (a, tau) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a, tau
       integer(c_int) :: gsl_linalg_symmtd_decomp
     end function gsl_linalg_symmtd_decomp
     function gsl_linalg_symmtd_unpack (a, tau, q, diag, subdiag) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a, tau, q, diag, subdiag
       integer(c_int) :: gsl_linalg_symmtd_unpack
     end function gsl_linalg_symmtd_unpack
     function gsl_linalg_symmtd_unpack_t (a, diag, subdiag) &
          bind(c, name='gsl_linalg_symmtd_unpack_T')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, diag, subdiag
       integer(c_int) :: gsl_linalg_symmtd_unpack_t
     end function gsl_linalg_symmtd_unpack_t


     !
     ! Tridiagonal Decomposition of Hermitian Matrices
     !
     function gsl_linalg_hermtd_decomp (a, tau) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a, tau
       integer(c_int) :: gsl_linalg_hermtd_decomp
     end function gsl_linalg_hermtd_decomp
     function gsl_linalg_hermtd_unpack (a, tau, q, diag, subdiag) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a, tau, q, diag, subdiag
       integer(c_int) :: gsl_linalg_hermtd_unpack
     end function gsl_linalg_hermtd_unpack
     function gsl_linalg_hermtd_unpack_t (a, diag, subdiag) &
          bind(c, name='gsl_linalg_hermtd_unpack_T')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, diag, subdiag
       integer(c_int) :: gsl_linalg_hermtd_unpack_t
     end function gsl_linalg_hermtd_unpack_t
     !
     ! Hessenberg
     !
     function gsl_linalg_hessenberg_decomp (a, tau) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a, tau
       integer(c_int) :: gsl_linalg_hessenberg_decomp
     end function gsl_linalg_hessenberg_decomp
     function gsl_linalg_hessenberg_unpack (h, tau, u) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: h, tau, u
       integer(c_int) :: gsl_linalg_hessenberg_unpack
     end function gsl_linalg_hessenberg_unpack
     function gsl_linalg_hessenberg_unpack_accum (h, tau, v) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: h, tau, v
       integer(c_int) :: gsl_linalg_hessenberg_unpack_accum
     end function gsl_linalg_hessenberg_unpack_accum
     function gsl_linalg_hessenberg_set_zero (h) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: h
       integer(c_int) :: gsl_linalg_hessenberg_set_zero
     end function gsl_linalg_hessenberg_set_zero
     function gsl_linalg_hesstri_decomp (a, b, u, v, work) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a, b, u, v, work
       integer(c_int) :: gsl_linalg_hesstri_decomp
     end function gsl_linalg_hesstri_decomp
     !
     ! Bidiag
     !
     function gsl_linalg_bidiag_decomp (a, tau_u, tau_v) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a, tau_u, tau_v
       integer(c_int) :: gsl_linalg_bidiag_decomp
     end function gsl_linalg_bidiag_decomp
     function gsl_linalg_bidiag_unpack (a, tau_u, u, tau_v, v, diag, &
          superdiag) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a, tau_u, tau_v, u, v, diag, superdiag
       integer(c_int) :: gsl_linalg_bidiag_unpack
     end function gsl_linalg_bidiag_unpack
     function gsl_linalg_bidiag_unpack2 (a, tau_u, tau_v, v) &
          bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a, tau_u, tau_v, v
       integer(c_int) :: gsl_linalg_bidiag_unpack2
     end function gsl_linalg_bidiag_unpack2
     function gsl_linalg_bidiag_unpack_b (a, diag, superdiag) &
          bind(c, name='gsl_linalg_bidiag_unpack_B')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, diag, superdiag
       integer(c_int) :: gsl_linalg_bidiag_unpack_b
     end function gsl_linalg_bidiag_unpack_b
     !
     ! Householder
     !
     function gsl_linalg_householder_transform (v) bind(c)
       import :: c_ptr, c_double
       type(c_ptr), value :: v
       real(c_double) :: gsl_linalg_householder_transform
     end function gsl_linalg_householder_transform
     function gsl_linalg_complex_householder_transform (v) bind(c)
       import :: c_ptr, c_double_complex
       type(c_ptr), value :: v
       complex(c_double_complex) :: gsl_linalg_complex_householder_transform
     end function gsl_linalg_complex_householder_transform
     function gsl_linalg_householder_hm (tau, v, a) bind(c)
       import :: c_ptr, c_double, c_int
       real(c_double), value :: tau
       type(c_ptr), value :: v, a
       integer(c_int) :: gsl_linalg_householder_hm
     end function gsl_linalg_householder_hm
     function gsl_linalg_complex_householder_hm (tau, v, a) bind(c)
       import :: c_ptr, c_int, c_double_complex
       complex(c_double_complex), value :: tau
       type(c_ptr), value :: v, a
       integer(c_int) :: gsl_linalg_complex_householder_hm
     end function gsl_linalg_complex_householder_hm
     function gsl_linalg_householder_mh (tau, v, a) bind(c)
       import :: c_ptr, c_double, c_int
       real(c_double), value :: tau
       type(c_ptr), value :: v, a
       integer(c_int) :: gsl_linalg_householder_mh
     end function gsl_linalg_householder_mh
     function gsl_linalg_complex_householder_mh (tau, v, a) bind(c)
       import :: c_ptr, c_int, c_double_complex
       complex(c_double_complex), value :: tau
       type(c_ptr), value :: v, a
       integer(c_int) :: gsl_linalg_complex_householder_mh
     end function gsl_linalg_complex_householder_mh
     function gsl_linalg_householder_hv (tau, v, w) bind(c)
       import :: c_ptr, c_double, c_int
       real(c_double), value :: tau
       type(c_ptr), value :: v, w
       integer(c_int) :: gsl_linalg_householder_hv
     end function gsl_linalg_householder_hv
     function gsl_linalg_complex_householder_hv (tau, v, w) bind(c)
       import :: c_ptr, c_int, c_double_complex
       complex(c_double_complex), value :: tau
       type(c_ptr), value :: v, w
       integer(c_int) :: gsl_linalg_complex_householder_hv
     end function gsl_linalg_complex_householder_hv
     function gsl_linalg_hh_solve (a, b, x) &
          bind(c, name='gsl_linalg_HH_solve')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, b, x
       integer(c_int) :: gsl_linalg_hh_solve
     end function gsl_linalg_hh_solve
     function gsl_linalg_hh_svx (a, x) &
          bind(c, name='gsl_linalg_HH_svx')
       import :: c_ptr, c_int
       type(c_ptr), value :: a, x
       integer(c_int) :: gsl_linalg_hh_svx
     end function gsl_linalg_hh_svx
     !
     ! Tridiagonal
     !
     function gsl_linalg_solve_tridiag(diag, e, f, b, x) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: diag, e, f, b, x
       integer(c_int) :: gsl_linalg_solve_tridiag
     end function gsl_linalg_solve_tridiag
     function gsl_linalg_solve_symm_tridiag(diag, e, b, x) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: diag, e, b, x
       integer(c_int) :: gsl_linalg_solve_symm_tridiag
     end function gsl_linalg_solve_symm_tridiag
     function gsl_linalg_solve_cyc_tridiag(diag, e, f, b, x) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: diag, e, f, b, x
       integer(c_int) :: gsl_linalg_solve_cyc_tridiag
     end function gsl_linalg_solve_cyc_tridiag
     function gsl_linalg_solve_symm_cyc_tridiag(diag, e, b, x) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: diag, e, b, x
       integer(c_int) :: gsl_linalg_solve_symm_cyc_tridiag
     end function gsl_linalg_solve_symm_cyc_tridiag
     function gsl_linalg_qr_matq(QR, tau, A) &
          bind(c, name='gsl_linalg_QR_matQ')
       import :: c_ptr, c_int
       type(c_ptr), value :: QR, tau, A
       integer(c_int) :: gsl_linalg_qr_matq
     end function gsl_linalg_qr_matq
     subroutine gsl_linalg_givens(a, b, c, s) bind(c)
       import :: c_double
       real(c_double), value :: a, b
       real(c_double):: c, s
     end subroutine gsl_linalg_givens
     subroutine gsl_linalg_givens_gv(v, i, j, c, s) bind(c)
       import :: c_ptr, c_size_t, c_double
       type(c_ptr), value :: v
       integer(c_size_t), value :: i, j
       real(c_double), value :: c, s
     end subroutine gsl_linalg_givens_gv
     !
     ! Triangular matrices 
     ! 
     integer(c_int) function gsl_linalg_tri_invert(uplo, diag, t) bind(c)
       import :: c_int, c_ptr
       integer(c_int), value :: uplo, diag
       type(c_ptr), value :: t
     end function gsl_linalg_tri_invert
     integer(c_int) function gsl_linalg_complex_tri_invert(uplo, diag, t) bind(c)
       import :: c_int, c_ptr
       integer(c_int), value :: uplo, diag
       type(c_ptr), value :: t
     end function gsl_linalg_complex_tri_invert
     integer(c_int) function gsl_linalg_tri_ltl(l) bind(c, name='gsl_linalg_tri_LTL')
       import :: c_int, c_ptr
       type(c_ptr), value :: l
     end function gsl_linalg_tri_ltl
     integer(c_int) function gsl_linalg_complex_tri_lhl(l) bind(c, name='gsl_linalg_complex_tri_LHL')
       import :: c_int, c_ptr
       type(c_ptr), value :: l
     end function gsl_linalg_complex_tri_lhl
     integer(c_int) function gsl_linalg_tri_ul(lu) bind(c, name='gsl_linalg_tri_UL')
       import :: c_int, c_ptr
       type(c_ptr), value :: lu
     end function gsl_linalg_tri_ul
     integer(c_int) function gsl_linalg_complex_tri_ul(lu) bind(c, name='gsl_linalg_complex_tri_UL')
       import :: c_int, c_ptr
       type(c_ptr), value :: lu
     end function gsl_linalg_complex_tri_ul
     integer(c_int) function gsl_linalg_tri_rcond(uplo, a, rcond, work) bind(c)
       import :: c_int, c_ptr, c_double
       integer(c_int), value :: uplo
       real(c_double), intent(inout) :: rcond
       type(c_ptr), value :: a, work
     end function gsl_linalg_tri_rcond
     !
     ! Triangular matrices (legacy)
     ! 
     integer(c_int) function gsl_linalg_tri_upper_invert(t) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: t
     end function gsl_linalg_tri_upper_invert
     integer(c_int) function gsl_linalg_tri_lower_invert(t) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: t
     end function gsl_linalg_tri_lower_invert
     integer(c_int) function gsl_linalg_tri_upper_unit_invert(t) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: t
     end function gsl_linalg_tri_upper_unit_invert
     integer(c_int) function gsl_linalg_tri_lower_unit_invert(t) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: t
     end function gsl_linalg_tri_lower_unit_invert
     integer(c_int) function gsl_linalg_tri_upper_rcond(t, rcond, work) bind(c)
       import :: c_int, c_double, c_ptr
       type(c_ptr), value :: t, work
       real(c_double) :: rcond
     end function gsl_linalg_tri_upper_rcond
     integer(c_int) function gsl_linalg_tri_lower_rcond(t, rcond, work) bind(c)
       import :: c_int, c_double, c_ptr
       type(c_ptr), value :: t, work
       real(c_double) :: rcond
     end function gsl_linalg_tri_lower_rcond
     !
     ! Banded systems
     !
     integer(c_int) function gsl_linalg_cholesky_band_decomp(a) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a
     end function gsl_linalg_cholesky_band_decomp
     integer(c_int) function gsl_linalg_cholesky_band_solve(llt, b, x) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: llt, b, x
     end function gsl_linalg_cholesky_band_solve
     integer(c_int) function gsl_linalg_cholesky_band_solvem(llt, b, x) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: llt, b, x
     end function gsl_linalg_cholesky_band_solvem
     integer(c_int) function gsl_linalg_cholesky_band_svx(llt, x) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: llt, x
     end function gsl_linalg_cholesky_band_svx
     integer(c_int) function gsl_linalg_cholesky_band_svxm(llt, x) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: llt, x
     end function gsl_linalg_cholesky_band_svxm
     integer(c_int) function gsl_linalg_cholesky_band_invert(llt, ainv) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: llt, ainv
     end function gsl_linalg_cholesky_band_invert
     integer(c_int) function gsl_linalg_cholesky_band_unpack(llt, l) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: llt, l
     end function gsl_linalg_cholesky_band_unpack
     integer(c_int) function gsl_linalg_cholesky_band_scale(a, s) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a, s
     end function gsl_linalg_cholesky_band_scale
     integer(c_int) function gsl_linalg_cholesky_band_scale_apply(a, s) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a, s
     end function gsl_linalg_cholesky_band_scale_apply
     integer(c_int) function gsl_linalg_cholesky_band_rcond(llt, rcond, w) bind(c)
       import :: c_int, c_ptr, c_double
       type(c_ptr), value :: llt, w
       real(c_double) :: rcond
     end function gsl_linalg_cholesky_band_rcond
     integer(c_int) function gsl_linalg_ldlt_band_decomp(a) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a
     end function gsl_linalg_ldlt_band_decomp
     integer(c_int) function gsl_linalg_ldlt_band_solve(ldlt, b, x) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: ldlt, b, x
     end function gsl_linalg_ldlt_band_solve
     integer(c_int) function gsl_linalg_ldlt_band_svx(ldlt, x) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: ldlt, x
     end function gsl_linalg_ldlt_band_svx
     integer(c_int) function gsl_linalg_ldlt_band_unpack(ldlt, l, d) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: ldlt, l, d
     end function gsl_linalg_ldlt_band_unpack
     integer(c_int) function gsl_linalg_ldlt_band_rcond(ldlt, rcond, w) bind(c)
       import :: c_int, c_ptr, c_double
       type(c_ptr), value :: ldlt, w
       real(c_double) :: rcond
     end function gsl_linalg_ldlt_band_rcond
     !
     ! Balancing
     !
     function gsl_linalg_balance_matrix (a, d) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: a, d
       integer(c_int) :: gsl_linalg_balance_matrix
     end function gsl_linalg_balance_matrix

  end interface

contains
!
! LU
!
  function fgsl_linalg_lu_decomp(a, p, signum)
    type(fgsl_matrix)  :: a
    type(fgsl_permutation) :: p
    integer(fgsl_int) :: signum
    integer(fgsl_int) :: fgsl_linalg_lu_decomp
    fgsl_linalg_lu_decomp = gsl_linalg_lu_decomp(a%gsl_matrix, &
         p%gsl_permutation, signum)
  end function fgsl_linalg_lu_decomp
  function fgsl_linalg_complex_lu_decomp(a, p, signum)
    type(fgsl_matrix_complex)  :: a
    type(fgsl_permutation) :: p
    integer(fgsl_int) :: signum
    integer(fgsl_int) :: fgsl_linalg_complex_lu_decomp
    fgsl_linalg_complex_lu_decomp = gsl_linalg_complex_lu_decomp( &
         a%gsl_matrix_complex, p%gsl_permutation, signum)
  end function fgsl_linalg_complex_lu_decomp
  function fgsl_linalg_lu_solve(lu, p, b, x)
    type(fgsl_matrix), intent(in) :: lu
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_lu_solve
    fgsl_linalg_lu_solve = gsl_linalg_lu_solve(lu%gsl_matrix, &
         p%gsl_permutation, b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_lu_solve
  function fgsl_linalg_complex_lu_solve(lu, p, b, x)
    type(fgsl_matrix_complex), intent(in) :: lu
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector_complex), intent(in) :: b
    type(fgsl_vector_complex), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_complex_lu_solve
    fgsl_linalg_complex_lu_solve = gsl_linalg_complex_lu_solve( &
         lu%gsl_matrix_complex, p%gsl_permutation, b%gsl_vector_complex, &
         x%gsl_vector_complex)
  end function fgsl_linalg_complex_lu_solve
  function fgsl_linalg_lu_svx(lu, p, x)
    type(fgsl_matrix), intent(in) :: lu
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_lu_svx
    fgsl_linalg_lu_svx = gsl_linalg_lu_svx(lu%gsl_matrix, &
         p%gsl_permutation, x%gsl_vector)
  end function fgsl_linalg_lu_svx
  function fgsl_linalg_complex_lu_svx(lu, p, x)
    type(fgsl_matrix_complex), intent(in) :: lu
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector_complex), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_complex_lu_svx
    fgsl_linalg_complex_lu_svx = gsl_linalg_complex_lu_svx(&
         lu%gsl_matrix_complex, p%gsl_permutation, x%gsl_vector_complex)
  end function fgsl_linalg_complex_lu_svx
  function fgsl_linalg_lu_refine (a, lu, p, b, x, residual)
   type(fgsl_matrix), intent(in) :: a, lu
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x
    type(fgsl_vector), intent(inout) :: residual
    integer(fgsl_int) :: fgsl_linalg_lu_refine
    fgsl_linalg_lu_refine = gsl_linalg_lu_refine (a%gsl_matrix, &
         lu%gsl_matrix, p%gsl_permutation, b%gsl_vector, x%gsl_vector, &
         residual%gsl_vector)
  end function fgsl_linalg_lu_refine
  function fgsl_linalg_complex_lu_refine (a, lu, p, b, x, residual)
   type(fgsl_matrix_complex), intent(in) :: a, lu
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector_complex), intent(in) :: b
    type(fgsl_vector_complex), intent(inout) :: x
    type(fgsl_vector_complex), intent(inout) :: residual
    integer(fgsl_int) :: fgsl_linalg_complex_lu_refine
    fgsl_linalg_complex_lu_refine = gsl_linalg_complex_lu_refine( &
         a%gsl_matrix_complex, lu%gsl_matrix_complex, p%gsl_permutation, &
         b%gsl_vector_complex, x%gsl_vector_complex, &
         residual%gsl_vector_complex)
  end function fgsl_linalg_complex_lu_refine
  function fgsl_linalg_lu_invert(lu, p, inverse)
    type(fgsl_matrix), intent(in) :: lu
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_matrix), intent(inout) :: inverse
    integer(fgsl_int) :: fgsl_linalg_lu_invert
    fgsl_linalg_lu_invert = gsl_linalg_lu_invert(lu%gsl_matrix, &
         p%gsl_permutation, inverse%gsl_matrix)
  end function fgsl_linalg_lu_invert
  function fgsl_linalg_complex_lu_invert(lu, p, inverse)
    type(fgsl_matrix_complex), intent(in) :: lu
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_matrix_complex), intent(inout) :: inverse
    integer(fgsl_int) :: fgsl_linalg_complex_lu_invert
    fgsl_linalg_complex_lu_invert = gsl_linalg_complex_lu_invert(&
         lu%gsl_matrix_complex, p%gsl_permutation, inverse%gsl_matrix_complex)
  end function fgsl_linalg_complex_lu_invert
  function fgsl_linalg_lu_invx(lu, p)
    type(fgsl_matrix), intent(in) :: lu
    type(fgsl_permutation), intent(in) :: p
    integer(fgsl_int) :: fgsl_linalg_lu_invx
    fgsl_linalg_lu_invx = gsl_linalg_lu_invx(lu%gsl_matrix, &
         p%gsl_permutation)
  end function fgsl_linalg_lu_invx
  function fgsl_linalg_complex_lu_invx(lu, p)
    type(fgsl_matrix_complex), intent(inout) :: lu
    type(fgsl_permutation), intent(in) :: p
    integer(fgsl_int) :: fgsl_linalg_complex_lu_invx
    fgsl_linalg_complex_lu_invx = gsl_linalg_complex_lu_invx(&
         lu%gsl_matrix_complex, p%gsl_permutation)
  end function fgsl_linalg_complex_lu_invx
  function fgsl_linalg_lu_det(lu, signum)
    type(fgsl_matrix), intent(in) :: lu
    integer(fgsl_int), intent(in) :: signum
    real(fgsl_double) :: fgsl_linalg_lu_det
    fgsl_linalg_lu_det = gsl_linalg_lu_det(lu%gsl_matrix, signum)
  end function fgsl_linalg_lu_det
  function fgsl_linalg_complex_lu_det(lu, signum)
    type(fgsl_matrix_complex), intent(in) :: lu
    integer(fgsl_int), intent(in) :: signum
    complex(fgsl_double_complex) :: fgsl_linalg_complex_lu_det
    fgsl_linalg_complex_lu_det = gsl_linalg_complex_lu_det(&
         lu%gsl_matrix_complex, signum)
  end function fgsl_linalg_complex_lu_det
  function fgsl_linalg_lu_lndet(lu)
    type(fgsl_matrix), intent(in) :: lu
    real(fgsl_double) :: fgsl_linalg_lu_lndet
    fgsl_linalg_lu_lndet = gsl_linalg_lu_lndet(lu%gsl_matrix)
  end function fgsl_linalg_lu_lndet
  function fgsl_linalg_complex_lu_lndet(lu)
    type(fgsl_matrix_complex), intent(in) :: lu
    real(fgsl_double) :: fgsl_linalg_complex_lu_lndet
    fgsl_linalg_complex_lu_lndet = gsl_linalg_complex_lu_lndet(&
         lu%gsl_matrix_complex)
  end function fgsl_linalg_complex_lu_lndet
  function fgsl_linalg_lu_sgndet(lu, signum)
    type(fgsl_matrix), intent(in) :: lu
    integer(fgsl_int), intent(in) :: signum
    integer(fgsl_int) :: fgsl_linalg_lu_sgndet
    fgsl_linalg_lu_sgndet = gsl_linalg_lu_sgndet(lu%gsl_matrix, signum)
  end function fgsl_linalg_lu_sgndet
  function fgsl_linalg_complex_lu_sgndet(lu, signum)
    type(fgsl_matrix_complex), intent(in) :: lu
    integer(fgsl_int), intent(in) :: signum
    complex(fgsl_double_complex) :: fgsl_linalg_complex_lu_sgndet
    fgsl_linalg_complex_lu_sgndet = gsl_linalg_complex_lu_sgndet(&
         lu%gsl_matrix_complex, signum)
  end function fgsl_linalg_complex_lu_sgndet
!
! QR
!
  function fgsl_linalg_qr_decomp (a, tau)
    type(fgsl_matrix), intent(inout) :: a
    type(fgsl_vector), intent(inout) :: tau
    integer(fgsl_int) :: fgsl_linalg_qr_decomp
    fgsl_linalg_qr_decomp = gsl_linalg_qr_decomp (a%gsl_matrix, tau%gsl_vector)
  end function fgsl_linalg_qr_decomp
  function fgsl_linalg_complex_qr_decomp (a, tau)
    type(fgsl_matrix_complex), intent(inout) :: a
    type(fgsl_vector_complex), intent(inout) :: tau
    integer(fgsl_int) :: fgsl_linalg_complex_qr_decomp
    fgsl_linalg_complex_qr_decomp = gsl_linalg_complex_qr_decomp (a%gsl_matrix_complex, tau%gsl_vector_complex)
  end function fgsl_linalg_complex_qr_decomp
  function fgsl_linalg_qr_decomp_r (a, t)
    type(fgsl_matrix), intent(inout) :: a
    type(fgsl_matrix), intent(inout) :: t
    integer(fgsl_int) :: fgsl_linalg_qr_decomp_r
    fgsl_linalg_qr_decomp_r = gsl_linalg_qr_decomp_r (a%gsl_matrix, t%gsl_matrix)
  end function fgsl_linalg_qr_decomp_r
  function fgsl_linalg_complex_qr_decomp_r (a, t)
    type(fgsl_matrix_complex), intent(inout) :: a
    type(fgsl_matrix_complex), intent(inout) :: t
    integer(fgsl_int) :: fgsl_linalg_complex_qr_decomp_r
    fgsl_linalg_complex_qr_decomp_r = gsl_linalg_complex_qr_decomp_r (a%gsl_matrix_complex, t%gsl_matrix_complex)
  end function fgsl_linalg_complex_qr_decomp_r
  function fgsl_linalg_qr_solve (qr, tau, b, x)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_vector), intent(in) :: tau, b
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_qr_solve
    fgsl_linalg_qr_solve = gsl_linalg_qr_solve(qr%gsl_matrix, tau%gsl_vector, &
         b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_qr_solve
  function fgsl_linalg_complex_qr_solve (qr, tau, b, x)
    type(fgsl_matrix_complex), intent(in) :: qr
    type(fgsl_vector_complex), intent(in) :: tau, b
    type(fgsl_vector_complex), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_complex_qr_solve
    fgsl_linalg_complex_qr_solve = gsl_linalg_complex_qr_solve(qr%gsl_matrix_complex, tau%gsl_vector_complex, &
         b%gsl_vector_complex, x%gsl_vector_complex)
  end function fgsl_linalg_complex_qr_solve
  function fgsl_linalg_qr_solve_r (qr, t, b, x)
    type(fgsl_matrix), intent(in) :: qr, t
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_qr_solve_r
    fgsl_linalg_qr_solve_r = gsl_linalg_qr_solve_r(qr%gsl_matrix, t%gsl_matrix, &
         b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_qr_solve_r
  function fgsl_linalg_complex_qr_solve_r (qr, t, b, x)
    type(fgsl_matrix_complex), intent(in) :: qr, t
    type(fgsl_vector_complex), intent(in) :: b
    type(fgsl_vector_complex), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_complex_qr_solve_r
    fgsl_linalg_complex_qr_solve_r = gsl_linalg_complex_qr_solve_r(qr%gsl_matrix_complex, t%gsl_matrix_complex, &
         b%gsl_vector_complex, x%gsl_vector_complex)
  end function fgsl_linalg_complex_qr_solve_r
  function fgsl_linalg_qr_svx (qr, tau, x)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_vector), intent(in) :: tau
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_qr_svx
    fgsl_linalg_qr_svx = gsl_linalg_qr_svx (qr%gsl_matrix, tau%gsl_vector, &
         x%gsl_vector)
  end function fgsl_linalg_qr_svx
  function fgsl_linalg_complex_qr_svx (qr, tau, x)
    type(fgsl_matrix_complex), intent(in) :: qr
    type(fgsl_vector_complex), intent(in) :: tau
    type(fgsl_vector_complex), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_complex_qr_svx
    fgsl_linalg_complex_qr_svx = gsl_linalg_complex_qr_svx (qr%gsl_matrix_complex, tau%gsl_vector_complex, &
         x%gsl_vector_complex)
  end function fgsl_linalg_complex_qr_svx
  function fgsl_linalg_qr_lssolve (qr, tau, b, x, residual)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_vector), intent(in) :: tau, b
    type(fgsl_vector), intent(inout) :: x, residual
    integer(fgsl_int) :: fgsl_linalg_qr_lssolve
    fgsl_linalg_qr_lssolve = gsl_linalg_qr_lssolve (qr%gsl_matrix, tau%gsl_vector, &
         b%gsl_vector, x%gsl_vector, residual%gsl_vector)
  end function fgsl_linalg_qr_lssolve
  function fgsl_linalg_complex_qr_lssolve (qr, tau, b, x, residual)
    type(fgsl_matrix_complex), intent(in) :: qr
    type(fgsl_vector_complex), intent(in) :: tau, b
    type(fgsl_vector_complex), intent(inout) :: x, residual
    integer(fgsl_int) :: fgsl_linalg_complex_qr_lssolve
    fgsl_linalg_complex_qr_lssolve = gsl_linalg_complex_qr_lssolve (qr%gsl_matrix_complex, tau%gsl_vector_complex, &
         b%gsl_vector_complex, x%gsl_vector_complex, residual%gsl_vector_complex)
  end function fgsl_linalg_complex_qr_lssolve
  function fgsl_linalg_qr_lssolve_r (qr, t, b, x, work)
    type(fgsl_matrix), intent(in) :: qr, t
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x, work
    integer(fgsl_int) :: fgsl_linalg_qr_lssolve_r
    fgsl_linalg_qr_lssolve_r = gsl_linalg_qr_lssolve_r (qr%gsl_matrix, t%gsl_matrix, &
         b%gsl_vector, x%gsl_vector, work%gsl_vector)
  end function fgsl_linalg_qr_lssolve_r
  function fgsl_linalg_complex_qr_lssolve_r (qr, t, b, x, work)
    type(fgsl_matrix_complex), intent(in) :: qr, t
    type(fgsl_vector_complex), intent(in) :: b
    type(fgsl_vector_complex), intent(inout) :: x, work
    integer(fgsl_int) :: fgsl_linalg_complex_qr_lssolve_r
    fgsl_linalg_complex_qr_lssolve_r = gsl_linalg_complex_qr_lssolve_r (qr%gsl_matrix_complex, t%gsl_matrix_complex, &
         b%gsl_vector_complex, x%gsl_vector_complex, work%gsl_vector_complex)
  end function fgsl_linalg_complex_qr_lssolve_r
  function fgsl_linalg_qr_qtvec (qr, tau, v)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_vector), intent(in) :: tau
    type(fgsl_vector), intent(inout) :: v
    integer(fgsl_int) :: fgsl_linalg_qr_qtvec
    fgsl_linalg_qr_qtvec = gsl_linalg_qr_qtvec (qr%gsl_matrix, tau%gsl_vector, &
         v%gsl_vector)
  end function fgsl_linalg_qr_qtvec
  function fgsl_linalg_complex_qr_qhvec (qr, tau, v)
    type(fgsl_matrix_complex), intent(in) :: qr
    type(fgsl_vector_complex), intent(in) :: tau
    type(fgsl_vector_complex), intent(inout) :: v
    integer(fgsl_int) :: fgsl_linalg_complex_qr_qhvec
    fgsl_linalg_complex_qr_qhvec = gsl_linalg_complex_qr_qhvec (qr%gsl_matrix_complex, tau%gsl_vector_complex, &
         v%gsl_vector_complex)
  end function fgsl_linalg_complex_qr_qhvec
  function fgsl_linalg_qr_qtvec_r (qr, t, v, work)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_matrix), intent(in) :: t
    type(fgsl_vector), intent(inout) :: v, work
    integer(fgsl_int) :: fgsl_linalg_qr_qtvec_r
    fgsl_linalg_qr_qtvec_r = gsl_linalg_qr_qtvec_r (qr%gsl_matrix, t%gsl_matrix, &
         v%gsl_vector, work%gsl_vector)
  end function fgsl_linalg_qr_qtvec_r
  function fgsl_linalg_complex_qr_qhvec_r (qr, t, v, work)
    type(fgsl_matrix_complex), intent(in) :: qr
    type(fgsl_matrix_complex), intent(in) :: t
    type(fgsl_vector_complex), intent(inout) :: v, work
    integer(fgsl_int) :: fgsl_linalg_complex_qr_qhvec_r
    fgsl_linalg_complex_qr_qhvec_r = gsl_linalg_complex_qr_qhvec_r (qr%gsl_matrix_complex, t%gsl_matrix_complex, &
         v%gsl_vector_complex, work%gsl_vector_complex)
  end function fgsl_linalg_complex_qr_qhvec_r
  function fgsl_linalg_qr_qvec (qr, tau, v)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_vector), intent(in) :: tau
    type(fgsl_vector), intent(inout) :: v
    integer(fgsl_int) :: fgsl_linalg_qr_qvec
    fgsl_linalg_qr_qvec = gsl_linalg_qr_qvec (qr%gsl_matrix, tau%gsl_vector, &
         v%gsl_vector)
  end function fgsl_linalg_qr_qvec
  function fgsl_linalg_complex_qr_qvec (qr, tau, v)
    type(fgsl_matrix_complex), intent(in) :: qr
    type(fgsl_vector_complex), intent(in) :: tau
    type(fgsl_vector_complex), intent(inout) :: v
    integer(fgsl_int) :: fgsl_linalg_complex_qr_qvec
    fgsl_linalg_complex_qr_qvec = gsl_linalg_complex_qr_qvec (qr%gsl_matrix_complex, tau%gsl_vector_complex, &
         v%gsl_vector_complex)
  end function fgsl_linalg_complex_qr_qvec
  function fgsl_linalg_qr_qtmat (qr, tau, a)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_vector), intent(in) :: tau
    type(fgsl_matrix), intent(inout) :: a
    integer(fgsl_int) :: fgsl_linalg_qr_qtmat
    fgsl_linalg_qr_qtmat = gsl_linalg_qr_qtmat (qr%gsl_matrix, tau%gsl_vector, &
         a%gsl_matrix)
  end function fgsl_linalg_qr_qtmat
  function fgsl_linalg_qr_qtmat_r (qr, t, a, work)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_matrix), intent(in) :: t
    type(fgsl_matrix), intent(inout) :: a, work
    integer(fgsl_int) :: fgsl_linalg_qr_qtmat_r
    fgsl_linalg_qr_qtmat_r = gsl_linalg_qr_qtmat_r (qr%gsl_matrix, t%gsl_matrix, &
         a%gsl_matrix, work%gsl_matrix)
  end function fgsl_linalg_qr_qtmat_r
  function fgsl_linalg_qr_rsolve (qr, b, x)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_qr_rsolve
    fgsl_linalg_qr_rsolve = gsl_linalg_qr_rsolve(qr%gsl_matrix, &
         b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_qr_rsolve
  function fgsl_linalg_qr_rsvx (qr, x)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_qr_rsvx
    fgsl_linalg_qr_rsvx = gsl_linalg_qr_rsvx(qr%gsl_matrix, x%gsl_vector)
  end function fgsl_linalg_qr_rsvx
  function fgsl_linalg_qr_unpack (qr, tau, q, r)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_vector), intent(in) :: tau
    type(fgsl_matrix), intent(inout) :: q, r
    integer(fgsl_int) :: fgsl_linalg_qr_unpack
    fgsl_linalg_qr_unpack = gsl_linalg_qr_unpack (qr%gsl_matrix, &
         tau%gsl_vector, q%gsl_matrix, r%gsl_matrix)
  end function fgsl_linalg_qr_unpack
  function fgsl_linalg_qr_unpack_r (qr, t, q, r)
    type(fgsl_matrix), intent(in) :: qr, t
    type(fgsl_matrix), intent(inout) :: q, r
    integer(fgsl_int) :: fgsl_linalg_qr_unpack_r
    fgsl_linalg_qr_unpack_r = gsl_linalg_qr_unpack_r (qr%gsl_matrix, &
         t%gsl_matrix, q%gsl_matrix, r%gsl_matrix)
  end function fgsl_linalg_qr_unpack_r
  function fgsl_linalg_complex_qr_unpack_r (qr, t, q, r)
    type(fgsl_matrix_complex), intent(in) :: qr, t
    type(fgsl_matrix_complex), intent(inout) :: q, r
    integer(fgsl_int) :: fgsl_linalg_complex_qr_unpack_r
    fgsl_linalg_complex_qr_unpack_r = gsl_linalg_complex_qr_unpack_r (qr%gsl_matrix_complex, &
         t%gsl_matrix_complex, q%gsl_matrix_complex, r%gsl_matrix_complex)
  end function fgsl_linalg_complex_qr_unpack_r
  function fgsl_linalg_qr_qrsolve (q, r, b, x)
    type(fgsl_matrix), intent(in) :: q, r
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_qr_qrsolve
    fgsl_linalg_qr_qrsolve = gsl_linalg_qr_qrsolve(q%gsl_matrix, &
         r%gsl_matrix, b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_qr_qrsolve
  function fgsl_linalg_qr_update (q, r, w, v)
    type(fgsl_matrix), intent(inout) :: q, r
    type(fgsl_vector), intent(inout) :: w
    type(fgsl_vector), intent(in) :: v
    integer(fgsl_int) :: fgsl_linalg_qr_update
    fgsl_linalg_qr_update = gsl_linalg_qr_update(q%gsl_matrix, &
         r%gsl_matrix, w%gsl_vector, v%gsl_vector)
  end function fgsl_linalg_qr_update
  function fgsl_linalg_r_solve (r, b, x)
    type(fgsl_matrix), intent(in) :: r
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_r_solve
    fgsl_linalg_r_solve = gsl_linalg_r_solve(r%gsl_matrix, &
         b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_r_solve
  function fgsl_linalg_r_svx (r, x)
    type(fgsl_matrix), intent(in) :: r
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_r_svx
    fgsl_linalg_r_svx = gsl_linalg_r_svx(r%gsl_matrix, x%gsl_vector)
  end function fgsl_linalg_r_svx
  function fgsl_linalg_qr_ur_decomp(u, a, t)
    type(fgsl_matrix), intent(inout) :: u, a, t
    integer(fgsl_int) :: fgsl_linalg_qr_ur_decomp
    fgsl_linalg_qr_ur_decomp = gsl_linalg_qr_ur_decomp(u%gsl_matrix, a%gsl_matrix, t%gsl_matrix)
  end function fgsl_linalg_qr_ur_decomp
  function fgsl_linalg_qr_uu_decomp(u1, u2, t)
    type(fgsl_matrix), intent(inout) :: u1, u2, t
    integer(fgsl_int) :: fgsl_linalg_qr_uu_decomp
    fgsl_linalg_qr_uu_decomp = gsl_linalg_qr_uu_decomp(u1%gsl_matrix, u2%gsl_matrix, t%gsl_matrix)
  end function fgsl_linalg_qr_uu_decomp
  function fgsl_linalg_qr_uu_lssolve(r, y, t, b, x, work)
    type(fgsl_matrix), intent(in) :: r, y, t
    type(fgsl_vector), intent(inout) :: b, x, work
    integer(fgsl_int) :: fgsl_linalg_qr_uu_lssolve
    fgsl_linalg_qr_uu_lssolve = gsl_linalg_qr_uu_lssolve(r%gsl_matrix, y%gsl_matrix, t%gsl_matrix, &
         b%gsl_vector, x%gsl_vector, work%gsl_vector)
  end function fgsl_linalg_qr_uu_lssolve
  function fgsl_linalg_qr_uu_qtvec(y, t, b, work)
    type(fgsl_matrix), intent(in) :: y, t
    type(fgsl_vector), intent(inout) :: b, work
    integer(fgsl_int) :: fgsl_linalg_qr_uu_qtvec
    fgsl_linalg_qr_uu_qtvec = gsl_linalg_qr_uu_qtvec(y%gsl_matrix, t%gsl_matrix, &
         b%gsl_vector, work%gsl_vector)
  end function fgsl_linalg_qr_uu_qtvec
  function fgsl_linalg_qr_uz_decomp(u, a, t)
    type(fgsl_matrix), intent(inout) :: u, a, t
    integer(fgsl_int) :: fgsl_linalg_qr_uz_decomp
    fgsl_linalg_qr_uz_decomp = gsl_linalg_qr_uz_decomp(u%gsl_matrix, a%gsl_matrix, t%gsl_matrix)
  end function fgsl_linalg_qr_uz_decomp
  function fgsl_linalg_qr_ud_decomp(u, d, y, t)
    type(fgsl_matrix), intent(inout) :: u, y, t
    type(fgsl_vector), intent(inout) :: d
    integer(fgsl_int) :: fgsl_linalg_qr_ud_decomp
    fgsl_linalg_qr_ud_decomp = gsl_linalg_qr_ud_decomp(u%gsl_matrix, d%gsl_vector, &
         y%gsl_matrix, t%gsl_matrix)
  end function fgsl_linalg_qr_ud_decomp
  function fgsl_linalg_qr_ud_lssolve(r, y, t, b, x, work)
    type(fgsl_matrix), intent(in) :: r, y, t
    type(fgsl_vector), intent(inout) :: b, x, work
    integer(fgsl_int) :: fgsl_linalg_qr_ud_lssolve
    fgsl_linalg_qr_ud_lssolve = gsl_linalg_qr_ud_lssolve(r%gsl_matrix, y%gsl_matrix, t%gsl_matrix, &
         b%gsl_vector, x%gsl_vector, work%gsl_vector)
  end function fgsl_linalg_qr_ud_lssolve
  function fgsl_linalg_qrpt_decomp (a, tau, p, signum, norm)
    type(fgsl_matrix), intent(inout) :: a
    type(fgsl_vector), intent(inout) :: tau, norm
    type(fgsl_permutation), intent(inout) :: p
    integer(fgsl_int), intent(out) :: signum
    integer(fgsl_int) :: fgsl_linalg_qrpt_decomp
    fgsl_linalg_qrpt_decomp = gsl_linalg_qrpt_decomp (a%gsl_matrix, &
         tau%gsl_vector, p%gsl_permutation, signum, norm%gsl_vector)
  end function fgsl_linalg_qrpt_decomp
  function fgsl_linalg_qrpt_decomp2 (a, q, r, tau, p, signum, norm)
    type(fgsl_matrix), intent(in) :: a
    type(fgsl_matrix), intent(inout) :: q, r
    type(fgsl_vector), intent(inout) :: tau, norm
    type(fgsl_permutation), intent(inout) :: p
    integer(fgsl_int), intent(out) :: signum
    integer(fgsl_int) :: fgsl_linalg_qrpt_decomp2
    fgsl_linalg_qrpt_decomp2 = gsl_linalg_qrpt_decomp2 (a%gsl_matrix, &
         q%gsl_matrix, r%gsl_matrix, tau%gsl_vector, p%gsl_permutation, &
         signum, norm%gsl_vector)
  end function fgsl_linalg_qrpt_decomp2
  function fgsl_linalg_qrpt_solve (qr, tau, p, b, x)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_vector), intent(in) :: tau, b
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_qrpt_solve
    fgsl_linalg_qrpt_solve = gsl_linalg_qrpt_solve(qr%gsl_matrix, &
         tau%gsl_vector, p%gsl_permutation, b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_qrpt_solve
  function fgsl_linalg_qrpt_svx (qr, tau, p, x)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_vector), intent(in) :: tau
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_qrpt_svx
    fgsl_linalg_qrpt_svx = gsl_linalg_qrpt_svx(qr%gsl_matrix, &
         tau%gsl_vector, p%gsl_permutation, x%gsl_vector)
  end function fgsl_linalg_qrpt_svx
  function fgsl_linalg_qrpt_lssolve (qr, tau, p, b, x, residual)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_vector), intent(in) :: tau, b
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector), intent(inout) :: x, residual
    integer(fgsl_int) :: fgsl_linalg_qrpt_lssolve
    fgsl_linalg_qrpt_lssolve = gsl_linalg_qrpt_lssolve(qr%gsl_matrix, &
         tau%gsl_vector, p%gsl_permutation, b%gsl_vector, &
         x%gsl_vector, residual%gsl_vector)
  end function fgsl_linalg_qrpt_lssolve
  function fgsl_linalg_qrpt_lssolve2 (qr, tau, p, b, rank, x, residual)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_vector), intent(in) :: tau, b
    integer(fgsl_size_t), intent(in) :: rank
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector), intent(inout) :: x, residual
    integer(fgsl_int) :: fgsl_linalg_qrpt_lssolve2
    fgsl_linalg_qrpt_lssolve2 = gsl_linalg_qrpt_lssolve2(qr%gsl_matrix, &
         tau%gsl_vector, p%gsl_permutation, b%gsl_vector, rank, &
         x%gsl_vector, residual%gsl_vector)
  end function fgsl_linalg_qrpt_lssolve2
  function fgsl_linalg_qrpt_qrsolve (q, r, p, b, x)
    type(fgsl_matrix), intent(in) :: q, r
    type(fgsl_vector), intent(in) :: b
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_qrpt_qrsolve
    fgsl_linalg_qrpt_qrsolve = gsl_linalg_qrpt_qrsolve(q%gsl_matrix, &
         r%gsl_matrix, p%gsl_permutation, b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_qrpt_qrsolve
  function fgsl_linalg_qrpt_update (q, r, p, w, v)
    type(fgsl_matrix), intent(inout) :: q, r
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector), intent(in) :: v
    type(fgsl_vector), intent(inout) :: w
    integer(fgsl_int) :: fgsl_linalg_qrpt_update
    fgsl_linalg_qrpt_update = gsl_linalg_qrpt_update(q%gsl_matrix, &
         r%gsl_matrix, p%gsl_permutation, w%gsl_vector, v%gsl_vector)
  end function fgsl_linalg_qrpt_update
  function fgsl_linalg_qrpt_rsolve (qr, p, b, x)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_vector), intent(in) :: b
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_qrpt_rsolve
    fgsl_linalg_qrpt_rsolve = gsl_linalg_qrpt_rsolve(qr%gsl_matrix, &
         p%gsl_permutation, b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_qrpt_rsolve
  function fgsl_linalg_qrpt_rsvx (qr, p, x)
    type(fgsl_matrix), intent(in) :: qr
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_qrpt_rsvx
    fgsl_linalg_qrpt_rsvx = gsl_linalg_qrpt_rsvx(qr%gsl_matrix, &
         p%gsl_permutation, x%gsl_vector)
  end function fgsl_linalg_qrpt_rsvx
  function fgsl_linalg_qrpt_rank (qr, tol)
    type(fgsl_matrix), intent(in) :: qr
    real(fgsl_double), intent(in) :: tol
    integer(fgsl_size_t) :: fgsl_linalg_qrpt_rank
    fgsl_linalg_qrpt_rank = gsl_linalg_qrpt_rank(qr%gsl_matrix, tol)
  end function fgsl_linalg_qrpt_rank
  function fgsl_linalg_qrpt_rcond (qr, rcond, work)
    type(fgsl_matrix), intent(in) :: qr
    real(fgsl_double), intent(inout) :: rcond
    type(fgsl_vector), intent(inout) :: work
    integer(fgsl_int) :: fgsl_linalg_qrpt_rcond
    fgsl_linalg_qrpt_rcond = gsl_linalg_qrpt_rcond(qr%gsl_matrix, rcond, work%gsl_vector)
  end function fgsl_linalg_qrpt_rcond
!
! LQ and QL decompositions
!
  integer(fgsl_int) function fgsl_linalg_lq_decomp(a, tau)
    type(fgsl_matrix), intent(inout) :: a
    type(fgsl_vector), intent(inout) :: tau
    fgsl_linalg_lq_decomp = gsl_linalg_lq_decomp(a%gsl_matrix, tau%gsl_vector)
  end function fgsl_linalg_lq_decomp
  integer(fgsl_int) function fgsl_linalg_lq_lssolve(lq, tau, b, x, residual)
    type(fgsl_matrix), intent(in) :: lq
    type(fgsl_vector), intent(in) :: tau, b
    type(fgsl_vector), intent(inout) :: x, residual
    fgsl_linalg_lq_lssolve = gsl_linalg_lq_lssolve(lq%gsl_matrix, tau%gsl_vector, &
         b%gsl_vector, x%gsl_vector, residual%gsl_vector)
  end function fgsl_linalg_lq_lssolve
  integer(fgsl_int) function fgsl_linalg_lq_unpack(lq, tau, q, l)
    type(fgsl_matrix), intent(in) :: lq
    type(fgsl_vector), intent(in) :: tau
    type(fgsl_matrix), intent(inout) :: q, l
    fgsl_linalg_lq_unpack = gsl_linalg_lq_unpack(lq%gsl_matrix, tau%gsl_vector, &
         q%gsl_matrix, l%gsl_matrix)
  end function fgsl_linalg_lq_unpack
  integer(fgsl_int) function fgsl_linalg_lq_qtvec(lq, tau, v)
    type(fgsl_matrix), intent(in) :: lq
    type(fgsl_vector), intent(in) :: tau
    type(fgsl_vector), intent(inout) :: v
    fgsl_linalg_lq_qtvec = gsl_linalg_lq_qtvec(lq%gsl_matrix, tau%gsl_vector, &
         v%gsl_vector)
  end function fgsl_linalg_lq_qtvec
  integer(fgsl_int) function fgsl_linalg_ql_decomp(a, tau)
    type(fgsl_matrix), intent(inout) :: a
    type(fgsl_vector), intent(inout) :: tau
    fgsl_linalg_ql_decomp = gsl_linalg_ql_decomp(a%gsl_matrix, tau%gsl_vector)
  end function fgsl_linalg_ql_decomp
  integer(fgsl_int) function fgsl_linalg_ql_unpack(ql, tau, q, l)
    type(fgsl_matrix), intent(in) :: ql
    type(fgsl_vector), intent(in) :: tau
    type(fgsl_matrix), intent(inout) :: q, l
    fgsl_linalg_ql_unpack = gsl_linalg_ql_unpack(ql%gsl_matrix, tau%gsl_vector, &
         q%gsl_matrix, l%gsl_matrix)
  end function fgsl_linalg_ql_unpack
  
!
! Complete orthogonal decomposition (COD)
!
  integer(fgsl_int) function fgsl_linalg_cod_decomp(a, tau_q, tau_z, p, rank, work)
    type(fgsl_matrix), intent(inout)  :: a
    type(fgsl_vector), intent(inout) :: tau_q, tau_z, work
    type(fgsl_permutation), intent(inout) :: p
    integer(fgsl_size_t), intent(inout) :: rank
    fgsl_linalg_cod_decomp = gsl_linalg_cod_decomp(a%gsl_matrix, tau_q%gsl_vector, &
          tau_z%gsl_vector, p%gsl_permutation, rank, work%gsl_vector)
  end function
  integer(fgsl_int) function fgsl_linalg_cod_decomp_e(a, tau_q, tau_z, p, tol, rank, work)
    type(fgsl_matrix), intent(inout)  :: a
    type(fgsl_vector), intent(inout) :: tau_q, tau_z, work
    real(fgsl_double), intent(in) :: tol
    type(fgsl_permutation), intent(inout) :: p
    integer(fgsl_size_t), intent(inout) :: rank
    fgsl_linalg_cod_decomp_e = gsl_linalg_cod_decomp_e(a%gsl_matrix, tau_q%gsl_vector, &
          tau_z%gsl_vector, p%gsl_permutation, tol, rank, work%gsl_vector)
  end function
  integer(fgsl_int) function fgsl_linalg_cod_lssolve(qrzt, tau_q, tau_z, p, rank, b, x, residual)
    type(fgsl_matrix), intent(in)  :: qrzt
    type(fgsl_vector), intent(in) :: tau_q, tau_z, b
    type(fgsl_vector), intent(inout) :: x, residual
    type(fgsl_permutation), intent(in) :: p
    integer(fgsl_size_t), intent(in) :: rank
    fgsl_linalg_cod_lssolve = gsl_linalg_cod_lssolve(qrzt%gsl_matrix, tau_q%gsl_vector, &
          tau_z%gsl_vector, p%gsl_permutation, rank, b%gsl_vector, x%gsl_vector, &
          residual%gsl_vector)
  end function
  integer(fgsl_int) function fgsl_linalg_cod_lssolve2(lambda, qrzt, tau_q, tau_z, p, &
       rank, b, x, residual, s, work)
    real(fgsl_double), intent(in) :: lambda
    type(fgsl_matrix), intent(in)  :: qrzt
    type(fgsl_vector), intent(in) :: tau_q, tau_z, b
    type(fgsl_vector), intent(inout) :: x, residual, work
    type(fgsl_matrix), intent(inout) :: s
    type(fgsl_permutation), intent(in) :: p
    integer(fgsl_size_t), intent(in) :: rank
    fgsl_linalg_cod_lssolve2 = gsl_linalg_cod_lssolve2(lambda, qrzt%gsl_matrix, & 
         tau_q%gsl_vector, tau_z%gsl_vector, p%gsl_permutation, rank, b%gsl_vector, &
         x%gsl_vector, residual%gsl_vector, s%gsl_matrix, work%gsl_vector)
  end function fgsl_linalg_cod_lssolve2
  integer(fgsl_int) function fgsl_linalg_cod_unpack(qrzt, tau_q, tau_z, p, rank, q, r, z)
    type(fgsl_matrix), intent(in)  :: qrzt
    type(fgsl_vector), intent(in) :: tau_q, tau_z
    type(fgsl_permutation), intent(in) :: p
    integer(fgsl_size_t), intent(in) :: rank
    type(fgsl_matrix), intent(inout)  :: q, r, z
    fgsl_linalg_cod_unpack = gsl_linalg_cod_unpack(qrzt%gsl_matrix, tau_q%gsl_vector, &
          tau_z%gsl_vector, p%gsl_permutation, rank, q%gsl_matrix, r%gsl_matrix, z%gsl_matrix)
  end function
  integer(fgsl_int) function fgsl_linalg_cod_matz(qrzt, tau_z, rank, a, work)
    type(fgsl_matrix), intent(in)  :: qrzt
    type(fgsl_vector), intent(in) ::  tau_z
    integer(fgsl_size_t), intent(in) :: rank
    type(fgsl_matrix), intent(inout)  :: a
    type(fgsl_vector), intent(inout) ::  work
    fgsl_linalg_cod_matz = gsl_linalg_cod_matz(qrzt%gsl_matrix, tau_z%gsl_vector, &
          rank, a%gsl_matrix, work%gsl_vector)
  end function
!
! SVD
!
  function fgsl_linalg_sv_decomp(a, v, s, work)
    type(fgsl_matrix), intent(inout)  :: a, v
    type(fgsl_vector), intent(inout) :: s, work
    integer(fgsl_int) :: fgsl_linalg_sv_decomp
    fgsl_linalg_sv_decomp = gsl_linalg_sv_decomp(a%gsl_matrix, &
         v%gsl_matrix, s%gsl_vector, work%gsl_vector)
  end function fgsl_linalg_sv_decomp
  function fgsl_linalg_sv_decomp_mod(a, x, v, s, work)
    type(fgsl_matrix), intent(inout)  :: a, x, v
    type(fgsl_vector), intent(inout) :: s, work
    integer(fgsl_int) :: fgsl_linalg_sv_decomp_mod
    fgsl_linalg_sv_decomp_mod = gsl_linalg_sv_decomp_mod(a%gsl_matrix, &
         x%gsl_matrix, v%gsl_matrix, s%gsl_vector, work%gsl_vector)
  end function fgsl_linalg_sv_decomp_mod
  function fgsl_linalg_sv_decomp_jacobi(a, v, s)
    type(fgsl_matrix), intent(inout)  :: a, v
    type(fgsl_vector), intent(inout) :: s
    integer(fgsl_int) :: fgsl_linalg_sv_decomp_jacobi
    fgsl_linalg_sv_decomp_jacobi = gsl_linalg_sv_decomp_jacobi(a%gsl_matrix, &
         v%gsl_matrix, s%gsl_vector)
  end function fgsl_linalg_sv_decomp_jacobi
  function fgsl_linalg_sv_solve(u, v, s, b, x)
    type(fgsl_matrix), intent(in)  :: u, v
    type(fgsl_vector), intent(in) :: s, b
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_sv_solve
    fgsl_linalg_sv_solve = gsl_linalg_sv_solve(u%gsl_matrix, &
         v%gsl_matrix, s%gsl_vector, b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_sv_solve
  function fgsl_linalg_sv_leverage(u, h)
    type(fgsl_matrix), intent(in)  :: u
    type(fgsl_vector), intent(inout) :: h
    integer(fgsl_int) :: fgsl_linalg_sv_leverage
    fgsl_linalg_sv_leverage = gsl_linalg_sv_leverage(u%gsl_matrix, h%gsl_vector)
  end function fgsl_linalg_sv_leverage
!
! Cholesky
!
  function fgsl_linalg_cholesky_decomp1(a)
    type(fgsl_matrix), intent(inout)  :: a
    integer(fgsl_int) :: fgsl_linalg_cholesky_decomp1
    fgsl_linalg_cholesky_decomp1 = gsl_linalg_cholesky_decomp1(a%gsl_matrix)
  end function fgsl_linalg_cholesky_decomp1
  function fgsl_linalg_cholesky_decomp(a)
    type(fgsl_matrix), intent(inout)  :: a
    integer(fgsl_int) :: fgsl_linalg_cholesky_decomp
    fgsl_linalg_cholesky_decomp = gsl_linalg_cholesky_decomp(a%gsl_matrix)
  end function fgsl_linalg_cholesky_decomp
  function fgsl_linalg_complex_cholesky_decomp(a)
    type(fgsl_matrix_complex), intent(inout)  :: a
    integer(fgsl_int) :: fgsl_linalg_complex_cholesky_decomp
    fgsl_linalg_complex_cholesky_decomp = &
         gsl_linalg_complex_cholesky_decomp(a%gsl_matrix_complex)
  end function fgsl_linalg_complex_cholesky_decomp
  function fgsl_linalg_cholesky_solve(chol, b, x)
    type(fgsl_matrix), intent(in) :: chol
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_cholesky_solve
    fgsl_linalg_cholesky_solve = gsl_linalg_cholesky_solve(chol%gsl_matrix, &
         b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_cholesky_solve
  function fgsl_linalg_complex_cholesky_solve(chol, b, x)
    type(fgsl_matrix_complex), intent(in) :: chol
    type(fgsl_vector_complex), intent(in) :: b
    type(fgsl_vector_complex), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_complex_cholesky_solve
    fgsl_linalg_complex_cholesky_solve = &
         gsl_linalg_complex_cholesky_solve(chol%gsl_matrix_complex, &
         b%gsl_vector_complex, x%gsl_vector_complex)
  end function fgsl_linalg_complex_cholesky_solve
  function fgsl_linalg_cholesky_svx(chol, x)
    type(fgsl_matrix), intent(in) :: chol
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_cholesky_svx
    fgsl_linalg_cholesky_svx = gsl_linalg_cholesky_svx(chol%gsl_matrix, &
         x%gsl_vector)
  end function fgsl_linalg_cholesky_svx
  function fgsl_linalg_complex_cholesky_svx(chol, x)
    type(fgsl_matrix_complex), intent(in) :: chol
    type(fgsl_vector_complex), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_complex_cholesky_svx
    fgsl_linalg_complex_cholesky_svx = &
         gsl_linalg_complex_cholesky_svx(chol%gsl_matrix_complex, &
         x%gsl_vector_complex)
  end function fgsl_linalg_complex_cholesky_svx
  function fgsl_linalg_cholesky_decomp2(a,s)
    type(fgsl_matrix), intent(inout)  :: a
    type(fgsl_vector), intent(inout) :: s
    integer(fgsl_int) :: fgsl_linalg_cholesky_decomp2
    fgsl_linalg_cholesky_decomp2 = gsl_linalg_cholesky_decomp2(a%gsl_matrix, &
         s%gsl_vector)
  end function fgsl_linalg_cholesky_decomp2
  function fgsl_linalg_cholesky_solve2(chol, s, b, x)
    type(fgsl_matrix), intent(in) :: chol
    type(fgsl_vector), intent(in) :: s
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_cholesky_solve2
    fgsl_linalg_cholesky_solve2 = gsl_linalg_cholesky_solve2(chol%gsl_matrix, &
         s%gsl_vector, b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_cholesky_solve2
  function fgsl_linalg_cholesky_svx2(chol, s, x)
    type(fgsl_matrix), intent(in) :: chol
    type(fgsl_vector), intent(in) :: s
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_cholesky_svx2
    fgsl_linalg_cholesky_svx2 = gsl_linalg_cholesky_svx2(chol%gsl_matrix, &
         s%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_cholesky_svx2
  function fgsl_linalg_cholesky_invert (chol)
    integer(fgsl_int) :: fgsl_linalg_cholesky_invert
    type(fgsl_matrix), intent(inout) :: chol
    fgsl_linalg_cholesky_invert = gsl_linalg_cholesky_invert(chol%gsl_matrix)
  end function fgsl_linalg_cholesky_invert
  function fgsl_linalg_complex_cholesky_invert (chol)
    integer(fgsl_int) :: fgsl_linalg_complex_cholesky_invert
    type(fgsl_matrix_complex), intent(inout) :: chol
    fgsl_linalg_complex_cholesky_invert = gsl_linalg_complex_cholesky_invert(chol%gsl_matrix_complex)
  end function fgsl_linalg_complex_cholesky_invert
  function fgsl_linalg_cholesky_scale(a,s)
    type(fgsl_matrix), intent(in)  :: a
    type(fgsl_vector), intent(inout) :: s
    integer(fgsl_int) :: fgsl_linalg_cholesky_scale
    fgsl_linalg_cholesky_scale = gsl_linalg_cholesky_scale(a%gsl_matrix, &
         s%gsl_vector)
  end function fgsl_linalg_cholesky_scale
  function fgsl_linalg_cholesky_scale_apply(a,s)
    type(fgsl_matrix), intent(inout)  :: a
    type(fgsl_vector), intent(in) :: s
    integer(fgsl_int) :: fgsl_linalg_cholesky_scale_apply
    fgsl_linalg_cholesky_scale_apply = gsl_linalg_cholesky_scale_apply(a%gsl_matrix, &
         s%gsl_vector)
  end function fgsl_linalg_cholesky_scale_apply
  function fgsl_linalg_cholesky_rcond (chol, rcond, work)
    integer(fgsl_int) :: fgsl_linalg_cholesky_rcond
    type(fgsl_matrix), intent(in) :: chol
    real(fgsl_double), intent(inout) :: rcond
    type(fgsl_vector), intent(inout) :: work
    fgsl_linalg_cholesky_rcond = gsl_linalg_cholesky_rcond(chol%gsl_matrix, rcond, &
         work%gsl_vector)
  end function fgsl_linalg_cholesky_rcond
!
! pivoted Cholesky
!
  function fgsl_linalg_pcholesky_decomp(a,p)
    type(fgsl_matrix), intent(inout)  :: a
    type(fgsl_permutation), intent(inout)  :: p
    integer(fgsl_int) :: fgsl_linalg_pcholesky_decomp
    fgsl_linalg_pcholesky_decomp = gsl_linalg_pcholesky_decomp(a%gsl_matrix, p%gsl_permutation)
  end function fgsl_linalg_pcholesky_decomp
  function fgsl_linalg_pcholesky_solve(ldlt, p, b, x)
    type(fgsl_matrix), intent(in) :: ldlt
    type(fgsl_permutation), intent(in)  :: p
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_pcholesky_solve
    fgsl_linalg_pcholesky_solve = gsl_linalg_pcholesky_solve(ldlt%gsl_matrix, &
         p%gsl_permutation, b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_pcholesky_solve
  function fgsl_linalg_pcholesky_svx(ldlt, p, x)
    type(fgsl_matrix), intent(in) :: ldlt
    type(fgsl_permutation), intent(in)  :: p
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_pcholesky_svx
    fgsl_linalg_pcholesky_svx = gsl_linalg_pcholesky_svx(ldlt%gsl_matrix, &
         p%gsl_permutation, x%gsl_vector)
  end function fgsl_linalg_pcholesky_svx
  function fgsl_linalg_pcholesky_decomp2(a,p,s)
    type(fgsl_matrix), intent(inout)  :: a
    type(fgsl_permutation), intent(inout)  :: p
    type(fgsl_vector), intent(inout) :: s
    integer(fgsl_int) :: fgsl_linalg_pcholesky_decomp2
    fgsl_linalg_pcholesky_decomp2 = gsl_linalg_pcholesky_decomp2(a%gsl_matrix, &
         p%gsl_permutation, s%gsl_vector)
  end function fgsl_linalg_pcholesky_decomp2
  function fgsl_linalg_pcholesky_solve2(ldlt, p, s, b, x)
    type(fgsl_matrix), intent(in) :: ldlt
    type(fgsl_permutation), intent(in)  :: p
    type(fgsl_vector), intent(in) :: s
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_pcholesky_solve2
    fgsl_linalg_pcholesky_solve2 = gsl_linalg_pcholesky_solve2(ldlt%gsl_matrix, &
         p%gsl_permutation, s%gsl_vector, b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_pcholesky_solve2
  function fgsl_linalg_pcholesky_svx2(ldlt, p, s, x)
    type(fgsl_matrix), intent(in) :: ldlt
    type(fgsl_permutation), intent(in)  :: p
    type(fgsl_vector), intent(in) :: s
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_pcholesky_svx2
    fgsl_linalg_pcholesky_svx2 = gsl_linalg_pcholesky_svx2(ldlt%gsl_matrix, &
         p%gsl_permutation, s%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_pcholesky_svx2
  function fgsl_linalg_pcholesky_invert (ldlt, p, ainv)
    integer(fgsl_int) :: fgsl_linalg_pcholesky_invert
    type(fgsl_matrix), intent(in) :: ldlt
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_matrix), intent(inout) :: ainv
    fgsl_linalg_pcholesky_invert = gsl_linalg_pcholesky_invert(ldlt%gsl_matrix, &
         p%gsl_permutation, ainv%gsl_matrix)
  end function fgsl_linalg_pcholesky_invert
  function fgsl_linalg_pcholesky_rcond (ldlt, p, rcond, work)
    integer(fgsl_int) :: fgsl_linalg_pcholesky_rcond
    type(fgsl_matrix), intent(in) :: ldlt
    type(fgsl_permutation), intent(in) :: p
    real(fgsl_double), intent(inout) :: rcond
    type(fgsl_vector), intent(inout) :: work
    fgsl_linalg_pcholesky_rcond = gsl_linalg_pcholesky_rcond(ldlt%gsl_matrix, &
         p%gsl_permutation, rcond, work%gsl_vector)
  end function fgsl_linalg_pcholesky_rcond
!
! modified Cholesky
!
  function fgsl_linalg_mcholesky_decomp(a,p,e)
    type(fgsl_matrix), intent(inout)  :: a
    type(fgsl_permutation), intent(inout)  :: p
    type(fgsl_vector), intent(inout)  :: e
    integer(fgsl_int) :: fgsl_linalg_mcholesky_decomp
    fgsl_linalg_mcholesky_decomp = gsl_linalg_mcholesky_decomp(a%gsl_matrix, &
         p%gsl_permutation, e%gsl_vector)
  end function fgsl_linalg_mcholesky_decomp
  function fgsl_linalg_mcholesky_solve(ldlt, p, b, x)
    type(fgsl_matrix), intent(in) :: ldlt
    type(fgsl_permutation), intent(in)  :: p
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_mcholesky_solve
    fgsl_linalg_mcholesky_solve = gsl_linalg_mcholesky_solve(ldlt%gsl_matrix, &
         p%gsl_permutation, b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_mcholesky_solve
  function fgsl_linalg_mcholesky_svx(ldlt, p, x)
    type(fgsl_matrix), intent(in) :: ldlt
    type(fgsl_permutation), intent(in)  :: p
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_mcholesky_svx
    fgsl_linalg_mcholesky_svx = gsl_linalg_mcholesky_svx(ldlt%gsl_matrix, &
         p%gsl_permutation, x%gsl_vector)
  end function fgsl_linalg_mcholesky_svx
  function fgsl_linalg_mcholesky_invert (ldlt, p, ainv)
    integer(fgsl_int) :: fgsl_linalg_mcholesky_invert
    type(fgsl_matrix), intent(in) :: ldlt
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_matrix), intent(inout) :: ainv
    fgsl_linalg_mcholesky_invert = gsl_linalg_mcholesky_invert(ldlt%gsl_matrix, &
         p%gsl_permutation, ainv%gsl_matrix)
  end function fgsl_linalg_mcholesky_invert
  function fgsl_linalg_mcholesky_rcond (ldlt, p, rcond, work)
    integer(fgsl_int) :: fgsl_linalg_mcholesky_rcond
    type(fgsl_matrix), intent(in) :: ldlt
    type(fgsl_permutation), intent(in) :: p
    real(fgsl_double), intent(inout) :: rcond
    type(fgsl_vector), intent(inout) :: work
    fgsl_linalg_mcholesky_rcond = gsl_linalg_mcholesky_rcond(ldlt%gsl_matrix, &
         p%gsl_permutation, rcond, work%gsl_vector)
  end function fgsl_linalg_mcholesky_rcond
  !
  ! LDLT
  !
  function fgsl_linalg_ldlt_decomp(a)
    type(fgsl_matrix), intent(inout)  :: a
    integer(fgsl_int) :: fgsl_linalg_ldlt_decomp
    fgsl_linalg_ldlt_decomp = gsl_linalg_ldlt_decomp(a%gsl_matrix)
  end function fgsl_linalg_ldlt_decomp
  integer(fgsl_int) function fgsl_linalg_ldlt_solve(ldlt, b, x)
    type(fgsl_matrix), intent(in) :: ldlt
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x
    fgsl_linalg_ldlt_solve = gsl_linalg_ldlt_solve(ldlt%gsl_matrix, b%gsl_vector, &
         x%gsl_vector)
  end function fgsl_linalg_ldlt_solve
  integer(fgsl_int) function fgsl_linalg_ldlt_svx(ldlt, x)
    type(fgsl_matrix), intent(in) :: ldlt
    type(fgsl_vector), intent(inout) :: x
    fgsl_linalg_ldlt_svx = gsl_linalg_ldlt_svx(ldlt%gsl_matrix, x%gsl_vector)
  end function fgsl_linalg_ldlt_svx
  integer(fgsl_int) function fgsl_linalg_ldlt_rcond(ldlt, rcond, w)
    type(fgsl_matrix), intent(in) :: ldlt
    real(fgsl_double), intent(inout) :: rcond
    type(fgsl_vector), intent(inout) :: w
    fgsl_linalg_ldlt_rcond = gsl_linalg_ldlt_rcond(ldlt%gsl_matrix, rcond, w%gsl_vector)
  end function fgsl_linalg_ldlt_rcond
!
! Tridiagonal Decomposition of Real Symmetric Matrices
!
  function fgsl_linalg_symmtd_decomp(a, tau)
    type(fgsl_matrix), intent(inout)  :: a
    type(fgsl_vector), intent(inout) :: tau
    integer(fgsl_int) :: fgsl_linalg_symmtd_decomp
    fgsl_linalg_symmtd_decomp = gsl_linalg_symmtd_decomp(a%gsl_matrix, &
         tau%gsl_vector)
  end function fgsl_linalg_symmtd_decomp
  function fgsl_linalg_symmtd_unpack(a, tau, q, diag, subdiag)
    type(fgsl_matrix), intent(in)  :: a
    type(fgsl_vector), intent(in) :: tau
    type(fgsl_matrix), intent(inout)  :: q
    type(fgsl_vector), intent(inout) :: diag, subdiag
    integer(fgsl_int) :: fgsl_linalg_symmtd_unpack
    fgsl_linalg_symmtd_unpack = gsl_linalg_symmtd_unpack(a%gsl_matrix, &
         tau%gsl_vector, q%gsl_matrix, diag%gsl_vector, subdiag%gsl_vector)
  end function fgsl_linalg_symmtd_unpack
  function fgsl_linalg_symmtd_unpack_t(a, diag, subdiag)
    type(fgsl_matrix), intent(in)  :: a
    type(fgsl_vector), intent(inout) :: diag, subdiag
    integer(fgsl_int) :: fgsl_linalg_symmtd_unpack_t
    fgsl_linalg_symmtd_unpack_t = gsl_linalg_symmtd_unpack_t(a%gsl_matrix, &
         diag%gsl_vector, subdiag%gsl_vector)
  end function fgsl_linalg_symmtd_unpack_t
  !
  ! Tridiagonal Decomposition of Hermitian Matrices
  !
  function fgsl_linalg_hermtd_decomp(a, tau)
    type(fgsl_matrix_complex), intent(inout)  :: a
    type(fgsl_vector_complex), intent(inout) :: tau
    integer(fgsl_int) :: fgsl_linalg_hermtd_decomp
    fgsl_linalg_hermtd_decomp = &
         gsl_linalg_hermtd_decomp(a%gsl_matrix_complex, tau%gsl_vector_complex)
  end function fgsl_linalg_hermtd_decomp
  function fgsl_linalg_hermtd_unpack(a, tau, q, diag, subdiag)
    type(fgsl_matrix_complex), intent(in)  :: a
    type(fgsl_vector_complex), intent(in) :: tau
    type(fgsl_matrix_complex), intent(inout)  :: q
    type(fgsl_vector), intent(inout) :: diag, subdiag
    integer(fgsl_int) :: fgsl_linalg_hermtd_unpack
    fgsl_linalg_hermtd_unpack = gsl_linalg_hermtd_unpack( &
         a%gsl_matrix_complex, tau%gsl_vector_complex, &
         q%gsl_matrix_complex, diag%gsl_vector, subdiag%gsl_vector)
  end function fgsl_linalg_hermtd_unpack
  function fgsl_linalg_hermtd_unpack_t(a, diag, subdiag)
    type(fgsl_matrix_complex), intent(in)  :: a
    type(fgsl_vector), intent(inout) :: diag, subdiag
    integer(fgsl_int) :: fgsl_linalg_hermtd_unpack_t
    fgsl_linalg_hermtd_unpack_t = gsl_linalg_hermtd_unpack_t( &
         a%gsl_matrix_complex, diag%gsl_vector, subdiag%gsl_vector)
  end function fgsl_linalg_hermtd_unpack_t
!
! Hessenberg
!
  function fgsl_linalg_hessenberg_decomp(a, tau)
    type(fgsl_matrix), intent(inout)  :: a
    type(fgsl_vector), intent(inout) :: tau
    integer(fgsl_int) :: fgsl_linalg_hessenberg_decomp
    fgsl_linalg_hessenberg_decomp = gsl_linalg_hessenberg_decomp(a%gsl_matrix, &
         tau%gsl_vector)
  end function fgsl_linalg_hessenberg_decomp
  function fgsl_linalg_hessenberg_unpack(h, tau, u)
    type(fgsl_matrix), intent(in)  :: h
    type(fgsl_vector), intent(in) :: tau
    type(fgsl_matrix), intent(inout)  :: u
    integer(fgsl_int) :: fgsl_linalg_hessenberg_unpack
    fgsl_linalg_hessenberg_unpack = gsl_linalg_hessenberg_unpack(h%gsl_matrix, &
         tau%gsl_vector, u%gsl_matrix)
  end function fgsl_linalg_hessenberg_unpack
  function fgsl_linalg_hessenberg_unpack_accum(h, tau, v)
    type(fgsl_matrix), intent(in)  :: h
    type(fgsl_vector), intent(in) :: tau
    type(fgsl_matrix), intent(inout)  :: v
    integer(fgsl_int) :: fgsl_linalg_hessenberg_unpack_accum
    fgsl_linalg_hessenberg_unpack_accum = &
         gsl_linalg_hessenberg_unpack_accum(h%gsl_matrix, &
         tau%gsl_vector, v%gsl_matrix)
  end function fgsl_linalg_hessenberg_unpack_accum
  function fgsl_linalg_hessenberg_set_zero(h)
    type(fgsl_matrix), intent(inout)  :: h
    integer(fgsl_int) :: fgsl_linalg_hessenberg_set_zero
    fgsl_linalg_hessenberg_set_zero = &
         gsl_linalg_hessenberg_set_zero(h%gsl_matrix)
  end function fgsl_linalg_hessenberg_set_zero
  function fgsl_linalg_hesstri_decomp(a, b, u, v, work)
    type(fgsl_matrix), intent(inout)  :: a, b, u, v
    type(fgsl_vector), intent(inout) :: work
    integer(fgsl_int) :: fgsl_linalg_hesstri_decomp
    fgsl_linalg_hesstri_decomp = gsl_linalg_hesstri_decomp(a%gsl_matrix, &
         b%gsl_matrix, u%gsl_matrix, v%gsl_matrix, work%gsl_vector)
  end function fgsl_linalg_hesstri_decomp
!
! Bidiag
!
  function fgsl_linalg_bidiag_decomp(a, tau_u, tau_v)
    type(fgsl_matrix), intent(inout)  :: a
    type(fgsl_vector), intent(inout) :: tau_u, tau_v
    integer(fgsl_int) :: fgsl_linalg_bidiag_decomp
    fgsl_linalg_bidiag_decomp = gsl_linalg_bidiag_decomp(a%gsl_matrix, &
         tau_u%gsl_vector, tau_v%gsl_vector)
  end function fgsl_linalg_bidiag_decomp
  function fgsl_linalg_bidiag_unpack(a, tau_u, u, tau_v, v, diag, &
       superdiag)
    type(fgsl_matrix), intent(in)  :: a
    type(fgsl_vector), intent(in) :: tau_u, tau_v
    type(fgsl_matrix), intent(inout) :: u, v
    type(fgsl_vector), intent(inout) :: diag, superdiag
    integer(fgsl_int) :: fgsl_linalg_bidiag_unpack
    fgsl_linalg_bidiag_unpack = gsl_linalg_bidiag_unpack(a%gsl_matrix, &
         tau_u%gsl_vector, u%gsl_matrix, tau_v%gsl_vector, v%gsl_matrix, &
         diag%gsl_vector, superdiag%gsl_vector)
  end function fgsl_linalg_bidiag_unpack
  function fgsl_linalg_bidiag_unpack2(a, tau_u, tau_v, v)
    type(fgsl_matrix), intent(inout)  :: a
    type(fgsl_vector), intent(in) :: tau_u, tau_v
    type(fgsl_matrix), intent(inout) :: v
    integer(fgsl_int) :: fgsl_linalg_bidiag_unpack2
    fgsl_linalg_bidiag_unpack2 = gsl_linalg_bidiag_unpack2(a%gsl_matrix, &
         tau_u%gsl_vector, tau_v%gsl_vector, v%gsl_matrix)
  end function fgsl_linalg_bidiag_unpack2
  function fgsl_linalg_bidiag_unpack_b(a, diag, superdiag)
    type(fgsl_matrix), intent(in)  :: a
    type(fgsl_vector), intent(inout) :: diag, superdiag
    integer(fgsl_int) :: fgsl_linalg_bidiag_unpack_b
    fgsl_linalg_bidiag_unpack_b = gsl_linalg_bidiag_unpack_b(a%gsl_matrix, &
         diag%gsl_vector, superdiag%gsl_vector)
  end function fgsl_linalg_bidiag_unpack_b
!
! Householder
!
  function fgsl_linalg_householder_transform (v)
    type(fgsl_vector), intent(inout) :: v
    real(fgsl_double) :: fgsl_linalg_householder_transform
    fgsl_linalg_householder_transform = &
         gsl_linalg_householder_transform(v%gsl_vector)
  end function fgsl_linalg_householder_transform
  function fgsl_linalg_complex_householder_transform (v)
    type(fgsl_vector), intent(inout) :: v
    complex(fgsl_double_complex) :: fgsl_linalg_complex_householder_transform
    fgsl_linalg_complex_householder_transform = &
         gsl_linalg_complex_householder_transform(v%gsl_vector)
  end function fgsl_linalg_complex_householder_transform
  function fgsl_linalg_householder_hm (tau, v, a)
    real(fgsl_double), intent(in) :: tau
    type(fgsl_vector), intent(in) :: v
    type(fgsl_matrix), intent(inout) :: a
    integer(fgsl_int) :: fgsl_linalg_householder_hm
    fgsl_linalg_householder_hm = &
         gsl_linalg_householder_hm(tau, v%gsl_vector, a%gsl_matrix)
  end function fgsl_linalg_householder_hm
  function fgsl_linalg_complex_householder_hm (tau, v, a)
    complex(fgsl_double_complex), intent(in) :: tau
    type(fgsl_vector_complex), intent(in) :: v
    type(fgsl_matrix_complex), intent(inout) :: a
    integer(fgsl_int) :: fgsl_linalg_complex_householder_hm
!
    complex(c_double_complex) :: targ
    targ = tau
    fgsl_linalg_complex_householder_hm = &
         gsl_linalg_complex_householder_hm(targ, v%gsl_vector_complex, &
         a%gsl_matrix_complex)
  end function fgsl_linalg_complex_householder_hm
  function fgsl_linalg_householder_mh (tau, v, a)
    real(fgsl_double), intent(in) :: tau
    type(fgsl_vector), intent(in) :: v
    type(fgsl_matrix), intent(inout) :: a
    integer(fgsl_int) :: fgsl_linalg_householder_mh
    fgsl_linalg_householder_mh = &
         gsl_linalg_householder_mh(tau, v%gsl_vector, a%gsl_matrix)
  end function fgsl_linalg_householder_mh
  function fgsl_linalg_complex_householder_mh (tau, v, a)
    complex(fgsl_double_complex), intent(in) :: tau
    type(fgsl_vector_complex), intent(in) :: v
    type(fgsl_matrix_complex), intent(inout) :: a
    integer(fgsl_int) :: fgsl_linalg_complex_householder_mh
!
    complex(c_double_complex) :: targ
    targ = tau
    fgsl_linalg_complex_householder_mh = &
         gsl_linalg_complex_householder_mh(targ, v%gsl_vector_complex, &
         a%gsl_matrix_complex)
  end function fgsl_linalg_complex_householder_mh
  function fgsl_linalg_householder_hv (tau, v, w)
    real(fgsl_double), intent(in) :: tau
    type(fgsl_vector), intent(in) :: v
    type(fgsl_vector), intent(inout) :: w
    integer(fgsl_int) :: fgsl_linalg_householder_hv
    fgsl_linalg_householder_hv = &
         gsl_linalg_householder_hv(tau, v%gsl_vector, w%gsl_vector)
  end function fgsl_linalg_householder_hv
  function fgsl_linalg_complex_householder_hv (tau, v, w)
    complex(fgsl_double_complex), intent(in) :: tau
    type(fgsl_vector_complex), intent(in) :: v
    type(fgsl_vector_complex), intent(inout) :: w
    integer(fgsl_int) :: fgsl_linalg_complex_householder_hv
!
    complex(c_double_complex) :: targ
    targ = tau
    fgsl_linalg_complex_householder_hv = &
         gsl_linalg_complex_householder_hv(targ, v%gsl_vector_complex, &
         w%gsl_vector_complex)
  end function fgsl_linalg_complex_householder_hv
  function fgsl_linalg_hh_solve(a, b, x)
    type(fgsl_matrix), intent(inout) :: a
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_hh_solve
    fgsl_linalg_hh_solve = gsl_linalg_hh_solve(a%gsl_matrix, &
         b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_hh_solve
  function fgsl_linalg_hh_svx(a, x)
    type(fgsl_matrix), intent(inout) :: a
    type(fgsl_vector), intent(inout) :: x
    integer(fgsl_int) :: fgsl_linalg_hh_svx
    fgsl_linalg_hh_svx = gsl_linalg_hh_svx(a%gsl_matrix, &
         x%gsl_vector)
  end function fgsl_linalg_hh_svx
!
! Tridiagonal
!
  function fgsl_linalg_solve_tridiag(diag, e, f, b, x)
    type(fgsl_vector), intent(in) :: diag, e, f, b
    type(fgsl_vector), intent(inout) :: x
    integer(c_int) :: fgsl_linalg_solve_tridiag
    fgsl_linalg_solve_tridiag = gsl_linalg_solve_tridiag(diag%gsl_vector, &
         e%gsl_vector, f%gsl_vector, b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_solve_tridiag
  function fgsl_linalg_solve_symm_tridiag(diag, e, b, x)
    type(fgsl_vector), intent(in) :: diag, e, b
    type(fgsl_vector), intent(inout) :: x
    integer(c_int) :: fgsl_linalg_solve_symm_tridiag
    fgsl_linalg_solve_symm_tridiag = &
         gsl_linalg_solve_symm_tridiag(diag%gsl_vector, &
         e%gsl_vector, b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_solve_symm_tridiag
  function fgsl_linalg_solve_cyc_tridiag(diag, e, f, b, x)
    type(fgsl_vector), intent(in) :: diag, e, f, b
    type(fgsl_vector), intent(inout) :: x
    integer(c_int) :: fgsl_linalg_solve_cyc_tridiag
    fgsl_linalg_solve_cyc_tridiag = gsl_linalg_solve_cyc_tridiag(diag%gsl_vector, &
         e%gsl_vector, f%gsl_vector, b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_solve_cyc_tridiag
  function fgsl_linalg_solve_symm_cyc_tridiag(diag, e, b, x)
    type(fgsl_vector), intent(in) :: diag, e, b
    type(fgsl_vector), intent(inout) :: x
    integer(c_int) :: fgsl_linalg_solve_symm_cyc_tridiag
    fgsl_linalg_solve_symm_cyc_tridiag = &
         gsl_linalg_solve_symm_cyc_tridiag(diag%gsl_vector, &
         e%gsl_vector, b%gsl_vector, x%gsl_vector)
  end function fgsl_linalg_solve_symm_cyc_tridiag
  function fgsl_linalg_qr_matq(QR, tau, A)
    type(fgsl_matrix), intent(in) :: QR
    type(fgsl_vector), intent(in) :: tau
    type(fgsl_matrix), intent(inout) :: A
    integer(fgsl_int) :: fgsl_linalg_qr_matq
    fgsl_linalg_qr_matq = gsl_linalg_qr_matq(QR%gsl_matrix, tau%gsl_vector, A%gsl_matrix)
  end function fgsl_linalg_qr_matq
  subroutine fgsl_linalg_givens(a, b, c, s)
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double), intent(out) :: c, s
    call gsl_linalg_givens(a, b, c, s)
  end subroutine fgsl_linalg_givens
  subroutine fgsl_linalg_givens_gv(v, i, j, c, s)
    type(fgsl_vector), intent(inout) :: v
    integer(fgsl_size_t), intent(in) :: i, j
    real(fgsl_double), intent(in) :: c, s
    call gsl_linalg_givens_gv(v%gsl_vector, i, j, c, s)
  end subroutine fgsl_linalg_givens_gv
  ! 
  ! Triangular matrices 
  !
  integer(fgsl_int) function fgsl_linalg_tri_invert(uplo, diag, t)
    integer(fgsl_int), intent(in) :: uplo, diag
    type(fgsl_matrix), intent(inout) :: t
    fgsl_linalg_tri_invert = gsl_linalg_tri_invert(uplo, diag, &
         t%gsl_matrix)
  end function fgsl_linalg_tri_invert
    integer(fgsl_int) function fgsl_linalg_complex_tri_invert(uplo, diag, t)
    integer(fgsl_int), intent(in) :: uplo, diag
    type(fgsl_matrix_complex), intent(inout) :: t
    fgsl_linalg_complex_tri_invert = gsl_linalg_complex_tri_invert( &
         uplo, diag, t%gsl_matrix_complex)
  end function fgsl_linalg_complex_tri_invert
  integer(fgsl_int) function fgsl_linalg_tri_ltl(l)
    type(fgsl_matrix), intent(inout) :: l
    fgsl_linalg_tri_ltl = gsl_linalg_tri_ltl(l%gsl_matrix)
  end function fgsl_linalg_tri_ltl
  integer(fgsl_int) function fgsl_linalg_complex_tri_lhl(l)
    type(fgsl_matrix_complex), intent(inout) :: l
    fgsl_linalg_complex_tri_lhl = gsl_linalg_complex_tri_lhl(l%gsl_matrix_complex)
  end function fgsl_linalg_complex_tri_lhl
  integer(fgsl_int) function fgsl_linalg_tri_ul(lu)
    type(fgsl_matrix), intent(inout) :: lu
    fgsl_linalg_tri_ul = gsl_linalg_tri_ul(lu%gsl_matrix)
  end function fgsl_linalg_tri_ul
  integer(fgsl_int) function fgsl_linalg_complex_tri_ul(lu)
    type(fgsl_matrix_complex), intent(inout) :: lu
    fgsl_linalg_complex_tri_ul = gsl_linalg_complex_tri_ul(lu%gsl_matrix_complex)
  end function fgsl_linalg_complex_tri_ul
  integer(fgsl_int) function fgsl_linalg_tri_rcond(uplo, a, rcond, work)
    integer(fgsl_int), intent(in) :: uplo
    type(fgsl_matrix), intent(in) :: a
    real(fgsl_double), intent(inout) :: rcond
    type(fgsl_vector), intent(inout) :: work
    fgsl_linalg_tri_rcond = gsl_linalg_tri_rcond(uplo, a%gsl_matrix, rcond, work%gsl_vector)
  end function fgsl_linalg_tri_rcond
  !
  ! Triangular matrices (legacy)
  !
  integer(fgsl_int) function fgsl_linalg_tri_upper_invert(t)
    type(fgsl_matrix), intent(inout) :: t
    fgsl_linalg_tri_upper_invert = gsl_linalg_tri_upper_invert(t%gsl_matrix)
  end function
  integer(fgsl_int) function fgsl_linalg_tri_lower_invert(t)
    type(fgsl_matrix), intent(inout) :: t
    fgsl_linalg_tri_lower_invert = gsl_linalg_tri_lower_invert(t%gsl_matrix)
  end function
  integer(fgsl_int) function fgsl_linalg_tri_upper_unit_invert(t)
    type(fgsl_matrix), intent(inout) :: t
    fgsl_linalg_tri_upper_unit_invert = gsl_linalg_tri_upper_unit_invert(t%gsl_matrix)
  end function
  integer(fgsl_int) function fgsl_linalg_tri_lower_unit_invert(t)
    type(fgsl_matrix), intent(inout) :: t
    fgsl_linalg_tri_lower_unit_invert = gsl_linalg_tri_lower_unit_invert(t%gsl_matrix)
  end function
  integer(fgsl_int) function fgsl_linalg_tri_upper_rcond(t, rcond, work)
    type(fgsl_matrix), intent(inout) :: t
    real(fgsl_double), intent(inout) :: rcond
    type(fgsl_vector), intent(inout) :: work
    fgsl_linalg_tri_upper_rcond = gsl_linalg_tri_upper_rcond(t%gsl_matrix, rcond, &
         work%gsl_vector)
  end function
  integer(fgsl_int) function fgsl_linalg_tri_lower_rcond(t, rcond, work)
    type(fgsl_matrix), intent(inout) :: t
    real(fgsl_double), intent(inout) :: rcond
    type(fgsl_vector), intent(inout) :: work
    fgsl_linalg_tri_lower_rcond = gsl_linalg_tri_lower_rcond(t%gsl_matrix, rcond, &
         work%gsl_vector)
  end function fgsl_linalg_tri_lower_rcond
  !
  ! Banded systems
  !
  integer(fgsl_int) function fgsl_linalg_cholesky_band_decomp(a)
    type(fgsl_matrix), intent(inout) :: a
    fgsl_linalg_cholesky_band_decomp = gsl_linalg_cholesky_band_decomp(a%gsl_matrix)
  end function fgsl_linalg_cholesky_band_decomp
  integer(fgsl_int) function fgsl_linalg_cholesky_band_solve(llt, b, x)
    type(fgsl_matrix), intent(in) :: llt
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x
    fgsl_linalg_cholesky_band_solve = gsl_linalg_cholesky_band_solve(llt%gsl_matrix, b%gsl_vector, &
         x%gsl_vector)
  end function fgsl_linalg_cholesky_band_solve
  integer(fgsl_int) function fgsl_linalg_cholesky_band_solvem(llt, b, x)
    type(fgsl_matrix), intent(in) :: llt
    type(fgsl_matrix), intent(in) :: b
    type(fgsl_matrix), intent(inout) :: x
    fgsl_linalg_cholesky_band_solvem = gsl_linalg_cholesky_band_solvem(llt%gsl_matrix, b%gsl_matrix, &
         x%gsl_matrix)
  end function fgsl_linalg_cholesky_band_solvem
  integer(fgsl_int) function fgsl_linalg_cholesky_band_svx(llt, x)
    type(fgsl_matrix), intent(in) :: llt
    type(fgsl_vector), intent(inout) :: x
    fgsl_linalg_cholesky_band_svx = gsl_linalg_cholesky_band_svx(llt%gsl_matrix, x%gsl_vector)
  end function fgsl_linalg_cholesky_band_svx
  integer(fgsl_int) function fgsl_linalg_cholesky_band_svxm(llt, x)
    type(fgsl_matrix), intent(in) :: llt
    type(fgsl_matrix), intent(inout) :: x
    fgsl_linalg_cholesky_band_svxm = gsl_linalg_cholesky_band_svxm(llt%gsl_matrix, x%gsl_matrix)
  end function fgsl_linalg_cholesky_band_svxm
  integer(fgsl_int) function fgsl_linalg_cholesky_band_invert(llt, ainv)
    type(fgsl_matrix), intent(in) :: llt
    type(fgsl_matrix), intent(inout) :: ainv
    fgsl_linalg_cholesky_band_invert = gsl_linalg_cholesky_band_invert(llt%gsl_matrix, ainv%gsl_matrix)
  end function fgsl_linalg_cholesky_band_invert
  integer(fgsl_int) function fgsl_linalg_cholesky_band_unpack(llt, l)
    type(fgsl_matrix), intent(in) :: llt
    type(fgsl_matrix), intent(inout) :: l
    fgsl_linalg_cholesky_band_unpack = gsl_linalg_cholesky_band_unpack(llt%gsl_matrix, l%gsl_matrix)
  end function fgsl_linalg_cholesky_band_unpack
  integer(fgsl_int) function fgsl_linalg_cholesky_band_scale(a, s)
    type(fgsl_matrix), intent(in) :: a
    type(fgsl_vector), intent(inout) :: s
    fgsl_linalg_cholesky_band_scale = gsl_linalg_cholesky_band_scale(a%gsl_matrix, s%gsl_vector)
  end function fgsl_linalg_cholesky_band_scale
  integer(fgsl_int) function fgsl_linalg_cholesky_band_scale_apply(a, s)
    type(fgsl_matrix), intent(inout) :: a
    type(fgsl_vector), intent(in) :: s
    fgsl_linalg_cholesky_band_scale_apply = gsl_linalg_cholesky_band_scale_apply(a%gsl_matrix, s%gsl_vector)
  end function fgsl_linalg_cholesky_band_scale_apply
  integer(fgsl_int) function fgsl_linalg_cholesky_band_rcond(llt, rcond, w)
    type(fgsl_matrix), intent(in) :: llt
    real(fgsl_double), intent(inout) :: rcond
    type(fgsl_vector), intent(inout) :: w
    fgsl_linalg_cholesky_band_rcond = gsl_linalg_cholesky_band_rcond(llt%gsl_matrix, rcond, w%gsl_vector)
  end function fgsl_linalg_cholesky_band_rcond
  integer(fgsl_int) function fgsl_linalg_ldlt_band_decomp(a)
    type(fgsl_matrix), intent(inout) :: a
    fgsl_linalg_ldlt_band_decomp = gsl_linalg_ldlt_band_decomp(a%gsl_matrix)
  end function fgsl_linalg_ldlt_band_decomp
  integer(fgsl_int) function fgsl_linalg_ldlt_band_solve(ldlt, b, x)
    type(fgsl_matrix), intent(in) :: ldlt
    type(fgsl_vector), intent(in) :: b
    type(fgsl_vector), intent(inout) :: x
    fgsl_linalg_ldlt_band_solve = gsl_linalg_ldlt_band_solve(ldlt%gsl_matrix, b%gsl_vector, &
         x%gsl_vector)
  end function fgsl_linalg_ldlt_band_solve
  integer(fgsl_int) function fgsl_linalg_ldlt_band_svx(ldlt, x)
    type(fgsl_matrix), intent(in) :: ldlt
    type(fgsl_vector), intent(inout) :: x
    fgsl_linalg_ldlt_band_svx = gsl_linalg_ldlt_band_svx(ldlt%gsl_matrix, x%gsl_vector)
  end function fgsl_linalg_ldlt_band_svx
  integer(fgsl_int) function fgsl_linalg_ldlt_band_unpack(ldlt, l, d)
    type(fgsl_matrix), intent(in) :: ldlt
    type(fgsl_matrix), intent(inout) :: l
    type(fgsl_vector), intent(inout) :: d
    fgsl_linalg_ldlt_band_unpack = gsl_linalg_ldlt_band_unpack(ldlt%gsl_matrix, l%gsl_matrix, &
         d%gsl_vector)
  end function fgsl_linalg_ldlt_band_unpack
  integer(fgsl_int) function fgsl_linalg_ldlt_band_rcond(ldlt, rcond, w)
    type(fgsl_matrix), intent(in) :: ldlt
    real(fgsl_double), intent(inout) :: rcond
    type(fgsl_vector), intent(inout) :: w
    fgsl_linalg_ldlt_band_rcond = gsl_linalg_ldlt_band_rcond(ldlt%gsl_matrix, rcond, w%gsl_vector)
  end function fgsl_linalg_ldlt_band_rcond
  !
  ! Balancing
  !
  function fgsl_linalg_balance_matrix(a, d)
    type(fgsl_matrix), intent(inout) :: a
    type(fgsl_vector), intent(inout) :: d
    integer(fgsl_int) :: fgsl_linalg_balance_matrix
    fgsl_linalg_balance_matrix = gsl_linalg_balance_matrix(a%gsl_matrix, &
         d%gsl_vector)
  end function fgsl_linalg_balance_matrix
  
end module fgsl_linalg
