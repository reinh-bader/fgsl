module fgsl_bspline
  !> Basis Splines
  use fgsl_base
  use fgsl_array
  implicit none
  
  private :: gsl_bspline_alloc, gsl_bspline_alloc_ncontrol, gsl_bspline_free, &
    gsl_bspline_init_augment, gsl_bspline_init_uniform, gsl_bspline_init_periodic, &
    gsl_bspline_init_interp, gsl_bspline_init_hermite, gsl_bspline_init, &
    gsl_bspline_ncontrol, gsl_bspline_order, gsl_bspline_nbreak, &
    gsl_bspline_calc, gsl_bspline_vector_calc, gsl_bspline_basis, gsl_bspline_eval_basis, &
    gsl_bspline_calc_deriv, gsl_bspline_vector_calc_deriv, gsl_bspline_basis_deriv, &
    gsl_bspline_eval_deriv_basis, gsl_bspline_calc_integ, gsl_bspline_basis_integ, &
    gsl_bspline_lssolve, gsl_bspline_wlssolve, gsl_bspline_lsnormal, &
    gsl_bspline_lsnormalm, gsl_bspline_residuals, gsl_bspline_covariance, &
    gsl_bspline_err, gsl_bspline_rcond, gsl_bspline_plssolve, gsl_bspline_pwlssolve, &
    gsl_bspline_plsqr
  private :: gsl_bspline_return_knots_vector
  private :: gsl_bspline_knots, &
    gsl_bspline_knots_uniform, gsl_bspline_eval, gsl_bspline_eval_nonzero, &
    gsl_bspline_deriv_eval, gsl_bspline_deriv_eval_nonzero, &
    gsl_bspline_ncoeffs, gsl_bspline_greville_abscissa, &
    gsl_bspline_knots_greville
  !> Legacy 
  ! private :: TBD
  !
  !> Types
  type, public :: fgsl_bspline_workspace
     private
     type(c_ptr) :: gsl_bspline_workspace = c_null_ptr
  end type fgsl_bspline_workspace
  !
  !> C interfaces
  interface
	function gsl_bspline_alloc(k, nbreak) bind(c)
	  import :: c_size_t, c_ptr
	  integer(c_size_t), value :: k, nbreak
	  type(c_ptr) :: gsl_bspline_alloc
	end function gsl_bspline_alloc
	function gsl_bspline_alloc_ncontrol(ncontrol) bind(c)
	  import :: c_size_t, c_ptr
	  integer(c_size_t), value :: ncontrol
	  type(c_ptr) :: gsl_bspline_alloc_ncontrol
	end function gsl_bspline_alloc_ncontrol
	subroutine gsl_bspline_free (w) bind(c)
	  import :: c_ptr
	  type(c_ptr), value :: w
	end subroutine gsl_bspline_free
	function gsl_bspline_init_augment(tau, w) bind(c)
	  import :: c_int, c_ptr
	  type(c_ptr), value :: tau, w
	  integer(c_int) :: gsl_bspline_init_augment
	end function gsl_bspline_init_augment
	function gsl_bspline_init_uniform(a, b, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  real(c_double), value :: a, b
	  type(c_ptr), value :: w
	  integer(c_int) :: gsl_bspline_init_uniform
	end function gsl_bspline_init_uniform
	function gsl_bspline_init_periodic(a, b, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  real(c_double), value :: a, b
	  type(c_ptr), value :: w
	  integer(c_int) :: gsl_bspline_init_periodic
	end function gsl_bspline_init_periodic
	function gsl_bspline_init_interp(x, w) bind(c)
	  import :: c_int, c_ptr
	  type(c_ptr), value :: x, w
	  integer(c_int) :: gsl_bspline_init_interp
	end function gsl_bspline_init_interp
	function gsl_bspline_init_hermite(nderiv, x, w) bind(c)
	  import :: c_int, c_ptr, c_size_t
	  type(c_ptr), value :: x, w
	  integer(c_size_t), value :: nderiv
	  integer(c_int) :: gsl_bspline_init_hermite
	end function gsl_bspline_init_hermite
	function gsl_bspline_init(t, w) bind(c)
	  import :: c_int, c_ptr
	  type(c_ptr), value :: t, w
	  integer(c_int) :: gsl_bspline_init
	end function gsl_bspline_init	
	!
	function gsl_bspline_ncontrol(w) bind(c)
	  import :: c_size_t, c_ptr
	  type(c_ptr), value :: w
	  integer(c_size_t) :: gsl_bspline_ncontrol
	end function gsl_bspline_ncontrol
	function gsl_bspline_order(w) bind(c)
	  import :: c_size_t, c_ptr
	  type(c_ptr), value :: w
	  integer(c_size_t) :: gsl_bspline_order
	end function gsl_bspline_order
	function gsl_bspline_nbreak(w) bind(c)
	  import :: c_size_t, c_ptr
	  type(c_ptr), value :: w
	  integer(c_size_t) :: gsl_bspline_nbreak
	end function gsl_bspline_nbreak
	!
	function gsl_bspline_calc(x, c, result, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  real(c_double), value :: x
	  type(c_ptr), value :: c, w
	  real(c_double) :: result
	  integer(c_int) :: gsl_bspline_calc
	end function gsl_bspline_calc
	function gsl_bspline_vector_calc(x, c, result, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  real(c_double), value :: x
	  type(c_ptr), value :: c, w
	  type(c_ptr), value :: result
	  integer(c_int) :: gsl_bspline_vector_calc
	end function gsl_bspline_vector_calc
	function gsl_bspline_basis(x, b, istart, w) bind(c)
	  import :: c_int, c_ptr, c_double, c_size_t
	  real(c_double), value :: x
	  type(c_ptr), value :: b, w
	  integer(c_size_t), intent(inout) :: istart
	  integer(c_int) :: gsl_bspline_basis
	end function gsl_bspline_basis
	function gsl_bspline_eval_basis(x, b, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  real(c_double), value :: x
	  type(c_ptr), value :: b, w
	  integer(c_int) :: gsl_bspline_eval_basis
	end function gsl_bspline_eval_basis
	function gsl_bspline_calc_deriv(x, c, nderiv, result, w) bind(c)
	  import :: c_int, c_ptr, c_double, c_size_t
	  real(c_double), value :: x
	  type(c_ptr), value :: c, w
	  integer(c_size_t), value :: nderiv
	  real(c_double) :: result
	  integer(c_int) :: gsl_bspline_calc_deriv
	end function gsl_bspline_calc_deriv
	function gsl_bspline_vector_calc_deriv(x, c, nderiv, result, w) bind(c)
	  import :: c_int, c_ptr, c_double, c_size_t
	  real(c_double), value :: x
	  type(c_ptr), value :: c, w
	  integer(c_size_t), value :: nderiv
	  type(c_ptr), value :: result
	  integer(c_int) :: gsl_bspline_vector_calc_deriv
	end function gsl_bspline_vector_calc_deriv
	function gsl_bspline_basis_deriv(x, b, nderiv, istart, w) bind(c)
	  import :: c_int, c_ptr, c_double, c_size_t
	  real(c_double), value :: x
	  type(c_ptr), value :: b, w
	  integer(c_size_t), value :: nderiv
	  integer(c_size_t), intent(inout) :: istart
	  integer(c_int) :: gsl_bspline_basis_deriv
	end function gsl_bspline_basis_deriv
	function gsl_bspline_eval_deriv_basis(x, b, nderiv, w) bind(c)
	  import :: c_int, c_ptr, c_double, c_size_t
	  real(c_double), value :: x
	  type(c_ptr), value :: b, w
	  integer(c_size_t), value :: nderiv
	  integer(c_int) :: gsl_bspline_eval_deriv_basis
	end function gsl_bspline_eval_deriv_basis
	function gsl_bspline_calc_integ(a, b, c, result, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  real(c_double), value :: a, b
	  type(c_ptr), value :: c, w
	  real(c_double) :: result
	  integer(c_int) :: gsl_bspline_calc_integ
	end function gsl_bspline_calc_integ
	function gsl_bspline_basis_integ(a, b, y, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  real(c_double), value :: a, b
	  type(c_ptr), value :: y, w
	  integer(c_int) :: gsl_bspline_basis_integ
	end function gsl_bspline_basis_integ
	!
	function gsl_bspline_lssolve(x, y, c, chisq, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  type(c_ptr), value :: x, y, c, w
	  real(c_double) :: chisq
	  integer(c_int) :: gsl_bspline_lssolve	
	end function gsl_bspline_lssolve
	function gsl_bspline_wlssolve(x, y, wts, c, chisq, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  type(c_ptr), value :: x, y, wts, c, w
	  real(c_double) :: chisq
	  integer(c_int) :: gsl_bspline_wlssolve	
	end function gsl_bspline_wlssolve
	function gsl_bspline_lsnormal(x, y, wts, xty, xtx, w) bind(c)
	  import :: c_int, c_ptr
	  type(c_ptr), value :: x, y, wts, xty, xtx, w
	  integer(c_int) :: gsl_bspline_lsnormal
	end function gsl_bspline_lsnormal
	function gsl_bspline_lsnormalm(x, y, wts, xty, xtx, w) bind(c)
	  import :: c_int, c_ptr
	  type(c_ptr), value :: x, y, wts, xty, xtx, w
	  integer(c_int) :: gsl_bspline_lsnormalm
	end function gsl_bspline_lsnormalm
	function gsl_bspline_residuals(x, y, c, r, w) bind(c)
	  import :: c_int, c_ptr
	  type(c_ptr), value :: x, y, c, r, w
	  integer(c_int) :: gsl_bspline_residuals
	end function gsl_bspline_residuals
	function gsl_bspline_covariance(cholesky, cov, w) bind(c)
	  import :: c_int, c_ptr
	  type(c_ptr), value :: cholesky, cov, w
	  integer(c_int) :: gsl_bspline_covariance
	end function gsl_bspline_covariance
	function gsl_bspline_err(x, nderiv, cov, err, w) bind(c)
	  import :: c_int, c_ptr, c_double, c_size_t
	  real(c_double), value :: x
	  type(c_ptr), value :: cov, w
	  integer(c_size_t), value :: nderiv
	  real(c_double) :: err
	  integer(c_int) :: gsl_bspline_err
	end function gsl_bspline_err
	function gsl_bspline_rcond(xtx, rcond, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  type(c_ptr), value :: xtx, w
	  real(c_double) :: rcond
	  integer(c_int) :: gsl_bspline_rcond
	end function gsl_bspline_rcond
	function gsl_bspline_plssolve(x, y, c, chisq, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  type(c_ptr), value :: x, y, c, w
	  real(c_double) :: chisq
	  integer(c_int) :: gsl_bspline_plssolve	
	end function gsl_bspline_plssolve
	function gsl_bspline_pwlssolve(x, y, wts, c, chisq, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  type(c_ptr), value :: x, y, wts, c, w
	  real(c_double) :: chisq
	  integer(c_int) :: gsl_bspline_pwlssolve	
	end function gsl_bspline_pwlssolve
	function gsl_bspline_plsqr(x, y, wts, r, qty, rnorm, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  type(c_ptr), value :: x, y, wts, r, qty, w
	  real(c_double) :: rnorm
	  integer(c_int) :: gsl_bspline_plsqr
	end function gsl_bspline_plsqr
	!
    function gsl_bspline_return_knots_vector(w) bind(c)
      import :: c_ptr
      type(c_ptr), value :: w
      type(c_ptr) :: gsl_bspline_return_knots_vector
    end function gsl_bspline_return_knots_vector
	!		
	function gsl_bspline_knots(breakpts, w) bind(c)
	  import :: c_int, c_ptr
	  integer(c_int) :: gsl_bspline_knots
	  type(c_ptr), value :: breakpts, w
	end function gsl_bspline_knots
	function gsl_bspline_knots_uniform(a, b, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  integer(c_int) :: gsl_bspline_knots_uniform
	  real(c_double), value :: a, b
	  type(c_ptr), value :: w
	end function gsl_bspline_knots_uniform
	function gsl_bspline_eval(x, b, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  integer(c_int) :: gsl_bspline_eval
	  real(c_double), value :: x
	  type(c_ptr), value :: b, w
	end function gsl_bspline_eval
	function gsl_bspline_eval_nonzero(x, b, istart, iend, w) bind(c)
	  import :: c_int, c_ptr, c_double, c_size_t
	  integer(c_int) :: gsl_bspline_eval_nonzero
	  real(c_double), value :: x
	  integer(c_size_t) :: istart, iend
	  type(c_ptr), value :: b, w
	end function gsl_bspline_eval_nonzero
	function gsl_bspline_deriv_eval(x, nderiv, db, w) bind(c)
	  import :: c_int, c_ptr, c_double, c_size_t
	  integer(c_int) :: gsl_bspline_deriv_eval
	  integer(c_size_t), value :: nderiv
	  real(c_double), value :: x
	  type(c_ptr), value :: db, w
	end function gsl_bspline_deriv_eval
	function gsl_bspline_deriv_eval_nonzero(x, nderiv, db, istart, &
	     iend, w) bind(c)
	  import :: c_int, c_ptr, c_double, c_size_t
	  integer(c_int) :: gsl_bspline_deriv_eval_nonzero
	  real(c_double), value :: x
	  integer(c_size_t), value :: nderiv
	  integer(c_size_t) :: istart, iend
	  type(c_ptr), value :: db, w
	end function gsl_bspline_deriv_eval_nonzero
	function gsl_bspline_ncoeffs(w) bind(c)
	  import :: c_size_t, c_ptr
	  integer(c_size_t) :: gsl_bspline_ncoeffs
	  type(c_ptr), value :: w
	end function gsl_bspline_ncoeffs
	function gsl_bspline_greville_abscissa(i, w) bind(c)
	  import :: c_size_t, c_double, c_ptr
	  real(c_double) :: gsl_bspline_greville_abscissa
	  integer(c_size_t) :: i
	  type(c_ptr), value :: w
	end function gsl_bspline_greville_abscissa
	function gsl_bspline_knots_greville(abscissae, w, abserr) bind(c)
	  import :: c_ptr, c_double, c_int
	  type(c_ptr), value :: abscissae, w
	  real(c_double) :: abserr
	  integer(c_int) :: gsl_bspline_knots_greville
	end function gsl_bspline_knots_greville
  end interface
contains
!> API:
	function fgsl_bspline_alloc(k, nbreak)
	  integer(fgsl_size_t), intent(in) :: k, nbreak
	  type(fgsl_bspline_workspace) :: fgsl_bspline_alloc
	  fgsl_bspline_alloc%gsl_bspline_workspace = gsl_bspline_alloc(k, nbreak)
	end function fgsl_bspline_alloc
	function fgsl_bspline_alloc_ncontrol(ncontrol)
	  integer(fgsl_size_t), intent(in) :: ncontrol
	  type(fgsl_bspline_workspace) :: fgsl_bspline_alloc_ncontrol
	  fgsl_bspline_alloc_ncontrol%gsl_bspline_workspace = gsl_bspline_alloc_ncontrol(ncontrol)
	end function fgsl_bspline_alloc_ncontrol
	subroutine fgsl_bspline_free (w)
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  call gsl_bspline_free (w%gsl_bspline_workspace)
	end subroutine fgsl_bspline_free
	function fgsl_bspline_init_augment(tau, w) 
	  type(fgsl_vector), intent(in) :: tau
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_init_augment
	  fgsl_bspline_init_augment = gsl_bspline_init_augment(tau%gsl_vector, &
	     w%gsl_bspline_workspace)
	end function fgsl_bspline_init_augment	
	function fgsl_bspline_init_uniform(a, b, w) 
      real(fgsl_double), intent(in) :: a, b
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_init_uniform
	  fgsl_bspline_init_uniform = gsl_bspline_init_uniform(a, b, &
	     w%gsl_bspline_workspace)
	end function fgsl_bspline_init_uniform	
	function fgsl_bspline_init_periodic(a, b, w) 
      real(fgsl_double), intent(in) :: a, b
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_init_periodic
	  fgsl_bspline_init_periodic = gsl_bspline_init_periodic(a, b, &
	     w%gsl_bspline_workspace)
	end function fgsl_bspline_init_periodic
	function fgsl_bspline_init_interp(x, w) 
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_init_interp
	  fgsl_bspline_init_interp = gsl_bspline_init_interp(x%gsl_vector, &
	     w%gsl_bspline_workspace)
	end function fgsl_bspline_init_interp	
	function fgsl_bspline_init_hermite(nderiv, x, w) 
	  integer(fgsl_size_t), intent(in) :: nderiv
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_init_hermite
	  fgsl_bspline_init_hermite = gsl_bspline_init_hermite(nderiv, x%gsl_vector, &
	     w%gsl_bspline_workspace)
	end function fgsl_bspline_init_hermite
	function fgsl_bspline_init(t, w) 
	  type(fgsl_vector), intent(in) :: t
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_init
	  fgsl_bspline_init = gsl_bspline_init(t%gsl_vector, &
	     w%gsl_bspline_workspace)
	end function fgsl_bspline_init
	!
	function fgsl_bspline_ncontrol(w) 
	  type(fgsl_bspline_workspace), intent(in) :: w
	  integer(fgsl_size_t) :: fgsl_bspline_ncontrol
	  fgsl_bspline_ncontrol = gsl_bspline_ncontrol(w%gsl_bspline_workspace)
	end function fgsl_bspline_ncontrol
	function fgsl_bspline_order(w) 
	  type(fgsl_bspline_workspace), intent(in) :: w
	  integer(fgsl_size_t) :: fgsl_bspline_order
	  fgsl_bspline_order = gsl_bspline_order(w%gsl_bspline_workspace)
	end function fgsl_bspline_order	
	function fgsl_bspline_nbreak(w) 
	  type(fgsl_bspline_workspace), intent(in) :: w
	  integer(fgsl_size_t) :: fgsl_bspline_nbreak
	  fgsl_bspline_nbreak = gsl_bspline_nbreak(w%gsl_bspline_workspace)
	end function fgsl_bspline_nbreak
	!	
	function fgsl_bspline_calc(x, c, result, w) 
	  real(fgsl_double), intent(in) :: x
	  type(fgsl_vector), intent(in) :: c
	  real(fgsl_double), intent(inout) :: result
	  type(fgsl_bspline_workspace), intent(in) :: w
	  integer(fgsl_int) :: fgsl_bspline_calc
	  fgsl_bspline_calc = gsl_bspline_calc(x, c%gsl_vector, result, &
	                      w%gsl_bspline_workspace)
	end function fgsl_bspline_calc
	function fgsl_bspline_vector_calc(x, c, result, w) 
	  real(fgsl_double), intent(in) :: x
	  type(fgsl_matrix), intent(in) :: c
	  type(fgsl_vector), intent(inout) :: result
	  type(fgsl_bspline_workspace), intent(in) :: w
	  integer(fgsl_int) :: fgsl_bspline_vector_calc
	  fgsl_bspline_vector_calc = gsl_bspline_vector_calc(x, c%gsl_matrix, &
	                      result%gsl_vector, w%gsl_bspline_workspace)
	end function fgsl_bspline_vector_calc	
	function fgsl_bspline_basis(x, b, istart, w) 
	  real(fgsl_double), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: b
	  integer(fgsl_size_t), intent(inout) :: istart
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_basis
	  fgsl_bspline_basis = gsl_bspline_basis(x, b%gsl_vector, istart, &
	                      w%gsl_bspline_workspace)
	end function fgsl_bspline_basis
	function fgsl_bspline_eval_basis(x, b, w) 
	  real(fgsl_double), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: b
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_eval_basis
	  fgsl_bspline_eval_basis = gsl_bspline_eval_basis(x, b%gsl_vector, &
	                      w%gsl_bspline_workspace)
	end function fgsl_bspline_eval_basis
	function fgsl_bspline_calc_deriv(x, c, nderiv, result, w) 
	  real(fgsl_double), intent(in) :: x
	  type(fgsl_vector), intent(in) :: c
	  integer(fgsl_size_t), intent(in) :: nderiv
	  real(fgsl_double), intent(inout) :: result
	  type(fgsl_bspline_workspace), intent(in) :: w
	  integer(fgsl_int) :: fgsl_bspline_calc_deriv
	  fgsl_bspline_calc_deriv = gsl_bspline_calc_deriv(x, c%gsl_vector, nderiv, &
	                      result, w%gsl_bspline_workspace)
	end function fgsl_bspline_calc_deriv
	function fgsl_bspline_vector_calc_deriv(x, c, nderiv, result, w) 
	  real(fgsl_double), intent(in) :: x
	  type(fgsl_matrix), intent(in) :: c
	  integer(fgsl_size_t), intent(in) :: nderiv
	  type(fgsl_vector), intent(inout) :: result
	  type(fgsl_bspline_workspace), intent(in) :: w
	  integer(fgsl_int) :: fgsl_bspline_vector_calc_deriv
	  fgsl_bspline_vector_calc_deriv = gsl_bspline_vector_calc_deriv(x, c%gsl_matrix, &
	                      nderiv, result%gsl_vector, w%gsl_bspline_workspace)
	end function fgsl_bspline_vector_calc_deriv
	function fgsl_bspline_basis_deriv(x, b, nderiv, istart, w) 
	  real(fgsl_double), intent(in) :: x
	  type(fgsl_vector), intent(in) :: b
	  integer(fgsl_size_t), intent(in) :: nderiv
	  integer(fgsl_size_t), intent(inout) :: istart
	  type(fgsl_bspline_workspace), intent(in) :: w
	  integer(fgsl_int) :: fgsl_bspline_basis_deriv
	  fgsl_bspline_basis_deriv = gsl_bspline_basis_deriv(x, b%gsl_vector, nderiv, istart, &
	                      w%gsl_bspline_workspace)
	end function fgsl_bspline_basis_deriv
	function fgsl_bspline_eval_deriv_basis(x, b, nderiv, w) 
	  real(fgsl_double), intent(in) :: x
	  type(fgsl_vector), intent(in) :: b
	  integer(fgsl_size_t), intent(in) :: nderiv
	  type(fgsl_bspline_workspace), intent(in) :: w
	  integer(fgsl_int) :: fgsl_bspline_eval_deriv_basis
	  fgsl_bspline_eval_deriv_basis = gsl_bspline_eval_deriv_basis(x, b%gsl_vector, nderiv, &
	                      w%gsl_bspline_workspace)
	end function fgsl_bspline_eval_deriv_basis
	function fgsl_bspline_calc_integ(a, b, c, result, w) 
	  real(fgsl_double), intent(in) :: a, b
	  type(fgsl_vector), intent(in) :: c
	  real(fgsl_double), intent(inout) :: result
	  type(fgsl_bspline_workspace), intent(in) :: w
	  integer(fgsl_int) :: fgsl_bspline_calc_integ
	  fgsl_bspline_calc_integ = gsl_bspline_calc_integ(a, b, c%gsl_vector, result, &
	                      w%gsl_bspline_workspace)
	end function fgsl_bspline_calc_integ
	function fgsl_bspline_basis_integ(a, b, y, w) 
	  real(fgsl_double), intent(in) :: a, b
	  type(fgsl_vector), intent(inout) :: y
	  type(fgsl_bspline_workspace), intent(in) :: w
	  integer(fgsl_int) :: fgsl_bspline_basis_integ
	  fgsl_bspline_basis_integ = gsl_bspline_basis_integ(a, b, y%gsl_vector, &
	                      w%gsl_bspline_workspace)
	end function fgsl_bspline_basis_integ
    !
	function fgsl_bspline_lssolve(x, y, c, chisq, w) 
	  type(fgsl_vector), intent(in) :: x, y
	  type(fgsl_vector), intent(inout) :: c
	  real(fgsl_double), intent(inout) :: chisq
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_lssolve
	  fgsl_bspline_lssolve = gsl_bspline_lssolve(x%gsl_vector, y%gsl_vector, &
	                      c%gsl_vector, chisq, w%gsl_bspline_workspace)
	end function fgsl_bspline_lssolve
	function fgsl_bspline_wlssolve(x, y, wts, c, chisq, w) 
	  type(fgsl_vector), intent(in) :: x, y, wts
	  type(fgsl_vector), intent(inout) :: c
	  real(fgsl_double), intent(inout) :: chisq
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_wlssolve
	  fgsl_bspline_wlssolve = gsl_bspline_wlssolve(x%gsl_vector, y%gsl_vector, &
	              wts%gsl_vector, c%gsl_vector, chisq, w%gsl_bspline_workspace)
	end function fgsl_bspline_wlssolve
	function fgsl_bspline_lsnormal(x, y, wts, xty, xtx, w) 
	  type(fgsl_vector), intent(in) :: x, y, wts
	  type(fgsl_vector), intent(inout) :: xtx, xty
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_lsnormal
	  fgsl_bspline_lsnormal = gsl_bspline_lsnormal(x%gsl_vector, y%gsl_vector, &
	              wts%gsl_vector, xty%gsl_vector, xtx%gsl_vector, w%gsl_bspline_workspace)
	end function fgsl_bspline_lsnormal
	function fgsl_bspline_lsnormalm(x, y, wts, xty, xtx, w) 
	  type(fgsl_vector), intent(in) :: x, y, wts
	  type(fgsl_matrix), intent(inout) :: xtx, xty
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_lsnormalm
	  fgsl_bspline_lsnormalm = gsl_bspline_lsnormalm(x%gsl_vector, y%gsl_vector, &
	              wts%gsl_vector, xty%gsl_matrix, xtx%gsl_matrix, w%gsl_bspline_workspace)
	end function fgsl_bspline_lsnormalm
	function fgsl_bspline_residuals(x, y, c, r, w) 
	  type(fgsl_vector), intent(in) :: x, y, c
	  type(fgsl_vector), intent(inout) :: r
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_residuals
	  fgsl_bspline_residuals = gsl_bspline_residuals(x%gsl_vector, y%gsl_vector, &
	                      c%gsl_vector, r%gsl_vector, w%gsl_bspline_workspace)
	end function fgsl_bspline_residuals
	function fgsl_bspline_covariance(cholesky, cov, w) 
	  type(fgsl_matrix), intent(in) :: cholesky
	  type(fgsl_matrix), intent(inout) :: cov
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_covariance
	  fgsl_bspline_covariance = gsl_bspline_covariance(cholesky%gsl_matrix, &
	                      cov%gsl_matrix, w%gsl_bspline_workspace)
	end function fgsl_bspline_covariance
	function fgsl_bspline_err(x, nderiv, cov, err, w) 
	  real(fgsl_double), intent(in) :: x
	  type(fgsl_matrix), intent(in) :: cov
	  integer(fgsl_size_t), intent(in) :: nderiv
	  real(fgsl_double), intent(inout) :: err
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_err
	  fgsl_bspline_err = gsl_bspline_err(x, nderiv, cov%gsl_matrix, err, w%gsl_bspline_workspace)
	end function fgsl_bspline_err
	function fgsl_bspline_rcond(xtx, rcond, w) 
	  type(fgsl_matrix), intent(in) :: xtx
	  real(fgsl_double), intent(inout) :: rcond
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_rcond
	  fgsl_bspline_rcond = gsl_bspline_rcond(xtx%gsl_matrix, rcond, &
	                        w%gsl_bspline_workspace)
	end function fgsl_bspline_rcond
	function fgsl_bspline_plssolve(x, y, c, chisq, w) 
	  type(fgsl_vector), intent(in) :: x, y
	  type(fgsl_vector), intent(inout) :: c
	  real(fgsl_double), intent(inout) :: chisq
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_plssolve
	  fgsl_bspline_plssolve = gsl_bspline_plssolve(x%gsl_vector, y%gsl_vector, &
	                      c%gsl_vector, chisq, w%gsl_bspline_workspace)
	end function fgsl_bspline_plssolve
	function fgsl_bspline_pwlssolve(x, y, wts, c, chisq, w) 
	  type(fgsl_vector), intent(in) :: x, y, wts
	  type(fgsl_vector), intent(inout) :: c
	  real(fgsl_double), intent(inout) :: chisq
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_pwlssolve
	  fgsl_bspline_pwlssolve = gsl_bspline_pwlssolve(x%gsl_vector, y%gsl_vector, &
	              wts%gsl_vector, c%gsl_vector, chisq, w%gsl_bspline_workspace)
	end function fgsl_bspline_pwlssolve
	function fgsl_bspline_plsqr(x, y, wts, r, qty, rnorm, w) 
	  type(fgsl_vector), intent(in) :: x, y, wts
	  type(fgsl_matrix), intent(inout) :: r
	  type(fgsl_vector), intent(inout) ::qty
	  real(fgsl_double), intent(inout) :: rnorm
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_bspline_plsqr
	  fgsl_bspline_plsqr = gsl_bspline_plsqr(x%gsl_vector, y%gsl_vector, &
	       wts%gsl_vector, r%gsl_matrix, qty%gsl_vector, rnorm, w%gsl_bspline_workspace)
	end function fgsl_bspline_plsqr
	!
    function fgsl_bspline_return_knots_vector(w) 
      type(fgsl_bspline_workspace), intent(in):: w
      type(fgsl_vector) :: fgsl_bspline_return_knots_vector
      
      fgsl_bspline_return_knots_vector%gsl_vector = &
           gsl_bspline_return_knots_vector(w%gsl_bspline_workspace) 
    end function fgsl_bspline_return_knots_vector	
	!
	function fgsl_bspline_knots(breakpts, w)
	  integer(fgsl_int) :: fgsl_bspline_knots
	  type(fgsl_vector), intent(in) :: breakpts
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  fgsl_bspline_knots = gsl_bspline_knots(breakpts%gsl_vector, &
	       w%gsl_bspline_workspace)
	end function fgsl_bspline_knots
	function fgsl_bspline_knots_uniform(a, b, w)
	  integer(fgsl_int) :: fgsl_bspline_knots_uniform
	  real(fgsl_double), intent(in) :: a, b
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  fgsl_bspline_knots_uniform = gsl_bspline_knots_uniform(a, b, &
	       w%gsl_bspline_workspace)
	end function fgsl_bspline_knots_uniform
	function fgsl_bspline_eval(x, b, w)
	  integer(fgsl_int) :: fgsl_bspline_eval
	  real(fgsl_double), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: b
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  fgsl_bspline_eval = gsl_bspline_eval(x, b%gsl_vector, &
	       w%gsl_bspline_workspace)
	end function fgsl_bspline_eval
	function fgsl_bspline_eval_nonzero(x, bk, istart, iend, w)
	  integer(fgsl_int) :: fgsl_bspline_eval_nonzero
	  real(fgsl_double), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: bk
	  integer(fgsl_size_t), intent(inout) :: istart, iend
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  fgsl_bspline_eval_nonzero = gsl_bspline_eval_nonzero(x, bk%gsl_vector, &
	       istart, iend, w%gsl_bspline_workspace)
	end function fgsl_bspline_eval_nonzero
	function fgsl_bspline_deriv_eval(x, nderiv, db, w)
	  integer(fgsl_int) :: fgsl_bspline_deriv_eval
	  real(fgsl_double), intent(in) :: x
	  integer(fgsl_size_t), intent(in) :: nderiv
	  type(fgsl_matrix), intent(inout) :: db
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  fgsl_bspline_deriv_eval = gsl_bspline_deriv_eval(x, nderiv, db%gsl_matrix, &
	       w%gsl_bspline_workspace)
	end function fgsl_bspline_deriv_eval
	function fgsl_bspline_deriv_eval_nonzero(x, nderiv, db, istart, iend, w)
	  integer(fgsl_int) :: fgsl_bspline_deriv_eval_nonzero
	  real(fgsl_double), intent(in) :: x
	  integer(fgsl_size_t), intent(in) :: nderiv
	  type(fgsl_matrix), intent(inout) :: db
	  integer(fgsl_size_t), intent(inout) :: istart, iend
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  fgsl_bspline_deriv_eval_nonzero = gsl_bspline_deriv_eval_nonzero(x, nderiv, &
	       db%gsl_matrix, istart, iend, w%gsl_bspline_workspace)
	end function fgsl_bspline_deriv_eval_nonzero
	function fgsl_bspline_ncoeffs(w)
	  integer(fgsl_size_t) :: fgsl_bspline_ncoeffs
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  fgsl_bspline_ncoeffs = gsl_bspline_ncoeffs(w%gsl_bspline_workspace)
	end function fgsl_bspline_ncoeffs
	function fgsl_bspline_greville_abscissa(i, w)
	  real(fgsl_double) :: fgsl_bspline_greville_abscissa
	  integer(fgsl_size_t) :: i
	  type(fgsl_bspline_workspace), intent(in) :: w
	  fgsl_bspline_greville_abscissa = gsl_bspline_greville_abscissa(i, w%gsl_bspline_workspace)
	end function fgsl_bspline_greville_abscissa
	function fgsl_bspline_knots_greville(abscissae,w,abserr)
	  type(fgsl_vector) :: abscissae
	  type(fgsl_bspline_workspace) :: w
	  real(fgsl_double), intent(out) :: abserr
	  integer(fgsl_int) :: fgsl_bspline_knots_greville
	  fgsl_bspline_knots_greville = gsl_bspline_knots_greville(&
	  abscissae%gsl_vector, w%gsl_bspline_workspace, abserr)
	end function fgsl_bspline_knots_greville
end module fgsl_bspline
