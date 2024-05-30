module fgsl_bspline
  !> Basis Splines
  use fgsl_base
  use fgsl_array
  implicit none
  
  private :: gsl_bspline_alloc, gsl_bspline_free, gsl_bspline_knots, &
    gsl_bspline_knots_uniform, gsl_bspline_eval, gsl_bspline_eval_nonzero, &
    gsl_bspline_deriv_eval, gsl_bspline_deriv_eval_nonzero, &
    gsl_bspline_ncoeffs, gsl_bspline_greville_abscissa, &
    gsl_bspline_knots_greville
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
	subroutine gsl_bspline_free (w) bind(c)
	  import :: c_ptr
	  type(c_ptr), value :: w
	end subroutine gsl_bspline_free
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
	!
	  fgsl_bspline_alloc%gsl_bspline_workspace = gsl_bspline_alloc(k, nbreak)
	end function fgsl_bspline_alloc
	subroutine fgsl_bspline_free (w)
	  type(fgsl_bspline_workspace), intent(inout) :: w
	  call gsl_bspline_free (w%gsl_bspline_workspace)
	end subroutine fgsl_bspline_free
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
