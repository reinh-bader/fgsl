module fgsl_multilarge
  !> Linear least-squeares fitting - large dense linear systems
  use fgsl_base
  use fgsl_array

  implicit none

  private :: gsl_multilarge_linear_alloc, gsl_multilarge_linear_free, &
    gsl_multilarge_linear_name, gsl_multilarge_linear_reset, &
    gsl_multilarge_linear_accumulate, gsl_multilarge_linear_solve, &
    gsl_multilarge_linear_rcond, gsl_multilarge_linear_lcurve, &
    gsl_multilarge_linear_matrix_ptr, gsl_multilarge_linear_rhs_ptr, &
    gsl_multilarge_linear_wstdform1, gsl_multilarge_linear_stdform1, &
    gsl_multilarge_linear_l_decomp, gsl_multilarge_linear_wstdform2, &
    gsl_multilarge_linear_stdform2, gsl_multilarge_linear_genform1, &
    gsl_multilarge_linear_genform2, &
    fgsl_aux_multilarge_linear_alloc
    
  !
  !> Types
  type, public :: fgsl_multilarge_linear_type
    private
    integer(fgsl_int) :: which = 0
  end type fgsl_multilarge_linear_type
  type(fgsl_multilarge_linear_type), parameter, public :: &
    fgsl_multilarge_linear_normal = fgsl_multilarge_linear_type(1), &
    fgsl_multilarge_linear_tsqr = fgsl_multilarge_linear_type(2)

  type, public :: fgsl_multilarge_linear_workspace
    private
    type(c_ptr) :: gsl_multilarge_linear_workspace
  end type fgsl_multilarge_linear_workspace
  
  !
  !> C interfaces
  interface
	function gsl_multilarge_linear_alloc(T, p) bind(c)
	  import :: c_ptr, c_size_t
	  type(c_ptr), value :: T
	  integer(c_size_t), value :: p
	  type(c_ptr) :: gsl_multilarge_linear_alloc
	end function gsl_multilarge_linear_alloc
	
	subroutine gsl_multilarge_linear_free(w) bind(c)
	  import :: c_ptr
	  type(c_ptr), value :: w
	end subroutine gsl_multilarge_linear_free
	
	function gsl_multilarge_linear_name(w) bind(c)
	  import :: c_ptr
	  type(c_ptr), value :: w
	  type(c_ptr) :: gsl_multilarge_linear_name
	end function gsl_multilarge_linear_name
	
	function gsl_multilarge_linear_reset(w) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: w
	  integer(c_int) :: gsl_multilarge_linear_reset
	end function gsl_multilarge_linear_reset
	
	function gsl_multilarge_linear_accumulate(X, y, w) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: X, y, w
	  integer(c_int) :: gsl_multilarge_linear_accumulate
	end function gsl_multilarge_linear_accumulate
	
	function gsl_multilarge_linear_solve(lambda, c, rnorm, snorm, w) bind(c)
	  import :: c_int, c_ptr, c_double
	  real(c_double), value :: lambda
	  type(c_ptr), value :: c, w
	  real(c_double) :: rnorm, snorm
	  integer(c_int) :: gsl_multilarge_linear_solve
	end function gsl_multilarge_linear_solve
	
	function gsl_multilarge_linear_rcond(rcond, w) bind(c)
	  import :: c_ptr, c_int, c_double
	  real(c_double) :: rcond
	  type(c_ptr), value :: w
	  integer(c_int) :: gsl_multilarge_linear_rcond
	end function gsl_multilarge_linear_rcond
	
	function gsl_multilarge_linear_lcurve(reg_param, rho, eta, w) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: reg_param, rho, eta, w
	  integer(c_int) :: gsl_multilarge_linear_lcurve
	end function gsl_multilarge_linear_lcurve
	
	function gsl_multilarge_linear_matrix_ptr(work) bind(c)
	  import :: c_ptr
	  type(c_ptr) :: gsl_multilarge_linear_matrix_ptr
	  type(c_ptr), value :: work
	end function gsl_multilarge_linear_matrix_ptr
	
	function gsl_multilarge_linear_rhs_ptr(work) bind(c)
	  import :: c_ptr
	  type(c_ptr) :: gsl_multilarge_linear_rhs_ptr
	  type(c_ptr), value :: work
	end function gsl_multilarge_linear_rhs_ptr
	
	function gsl_multilarge_linear_wstdform1(L, X, w, y, Xs, ys, work) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: L, X, w, y, Xs, ys, work
	  integer(c_int) :: gsl_multilarge_linear_wstdform1
	end function gsl_multilarge_linear_wstdform1
	
	function gsl_multilarge_linear_stdform1(L, X, y, Xs, ys, work) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: L, X, y, Xs, ys, work
	  integer(c_int) :: gsl_multilarge_linear_stdform1
	end function gsl_multilarge_linear_stdform1
	
	function gsl_multilarge_linear_l_decomp(L, tau) &
	  bind(c, name='gsl_multilarge_linear_L_decomp')
	  import :: c_ptr, c_int
	  type(c_ptr), value :: L, tau
	  integer(c_int) :: gsl_multilarge_linear_l_decomp
	end function gsl_multilarge_linear_l_decomp
	
	function gsl_multilarge_linear_wstdform2(LQR, Ltau, X, w, y, Xs, ys, work) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: LQR, Ltau, X, w, y, Xs, ys, work
	  integer(c_int) :: gsl_multilarge_linear_wstdform2
	end function gsl_multilarge_linear_wstdform2
	
	function gsl_multilarge_linear_stdform2(LQR, Ltau, X, y, Xs, ys, work) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: LQR, Ltau, X, y, Xs, ys, work
	  integer(c_int) :: gsl_multilarge_linear_stdform2
	end function gsl_multilarge_linear_stdform2
	
	function gsl_multilarge_linear_genform1(L, cs, c, work) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: L, cs, c, work
	  integer(c_int) :: gsl_multilarge_linear_genform1
	end function gsl_multilarge_linear_genform1
	
	function gsl_multilarge_linear_genform2(LQR, Ltau, cs, c, work) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: LQR, Ltau, cs, c, work
	  integer(c_int) :: gsl_multilarge_linear_genform2
	end function gsl_multilarge_linear_genform2
	
	function fgsl_aux_multilarge_linear_alloc(i) bind(c)
	  import :: c_ptr, c_int
	  integer(c_int), value :: i
	  type(c_ptr) :: fgsl_aux_multilarge_linear_alloc
	end function fgsl_aux_multilarge_linear_alloc

  end interface
contains
!
!>  API
	function fgsl_multilarge_linear_alloc(T, p)
	  type(fgsl_multilarge_linear_type), intent(in) :: T
	  integer(fgsl_size_t), intent(in) :: p
	  type(fgsl_multilarge_linear_workspace) :: fgsl_multilarge_linear_alloc
	  type(c_ptr) :: ftype
	  ftype = fgsl_aux_multilarge_linear_alloc(t%which)
	  fgsl_multilarge_linear_alloc%gsl_multilarge_linear_workspace = &
	  gsl_multilarge_linear_alloc(ftype, p)
	end function fgsl_multilarge_linear_alloc
	
	subroutine fgsl_multilarge_linear_free(w)
	  type(fgsl_multilarge_linear_workspace), intent(inout) :: w
	  call gsl_multilarge_linear_free(w%gsl_multilarge_linear_workspace)
	end subroutine fgsl_multilarge_linear_free
	
	function fgsl_multilarge_linear_name(w)
	  type(fgsl_multilarge_linear_workspace), intent(in) :: w
	  character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_multilarge_linear_name
	  type(c_ptr) :: name
	  name = gsl_multilarge_linear_name(w%gsl_multilarge_linear_workspace)
	  fgsl_multilarge_linear_name = fgsl_name(name)
	end function fgsl_multilarge_linear_name
	
	function fgsl_multilarge_linear_reset(w)
	  type(fgsl_multilarge_linear_workspace), intent(in) :: w
	  integer(fgsl_int) :: fgsl_multilarge_linear_reset
	  fgsl_multilarge_linear_reset = gsl_multilarge_linear_reset(&
	  w%gsl_multilarge_linear_workspace)
	end function fgsl_multilarge_linear_reset
	
	function fgsl_multilarge_linear_accumulate(X, y, w)
	  type(fgsl_matrix), intent(inout) :: X
	  type(fgsl_vector), intent(inout) :: y
	  type(fgsl_multilarge_linear_workspace), intent(in) :: w
	  integer(fgsl_int) :: fgsl_multilarge_linear_accumulate
	  fgsl_multilarge_linear_accumulate = gsl_multilarge_linear_accumulate(&
	  X%gsl_matrix, y%gsl_vector, &
	  w%gsl_multilarge_linear_workspace)
	end function fgsl_multilarge_linear_accumulate
	
	function fgsl_multilarge_linear_solve(lambda, c, rnorm, snorm, w)
	  real(c_double), intent(in) :: lambda
	  type(fgsl_vector), intent(inout) :: c
	  real(c_double), intent(out) :: rnorm, snorm
	  type(fgsl_multilarge_linear_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_multilarge_linear_solve
	  fgsl_multilarge_linear_solve = gsl_multilarge_linear_solve(&
	  lambda, c%gsl_vector, rnorm, snorm, w%gsl_multilarge_linear_workspace)
	end function fgsl_multilarge_linear_solve
	
	function fgsl_multilarge_linear_rcond(rcond, w)
	  real(c_double), intent(out) :: rcond
	  type(fgsl_multilarge_linear_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_multilarge_linear_rcond
	  fgsl_multilarge_linear_rcond = gsl_multilarge_linear_rcond(rcond, &
	  w%gsl_multilarge_linear_workspace)
	end function fgsl_multilarge_linear_rcond
	
	function fgsl_multilarge_linear_lcurve(reg_param, rho, eta, w)
	  type(fgsl_vector), intent(inout) :: reg_param, rho, eta
	  type(fgsl_multilarge_linear_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_multilarge_linear_lcurve
	  fgsl_multilarge_linear_lcurve = gsl_multilarge_linear_lcurve(&
	  reg_param%gsl_vector, rho%gsl_vector, eta%gsl_vector, &
	  w%gsl_multilarge_linear_workspace)
	end function fgsl_multilarge_linear_lcurve
	
	function fgsl_multilarge_linear_matrix_ptr(work)
	  real(fgsl_double), pointer :: fgsl_multilarge_linear_matrix_ptr(:,:)
	  type(fgsl_multilarge_linear_workspace), intent(in) :: work
	  type(fgsl_matrix) :: matrix
	
	  matrix%gsl_matrix = gsl_multilarge_linear_matrix_ptr(work%gsl_multilarge_linear_workspace)
	  fgsl_multilarge_linear_matrix_ptr => fgsl_matrix_to_fptr(matrix)  
	end function fgsl_multilarge_linear_matrix_ptr
	
	function fgsl_multilarge_linear_rhs_ptr(work)
	  real(fgsl_double), pointer :: fgsl_multilarge_linear_rhs_ptr(:)
	  type(fgsl_multilarge_linear_workspace), intent(in) :: work
	  type(fgsl_vector) :: vector
	
	  vector%gsl_vector = gsl_multilarge_linear_rhs_ptr(work%gsl_multilarge_linear_workspace)
	  fgsl_multilarge_linear_rhs_ptr => fgsl_vector_to_fptr(vector)  
	end function fgsl_multilarge_linear_rhs_ptr
	
	function fgsl_multilarge_linear_wstdform1(L, X, w, y, Xs, ys, work)
	  type(fgsl_vector), intent(in) :: L, X, w, y
	  type(fgsl_matrix), intent(inout) :: Xs
	  type(fgsl_vector), intent(inout) :: ys
	  type(fgsl_multilarge_linear_workspace), intent(inout) :: work
	  integer(fgsl_int) :: fgsl_multilarge_linear_wstdform1
	  fgsl_multilarge_linear_wstdform1 = gsl_multilarge_linear_wstdform1(&
	  L%gsl_vector, X%gsl_vector, w%gsl_vector, y%gsl_vector, &
	  Xs%gsl_matrix, ys%gsl_vector, work%gsl_multilarge_linear_workspace)
	end function fgsl_multilarge_linear_wstdform1
	
	function fgsl_multilarge_linear_stdform1(L, X, y, Xs, ys, work)
	  type(fgsl_vector), intent(in) :: L, X, y
	  type(fgsl_matrix), intent(inout) :: Xs
	  type(fgsl_vector), intent(inout) :: ys
	  type(fgsl_multilarge_linear_workspace), intent(inout) :: work
	  integer(fgsl_int) :: fgsl_multilarge_linear_stdform1
	  fgsl_multilarge_linear_stdform1 = gsl_multilarge_linear_stdform1(&
	  L%gsl_vector, X%gsl_vector, y%gsl_vector, &
	  Xs%gsl_matrix, ys%gsl_vector, work%gsl_multilarge_linear_workspace)
	end function fgsl_multilarge_linear_stdform1
	
	function fgsl_multilarge_linear_l_decomp(L, tau)
	  type(fgsl_matrix), intent(inout) :: L
	  type(fgsl_vector), intent(inout) :: tau
	  integer(fgsl_int) :: fgsl_multilarge_linear_l_decomp
	  fgsl_multilarge_linear_l_decomp = gsl_multilarge_linear_l_decomp(&
	  L%gsl_matrix, tau%gsl_vector)
	end function fgsl_multilarge_linear_l_decomp
	
	function fgsl_multilarge_linear_wstdform2(LQR, Ltau, X, w, y, Xs, ys, work)
	  type(fgsl_matrix), intent(in) :: LQR, X
	  type(fgsl_vector), intent(in) :: Ltau, w, y
	  type(fgsl_matrix), intent(inout) :: Xs
	  type(fgsl_vector), intent(inout) :: ys
	  type(fgsl_multilarge_linear_workspace), intent(inout) :: work
	  integer(fgsl_int) :: fgsl_multilarge_linear_wstdform2
	  fgsl_multilarge_linear_wstdform2 = gsl_multilarge_linear_wstdform2(&
	  LQR%gsl_matrix, Ltau%gsl_vector, X%gsl_matrix, w%gsl_vector, y%gsl_vector,&
	  Xs%gsl_matrix, ys%gsl_vector, work%gsl_multilarge_linear_workspace)
	end function fgsl_multilarge_linear_wstdform2
	
	function fgsl_multilarge_linear_stdform2(LQR, Ltau, X, y, Xs, ys, work)
	  type(fgsl_matrix), intent(in) :: LQR, X
	  type(fgsl_vector), intent(in) :: Ltau, y
	  type(fgsl_matrix), intent(inout) :: Xs
	  type(fgsl_vector), intent(inout) :: ys
	  type(fgsl_multilarge_linear_workspace), intent(inout) :: work
	  integer(fgsl_int) :: fgsl_multilarge_linear_stdform2
	  fgsl_multilarge_linear_stdform2 = gsl_multilarge_linear_stdform2(&
	  LQR%gsl_matrix, Ltau%gsl_vector, X%gsl_matrix, y%gsl_vector,&
	  Xs%gsl_matrix, ys%gsl_vector, work%gsl_multilarge_linear_workspace)
	end function fgsl_multilarge_linear_stdform2
	
	function fgsl_multilarge_linear_genform1(L, cs, c, work)
	  type(fgsl_vector), intent(in) :: L, cs
	  type(fgsl_vector), intent(inout) :: c
	  type(fgsl_multilarge_linear_workspace), intent(inout) :: work
	  integer(fgsl_int) :: fgsl_multilarge_linear_genform1
	  fgsl_multilarge_linear_genform1 = gsl_multilarge_linear_genform1(&
	  L%gsl_vector, cs%gsl_vector, c%gsl_vector, &
	  work%gsl_multilarge_linear_workspace)
	end function fgsl_multilarge_linear_genform1
	
	function fgsl_multilarge_linear_genform2(LQR, Ltau, cs, c, work)
	  type(fgsl_matrix), intent(in) :: LQR
	  type(fgsl_vector), intent(in) :: Ltau, cs
	  type(fgsl_vector), intent(inout) :: c
	  type(fgsl_multilarge_linear_workspace), intent(inout) :: work
	  integer(fgsl_int) :: fgsl_multilarge_linear_genform2
	  fgsl_multilarge_linear_genform2 = gsl_multilarge_linear_genform2(&
	  LQR%gsl_matrix, Ltau%gsl_vector, cs%gsl_vector, c%gsl_vector, &
	  work%gsl_multilarge_linear_workspace)
	end function fgsl_multilarge_linear_genform2

end module fgsl_multilarge
