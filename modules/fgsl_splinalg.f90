module fgsl_splinalg
  !> Sparse linear algebra
  use fgsl_base
  use fgsl_array
  use fgsl_sparse_matrix
  implicit none
  
  private :: gsl_splinalg_itersolve_alloc, gsl_splinalg_itersolve_free, &
    gsl_splinalg_itersolve_name, gsl_splinalg_itersolve_iterate, &
    gsl_splinalg_itersolve_normr, fgsl_aux_splinalg_itersolve_alloc
  
  !
  !> Types: sparse matrix linear algebra
  type, public :: fgsl_splinalg_itersolve_type
	private
	integer(c_int) :: which = 0
  end type fgsl_splinalg_itersolve_type
  type(fgsl_splinalg_itersolve_type), public, parameter :: &
	     fgsl_splinalg_itersolve_gmres = fgsl_splinalg_itersolve_type(1)
  type, public :: fgsl_splinalg_itersolve
	private
	type(c_ptr) :: gsl_splinalg_itersolve
  end type fgsl_splinalg_itersolve
  !
  !> C interfaces
  interface
	function gsl_splinalg_itersolve_alloc(T, n, m) bind(c)
	  import :: c_ptr, c_size_t
	  type(c_ptr), value :: T
	  integer(c_size_t), value :: n, m
	  type(c_ptr) :: gsl_splinalg_itersolve_alloc
	end function gsl_splinalg_itersolve_alloc
	subroutine gsl_splinalg_itersolve_free(w) bind(c)
	  import :: c_ptr
	  type(c_ptr), value :: w
	end subroutine gsl_splinalg_itersolve_free
	function gsl_splinalg_itersolve_name(w) bind(c)
	  import :: c_ptr
	  type(c_ptr), value :: w
	  type(c_ptr) :: gsl_splinalg_itersolve_name
	end function gsl_splinalg_itersolve_name
	function gsl_splinalg_itersolve_iterate(A, b, tol, x, w) bind(c)
	  import :: c_ptr, c_double, c_int
	  type(c_ptr), value :: A, b, x, w
	  real(c_double), value :: tol
	  integer(c_int) :: gsl_splinalg_itersolve_iterate
	end function gsl_splinalg_itersolve_iterate
	function gsl_splinalg_itersolve_normr(w) bind(c)
	  import :: c_ptr, c_double
	  type(c_ptr), value :: w
	  real(c_double) :: gsl_splinalg_itersolve_normr
	end function gsl_splinalg_itersolve_normr
	function fgsl_aux_splinalg_itersolve_alloc(i) bind(c)
	  import :: c_int, c_ptr
	  integer(c_int), value :: i
	  type(c_ptr) :: fgsl_aux_splinalg_itersolve_alloc
	end function fgsl_aux_splinalg_itersolve_alloc

  end interface
contains
!
!>  API
	function fgsl_splinalg_itersolve_alloc(T, n, m)
	  type(fgsl_splinalg_itersolve_type), intent(in) :: T
	  integer(fgsl_size_t), intent(in) :: n, m
	  type(fgsl_splinalg_itersolve) :: fgsl_splinalg_itersolve_alloc
	  fgsl_splinalg_itersolve_alloc%gsl_splinalg_itersolve = &
	  gsl_splinalg_itersolve_alloc(fgsl_aux_splinalg_itersolve_alloc(&
	  T%which), n, m)
	end function fgsl_splinalg_itersolve_alloc
	subroutine fgsl_splinalg_itersolve_free(w)
	  type(fgsl_splinalg_itersolve), intent(inout) :: w
	  call gsl_splinalg_itersolve_free(w%gsl_splinalg_itersolve)
	end subroutine fgsl_splinalg_itersolve_free
	function fgsl_splinalg_itersolve_name(w)
	  type(fgsl_splinalg_itersolve), intent(in) :: w
	  character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_splinalg_itersolve_name
	!
	  type(c_ptr) :: name
	!
	  name = gsl_splinalg_itersolve_name(w%gsl_splinalg_itersolve)
	  fgsl_splinalg_itersolve_name = fgsl_name(name)
	end function fgsl_splinalg_itersolve_name
	function fgsl_splinalg_itersolve_iterate(A, b, tol, x, w)
	  type(fgsl_spmatrix), intent(in) :: A
	  type(fgsl_vector), intent(in) :: b
	  real(fgsl_double), intent(in) :: tol
	  type(fgsl_vector), intent(inout) :: x
	  type(fgsl_splinalg_itersolve), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_splinalg_itersolve_iterate
	  fgsl_splinalg_itersolve_iterate = gsl_splinalg_itersolve_iterate(&
	  A%gsl_spmatrix, b%gsl_vector, tol, x%gsl_vector, w%gsl_splinalg_itersolve)
	end function fgsl_splinalg_itersolve_iterate
	function fgsl_splinalg_itersolve_normr(w)
	  type(fgsl_splinalg_itersolve), intent(in) :: w
	  real(fgsl_double) :: fgsl_splinalg_itersolve_normr
	  fgsl_splinalg_itersolve_normr = gsl_splinalg_itersolve_normr(&
	  w%gsl_splinalg_itersolve)
	end function fgsl_splinalg_itersolve_normr

end module fgsl_splinalg
