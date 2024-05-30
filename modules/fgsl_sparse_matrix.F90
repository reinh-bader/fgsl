#include "config.h"
module fgsl_sparse_matrix
  !> Sparse matrices
  !> Note: This module currently only supports double precision
  !> interfaces to gsl_spmatrix.h
  use fgsl_base
  use fgsl_array
  use fgsl_io
  implicit none
  
  private :: gsl_spmatrix_alloc, gsl_spmatrix_alloc_nzmax, gsl_spmatrix_size, &
    gsl_spmatrix_free, gsl_spmatrix_realloc, gsl_spmatrix_set_zero, &
    gsl_spmatrix_nnz, gsl_spmatrix_memcpy, gsl_spmatrix_get, gsl_spmatrix_set, &
    gsl_spmatrix_compcol, gsl_spmatrix_cumsum, gsl_spmatrix_scale, &
    gsl_spmatrix_scale_columns, gsl_spmatrix_scale_rows, gsl_spmatrix_norm1, &
    gsl_spmatrix_minmax, gsl_spmatrix_min_index, gsl_spmatrix_add, &
    gsl_spmatrix_dense_add, gsl_spmatrix_dense_sub, gsl_spmatrix_csc, &
    gsl_spmatrix_csr, gsl_spmatrix_compress, gsl_spmatrix_d2sp, &
    gsl_spmatrix_sp2d, gsl_spmatrix_equal, gsl_spmatrix_transpose_memcpy, &
    gsl_spmatrix_transpose
  private :: gsl_spmatrix_fwrite, gsl_spmatrix_fread, gsl_spmatrix_fprintf, &
    gsl_spmatrix_fscanf
  private :: gsl_aux_spmatrix_getfields
  
  !
  !> Types
  integer(fgsl_size_t), public, parameter :: fgsl_spmatrix_triplet = 0 
  integer(fgsl_size_t), public, parameter :: fgsl_spmatrix_ccs = 1     
  integer(fgsl_size_t), public, parameter :: fgsl_spmatrix_crs = 2     
  integer(fgsl_size_t), public, parameter :: fgsl_spmatrix_type_coo = fgsl_spmatrix_triplet 
  integer(fgsl_size_t), public, parameter :: fgsl_spmatrix_type_csc = fgsl_spmatrix_ccs
  integer(fgsl_size_t), public, parameter :: fgsl_spmatrix_type_csr = fgsl_spmatrix_crs
  type, public :: fgsl_spmatrix
    type(c_ptr) :: gsl_spmatrix = c_null_ptr
  end type fgsl_spmatrix
  !
  !> C interfaces
  interface
	function gsl_spmatrix_alloc(n1, n2) bind(c)
	  import :: c_ptr, c_size_t
	  integer(c_size_t), value :: n1, n2
	  type(c_ptr) :: gsl_spmatrix_alloc
	end function gsl_spmatrix_alloc
	function gsl_spmatrix_alloc_nzmax(n1, n2, nzmax, flags) bind(c)
	  import :: c_ptr, c_size_t
	  integer(c_size_t), value :: n1, n2, nzmax, flags
	  type(c_ptr) :: gsl_spmatrix_alloc_nzmax
	end function gsl_spmatrix_alloc_nzmax
	subroutine gsl_spmatrix_size(m, n1, n2) bind(c)
	  import :: c_ptr, c_size_t
	  type(c_ptr), value :: m
	  integer(c_size_t) :: n1, n2
	end subroutine gsl_spmatrix_size
	subroutine gsl_spmatrix_free(m) bind(c)
	  import :: c_ptr
	  type(c_ptr), value :: m
	end subroutine gsl_spmatrix_free
	function gsl_spmatrix_realloc(nzmax, m) bind(c)
	  import :: c_ptr, c_int, c_size_t
	  integer(c_size_t), value :: nzmax
	  type(c_ptr), value :: m
	  integer(c_int) :: gsl_spmatrix_realloc
	end function gsl_spmatrix_realloc
	function gsl_spmatrix_set_zero(m) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: m
	  integer(c_int) :: gsl_spmatrix_set_zero
	end function gsl_spmatrix_set_zero
	function gsl_spmatrix_nnz(m) bind(c)
	  import :: c_ptr, c_size_t
	  type(c_ptr), value :: m
	  integer(c_size_t) :: gsl_spmatrix_nnz
	end function gsl_spmatrix_nnz
	function gsl_spmatrix_memcpy(dest, src) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: dest, src
	  integer(c_int) :: gsl_spmatrix_memcpy
	end function gsl_spmatrix_memcpy
	function gsl_spmatrix_get(m, i, j) bind(c)
	  import :: c_ptr, c_double, c_size_t
	  type(c_ptr), value :: m
	  integer(c_size_t), value :: i, j
	  real(c_double) :: gsl_spmatrix_get
	end function gsl_spmatrix_get
	function gsl_spmatrix_set(m, i, j, x) bind(c)
	  import :: c_ptr, c_double, c_size_t, c_int
	  type(c_ptr), value :: m
	  integer(c_size_t), value :: i, j
	  real(c_double), value :: x
	  integer(c_int) :: gsl_spmatrix_set
	end function gsl_spmatrix_set
	function gsl_spmatrix_compcol(T) bind(c)
	  import :: c_ptr
	  type(c_ptr) :: gsl_spmatrix_compcol
	  type(c_ptr), value :: T
	end function gsl_spmatrix_compcol
	subroutine gsl_spmatrix_cumsum(n, c) bind(c)
	  import :: c_size_t, c_ptr
	  integer(c_size_t), value :: n
	  type(c_ptr), value :: c ! c is array of type c_size_t with n+1 elements
	end subroutine gsl_spmatrix_cumsum
	function gsl_spmatrix_scale(m, x) bind(c)
	  import :: c_ptr, c_double, c_int
	  type(c_ptr), value :: m
	  real(c_double), value :: x
	  integer(c_int) :: gsl_spmatrix_scale
	end function gsl_spmatrix_scale
	function gsl_spmatrix_scale_columns(a, x) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: a, x
	  integer(c_int) :: gsl_spmatrix_scale_columns
	end function gsl_spmatrix_scale_columns
	function gsl_spmatrix_scale_rows(a, x) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: a, x
	  integer(c_int) :: gsl_spmatrix_scale_rows
	end function gsl_spmatrix_scale_rows
	function gsl_spmatrix_norm1(a) bind(c)
	  import :: c_ptr, c_double
	  type(c_ptr), value :: a
	  real(c_double) :: gsl_spmatrix_norm1
	end function gsl_spmatrix_norm1
	function gsl_spmatrix_minmax(m, min_out, max_out) bind(c)
	  import :: c_ptr, c_double, c_int
	  type(c_ptr), value :: m
	  real(c_double) :: min_out, max_out
	  integer(c_int) :: gsl_spmatrix_minmax
	end function gsl_spmatrix_minmax
	function gsl_spmatrix_min_index(m, imin, jmin) bind(c)
	  import :: c_ptr, c_double, c_int
	  type(c_ptr), value :: m
	  real(c_double) :: imin, jmin
	  integer(c_int) :: gsl_spmatrix_min_index
	end function gsl_spmatrix_min_index
	function gsl_spmatrix_add(c, a, b) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: c, a, b
	  integer(c_int) :: gsl_spmatrix_add
	end function gsl_spmatrix_add
	function gsl_spmatrix_dense_add(a, b) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: a, b
	  integer(c_int) :: gsl_spmatrix_dense_add
	end function gsl_spmatrix_dense_add
	function gsl_spmatrix_dense_sub(a, b) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: a, b
	  integer(c_int) :: gsl_spmatrix_dense_sub
	end function gsl_spmatrix_dense_sub
	function gsl_spmatrix_csc(dest, src) bind(c)
	  import :: c_ptr, c_double, c_int
	  type(c_ptr), value :: dest, src
	  integer(c_int) :: gsl_spmatrix_csc
	end function gsl_spmatrix_csc
	function gsl_spmatrix_csr(dest, src) bind(c)
	  import :: c_ptr, c_double, c_int
	  type(c_ptr), value :: dest, src
	  integer(c_int) :: gsl_spmatrix_csr
	end function gsl_spmatrix_csr
	function gsl_spmatrix_compress(src, sptype) bind(c)
	  import :: c_ptr, c_double, c_int
	  type(c_ptr), value :: src
	  integer(c_int), value :: sptype
	  type(c_ptr) :: gsl_spmatrix_compress
	end function gsl_spmatrix_compress
	function gsl_spmatrix_d2sp(S, A) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: S, A
	  integer(c_int) :: gsl_spmatrix_d2sp
	end function gsl_spmatrix_d2sp
	function gsl_spmatrix_sp2d(A, S) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: A, S
	  integer(c_int) :: gsl_spmatrix_sp2d
	end function gsl_spmatrix_sp2d
	function gsl_spmatrix_equal(a, b) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: a, b
	  integer(c_int) :: gsl_spmatrix_equal
	end function gsl_spmatrix_equal
	function gsl_spmatrix_transpose_memcpy(dest, src) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: dest, src
	  integer(c_int) :: gsl_spmatrix_transpose_memcpy
	end function gsl_spmatrix_transpose_memcpy
	function gsl_spmatrix_transpose(m) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: m
	  integer(c_int) :: gsl_spmatrix_transpose
	end function gsl_spmatrix_transpose
	!
	!> I/O
	integer(c_int) function gsl_spmatrix_fwrite(stream, m) bind(c)
	  import :: c_int, c_ptr
	  type(c_ptr), value :: stream, m
	end function gsl_spmatrix_fwrite
	integer(c_int) function gsl_spmatrix_fread(stream, m) bind(c)
	  import :: c_int, c_ptr
	  type(c_ptr), value :: stream, m
	end function gsl_spmatrix_fread
	integer(c_int) function gsl_spmatrix_fprintf(stream, m, format) bind(c)
	  import :: c_int, c_ptr, c_char
	  type(c_ptr), value :: stream, m
	  character(kind=c_char), intent(in) :: format(*)
	end function gsl_spmatrix_fprintf
	type(c_ptr) function gsl_spmatrix_fscanf(stream) bind(c)
	  import :: c_int, c_ptr
	  type(c_ptr), value :: stream
	end function gsl_spmatrix_fscanf
	!
	!> helper
	subroutine gsl_aux_spmatrix_getfields(m, ip, dp, pp, psize) bind(c)
	  import :: c_size_t, c_ptr
	  type(c_ptr), value :: m 
	  type(c_ptr) :: ip, dp, pp
	  integer(c_size_t) :: psize
	end subroutine
  end interface
contains
	!
	!>  API
	function fgsl_spmatrix_alloc(n1, n2)
	  integer(fgsl_size_t), intent(in) :: n1, n2
	  type(fgsl_spmatrix) :: fgsl_spmatrix_alloc
	  fgsl_spmatrix_alloc%gsl_spmatrix = gsl_spmatrix_alloc(n1, n2)
	end function fgsl_spmatrix_alloc
	function fgsl_spmatrix_alloc_nzmax(n1, n2, nzmax, flags)
	  integer(fgsl_size_t), intent(in) :: n1, n2, nzmax, flags
	  type(fgsl_spmatrix) :: fgsl_spmatrix_alloc_nzmax
	  fgsl_spmatrix_alloc_nzmax%gsl_spmatrix = &
	  gsl_spmatrix_alloc_nzmax(n1, n2, nzmax, flags)
	end function fgsl_spmatrix_alloc_nzmax
	subroutine fgsl_spmatrix_size(m, n1, n2)
	  type(fgsl_spmatrix), intent(in) :: m
	  integer(fgsl_size_t), intent(inout) :: n1, n2
	  call gsl_spmatrix_size(m%gsl_spmatrix, n1, n2)
	end subroutine
	subroutine fgsl_spmatrix_free(m)
	  type(fgsl_spmatrix), intent(in) :: m
	  call gsl_spmatrix_free(m%gsl_spmatrix)
	end subroutine fgsl_spmatrix_free
	function fgsl_spmatrix_realloc(nzmax, m)
	  integer(fgsl_size_t), intent(in) :: nzmax
	  type(fgsl_spmatrix), intent(inout) :: m
	  integer(fgsl_int) :: fgsl_spmatrix_realloc
	  fgsl_spmatrix_realloc = gsl_spmatrix_realloc(nzmax, m%gsl_spmatrix)
	end function fgsl_spmatrix_realloc
	function fgsl_spmatrix_set_zero(m)
	  type(fgsl_spmatrix), intent(inout) :: m
	  integer(fgsl_int) :: fgsl_spmatrix_set_zero
	  fgsl_spmatrix_set_zero = gsl_spmatrix_set_zero(m%gsl_spmatrix)
	end function fgsl_spmatrix_set_zero
	function fgsl_spmatrix_nnz(m)
	  type(fgsl_spmatrix), intent(in) :: m
	  integer(fgsl_size_t) :: fgsl_spmatrix_nnz
	  fgsl_spmatrix_nnz = gsl_spmatrix_nnz(m%gsl_spmatrix)
	end function fgsl_spmatrix_nnz
	function fgsl_spmatrix_memcpy(dest, src)
	  type(fgsl_spmatrix), intent(inout) :: dest
	  type(fgsl_spmatrix), intent(in) :: src
	  integer(fgsl_int) :: fgsl_spmatrix_memcpy
	  fgsl_spmatrix_memcpy = gsl_spmatrix_memcpy(dest%gsl_spmatrix, src%gsl_spmatrix)
	end function fgsl_spmatrix_memcpy
	function fgsl_spmatrix_get(m, i, j)
	  type(fgsl_spmatrix), intent(in) :: m
	  integer(fgsl_size_t), intent(in) :: i, j
	  real(fgsl_double) :: fgsl_spmatrix_get
	  fgsl_spmatrix_get = gsl_spmatrix_get(m%gsl_spmatrix, i ,j)
	end function fgsl_spmatrix_get
	function fgsl_spmatrix_set(m, i, j, x)
	  type(fgsl_spmatrix), intent(in) :: m
	  integer(fgsl_size_t), intent(in) :: i, j
	  real(fgsl_double), intent(in) :: x
	  integer(fgsl_int) :: fgsl_spmatrix_set
	  fgsl_spmatrix_set = gsl_spmatrix_set(m%gsl_spmatrix, i ,j, x)
	end function fgsl_spmatrix_set
	function fgsl_spmatrix_compcol(T)
	  type(fgsl_spmatrix), intent(in) :: T
	  type(fgsl_spmatrix) :: fgsl_spmatrix_compcol
	  fgsl_spmatrix_compcol%gsl_spmatrix = gsl_spmatrix_compcol(T%gsl_spmatrix)
	end function fgsl_spmatrix_compcol
	subroutine fgsl_spmatrix_cumsum(n, c)
	  integer(fgsl_size_t), intent(in) :: n
	  integer(fgsl_size_t), dimension(:), intent(inout), target, contiguous :: c
	  if (size(c, dim=1) /= n+1) then
	    call fgsl_error('c must have n+1 elements', 'fgsl_spmatrix', __LINE__, fgsl_ebadlen)
	    return
	  endif
	  call gsl_spmatrix_cumsum(n, c_loc(c))
	end subroutine fgsl_spmatrix_cumsum
	function fgsl_spmatrix_scale(m , x)
	  type(fgsl_spmatrix), intent(inout) :: m
	  real(fgsl_double), intent(in) :: x
	  integer(fgsl_int) :: fgsl_spmatrix_scale
	  fgsl_spmatrix_scale = gsl_spmatrix_scale(m%gsl_spmatrix, x)
	end function fgsl_spmatrix_scale
	function fgsl_spmatrix_scale_columns(a , x)
	  type(fgsl_spmatrix), intent(inout) :: a
	  type(fgsl_vector), intent(in) :: x
	  integer(fgsl_int) :: fgsl_spmatrix_scale_columns
	  fgsl_spmatrix_scale_columns = gsl_spmatrix_scale_columns(a%gsl_spmatrix, x%gsl_vector)
	end function fgsl_spmatrix_scale_columns
	function fgsl_spmatrix_scale_rows(a , x)
	  type(fgsl_spmatrix), intent(inout) :: a
	  type(fgsl_vector), intent(in) :: x
	  integer(fgsl_int) :: fgsl_spmatrix_scale_rows
	  fgsl_spmatrix_scale_rows = gsl_spmatrix_scale_rows(a%gsl_spmatrix, x%gsl_vector)
	end function fgsl_spmatrix_scale_rows
	function fgsl_spmatrix_norm1(a)
	  type(fgsl_spmatrix), intent(in) :: a
	  real(fgsl_double) :: fgsl_spmatrix_norm1
	  fgsl_spmatrix_norm1 = gsl_spmatrix_norm1(a%gsl_spmatrix)
	end function fgsl_spmatrix_norm1
	function fgsl_spmatrix_minmax(m , min_out, max_out)
	  type(fgsl_spmatrix), intent(in) :: m
	  real(fgsl_double), intent(out) :: min_out, max_out
	  integer(fgsl_int) :: fgsl_spmatrix_minmax
	  fgsl_spmatrix_minmax = gsl_spmatrix_minmax(m%gsl_spmatrix, min_out, max_out)
	end function fgsl_spmatrix_minmax
	function fgsl_spmatrix_min_index(m , imin, jmin)
	  type(fgsl_spmatrix), intent(in) :: m
	  real(fgsl_double), intent(out) :: imin, jmin
	  integer(fgsl_int) :: fgsl_spmatrix_min_index
	  fgsl_spmatrix_min_index = gsl_spmatrix_min_index(m%gsl_spmatrix, imin, jmin)
	end function fgsl_spmatrix_min_index
	function fgsl_spmatrix_csc(dest, src)
	  type(fgsl_spmatrix), intent(inout) :: dest
	  type(fgsl_spmatrix), intent(in) :: src
	  integer(fgsl_int) :: fgsl_spmatrix_csc
	  fgsl_spmatrix_csc = gsl_spmatrix_csc(dest%gsl_spmatrix, src%gsl_spmatrix)
	end function fgsl_spmatrix_csc
	function fgsl_spmatrix_csr(dest, src)
	  type(fgsl_spmatrix), intent(inout) :: dest
	  type(fgsl_spmatrix), intent(in) :: src
	  integer(fgsl_int) :: fgsl_spmatrix_csr
	  fgsl_spmatrix_csr = gsl_spmatrix_csr(dest%gsl_spmatrix, src%gsl_spmatrix)
	end function fgsl_spmatrix_csr
	function fgsl_spmatrix_compress(src, sptype)
	  type(fgsl_spmatrix), intent(in) :: src
	  integer(fgsl_int), intent(in) :: sptype
	  type(fgsl_spmatrix) :: fgsl_spmatrix_compress
	  fgsl_spmatrix_compress%gsl_spmatrix = gsl_spmatrix_compress(src%gsl_spmatrix, sptype)
	end function fgsl_spmatrix_compress
	function fgsl_spmatrix_add(c, a, b)
	  type(fgsl_spmatrix), intent(inout) :: c
	  type(fgsl_spmatrix), intent(in) :: a, b
	  integer(fgsl_int) :: fgsl_spmatrix_add
	  fgsl_spmatrix_add = gsl_spmatrix_add(c%gsl_spmatrix, &
	  a%gsl_spmatrix, b%gsl_spmatrix)
	end function fgsl_spmatrix_add
	function fgsl_spmatrix_dense_add(a, b)
	  type(fgsl_matrix), intent(inout) :: a
	  type(fgsl_spmatrix), intent(in) ::  b
	  integer(fgsl_int) :: fgsl_spmatrix_dense_add
	  fgsl_spmatrix_dense_add = gsl_spmatrix_dense_add(a%gsl_matrix, b%gsl_spmatrix)
	end function fgsl_spmatrix_dense_add
	! the following call deprecated
	function fgsl_spmatrix_add_to_dense(a, b)
	  type(fgsl_matrix), intent(inout) :: a
	  type(fgsl_spmatrix), intent(in) ::  b
	  integer(fgsl_int) :: fgsl_spmatrix_add_to_dense
	  fgsl_spmatrix_add_to_dense = gsl_spmatrix_dense_add(a%gsl_matrix, b%gsl_spmatrix)
	end function fgsl_spmatrix_add_to_dense
	function fgsl_spmatrix_dense_sub(a, b)
	  type(fgsl_matrix), intent(inout) :: a
	  type(fgsl_spmatrix), intent(in) ::  b
	  integer(fgsl_int) :: fgsl_spmatrix_dense_sub
	  fgsl_spmatrix_dense_sub = gsl_spmatrix_dense_sub(a%gsl_matrix, b%gsl_spmatrix)
	end function fgsl_spmatrix_dense_sub
	function fgsl_spmatrix_d2sp(S, A)
	  type(fgsl_spmatrix), intent(inout) :: S
	  type(fgsl_matrix), intent(in) :: A
	  integer(fgsl_int) :: fgsl_spmatrix_d2sp
	  fgsl_spmatrix_d2sp = gsl_spmatrix_d2sp(S%gsl_spmatrix, A%gsl_matrix)
	end function fgsl_spmatrix_d2sp
	function fgsl_spmatrix_sp2d(A, S)
	  type(fgsl_matrix), intent(inout) :: A
	  type(fgsl_spmatrix), intent(in) :: S
	  integer(fgsl_int) :: fgsl_spmatrix_sp2d
	  fgsl_spmatrix_sp2d = gsl_spmatrix_sp2d(A%gsl_matrix, S%gsl_spmatrix)
	end function fgsl_spmatrix_sp2d
	function fgsl_spmatrix_equal(a, b)
	  type(fgsl_spmatrix), intent(in) :: a, b
	  integer(fgsl_int) :: fgsl_spmatrix_equal
	  fgsl_spmatrix_equal = gsl_spmatrix_equal(a%gsl_spmatrix, b%gsl_spmatrix)
	end function fgsl_spmatrix_equal
	function fgsl_spmatrix_transpose_memcpy(dest, src)
	  type(fgsl_spmatrix), intent(inout) :: dest
	  type(fgsl_spmatrix), intent(in) :: src
	  integer(fgsl_int) :: fgsl_spmatrix_transpose_memcpy
	  fgsl_spmatrix_transpose_memcpy = gsl_spmatrix_transpose_memcpy(dest%gsl_spmatrix, src%gsl_spmatrix)
	end function fgsl_spmatrix_transpose_memcpy
	function fgsl_spmatrix_transpose(m)
	  type(fgsl_spmatrix), intent(inout) :: m
	  integer(fgsl_int) :: fgsl_spmatrix_transpose
	  fgsl_spmatrix_transpose = gsl_spmatrix_transpose(m%gsl_spmatrix)
	end function fgsl_spmatrix_transpose
	!
	! I/O
	!
	integer(fgsl_int) function fgsl_spmatrix_fwrite(stream, m)
	  type(fgsl_file) :: stream
	  type(fgsl_spmatrix), intent(in) :: m
	  fgsl_spmatrix_fwrite = gsl_spmatrix_fwrite(stream%gsl_file, m%gsl_spmatrix)
	end function fgsl_spmatrix_fwrite
	integer(fgsl_int) function fgsl_spmatrix_fread(stream, m)
	  type(fgsl_file) :: stream
	  type(fgsl_spmatrix), intent(inout) :: m
	  fgsl_spmatrix_fread = gsl_spmatrix_fread(stream%gsl_file, m%gsl_spmatrix)
	end function fgsl_spmatrix_fread
	integer(fgsl_int) function fgsl_spmatrix_fprintf(stream, m, format)
	  type(fgsl_file) :: stream
	  type(fgsl_spmatrix), intent(in) :: m
	  character(kind=fgsl_char, len=*), intent(in) :: format
	  fgsl_spmatrix_fprintf = gsl_spmatrix_fprintf(stream%gsl_file, m%gsl_spmatrix, format // c_null_char)
	end function fgsl_spmatrix_fprintf
	type(fgsl_spmatrix) function fgsl_spmatrix_fscanf(stream)
	  type(fgsl_file) :: stream
	  fgsl_spmatrix_fscanf%gsl_spmatrix = gsl_spmatrix_fscanf(stream%gsl_file)
	end function fgsl_spmatrix_fscanf
	!
	! helper function
	!
	subroutine fgsl_spmatrix_getfields(m, i, p, d)
	  type(fgsl_spmatrix), intent(in) :: m
	  integer(fgsl_int), pointer, intent(inout) :: i(:), p(:)
	  real(fgsl_double), pointer, intent(inout) :: d(:)
	
	  type(c_ptr) :: ip, pp, dp
	  integer(fgsl_size_t) :: nz, psize
	
	  call gsl_aux_spmatrix_getfields(m%gsl_spmatrix, ip, dp, pp, psize)
	  nz = gsl_spmatrix_nnz(m%gsl_spmatrix) 
	  call c_f_pointer(ip, i, [nz])
	  call c_f_pointer(dp, d, [nz])
	  call c_f_pointer(pp, p, [psize])
	end subroutine fgsl_spmatrix_getfields
end module fgsl_sparse_matrix
