module fgsl_permutations
  !> Permutations
  use fgsl_array
  use fgsl_io
  implicit none

  private :: gsl_permutation_alloc, gsl_permutation_calloc, gsl_permutation_free, &
       gsl_permutation_memcpy, gsl_permutation_get, gsl_permutation_size, &
       gsl_permutation_data, gsl_permutation_valid, gsl_permutation_reverse, &
       gsl_permutation_inverse, gsl_permutation_next, gsl_permutation_prev, &
       gsl_permute, gsl_permute_long, gsl_permute_inverse, gsl_permute_long_inverse, &
       gsl_permute_vector, gsl_permute_vector_inverse, gsl_permute_matrix, &
       gsl_permutation_mul, gsl_permutation_fwrite, gsl_permutation_fread, &
       gsl_permutation_fprintf, gsl_permutation_fscanf, &
       gsl_permutation_linear_to_canonical, gsl_permutation_canonical_to_linear, &
       gsl_permutation_inversions, gsl_permutation_linear_cycles, &
       gsl_permutation_canonical_cycles, gsl_aux_sizeof_permutation, &
       fgsl_sizeof_permutation

  !
  ! Types
  type, public :: fgsl_permutation
     type(c_ptr) :: gsl_permutation = c_null_ptr
  end type fgsl_permutation
  !
  ! Generic interfaces
  interface fgsl_well_defined
     module procedure fgsl_permutation_status
  end interface fgsl_well_defined
  interface fgsl_sizeof
     module procedure fgsl_sizeof_permutation
  end interface fgsl_sizeof
  interface fgsl_permute
     module procedure fgsl_permute
     module procedure fgsl_permute_long
  end interface fgsl_permute
  interface fgsl_permute_inverse
     module procedure fgsl_permute_inverse
     module procedure fgsl_permute_long_inverse
  end interface fgsl_permute_inverse
  !
  ! C interfaces
  interface
     function gsl_permutation_alloc(n) bind(c)
       import
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_permutation_alloc
     end function gsl_permutation_alloc
     function gsl_permutation_calloc(n) bind(c)
       import
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_permutation_calloc
     end function gsl_permutation_calloc
     subroutine gsl_permutation_init(p) bind(c)
       import
       type(c_ptr), value :: p
     end subroutine gsl_permutation_init
     subroutine gsl_permutation_free(p) bind(c)
       import
       type(c_ptr), value :: p
     end subroutine gsl_permutation_free
     function gsl_permutation_memcpy(dest, src) bind(c)
       import
       type(c_ptr), value :: dest
       type(c_ptr), value :: src
       integer(c_int) :: gsl_permutation_memcpy
     end function gsl_permutation_memcpy
     function gsl_permutation_get(p, i) bind(c)
       import
       type(c_ptr), value :: p
       integer(c_size_t), value :: i
       integer(c_size_t) :: gsl_permutation_get
     end function gsl_permutation_get
     function gsl_permutation_swap(p, i, j) bind(c)
       import
       type(c_ptr), value :: p
       integer(c_size_t), value :: i, j
       integer(c_int) :: gsl_permutation_swap
     end function gsl_permutation_swap
     function gsl_permutation_size(p) bind(c)
       import
       type(c_ptr), value :: p
       integer(c_size_t) :: gsl_permutation_size
     end function gsl_permutation_size
     function gsl_permutation_data(p) bind(c)
       import
       type(c_ptr), value :: p
       type(c_ptr) :: gsl_permutation_data
     end function gsl_permutation_data
     function gsl_permutation_valid(p) bind(c)
       import
       type(c_ptr), value :: p
       integer(c_int) :: gsl_permutation_valid
     end function gsl_permutation_valid
     subroutine gsl_permutation_reverse(p) bind(c)
       import
       type(c_ptr), value :: p
     end subroutine gsl_permutation_reverse
     function gsl_permutation_inverse(inv, p) bind(c)
       import
       type(c_ptr), value :: inv
       type(c_ptr), value :: p
       integer(c_int) :: gsl_permutation_inverse
     end function gsl_permutation_inverse
     function gsl_permutation_next(p) bind(c)
       import
       type(c_ptr), value :: p
       integer(c_int) :: gsl_permutation_next
     end function gsl_permutation_next
     function gsl_permutation_prev(p) bind(c)
       import
       type(c_ptr), value :: p
       integer(c_int) :: gsl_permutation_prev
     end function gsl_permutation_prev
     function gsl_permute(p, data, stride, n) bind(c)
       import
       type(c_ptr), value :: p
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data
       integer(c_int) :: gsl_permute
     end function gsl_permute
     function gsl_permute_long(p, data, stride, n) bind(c)
       import
       type(c_ptr), value :: p
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data
       integer(c_int) :: gsl_permute_long
     end function gsl_permute_long
     function gsl_permute_inverse(p, data, stride, n) bind(c)
       import
       type(c_ptr), value :: p
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data
       integer(c_int) :: gsl_permute_inverse
     end function gsl_permute_inverse
     function gsl_permute_long_inverse(p, data, stride, n) bind(c)
       import
       type(c_ptr), value :: p
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data
       integer(c_int) :: gsl_permute_long_inverse
     end function gsl_permute_long_inverse
     function gsl_permute_vector(p,v) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: p,v
       integer(c_int) :: gsl_permute_vector
     end function gsl_permute_vector
     function gsl_permute_vector_inverse(p,v) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: p,v
       integer(c_int) :: gsl_permute_vector_inverse
     end function gsl_permute_vector_inverse
     function gsl_permute_matrix(p,v) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: p,v
       integer(c_int) :: gsl_permute_matrix
     end function gsl_permute_matrix
     function gsl_permutation_mul(p, pa, pb) bind(c)
       import
       type(c_ptr), value :: p
       type(c_ptr), value :: pa, pb
       integer(c_int) :: gsl_permutation_mul
     end function gsl_permutation_mul
     function gsl_permutation_fwrite(stream, p) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: stream, p
       integer(c_int) :: gsl_permutation_fwrite
     end function gsl_permutation_fwrite
     function gsl_permutation_fread(stream, p) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: stream, p
       integer(c_int) :: gsl_permutation_fread
     end function gsl_permutation_fread
     function gsl_permutation_fprintf(stream, p, format) bind(c)
       import :: c_ptr, c_int, c_char
       type(c_ptr), value :: stream, p
       character(kind=c_char) :: format
       integer(c_int) :: gsl_permutation_fprintf
     end function gsl_permutation_fprintf
     function gsl_permutation_fscanf(stream, p) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: stream, p
       integer(c_int) :: gsl_permutation_fscanf
     end function gsl_permutation_fscanf
     function gsl_permutation_linear_to_canonical(q, p) bind(c)
       import
       type(c_ptr), value :: q
       type(c_ptr), value :: p
       integer(c_int) :: gsl_permutation_linear_to_canonical
     end function gsl_permutation_linear_to_canonical
     function gsl_permutation_canonical_to_linear(p, q) bind(c)
       import
       type(c_ptr), value :: p
       type(c_ptr), value :: q
       integer(c_int) :: gsl_permutation_canonical_to_linear
     end function gsl_permutation_canonical_to_linear
     function gsl_permutation_inversions(p) bind(c)
       import
       type(c_ptr), value :: p
       integer(c_size_t) :: gsl_permutation_inversions
     end function gsl_permutation_inversions
     function gsl_permutation_linear_cycles(p) bind(c)
       import
       type(c_ptr), value :: p
       integer(c_size_t) :: gsl_permutation_linear_cycles
     end function gsl_permutation_linear_cycles
     function gsl_permutation_canonical_cycles(p) bind(c)
       import
       type(c_ptr), value :: p
       integer(c_size_t) :: gsl_permutation_canonical_cycles
     end function gsl_permutation_canonical_cycles
     function gsl_aux_sizeof_permutation() bind(c)
       import :: c_size_t
       integer(c_size_t) :: gsl_aux_sizeof_permutation
     end function gsl_aux_sizeof_permutation
  end interface

contains
  function fgsl_permutation_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_permutation) :: fgsl_permutation_alloc
    fgsl_permutation_alloc%gsl_permutation = gsl_permutation_alloc(n)
  end function fgsl_permutation_alloc
  function fgsl_permutation_calloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_permutation) :: fgsl_permutation_calloc
    fgsl_permutation_calloc%gsl_permutation = gsl_permutation_calloc(n)
  end function fgsl_permutation_calloc
  subroutine fgsl_permutation_init(p)
    type(fgsl_permutation), intent(inout) :: p
    call gsl_permutation_init(p%gsl_permutation)
  end subroutine fgsl_permutation_init
  subroutine fgsl_permutation_free(p)
    type(fgsl_permutation), intent(inout) :: p
    call gsl_permutation_free(p%gsl_permutation)
  end subroutine fgsl_permutation_free
  function fgsl_permutation_memcpy(dest, src)
    type(fgsl_permutation), intent(inout) :: dest
    type(fgsl_permutation),  intent(in) :: src
    integer(fgsl_int) :: fgsl_permutation_memcpy
    fgsl_permutation_memcpy = gsl_permutation_memcpy(dest%gsl_permutation, &
         src%gsl_permutation)
  end function fgsl_permutation_memcpy
  function fgsl_permutation_get(p, i)
    type(fgsl_permutation), intent(inout) :: p
    integer(fgsl_size_t), intent(in) :: i
    integer(fgsl_size_t) :: fgsl_permutation_get
    fgsl_permutation_get = gsl_permutation_get(p%gsl_permutation, i)
  end function fgsl_permutation_get
  function fgsl_permutation_swap(p, i, j)
    type(fgsl_permutation), intent(inout) :: p
    integer(fgsl_size_t), intent(in) :: i, j
    integer(fgsl_int) :: fgsl_permutation_swap
    fgsl_permutation_swap = gsl_permutation_swap(p%gsl_permutation, i, j)
  end function fgsl_permutation_swap
  function fgsl_permutation_size(p)
    type(fgsl_permutation), intent(in) :: p
    integer(fgsl_size_t) :: fgsl_permutation_size
    fgsl_permutation_size = gsl_permutation_size(p%gsl_permutation)
  end function fgsl_permutation_size
  function fgsl_permutation_data(p)
    type(fgsl_permutation), intent(in) :: p
    integer(fgsl_size_t), pointer :: fgsl_permutation_data(:)
    !
    integer(fgsl_size_t) :: size
    type(c_ptr) :: pdata
    size = gsl_permutation_size(p%gsl_permutation)
    pdata = gsl_permutation_data(p%gsl_permutation)
    if (c_associated(pdata)) then
       call c_f_pointer(pdata,fgsl_permutation_data,(/size/))
    else
       nullify(fgsl_permutation_data)
    end if
  end function fgsl_permutation_data
  function fgsl_permutation_valid(p)
    type(fgsl_permutation), intent(in) :: p
    integer(fgsl_int) :: fgsl_permutation_valid
    fgsl_permutation_valid = gsl_permutation_valid(p%gsl_permutation)
  end function fgsl_permutation_valid
  subroutine fgsl_permutation_reverse(p)
    type(fgsl_permutation), intent(inout) :: p
    call gsl_permutation_reverse(p%gsl_permutation)
  end subroutine fgsl_permutation_reverse
  function fgsl_permutation_inverse(inv, p)
    type(fgsl_permutation), intent(inout) :: inv
    type(fgsl_permutation),  intent(in) :: p
    integer(fgsl_int) :: fgsl_permutation_inverse
    fgsl_permutation_inverse = gsl_permutation_inverse(inv%gsl_permutation, &
         p%gsl_permutation)
  end function fgsl_permutation_inverse
  function fgsl_permutation_next(p)
    type(fgsl_permutation), intent(in) :: p
    integer(fgsl_int) :: fgsl_permutation_next
    fgsl_permutation_next = gsl_permutation_next(p%gsl_permutation)
  end function fgsl_permutation_next
  function fgsl_permutation_prev(p)
    type(fgsl_permutation), intent(in) :: p
    integer(fgsl_int) :: fgsl_permutation_prev
    fgsl_permutation_prev = gsl_permutation_prev(p%gsl_permutation)
  end function fgsl_permutation_prev
  function fgsl_permute(p, data, stride, n)
    integer(fgsl_size_t), intent(in), target, contiguous :: p(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double), dimension(:), intent(inout), target, contiguous :: data
    integer(fgsl_int) :: fgsl_permute
    fgsl_permute = gsl_permute(c_loc(p), c_loc(data), stride, n)
  end function fgsl_permute
  function fgsl_permute_long(p, data, stride, n)
    integer(fgsl_size_t), intent(in), target, contiguous :: p(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_long), dimension(:), intent(inout), target, contiguous :: data
    integer(fgsl_int) :: fgsl_permute_long
    fgsl_permute_long = gsl_permute_long(c_loc(p), c_loc(data), stride, n)
  end function fgsl_permute_long
  function fgsl_permute_inverse(p, data, stride, n)
    integer(fgsl_size_t), intent(in), target, contiguous :: p(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double), dimension(:), intent(inout), target, contiguous :: data
    integer(fgsl_int) :: fgsl_permute_inverse
    fgsl_permute_inverse = gsl_permute_inverse(c_loc(p), c_loc(data), stride, n)
  end function fgsl_permute_inverse
  function fgsl_permute_long_inverse(p, data, stride, n)
    integer(fgsl_size_t), intent(in), target, contiguous :: p(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_long), dimension(:), intent(inout), target, contiguous :: data
    integer(fgsl_int) :: fgsl_permute_long_inverse
    fgsl_permute_long_inverse = gsl_permute_long_inverse(c_loc(p), c_loc(data), stride, n)
  end function fgsl_permute_long_inverse
  function fgsl_permute_vector(p,v)
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector), intent(inout) :: v
    integer(fgsl_int) :: fgsl_permute_vector
    fgsl_permute_vector = &
         gsl_permute_vector(p%gsl_permutation,v%gsl_vector)
  end function fgsl_permute_vector
  function fgsl_permute_vector_inverse(p,v)
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_vector), intent(inout) :: v
    integer(fgsl_int) :: fgsl_permute_vector_inverse
    fgsl_permute_vector_inverse = &
         gsl_permute_vector_inverse(p%gsl_permutation,v%gsl_vector)
  end function fgsl_permute_vector_inverse
  function fgsl_permute_matrix(p,a)
    type(fgsl_permutation), intent(in) :: p
    type(fgsl_matrix), intent(inout) :: a
    integer(fgsl_int) :: fgsl_permute_matrix
    fgsl_permute_matrix = &
         gsl_permute_matrix(p%gsl_permutation,a%gsl_matrix)
  end function fgsl_permute_matrix
  function fgsl_permutation_mul(p, pa, pb)
    type(fgsl_permutation), intent(inout) :: p
    type(fgsl_permutation),  intent(in) :: pa, pb
    integer(fgsl_int) :: fgsl_permutation_mul
    fgsl_permutation_mul = gsl_permutation_mul(p%gsl_permutation, &
         pa%gsl_permutation, pb%gsl_permutation)
  end function fgsl_permutation_mul
  function fgsl_permutation_fwrite(stream, p)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_permutation), intent(in) :: p
    integer(fgsl_int) :: fgsl_permutation_fwrite
    fgsl_permutation_fwrite = gsl_permutation_fwrite(stream%gsl_file, &
         p%gsl_permutation)
  end function fgsl_permutation_fwrite
  function fgsl_permutation_fread(stream, p)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_permutation), intent(inout) :: p
    integer(fgsl_int) :: fgsl_permutation_fread
    fgsl_permutation_fread = gsl_permutation_fread(stream%gsl_file, &
         p%gsl_permutation)
  end function fgsl_permutation_fread
  function fgsl_permutation_fprintf(stream, p, format)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_permutation), intent(in) :: p
    character(kind=fgsl_char, len=*), intent(in) :: format
    integer(fgsl_int) :: fgsl_permutation_fprintf
    !
    fgsl_permutation_fprintf = &
         gsl_permutation_fprintf(stream%gsl_file, p%gsl_permutation, &
         format // c_null_char)
  end function fgsl_permutation_fprintf
  function fgsl_permutation_fscanf(stream, p)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_permutation), intent(inout) :: p
    integer(fgsl_int) :: fgsl_permutation_fscanf
    fgsl_permutation_fscanf = gsl_permutation_fscanf(stream%gsl_file, &
         p%gsl_permutation)
  end function fgsl_permutation_fscanf
  function fgsl_permutation_linear_to_canonical(q, p)
    type(fgsl_permutation), intent(inout) :: q
    type(fgsl_permutation),  intent(in) :: p
    integer(fgsl_int) :: fgsl_permutation_linear_to_canonical
    fgsl_permutation_linear_to_canonical = &
         gsl_permutation_linear_to_canonical(q%gsl_permutation, &
         p%gsl_permutation)
  end function fgsl_permutation_linear_to_canonical
  function fgsl_permutation_canonical_to_linear(p, q)
    type(fgsl_permutation), intent(inout) :: p
    type(fgsl_permutation),  intent(in) :: q
    integer(fgsl_int) :: fgsl_permutation_canonical_to_linear
    fgsl_permutation_canonical_to_linear = &
         gsl_permutation_canonical_to_linear(p%gsl_permutation, &
         q%gsl_permutation)
  end function fgsl_permutation_canonical_to_linear
  function fgsl_permutation_inversions(p)
    type(fgsl_permutation), intent(in) :: p
    integer(fgsl_size_t) :: fgsl_permutation_inversions
    fgsl_permutation_inversions = gsl_permutation_inversions(p%gsl_permutation)
  end function fgsl_permutation_inversions
  function fgsl_permutation_linear_cycles(p)
    type(fgsl_permutation), intent(in) :: p
    integer(fgsl_size_t) :: fgsl_permutation_linear_cycles
    fgsl_permutation_linear_cycles = &
         gsl_permutation_linear_cycles(p%gsl_permutation)
  end function fgsl_permutation_linear_cycles
  function fgsl_permutation_canonical_cycles(p)
    type(fgsl_permutation), intent(in) :: p
    integer(fgsl_size_t) :: fgsl_permutation_canonical_cycles
    fgsl_permutation_canonical_cycles = &
         gsl_permutation_canonical_cycles(p%gsl_permutation)
  end function fgsl_permutation_canonical_cycles
  function fgsl_permutation_status(permutation)
    type(fgsl_permutation), intent(in) :: permutation
    logical :: fgsl_permutation_status
    fgsl_permutation_status = .true.
    if (.not. c_associated(permutation%gsl_permutation)) &
         fgsl_permutation_status = .false.
  end function fgsl_permutation_status
  function fgsl_sizeof_permutation(p)
    type(fgsl_permutation), intent(in) :: p
    integer(fgsl_size_t) :: fgsl_sizeof_permutation
    fgsl_sizeof_permutation = gsl_aux_sizeof_permutation()
  end function fgsl_sizeof_permutation
end module fgsl_permutations
