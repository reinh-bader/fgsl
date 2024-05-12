module fgsl_sorting
  !> Sorting
  use fgsl_array
  use fgsl_permutations
  implicit none

  private :: gsl_heapsort, gsl_heapsort_index, gsl_sort, gsl_sort2, &
       gsl_sort_index, gsl_sort_smallest, gsl_sort_smallest_index, &
       gsl_sort_largest, gsl_sort_largest_index, gsl_sort_long, &
       gsl_sort_long_index, gsl_sort_long_smallest, gsl_sort_long_largest, &
       gsl_sort_long_largest_index, gsl_sort_vector, gsl_sort_vector2, &
       gsl_sort_vector_index, gsl_sort_vector_smallest, gsl_sort_vector_largest, &
       gsl_sort_vector_smallest_index, gsl_sort_vector_largest_index
       
  !
  ! comparison function 
  abstract interface
     function compare_func(x, y) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: x, y
       integer(c_int) :: compare_func
     end function compare_func
  end interface
  
! 
! Generic interfaces
  interface fgsl_sort
     module procedure fgsl_sort_double
     module procedure fgsl_sort2_double
     module procedure fgsl_sort_long
     module procedure fgsl_sort_vector
     module procedure fgsl_sort_vector2
  end interface
  interface fgsl_sort_index
     module procedure fgsl_sort_double_index
     module procedure fgsl_sort_long_index
     module procedure fgsl_sort_vector_index
  end interface
  interface fgsl_sort_smallest
     module procedure fgsl_sort_double_smallest
     module procedure fgsl_sort_long_smallest
     module procedure fgsl_sort_vector_smallest
 end interface
  interface fgsl_sort_smallest_index
     module procedure fgsl_sort_double_smallest_index
     module procedure fgsl_sort_long_smallest_index
     module procedure fgsl_sort_vector_smallest_index
  end interface
  interface fgsl_sort_largest
     module procedure fgsl_sort_double_largest
     module procedure fgsl_sort_long_largest
     module procedure fgsl_sort_vector_largest
  end interface
  interface fgsl_sort_largest_index
     module procedure fgsl_sort_double_largest_index
     module procedure fgsl_sort_long_largest_index
     module procedure fgsl_sort_vector_largest_index
  end interface
  !
  ! C interfaces
  interface
     subroutine gsl_heapsort(array, count, size, compare) bind(c)
       import
       type(c_ptr), value :: array
       integer(c_size_t), value :: count, size
       type(c_funptr), value :: compare
     end subroutine gsl_heapsort
     function gsl_heapsort_index(p, array, count, size, compare) bind(c)
       import
       type(c_ptr), value :: array
       integer(c_size_t), value :: count, size
       type(c_ptr), value :: p
       type(c_funptr), value :: compare
       integer(c_int) :: gsl_heapsort_index
     end function gsl_heapsort_index
     subroutine gsl_sort(data, stride, n) bind(c)
       import
       type(c_ptr), value :: data
       integer(c_size_t), value :: stride, n
     end subroutine gsl_sort
     subroutine gsl_sort2(data1, stride1, data2, stride2, n) bind(c)
       import
       type(c_ptr), value :: data1, data2
       integer(c_size_t), value :: stride1, stride2, n
     end subroutine gsl_sort2
     subroutine gsl_sort_index(p, data, stride, n) bind(c)
       import
       type(c_ptr), value :: p
       type(c_ptr), value :: data
       integer(c_size_t), value :: stride, n
     end subroutine gsl_sort_index
     function gsl_sort_smallest(dest, k, src, stride, n) bind(c)
       import
       integer(c_size_t), value :: k, stride, n
       type(c_ptr), value :: dest
       type(c_ptr), value :: src
       integer(c_int) :: gsl_sort_smallest
     end function gsl_sort_smallest
     function gsl_sort_smallest_index(p, k, src, stride, n) bind(c)
       import
       integer(c_size_t), value :: k, stride, n
       type(c_ptr), value :: p
       type(c_ptr), value :: src
       integer(c_int) :: gsl_sort_smallest_index
     end function gsl_sort_smallest_index
     function gsl_sort_largest(dest, k, src, stride, n) bind(c)
       import
       integer(c_size_t), value :: k, stride, n
       type(c_ptr), value :: dest
       type(c_ptr), value :: src
       integer(c_int) :: gsl_sort_largest
     end function gsl_sort_largest
     function gsl_sort_largest_index(p, k, src, stride, n) bind(c)
       import
       integer(c_size_t), value :: k, stride, n
       type(c_ptr), value :: p
       type(c_ptr), value :: src
       integer(c_int) :: gsl_sort_largest_index
     end function gsl_sort_largest_index
     subroutine gsl_sort_long(data, stride, n) bind(c)
       import
       type(c_ptr), value :: data
       integer(c_size_t), value :: stride, n
     end subroutine gsl_sort_long
     subroutine gsl_sort_long_index(p, data, stride, n) bind(c)
       import
       type(c_ptr), value :: p
       type(c_ptr), value :: data
       integer(c_size_t), value :: stride, n
     end subroutine gsl_sort_long_index
     function gsl_sort_long_smallest(dest, k, src, stride, n) bind(c)
       import
       integer(c_size_t), value :: k, stride, n
       type(c_ptr), value :: dest
       type(c_ptr), value :: src
       integer(c_int) :: gsl_sort_long_smallest
     end function gsl_sort_long_smallest
     function gsl_sort_long_smallest_index(p, k, src, stride, n) bind(c)
       import
       integer(c_size_t), value :: k, stride, n
       type(c_ptr), value :: p
       type(c_ptr), value :: src
       integer(c_int) :: gsl_sort_long_smallest_index
     end function gsl_sort_long_smallest_index
     function gsl_sort_long_largest(dest, k, src, stride, n) bind(c)
       import
       integer(c_size_t), value :: k, stride, n
       type(c_ptr), value :: dest
       type(c_ptr), value :: src
       integer(c_int) :: gsl_sort_long_largest
     end function gsl_sort_long_largest
     function gsl_sort_long_largest_index(p, k, src, stride, n) bind(c)
       import
       integer(c_size_t), value :: k, stride, n
       type(c_ptr), value :: p
       type(c_ptr), value :: src
       integer(c_int) :: gsl_sort_long_largest_index
     end function gsl_sort_long_largest_index
     subroutine gsl_sort_vector(v) bind(c)
       import :: c_ptr
       type(c_ptr), value :: v
     end subroutine gsl_sort_vector
     subroutine gsl_sort_vector2(v1, v2) bind(c)
       import :: c_ptr
       type(c_ptr), value :: v1, v2
     end subroutine gsl_sort_vector2
     function gsl_sort_vector_index(p,v) bind(c)
       import :: c_ptr, c_int
       integer(c_int) :: gsl_sort_vector_index
       type(c_ptr), value :: p,v
     end function gsl_sort_vector_index
     function gsl_sort_vector_smallest(dest, k, v) bind(c)
       import :: c_ptr, c_double, c_size_t, c_int
       integer(c_int) :: gsl_sort_vector_smallest
       integer(c_size_t), value :: k
       real(c_double) :: dest(k)
       type(c_ptr), value :: v
     end function gsl_sort_vector_smallest
     function gsl_sort_vector_largest(dest, k, v) bind(c)
       import :: c_ptr, c_double, c_size_t, c_int
       integer(c_int) :: gsl_sort_vector_largest
       integer(c_size_t), value :: k
       real(c_double) :: dest(k)
       type(c_ptr), value :: v
     end function gsl_sort_vector_largest
     function gsl_sort_vector_smallest_index(p, k, v) bind(c)
       import :: c_ptr, c_double, c_size_t, c_int
       integer(c_int) :: gsl_sort_vector_smallest_index
       integer(c_size_t), value :: k
       integer(c_size_t) :: p(k)
       type(c_ptr), value :: v
     end function gsl_sort_vector_smallest_index
     function gsl_sort_vector_largest_index(p, k, v) bind(c)
       import :: c_ptr, c_double, c_size_t, c_int
       integer(c_int) :: gsl_sort_vector_largest_index
       integer(c_size_t), value :: k
       integer(c_size_t) :: p(k)
       type(c_ptr), value :: v
     end function gsl_sort_vector_largest_index
  end interface

contains
  subroutine fgsl_heapsort(array, count, size, compare)
    type(c_ptr) :: array
    integer(fgsl_size_t), intent(in) :: count, size
    procedure(compare_func) :: compare
    !
    type(c_funptr) :: fp
    fp = c_funloc(compare)
    call gsl_heapsort(array, count, size, fp)
  end subroutine fgsl_heapsort
  function fgsl_heapsort_index(p, array, count, size, compare)
    integer(fgsl_size_t), intent(in) :: count, size
    integer(fgsl_size_t), intent(out), target :: p(count)
    type(c_ptr) :: array
    procedure(compare_func) :: compare
    integer(fgsl_int) :: fgsl_heapsort_index
    !
    type(c_funptr) :: fp
    fp = c_funloc(compare)
    fgsl_heapsort_index = gsl_heapsort_index(c_loc(p), array, count, size, fp)
  end function fgsl_heapsort_index
  subroutine fgsl_sort_double(data, stride, n)
    real(fgsl_double), intent(inout), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    call gsl_sort(c_loc(data), stride, n)
  end subroutine fgsl_sort_double
  subroutine fgsl_sort2_double(data1, stride1, data2, stride2, n)
    real(fgsl_double), intent(inout), target, contiguous :: data1(:), data2(:)
    integer(fgsl_size_t), intent(in) :: stride1, stride2, n
    call gsl_sort2(c_loc(data1), stride1, c_loc(data2), stride2, n)
  end subroutine fgsl_sort2_double
  subroutine fgsl_sort_double_index(p, data, stride, n)
    integer(fgsl_size_t), intent(out), target, contiguous :: p(:)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    call gsl_sort_index(c_loc(p), c_loc(data), stride, n)
  end subroutine fgsl_sort_double_index
  function fgsl_sort_double_smallest(dest, k, src, stride, n)
    integer(fgsl_size_t), intent(in) :: k, stride, n
    real(fgsl_double), intent(out), target :: dest(k)
    real(fgsl_double), intent(in), target, contiguous :: src(:)
    integer(fgsl_int) :: fgsl_sort_double_smallest
    fgsl_sort_double_smallest = gsl_sort_smallest(c_loc(dest), k, c_loc(src), stride, n)
  end function fgsl_sort_double_smallest
  function fgsl_sort_double_smallest_index(p, k, src, stride, n)
    integer(fgsl_size_t), intent(in) :: k, stride, n
    integer(fgsl_size_t), intent(out), target :: p(k)
    real(fgsl_double), intent(in), target, contiguous :: src(:)
    integer(fgsl_int) :: fgsl_sort_double_smallest_index
    fgsl_sort_double_smallest_index = gsl_sort_smallest_index(c_loc(p), k, &
         c_loc(src), stride, n)
  end function fgsl_sort_double_smallest_index
  function fgsl_sort_double_largest(dest, k, src, stride, n)
    integer(fgsl_size_t), intent(in) :: k, stride, n
    real(fgsl_double), intent(out), target :: dest(k)
    real(fgsl_double), intent(in), target, contiguous :: src(:)
    integer(fgsl_int) :: fgsl_sort_double_largest
    fgsl_sort_double_largest = gsl_sort_largest(c_loc(dest), k, c_loc(src), stride, n)
  end function fgsl_sort_double_largest
  function fgsl_sort_double_largest_index(p, k, src, stride, n)
    integer(fgsl_size_t), intent(in) :: k, stride, n
    integer(fgsl_size_t), intent(out), target :: p(k)
    real(fgsl_double), intent(in), target, contiguous :: src(:)
    integer(fgsl_int) :: fgsl_sort_double_largest_index
    fgsl_sort_double_largest_index = gsl_sort_largest_index(c_loc(p), k, &
         c_loc(src), stride, n)
  end function fgsl_sort_double_largest_index
  subroutine fgsl_sort_long(data, stride, n)
    integer(fgsl_long), intent(inout), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    call gsl_sort_long(c_loc(data), stride, n)
  end subroutine fgsl_sort_long
  subroutine fgsl_sort_long_index(p, data, stride, n)
    integer(fgsl_size_t), intent(out), target, contiguous :: p(:)
    integer(fgsl_long), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    call gsl_sort_long_index(c_loc(p), c_loc(data), stride, n)
  end subroutine fgsl_sort_long_index
  function fgsl_sort_long_smallest(dest, k, src, stride, n)
    integer(fgsl_size_t), intent(in) :: k, stride, n
    integer(fgsl_long), intent(out), target :: dest(k)
    integer(fgsl_long), intent(in), target, contiguous :: src(:)
    integer(fgsl_int) :: fgsl_sort_long_smallest
    fgsl_sort_long_smallest = gsl_sort_long_smallest(c_loc(dest), k, c_loc(src), stride, n)
  end function fgsl_sort_long_smallest
  function fgsl_sort_long_smallest_index(p, k, src, stride, n)
    integer(fgsl_size_t), intent(in) :: k, stride, n
    integer(fgsl_size_t), intent(out), target :: p(k)
    integer(fgsl_long), intent(in), target, contiguous :: src(:)
    integer(fgsl_int) :: fgsl_sort_long_smallest_index
    fgsl_sort_long_smallest_index = gsl_sort_long_smallest_index(c_loc(p), k, &
         c_loc(src), stride, n)
  end function fgsl_sort_long_smallest_index
  function fgsl_sort_long_largest(dest, k, src, stride, n)
    integer(fgsl_size_t), intent(in) :: k, stride, n
    integer(fgsl_long), intent(out), target :: dest(k)
    integer(fgsl_long), intent(in), target, contiguous :: src(:)
    integer(fgsl_int) :: fgsl_sort_long_largest
    fgsl_sort_long_largest = gsl_sort_long_largest(c_loc(dest), k, c_loc(src), stride, n)
  end function fgsl_sort_long_largest
  function fgsl_sort_long_largest_index(p, k, src, stride, n)
    integer(fgsl_size_t), intent(in) :: k, stride, n
    integer(fgsl_size_t), intent(out), target :: p(k)
    integer(fgsl_long), intent(in), target, contiguous :: src(:)
    integer(fgsl_int) :: fgsl_sort_long_largest_index
    fgsl_sort_long_largest_index = gsl_sort_long_largest_index(c_loc(p), k, &
         c_loc(src), stride, n)
  end function fgsl_sort_long_largest_index
  subroutine fgsl_sort_vector(v)
    type(fgsl_vector), intent(inout) :: v
    call gsl_sort_vector(v%gsl_vector)
  end subroutine fgsl_sort_vector
  subroutine fgsl_sort_vector2(v1, v2)
    type(fgsl_vector), intent(inout) :: v1, v2
    call gsl_sort_vector2(v1%gsl_vector, v2%gsl_vector)
  end subroutine fgsl_sort_vector2
  subroutine fgsl_sort_vector_index(p,v,status)
    type(fgsl_permutation), intent(inout) :: p
    type(fgsl_vector), intent(in) :: v
    integer(fgsl_int), optional :: status
    integer(fgsl_int) :: status_loc
    status_loc = gsl_sort_vector_index(&
         p%gsl_permutation,v%gsl_vector)
    if (present(status)) status = status_loc
  end subroutine fgsl_sort_vector_index
  function fgsl_sort_vector_smallest(dest, k, v)
    integer(fgsl_int) :: fgsl_sort_vector_smallest
    integer(fgsl_size_t), intent(in) :: k
    real(fgsl_double), intent(out) :: dest(k)
    type(fgsl_vector), intent(inout) :: v
    fgsl_sort_vector_smallest = gsl_sort_vector_smallest(dest, k, v%gsl_vector)
  end function fgsl_sort_vector_smallest
  function fgsl_sort_vector_largest(dest, k, v)
    integer(fgsl_int) :: fgsl_sort_vector_largest
    integer(fgsl_size_t), intent(in) :: k
    real(fgsl_double), intent(out) :: dest(k)
    type(fgsl_vector), intent(inout) :: v
    fgsl_sort_vector_largest = gsl_sort_vector_largest(dest, k, v%gsl_vector)
  end function fgsl_sort_vector_largest
  function fgsl_sort_vector_smallest_index(p, k, v)
    integer(fgsl_int) :: fgsl_sort_vector_smallest_index
    integer(fgsl_size_t), intent(in) :: k
    integer(fgsl_size_t), intent(out) :: p(k)
    type(fgsl_vector), intent(inout) :: v
    fgsl_sort_vector_smallest_index = gsl_sort_vector_smallest_index(p, k, v%gsl_vector)
  end function fgsl_sort_vector_smallest_index
  function fgsl_sort_vector_largest_index(p, k, v)
    integer(fgsl_int) :: fgsl_sort_vector_largest_index
    integer(fgsl_size_t), intent(in) :: k
    integer(fgsl_size_t), intent(out) :: p(k)
    type(fgsl_vector), intent(inout) :: v
    fgsl_sort_vector_largest_index = gsl_sort_vector_largest_index(p, k, v%gsl_vector)
  end function fgsl_sort_vector_largest_index

end module fgsl_sorting
