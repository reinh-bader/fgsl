#include "config.h"

!FIXME - LLP model fixes are needed
#if SIZEOF_LONG == SIZEOF_SIZE_T
module mod_sort
  use fgsl
  use mod_unit
  use, intrinsic :: iso_c_binding
  implicit none
  integer(fgsl_size_t), parameter :: dim = 40
  real(fgsl_double), parameter :: eps10=1.0d-10
  type(c_ptr) :: array_ptr
  real(c_double), dimension(dim), target :: array, array2, t_array
  integer(fgsl_size_t), dimension(dim) :: p_array
  integer(fgsl_long), dimension(dim) :: i_array
contains
  function compare(x, y) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_double
    type(c_ptr), value :: x, y
    integer(c_int) :: compare
!
    real(c_double), pointer :: rx, ry
    call c_f_pointer(x, rx); call c_f_pointer(y, ry)
    compare = 0 ;  if (rx < ry) compare = -1
  end function compare
end module mod_sort
program sort
  use mod_sort
  implicit none
  type(fgsl_error_handler_t) :: std
  type(fgsl_vector) :: v_array, v_array2
  real(c_double), pointer :: pv(:)
  integer(fgsl_size_t), pointer :: pm_data(:)
  type(fgsl_permutation) :: p
  integer :: i, status
!
! Test sorting routines
!
  call unit_init(200)
  std = fgsl_set_error_handler_off()
  array = (/(dble(i),i=dim,1,-1)/)
  array_ptr = c_loc(array)
  v_array = fgsl_vector_init(1.0_fgsl_double)
  v_array2 = fgsl_vector_init(1.0_fgsl_double)
  status = fgsl_vector_align(array, dim, v_array, dim, 0_fgsl_size_t, 1_fgsl_size_t)
  status = fgsl_vector_align(array, dim, v_array2, dim, 0_fgsl_size_t, 1_fgsl_size_t)
  p = fgsl_permutation_alloc(dim)

  call fgsl_heapsort(array_ptr,dim,fgsl_sizeof(1.0d0),compare)
  call unit_assert_equal_within('fgsl_heapsort',(/(dble(i),i=1,dim)/),array,eps10)
  array = (/(dble(i),i=dim,1,-1)/)
  status = fgsl_heapsort_index(p_array,array_ptr,dim,fgsl_sizeof(1.0d0),compare)
  call unit_assert_equal('fgsl_heapsort_index_status',fgsl_success,status)
! NOTE: results use zero base!
  call unit_assert_equal('fgsl_heapsort_index',(/(i,i=dim-1,0,-1)/),int(p_array))
  array = (/(dble(i),i=dim,1,-1)/)
  call fgsl_sort(array,1_fgsl_size_t)
  call unit_assert_equal_within('fgsl_sort_double',(/(dble(i),i=1,dim)/),array,eps10)
  p_array = (/(int(i,fgsl_size_t),i=dim,1,-1)/)
  call fgsl_sort(p_array,1_fgsl_size_t)
  call unit_assert_equal('fgsl_sort_long',(/(i,i=1,dim)/),int(p_array))
  array = (/(dble(i),i=dim,1,-1)/)
  call fgsl_sort_index(p_array,array,1_fgsl_size_t)
  call unit_assert_equal('fgsl_sort_index',(/(i,i=dim-1,0,-1)/),int(p_array))
  i_array = (/(i,i=dim,1,-1)/)
  call fgsl_sort_index(p_array,i_array,1_fgsl_size_t)
  call unit_assert_equal('fgsl_sort_index',(/(i,i=dim-1,0,-1)/),int(p_array))
  array = (/(dble(i),i=dim,1,-1)/)
  status = fgsl_sort_smallest(t_array,dim,array,1_fgsl_size_t)
  call unit_assert_equal('fgsl_sort_smallest:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sort_smallest',&
       (/(dble(i),i=1,dim)/),t_array,eps10)
  i_array = (/(i,i=dim,1,-1)/)
  status = fgsl_sort_smallest(p_array,dim,i_array,1_fgsl_size_t)
  call unit_assert_equal('fgsl_sort_smallest:status',fgsl_success,status)
  call unit_assert_equal('fgsl_sort_smallest',(/(i,i=1,dim)/),int(p_array))
  array = (/(dble(i),i=dim,1,-1)/)
  status = fgsl_sort_smallest_index(p_array,dim,array,1_fgsl_size_t)
  call unit_assert_equal('fgsl_sort_smallest_index',(/(i,i=dim-1,0,-1)/),int(p_array))
  i_array = (/(i,i=dim,1,-1)/)
  status = fgsl_sort_smallest_index(p_array,dim,i_array,1_fgsl_size_t)
  call unit_assert_equal('fgsl_sort_smallest_index',(/(i,i=dim-1,0,-1)/),int(p_array))
  array = (/(dble(i),i=dim,1,-1)/)
  status = fgsl_sort_largest(t_array,dim,array,1_fgsl_size_t)
  call unit_assert_equal('fgsl_sort_largest:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sort_largest',&
       (/(dble(i),i=dim,1,-1)/),t_array,eps10)
  i_array = (/(i,i=dim,1,-1)/)
  status = fgsl_sort_largest(p_array,dim,i_array,1_fgsl_size_t)
  call unit_assert_equal('fgsl_sort_largest:status',fgsl_success,status)
  call unit_assert_equal('fgsl_sort_largest',(/(i,i=dim,1,-1)/),int(p_array))
  array = (/(dble(i),i=dim,1,-1)/)
  status = fgsl_sort_largest_index(p_array,dim,array,1_fgsl_size_t)
  call unit_assert_equal('fgsl_sort_largest_index',(/(i,i=0,dim-1)/),int(p_array))
  i_array = (/(i,i=dim,1,-1)/)
  status = fgsl_sort_largest_index(p_array,dim,i_array,1_fgsl_size_t)
  call unit_assert_equal('fgsl_sort_largest_index',(/(i,i=0,dim-1)/),int(p_array))

  array = (/(dble(i),i=dim,1,-1)/)
  call fgsl_sort(v_array)
  call unit_assert_equal_within('fgsl_sort_vector',(/(dble(i),i=1,dim)/),array,eps10)
  array = (/(dble(i),i=dim,1,-1)/)
  call fgsl_sort_index(p,v_array)
  pm_data => fgsl_permutation_data(p)
  call unit_assert_equal('fgsl_sort_vector_index',(/(i,i=dim-1,0,-1)/),int(p_array))
  array = (/(dble(i),i=dim,1,-1)/)
  status = fgsl_sort_smallest(t_array,dim,v_array)
  call unit_assert_equal('fgsl_sort_vector_smallest:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sort_vector_smallest',&
       (/(dble(i),i=1,dim)/),t_array,eps10)
  array = (/(dble(i),i=1,dim)/)
  status = fgsl_sort_largest(t_array,dim,v_array)
  call unit_assert_equal('fgsl_sort_vector_largest:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sort_vector_largest',&
       (/(dble(i),i=dim,1,-1)/),t_array,eps10)
  array = (/(dble(i),i=dim,1,-1)/)
  status = fgsl_sort_smallest_index(p_array,dim,v_array)
  call unit_assert_equal('fgsl_sort_vector_smallest_index',(/(i,i=dim-1,0,-1)/),int(p_array))
  array = (/(dble(i),i=dim,1,-1)/)
  status = fgsl_sort_largest_index(p_array,dim,v_array)
  call unit_assert_equal('fgsl_sort_vector_largest_index',(/(i,i=0,dim-1)/),int(p_array))
!  array = (/(dble(i),i=dim,1,-1)/)
!  array2 = array
!  call fgsl_sort2(array,1_fgsl_size_t,array2,1_fgsl_size_t,dim)
!  call unit_assert_equal_within('fgsl_sort2::array',(/(dble(i),i=1,dim)/),array,eps10)
!  call unit_assert_equal_within('fgsl_sort2::array2',(/(dble(i),i=1,dim)/),array2,eps10)
!  array = (/(dble(i),i=dim,1,-1)/)
!  array2 = array
!  call fgsl_sort2(v_array,v_array2)
!  status = fgsl_vector_align(pv, v_array)
!  call unit_assert_equal_within('fgsl_sort2::v_array',(/(dble(i),i=1,dim)/),pv,eps10)
!  status = fgsl_vector_align(pv, v_array2)
!  call unit_assert_equal_within('fgsl_sort2::v_array2',(/(dble(i),i=1,dim)/),pv,eps10)
!  nullify(pv)
  call fgsl_permutation_free(p)
  call fgsl_vector_free(v_array)
  call fgsl_vector_free(v_array2)
!
! Done
!
  call unit_finalize()
end program sort
#else
program sort
call exit(77)
end program sort
#endif
