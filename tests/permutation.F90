#include "config.h"
program permutation
  use mod_unit
  use fgsl
  implicit none
  real(fgsl_double), parameter :: eps10 = 1.0E-10_fgsl_double
  type(fgsl_permutation) :: p1, p2, p3
  type(fgsl_combination) :: c1, c2
#if GSL_VERSION_MAJOR_FORTRAN >= 1 && GSL_VERSION_MINOR_FORTRAN >= 14
  type(fgsl_multiset) :: m1, m2 
#endif
  type(fgsl_error_handler_t) :: std
  type(fgsl_vector) :: vec
  type(fgsl_file) :: pfile
  integer(fgsl_int) :: status
  integer(fgsl_size_t) :: index
  integer(fgsl_size_t), pointer, dimension(:) :: p_data
  real(fgsl_double) :: da(8)
  integer(fgsl_long) :: ida(8)
!
! Test permutations, combinations and multisets
!
  call unit_init(150)
  std = fgsl_set_error_handler_off()
  call unit_assert_true('fgsl_well_defined',.not. fgsl_well_defined(p1),.false.)
!
! permutations
!
  p1 = fgsl_permutation_alloc(4_fgsl_size_t)
  p2 = fgsl_permutation_alloc(4_fgsl_size_t)
  p3 = fgsl_permutation_alloc(4_fgsl_size_t)
  call unit_assert_true('fgsl_well_defined',fgsl_well_defined(p1),.true.)
  call unit_assert_true('fgsl_well_defined',fgsl_well_defined(p2),.true.)
  call fgsl_permutation_init(p1)
  call fgsl_permutation_init(p2)
  status = fgsl_permutation_swap(p1, 1_fgsl_size_t, 3_fgsl_size_t)
  index = fgsl_permutation_get(p1,1_fgsl_size_t)
  call unit_assert_equal('fgsl_permutation_get:after swap',3,int(index))
  index = fgsl_permutation_get(p1,3_fgsl_size_t)
  call unit_assert_equal('fgsl_permutation_get:after swap',1,int(index))
  status = fgsl_permutation_valid(p1)
  call unit_assert_equal('fgsl_permutation_valid',fgsl_success,status)
  index = fgsl_permutation_size(p1)
  call unit_assert_equal('fgsl_permutation_size',4,int(index))
  p_data => fgsl_permutation_data(p1)
  call unit_assert_true('fgsl_well_defined',associated(p_data),.true.)
  call unit_assert_equal('fgsl_permutation_data',(/0,3,2,1/),int(p_data))
  status = fgsl_permutation_memcpy(p2,p1)
  call unit_assert_equal('fgsl_permutation_memcpy:status',fgsl_success,status)
  p_data => fgsl_permutation_data(p2)
  call unit_assert_equal('fgsl_permutation_memcpy',(/0,3,2,1/),int(p_data))
  status = fgsl_permutation_valid(p2)
  call unit_assert_equal('fgsl_permutation_valid:yes',fgsl_success,status)
  p_data(2) = 0
  status = fgsl_permutation_valid(p2)
  call unit_assert_equal('fgsl_permutation_valid:no',fgsl_failure,status)
  call fgsl_permutation_free(p1)
  p1 = fgsl_permutation_calloc(4_fgsl_size_t)
  p_data => fgsl_permutation_data(p1)
  call unit_assert_equal('fgsl_permutation_data',(/0,1,2,3/),int(p_data))
  call fgsl_permutation_reverse(p1)
  call unit_assert_equal('fgsl_permutation_reverse',(/3,2,1,0/),int(p_data))
  status = fgsl_permutation_inverse(p2,p1)
  p_data => fgsl_permutation_data(p2)
  call unit_assert_equal('fgsl_permutation_inverse',(/3,2,1,0/),int(p_data))
  call fgsl_permutation_reverse(p2)
  status = fgsl_permutation_next(p2)
  call unit_assert_equal('fgsl_permutation_next:status',fgsl_success,status)
  status = fgsl_permutation_prev(p2)
  call unit_assert_equal('fgsl_permutation_prev:status',fgsl_success,status)
  call unit_assert_equal('fgsl_permutation_next/prev',(/0,1,2,3/),int(p_data))
!
  da = (/1.0d0,2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0/)
  call fgsl_permutation_reverse(p2)
  status = fgsl_permute(p_data,da,2_fgsl_size_t,4_fgsl_size_t)
  call unit_assert_equal('fgsl_permute:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_permute',&
       (/7.0d0,2.0d0,5.0d0,4.0d0,3.0d0,6.0d0,1.0d0,8.0d0/),da,eps10)
  da = (/1.0d0,2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0/)
  status = fgsl_permute_inverse(p_data,da,2_fgsl_size_t,4_fgsl_size_t)
  call unit_assert_equal('fgsl_permute:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_permute',&
       (/7.0d0,2.0d0,5.0d0,4.0d0,3.0d0,6.0d0,1.0d0,8.0d0/),da,eps10)
  ida = (/1_fgsl_long,2_fgsl_long,3_fgsl_long,4_fgsl_long,&
       5_fgsl_long,6_fgsl_long,7_fgsl_long,8_fgsl_long/)
  status = fgsl_permute(p_data,ida,2_fgsl_size_t,4_fgsl_size_t)
  call unit_assert_equal('fgsl_permute_long:status',fgsl_success,status)
  call unit_assert_equal('fgsl_permute_long',&
       (/7,2,5,4,3,6,1,8/),int(ida))
  ida = (/1_fgsl_long,2_fgsl_long,3_fgsl_long,4_fgsl_long,&
       5_fgsl_long,6_fgsl_long,7_fgsl_long,8_fgsl_long/)
  status = fgsl_permute_inverse(p_data,ida,2_fgsl_size_t,4_fgsl_size_t)
  call unit_assert_equal('fgsl_permute_long_inverse:status',fgsl_success,status)
  call unit_assert_equal('fgsl_permute_long_inverse',&
        (/7,2,5,4,3,6,1,8/),int(ida))
  da = (/1.0d0,2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0/)
  vec = fgsl_vector_init(1.0_fgsl_double) 
  status = fgsl_vector_align(da, 4_fgsl_size_t, vec, 4_fgsl_size_t, 0_fgsl_size_t, &
       1_fgsl_size_t)
  status = fgsl_permute_vector(p2,vec)
  call unit_assert_equal_within('fgsl_permute',&
       (/4.0d0,3.0d0,2.0d0,1.0d0/),da(1:4),eps10)  
  da = (/1.0d0,2.0d0,3.0d0,4.0d0,5.0d0,6.0d0,7.0d0,8.0d0/)
  status = fgsl_permute_vector_inverse(p2,vec)
  call unit_assert_equal_within('fgsl_permute_inverse',&
       (/4.0d0,3.0d0,2.0d0,1.0d0/),da(1:4),eps10)  
  call fgsl_vector_free(vec)
! multiply
  status = fgsl_permutation_mul(p3,p1,p2)
  call unit_assert_equal('fgsl_permute_mul:status',fgsl_success,status)
  p_data => fgsl_permutation_data(p3)
  call unit_assert_equal('fgsl_permutation_mul',(/0,1,2,3/),int(p_data))
! I/O
  pfile = fgsl_open('permutation.dat','w')
  call unit_assert_true('fgsl_open',fgsl_well_defined(pfile),.true.)
  status = fgsl_permutation_fwrite(pfile,p1)
  call unit_assert_equal('fgsl_permutation_fwrite:status',fgsl_success,status)
  status = fgsl_close(pfile)
  call unit_assert_equal('fgsl_close:status',fgsl_success,status)
  pfile = fgsl_open('permutation.dat','r')
  call unit_assert_true('fgsl_open',fgsl_well_defined(pfile),.true.)
  status = fgsl_permutation_fread(pfile,p3)
  call unit_assert_equal('fgsl_permutation_fread:status',fgsl_success,status)
  status = fgsl_close(pfile)
  call unit_assert_equal('fgsl_close:status',fgsl_success,status)
  call unit_assert_equal('fgsl_permutation_fread',(/3,2,1,0/),int(p_data))
  pfile = fgsl_open('permutation.dat','w')
  call unit_assert_true('fgsl_open',fgsl_well_defined(pfile),.true.)
  status = fgsl_permutation_fprintf(pfile,p1,'%zu\n')
  call unit_assert_equal('fgsl_permutation_fprintf:status',fgsl_success,status)
  status = fgsl_close(pfile)
  call unit_assert_equal('fgsl_close:status',fgsl_success,status)
  pfile = fgsl_open('permutation.dat','r')
  call unit_assert_true('fgsl_open',fgsl_well_defined(pfile),.true.)
  p_data(1) = 0
  status = fgsl_permutation_fscanf(pfile,p3)
  status = fgsl_close(pfile)
  call unit_assert_equal('fgsl_close:status',fgsl_success,status)
  call unit_assert_equal('fgsl_permutation_fscanf',(/3,2,1,0/),int(p_data))
!
  p_data = (/1,2,3,0/)
  status = fgsl_permutation_linear_to_canonical(p2, p3)
  call unit_assert_equal('fgsl_permutation_linear_to_canonical:status',&
       fgsl_success,status)
  p_data => fgsl_permutation_data(p2)
  call unit_assert_equal('fgsl_permutation_linear_to_canonical',&
       (/0,1,2,3/),int(p_data))
  p_data => fgsl_permutation_data(p3)
  p_data = (/0,0,0,0/)
  status = fgsl_permutation_canonical_to_linear(p3, p2)
  call unit_assert_equal('fgsl_permutation_canonical_to_linear:status',&
       fgsl_success,status)
  call unit_assert_equal('fgsl_permutation_canonical_to_linear',&
       (/1,2,3,0/),int(p_data))
  index = fgsl_permutation_linear_cycles(p3)
  call unit_assert_equal('fgsl_permutation_linear_cycles',1,int(index))
  index = fgsl_permutation_canonical_cycles(p2)
  call unit_assert_equal('fgsl_permutation_linear_cycles',1,int(index))
!
  call fgsl_permutation_free(p1)
  call fgsl_permutation_free(p2)
  call fgsl_permutation_free(p3)
!
! Combinations
!
  c1 = fgsl_combination_alloc(4_fgsl_size_t,2_fgsl_size_t)
  c2 = fgsl_combination_alloc(4_fgsl_size_t,2_fgsl_size_t)
  call unit_assert_true('fgsl_well_defined',fgsl_well_defined(c1),.true.)
  call unit_assert_true('fgsl_well_defined',fgsl_well_defined(c2),.true.)
  call fgsl_combination_init_first(c1)
  call fgsl_combination_init_last(c2)
  index = fgsl_combination_get(c1,0_fgsl_size_t)
  call unit_assert_equal('fgsl_permutation_init_first:0',0,int(index))
  index = fgsl_combination_get(c1,1_fgsl_size_t)
  call unit_assert_equal('fgsl_permutation_init_first:1',1,int(index))
  index = fgsl_combination_get(c2,0_fgsl_size_t)
  call unit_assert_equal('fgsl_permutation_init_first:2',2,int(index))
  index = fgsl_combination_get(c2,1_fgsl_size_t)
  call unit_assert_equal('fgsl_permutation_init_first:3',3,int(index))
  c1 = fgsl_combination_calloc(4_fgsl_size_t,2_fgsl_size_t)
  p_data => fgsl_combination_data(c1)
  call unit_assert_true('fgsl_well_defined',associated(p_data),.true.)
  call unit_assert_equal('fgsl_combination_data',(/0,1/),int(p_data))
  p_data => fgsl_combination_data(c2)
  p_data(1) = 4
  status = fgsl_combination_valid(c2)
  call unit_assert_equal('fgsl_combination_valid:status',fgsl_failure,status)
  status = fgsl_combination_memcpy(c2,c1)
  call unit_assert_equal('fgsl_combination_memcpy:status',fgsl_success,status)
  call unit_assert_equal('fgsl_combination_memcpy',(/0,1/),int(p_data))
  index = fgsl_combination_n(c1)
  call unit_assert_equal('fgsl_combination_n',4,int(index))
  index = fgsl_combination_k(c1)
  call unit_assert_equal('fgsl_combination_k',2,int(index))
  status = fgsl_combination_next(c2)
  call unit_assert_equal('fgsl_combination_next',(/0,2/),int(p_data))
  status = fgsl_combination_prev(c2)
  call unit_assert_equal('fgsl_combination_prev',(/0,1/),int(p_data))
! I/O
  status = fgsl_combination_next(c1)
  pfile = fgsl_open('combination.dat','w')
  call unit_assert_true('fgsl_open',fgsl_well_defined(pfile),.true.)
  status = fgsl_combination_fwrite(pfile,c1)
  call unit_assert_equal('fgsl_combination_fwrite:status',fgsl_success,status)
  status = fgsl_close(pfile)
  call unit_assert_equal('fgsl_close:status',fgsl_success,status)
  pfile = fgsl_open('combination.dat','r')
  call unit_assert_true('fgsl_open',fgsl_well_defined(pfile),.true.)
  status = fgsl_combination_fread(pfile,c2)
  call unit_assert_equal('fgsl_combination_fread:status',fgsl_success,status)
  status = fgsl_close(pfile)
  call unit_assert_equal('fgsl_close:status',fgsl_success,status)
  call unit_assert_equal('fgsl_combination_fread',(/0,2/),int(p_data))
  pfile = fgsl_open('combination.dat','w')
  call unit_assert_true('fgsl_open',fgsl_well_defined(pfile),.true.)
  status = fgsl_combination_fprintf(pfile,c1,'%zu\n')
  call unit_assert_equal('fgsl_combination_fprintf:status',fgsl_success,status)
  status = fgsl_close(pfile)
  call unit_assert_equal('fgsl_close:status',fgsl_success,status)
  pfile = fgsl_open('combination.dat','r')
  call unit_assert_true('fgsl_open',fgsl_well_defined(pfile),.true.)
  p_data(1) = 0
  status = fgsl_combination_fscanf(pfile,c2)
  status = fgsl_close(pfile)
  call unit_assert_equal('fgsl_close:status',fgsl_success,status)
  call unit_assert_equal('fgsl_combination_fscanf',(/0,2/),int(p_data))
!
  call fgsl_combination_free(c1)
  call fgsl_combination_free(c2)
!
! Multisets
!
#if GSL_VERSION_MAJOR_FORTRAN >= 1 && GSL_VERSION_MINOR_FORTRAN >= 14
  m1 = fgsl_multiset_alloc(4_fgsl_size_t,2_fgsl_size_t)
  m2 = fgsl_multiset_alloc(4_fgsl_size_t,2_fgsl_size_t)
  call unit_assert_true('fgsl_well_defined',fgsl_well_defined(m1),.true.)
  call unit_assert_true('fgsl_well_defined',fgsl_well_defined(m2),.true.)
  call fgsl_multiset_init_first(m1)
  call fgsl_multiset_init_last(m2)
  index = fgsl_multiset_get(m1,0_fgsl_size_t)
  call unit_assert_equal('fgsl_permutation_init_first:0',0,int(index))
  index = fgsl_multiset_get(m1,1_fgsl_size_t)
  call unit_assert_equal('fgsl_permutation_init_first:0',0,int(index))
  index = fgsl_multiset_get(m2,0_fgsl_size_t)
  call unit_assert_equal('fgsl_permutation_init_first:1',3,int(index))
  index = fgsl_multiset_get(m2,1_fgsl_size_t)
  call unit_assert_equal('fgsl_permutation_init_first:1',3,int(index))
  m1 = fgsl_multiset_calloc(4_fgsl_size_t,2_fgsl_size_t)
  p_data => fgsl_multiset_data(m1)
  call unit_assert_true('fgsl_well_defined',associated(p_data),.true.)
  call unit_assert_equal('fgsl_multiset_data',(/0,0/),int(p_data))
  p_data => fgsl_multiset_data(m2)
  p_data(1) = 4
  status = fgsl_multiset_valid(m2)
  call unit_assert_equal('fgsl_multiset_valid:status',fgsl_failure,status)
  status = fgsl_multiset_memcpy(m2,m1)
  call unit_assert_equal('fgsl_multiset_memcpy:status',fgsl_success,status)
  call unit_assert_equal('fgsl_multiset_memcpy',(/0,0/),int(p_data))
  index = fgsl_multiset_n(m1)
  call unit_assert_equal('fgsl_multiset_n',4,int(index))
  index = fgsl_multiset_k(m1)
  call unit_assert_equal('fgsl_multiset_k',2,int(index))
  status = fgsl_multiset_next(m2)
  call unit_assert_equal('fgsl_multiset_next',(/0,1/),int(p_data))
  status = fgsl_multiset_prev(m2)
  call unit_assert_equal('fgsl_multiset_prev',(/0,0/),int(p_data))
! I/O
  status = fgsl_multiset_next(m1)
  pfile = fgsl_open('multiset.dat','w')
  call unit_assert_true('fgsl_open',fgsl_well_defined(pfile),.true.)
  status = fgsl_multiset_fwrite(pfile,m1)
  call unit_assert_equal('fgsl_multiset_fwrite:status',fgsl_success,status)
  status = fgsl_close(pfile)
  call unit_assert_equal('fgsl_close:status',fgsl_success,status)
  pfile = fgsl_open('multiset.dat','r')
  call unit_assert_true('fgsl_open',fgsl_well_defined(pfile),.true.)
  status = fgsl_multiset_fread(pfile,m2)
  call unit_assert_equal('fgsl_multiset_fread:status',fgsl_success,status)
  status = fgsl_close(pfile)
  call unit_assert_equal('fgsl_close:status',fgsl_success,status)
  call unit_assert_equal('fgsl_multiset_fread',(/0,1/),int(p_data))
  pfile = fgsl_open('multiset.dat','w')
  call unit_assert_true('fgsl_open',fgsl_well_defined(pfile),.true.)
  status = fgsl_multiset_fprintf(pfile,m1,'%zu\n')
  call unit_assert_equal('fgsl_multiset_fprintf:status',fgsl_success,status)
  status = fgsl_close(pfile)
  call unit_assert_equal('fgsl_close:status',fgsl_success,status)
  pfile = fgsl_open('multiset.dat','r')
  call unit_assert_true('fgsl_open',fgsl_well_defined(pfile),.true.)
  p_data(1) = 0
  status = fgsl_multiset_fscanf(pfile,m2)
  status = fgsl_close(pfile)
  call unit_assert_equal('fgsl_close:status',fgsl_success,status)
  call unit_assert_equal('fgsl_multiset_fscanf',(/0,1/),int(p_data))
!
  call fgsl_multiset_free(m1)
  call fgsl_multiset_free(m2)
#endif
!
! Done
!
  call unit_finalize()
end program permutation
