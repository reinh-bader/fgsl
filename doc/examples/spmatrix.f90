PROGRAM spmatrix
  use :: fgsl
  use, intrinsic :: iso_fortran_env, only : output_unit, error_unit
  implicit none
  type(fgsl_spmatrix) :: A, C
  integer(fgsl_size_t) :: i, j
  integer(fgsl_int) :: status

  A = fgsl_spmatrix_alloc(5_fgsl_size_t, 4_fgsl_size_t) ! triplet format


  ! build the sparse matrix
  status = fgsl_spmatrix_set(A, 0_fgsl_size_t, 2_fgsl_size_t, 3.1_fgsl_double)
  status = fgsl_spmatrix_set(A, 0_fgsl_size_t, 3_fgsl_size_t, 4.6_fgsl_double)
  status = fgsl_spmatrix_set(A, 1_fgsl_size_t, 0_fgsl_size_t, 1.0_fgsl_double)
  status = fgsl_spmatrix_set(A, 1_fgsl_size_t, 2_fgsl_size_t, 7.2_fgsl_double)
  status = fgsl_spmatrix_set(A, 3_fgsl_size_t, 0_fgsl_size_t, 2.1_fgsl_double)
  status = fgsl_spmatrix_set(A, 3_fgsl_size_t, 1_fgsl_size_t, 2.9_fgsl_double)
  status = fgsl_spmatrix_set(A, 3_fgsl_size_t, 3_fgsl_size_t, 8.5_fgsl_double)
  status = fgsl_spmatrix_set(A, 4_fgsl_size_t, 0_fgsl_size_t, 4.1_fgsl_double)

  write(output_unit, '(A)') 'printing all matrix elements:'
  do i=0,4
    do j=0,3
      write(output_unit, '(A,I1,A,I1,A,G15.7)') 'A(',i,',',j,') = ', &
      fgsl_spmatrix_get(A, i, j)
    end do
  end do

  ! convert to compressed column format
  C = fgsl_spmatrix_compcol(A);

  call fgsl_spmatrix_free(A);
  call fgsl_spmatrix_free(C);
end program spmatrix
