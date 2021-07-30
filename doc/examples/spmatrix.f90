PROGRAM spmatrix
  use :: fgsl
  use, intrinsic :: iso_fortran_env, only : output_unit, error_unit
  implicit none
  type(fgsl_spmatrix) :: A, B, C
  type(fgsl_file) :: stdout
  integer(fgsl_size_t) :: i, j
  integer(fgsl_int), pointer :: pi(:), pp(:)
  real(fgsl_double), pointer :: pd(:)
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

  write(output_unit, '(A)') 'matrix in triplet format:'
  stdout = fgsl_stdout()
  status = fgsl_spmatrix_fprintf(stdout, A, "%.1f")

  ! convert to compressed column format
  B = fgsl_spmatrix_alloc_nzmax(5_fgsl_size_t, 4_fgsl_size_t, &
        10_fgsl_size_t, fgsl_spmatrix_type_csc)
  status = fgsl_spmatrix_csc(B,A)
  call fgsl_spmatrix_getfields(B, pi, pp, pd)

  write(output_unit, '(A)') 'matrix in compressed column format:'
  write(output_unit, fmt='("i = ",*(i0,:,", "))') pi
  write(output_unit, fmt='("p = ",*(i0,:,", "))') pp
  write(output_unit, fmt='("d = ",*(f3.1,:,", "))') pd

  ! convert to compressed row format
  C = fgsl_spmatrix_alloc_nzmax(5_fgsl_size_t, 4_fgsl_size_t, &
        10_fgsl_size_t, fgsl_spmatrix_type_csr)
  status = fgsl_spmatrix_csr(C,A)
  call fgsl_spmatrix_getfields(C, pi, pp, pd)

  write(output_unit, '(A)') 'matrix in compressed row format:'
  write(output_unit, fmt='("i = ",*(i0,:,", "))') pi
  write(output_unit, fmt='("p = ",*(i0,:,", "))') pp
  write(output_unit, fmt='("d = ",*(f3.1,:,", "))') pd

  call fgsl_spmatrix_free(A);
  call fgsl_spmatrix_free(B);
  call fgsl_spmatrix_free(C);
end program spmatrix
