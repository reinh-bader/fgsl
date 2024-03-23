program matrix
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: n = 6, m = 4
  integer(fgsl_size_t) :: i, j 
  integer(fgsl_int) :: status
  type(fgsl_matrix) :: mat
  real(fgsl_double), target :: fmat(m, n)
  real(fgsl_double), pointer :: pfm(:,:)
!
  mat = fgsl_matrix_init(fmat)
  do i=1, n
     do j=1, m
        fmat(j, i) = 0.23_fgsl_double + &
             100._fgsl_double * dble(i-1) + dble(j-1)
     end do
  end do
  pfm => fgsl_matrix_to_fptr(mat)
  write(*,'("Pointer to matrix")')
  do i=1, size(pfm,2)
     do j=1, size(pfm,1)
        write(*, '(''mat('',I2,'','',I2,'') = '',F6.2)') j, i, pfm(j,i)
     end do
  end do
  pfm => null()
  call fgsl_matrix_free(mat)
  mat = fgsl_matrix_init(fmat, 2_fgsl_size_t, 3_fgsl_size_t)
  pfm => fgsl_matrix_to_fptr(mat)
  write(*,'("Pointer to submatrix")')
  do i=1, size(pfm,2)
     do j=1, size(pfm,1)
        write(*, '(''mat('',I2,'','',I2,'') = '',F6.2)') j, i, pfm(j,i)
     end do
  end do

end program matrix
