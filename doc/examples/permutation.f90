program permutation
  use fgsl
  implicit none
  integer(fgsl_size_t) :: i
  integer(fgsl_int) :: status
  type(fgsl_permutation) :: p
  type(fgsl_file) :: stdout
!
  p = fgsl_permutation_alloc(3_fgsl_size_t)
  call fgsl_permutation_init(p)
  stdout = fgsl_stdout()
  do 
     status = fgsl_permutation_fprintf(stdout, p, " %u")
     status = fgsl_flush(stdout)
     write(*, '('''')')
     if (fgsl_permutation_next(p) /= FGSL_SUCCESS) exit 
  end do
  call fgsl_permutation_free(p)
end program permutation
