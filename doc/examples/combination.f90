program combination
  use fgsl
  implicit none
  integer(fgsl_size_t) :: i
  integer(fgsl_int) :: status
  type(fgsl_combination) :: c
  type(fgsl_file) :: stdout
!
  write(6, '(''All subsets of {0,1,2,3} by size:'')') 
  stdout = fgsl_stdout()
  do i=0,4
     c = fgsl_combination_calloc(4_fgsl_size_t,i)
     do 
#ifdef __INTEL_COMPILER
! The following relies on the extension $ edit descriptor
! to achieve proper interleaving of Fortran and C output
! for Intel's compiler
        write(6, fmt='(''{'',$)')
#else
        write(6, advance='NO', fmt='(''{'')')
#endif
        status = fgsl_combination_fprintf(stdout, c, " %u")
        status = fgsl_flush(stdout)
        write(6, '(''}'')')
        if (fgsl_combination_next(c) /= FGSL_SUCCESS) exit 
     end do
     call fgsl_combination_free(c)
  end do
end program combination
