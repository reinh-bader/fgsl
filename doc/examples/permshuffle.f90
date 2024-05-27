program permshuffle
  use fgsl
  use, intrinsic :: iso_c_binding
  implicit none
  integer(fgsl_size_t), parameter :: n = 10
  integer(fgsl_int) :: status
  integer(c_size_t), pointer :: data(:)
  type(fgsl_file) :: stdout
  type(fgsl_rng) :: r
  type(fgsl_rng_type) :: t
  type(fgsl_permutation) :: p, q
!
  p = fgsl_permutation_alloc(n)
  q = fgsl_permutation_alloc(n)
  t = fgsl_rng_env_setup()
  t = fgsl_rng_default
  r = fgsl_rng_alloc (t)
 
  write(*, advance='no', fmt='(''initial permutation:'')') 
  call fgsl_permutation_init(p)
  stdout = fgsl_stdout()
  status = fgsl_permutation_fprintf (stdout, p, ' %u')
  status = fgsl_flush(stdout); write(*, '('''')')

  write(*, advance='no', fmt='(''random permutation:'')') 
  data => fgsl_permutation_data(p)
  if (associated(data)) then
     call fgsl_ran_shuffle (r, data, n);
     status = fgsl_permutation_fprintf (stdout, p, ' %u')
     status = fgsl_flush(stdout); write(*, '('''')')
  else
     write(*, '('' failed to acquire pointer to permutation data'')')
  end if

  write(*, advance='no', fmt='(''inverse permutation:'')') 
  status = fgsl_permutation_inverse(q, p)
  status = fgsl_permutation_fprintf (stdout, q, ' %u')
  status = fgsl_flush(stdout);  write(*, '('''')')

  call fgsl_permutation_free(p)
  call fgsl_permutation_free(q)
  call fgsl_rng_free(r)

end program permshuffle
