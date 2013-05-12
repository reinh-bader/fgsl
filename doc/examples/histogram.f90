program histogram
  use fgsl
  implicit none
  real(fgsl_double) :: a, b, x
  integer(fgsl_size_t) :: n
  integer(fgsl_int) :: status
  type(fgsl_histogram) :: h
  type(fgsl_file) :: stdout
  namelist / histpar / a, b, n
!
  write(6, *) 'Reading histogram parameters from file histogram.dat ... '
  open(20, file='histogram.dat', form='formatted', status='old')
  read(20, nml=histpar) 
  close(20)
  stdout = fgsl_stdout()
  h = fgsl_histogram_alloc(n)
  status = fgsl_histogram_set_ranges_uniform(h, a, b)
  write(6, *) '... Done. Now please enter histogram data.'
  write(6, *) 'One number per line. CTRL-D concludes input.'
  do
     read(5, end=1, fmt=*) x 
     status = fgsl_histogram_increment (h, x)
  end do
1 continue
  write(6, *) 'Printing histogram:'
  status = fgsl_histogram_fprintf(stdout, h,'%12.5f','%7.0f')
  call fgsl_histogram_free(h)
end program histogram
