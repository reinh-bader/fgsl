program histogram
  use fgsl
  implicit none
  real(fgsl_double) :: a, b, x
  integer(fgsl_size_t) :: n
  integer(fgsl_int) :: status
  integer(8) :: i
  type(fgsl_histogram) :: h
  type(fgsl_file) :: stdout
  namelist / histpar / a, b, n
!
  write(6, *) 'Reading histogram parameters from file histogram.dat ... '
  open(20, file=HISTOGRAM_DAT, form='formatted', status='old')
  read(20, nml=histpar) 
  close(20)
  stdout = fgsl_stdout()
  h = fgsl_histogram_alloc(n)
  status = fgsl_histogram_set_ranges_uniform(h, a, b)
  open(20, file=CAUCHY_DAT,  status='old')
  do i=1,10000
     read(20, fmt='(F15.7)') x 
     status = fgsl_histogram_increment (h, x)
  end do
  close(20)
  write(6, *) 'Printing histogram:'
  status = fgsl_histogram_fprintf(stdout, h,'%12.5f','%7.0f')
  call fgsl_histogram_free(h)
end program histogram
