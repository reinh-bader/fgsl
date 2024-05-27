program histogram
  use fgsl
  implicit none
  real(fgsl_double) :: a, b, x
  integer(fgsl_size_t) :: n
  integer(fgsl_int) :: status
  integer :: i
  type(fgsl_histogram) :: h
  type(fgsl_file) :: stdout
  namelist / histpar / a, b, n
  integer :: iu
!
  write(*,*) 'Reading histogram parameters from file histogram.dat ... '
  open(newunit=iu, file=HISTOGRAM_DAT, form='formatted', status='old')
  read(iu, nml=histpar) 
  close(iu)
  stdout = fgsl_stdout()
  h = fgsl_histogram_alloc(n)
  status = fgsl_histogram_set_ranges_uniform(h, a, b)
  open(newunit=iu, file=CAUCHY_DAT, status='old')
  do i=1,10000
     read(iu, fmt='(F15.7)') x 
     status = fgsl_histogram_increment (h, x)
  end do
  close(iu)
  write(*, *) 'Printing histogram:'
  status = fgsl_histogram_fprintf(stdout, h,'%12.5f','%7.0f')
  call fgsl_histogram_free(h)
end program histogram
