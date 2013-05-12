program ieee
  use fgsl
  implicit none
  type(fgsl_file) :: stdout
  integer(fgsl_int) :: status
  real(fgsl_float) :: f
  real(fgsl_double) :: d, fd
  f = 1.0/3.0
  d = 1.D0/3.D0
  fd = f

  stdout = fgsl_stdout()
  write(6, advance='no', fmt='('' f='')') 
  call fgsl_ieee_printf(f)
  status = fgsl_flush(stdout)
  write(6, '('''')')
  
  write(6, advance='no', fmt='('' fd='')') 
  call fgsl_ieee_printf(fd)
  status = fgsl_flush(stdout)
  write(6, '('''')')

  write(6, advance='no', fmt='('' d='')') 
  call fgsl_ieee_printf(d)
  status = fgsl_flush(stdout)
  write(6, '('''')')

end program ieee
