program ieee_utils
  use fgsl
  use mod_unit
  implicit none
  integer(fgsl_int) :: status
  real(fgsl_double) :: t
  real(fgsl_float) :: ts
  type(fgsl_file) :: tfile
  character(kind=fgsl_char,len=fgsl_strmax) :: ieee_str1, ieee_str2
!
  call unit_init(20)
!
  tfile = fgsl_open(fgsl_char_'ieee.dat',fgsl_char_'w+')
  call unit_assert_true('fgsl_open', fgsl_well_defined(tfile), .true.)
  t = 1.0_fgsl_double/3.0_fgsl_double
  call fgsl_ieee_fprintf(tfile, t)
  ts = 1.0_fgsl_float/3.0_fgsl_float
  call fgsl_ieee_fprintf(tfile, ts)
  status = fgsl_close(tfile)
  call unit_assert_equal('fgsl_close:status',fgsl_success,status)
  open(unit=20, file='ieee.dat',form='FORMATTED',status='OLD')
  read(20, fmt=*) ieee_str1, ieee_str2
  call unit_assert_equal('fgsl_ieee_double',ieee_str1,&
       '1.0101010101010101010101010101010101010101010101010101*2^-2')
  call unit_assert_equal('fgsl_ieee_float',ieee_str2,&
       '1.01010101010101010101011*2^-2')
  close(20,status='DELETE')
! 
! Done
!
  call unit_finalize()
end program ieee_utils
