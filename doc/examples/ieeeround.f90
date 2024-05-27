program ieeeround
  use fgsl
  implicit none
  real(fgsl_double) :: x, oldsum, sum
  integer(fgsl_int) :: i
  call fgsl_ieee_env_setup()
  i = 0
  x = 1.D0; oldsum = -1.D0; sum=0.D0
  do while (sum /= oldsum)
     i = i + 1
     oldsum = sum
     sum = sum + x
     x = x/dble(i)
     write(*, '(''i='',I0,'' sum='',F20.18,'' error='',e10.3)') &
          i, sum, sum - m_e
     if (i > 30) exit
  end do
! Note: Setting rounding modes as described in GSL reference manual
! does not appear to work as documented for x86_64 
! (not even for C version)
! works OK on IA-32, IA-64
end program ieeeround
