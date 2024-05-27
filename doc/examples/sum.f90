program sum
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: N = 20
  integer(fgsl_int) :: i, status
  real(fgsl_double) :: t(N), err, sum_t, sum_accel, zeta_2
  type(fgsl_sum_levin_u_workspace) :: w
!
  sum_t = 0.0_fgsl_double
  zeta_2 = M_PI * M_PI / 6.0_fgsl_double
  w = fgsl_sum_levin_u_alloc(N)
  do i=1,n
     t(i) = 1/dble(i)**2
     sum_t = sum_t + t(i)
  end do
  status = fgsl_sum_levin_u_accel (t, n, w, sum_accel, err)
  write(*,'(''term-by-term sum = '',1PE25.16,'' using '',I2,'' terms.'')') &
       sum_t, n
  write(*,'(''Exact value      = '',1PE25.16)') zeta_2
  write(*,'(''Accelerated sum  = '',1PE25.16)') sum_accel
  write(*,'(''Estimated error  = '',1PE25.16)') err
  write(*,'(''Actual error     = '',1PE25.16)') sum_accel - zeta_2
  status = fgsl_sum_levin_u_free(w)
end program sum
