program interp
  use fgsl
  implicit none
  integer :: i
  integer(fgsl_int) :: status
  real(fgsl_double) :: xi, yi, x(10), y(10)
  type(fgsl_interp_accel) :: acc
  type(fgsl_spline) :: spline
!
  do i=1, 10
     x(i) = dble(i-1) + 0.5d0*sin(dble(i-1))
     y(i) = dble(i-1) + cos(dble(i)**2)
     write(6, '(2(F10.5,1X))') x(i), y(i)
  end do
  acc = fgsl_interp_accel_alloc()
  write(6, *) 'Values of interpolating spline:'
  spline = fgsl_spline_alloc(fgsl_interp_cspline, 10_fgsl_size_t)
  status = fgsl_spline_init (spline, x, y, 10_fgsl_size_t)
  do i=1, 101
     xi = x(1) + dble(i-1)/100.D0 * (x(10) - x(1))
     yi = fgsl_spline_eval (spline, xi, acc)
     write(6, '(2(F10.5,1X))') xi, yi
  end do
  call fgsl_spline_free(spline)
  call fgsl_interp_accel_free(acc)
end program interp
