program interpp
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: n = 4
  integer(fgsl_int) :: i, status
  real(fgsl_double) :: xi, yi
  real(fgsl_double) :: x(4) = (/0.0D0, 0.1D0, 0.27D0, 0.3D0 /), &
       y(4) = (/0.15D0, 0.7D0, -0.1D0, 0.15D0 /)
!      Note: first = last for periodic data
  type(fgsl_interp_accel) :: acc
  type(fgsl_spline) :: spline

  spline =  fgsl_spline_alloc(fgsl_interp_cspline_periodic, n)
  write(*, '(''#m=0,S=5'')')
  do i=1,n
     write(*, '(2(F10.5,1X))') x(i), y(i)
  end do
  write(*, '(''#m=1,S=0'')')
  status = fgsl_spline_init(spline, x, y)
  do i=1, 100
     xi = (1.D0 - dble(i-1)/100.D0) * x(1) + dble(i-1)/100.D0 * x(n)
     yi = fgsl_spline_eval(spline, xi, acc)
     write(*, '(2(F10.5,1X))') xi, yi
  end do
  call fgsl_spline_free (spline)
  call fgsl_interp_accel_free (acc)
end program interpp
