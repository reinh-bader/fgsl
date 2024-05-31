program integration_lebedev
  use fgsl_integration
  implicit none
  
  integer(fgsl_size_t), parameter :: n = 6
  ! exact up to spherical harmonics of degree 3
  
  type(fgsl_integration_lebedev_workspace) :: wk
  integer(fgsl_size_t) :: i
  real(fgsl_double) :: result
  
  wk = fgsl_integration_lebedev_alloc(n)
  
  result = 0.0_fgsl_double
  do i = 1, n
    result = result + wk%weights(i) * f(wk%theta(i), wk%phi(i))
  end do
  result = result * 4._fgsl_double * m_pi
  
  write(*,fmt='("Result for integral of (a*r)^2 is", F11.4)') result
  ! Result value expected: 21.9911, analytically: a^2 * 4 * m_pi / 3

  call fgsl_integration_lebedev_free(wk)
contains
  real(fgsl_double) function f(theta, phi)
     real(fgsl_double), intent(in) :: theta, phi
     
     real(fgsl_double) :: a(3) 
     
     a = [ 1._fgsl_double, 2._fgsl_double, 0.5_fgsl_double ]
     
     f = ( a(1) * sin(theta) * sin(phi) + &
           a(2) * sin(theta) * cos(phi) + &
           a(3) * cos(theta) ) ** 2
  end function
end program
