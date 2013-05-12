program fitting
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: n = 4
  integer(fgsl_int) :: i, status
  real(fgsl_double), dimension(n) :: x = &
       (/ 1970.D0, 1980.D0, 1990.D0, 2000.D0 /) 
  real(fgsl_double), dimension(n) :: y = &
       (/ 12.D0, 11.D0, 14.D0, 13.D0 /)
  real(fgsl_double), dimension(n) :: w = &
       (/ 0.1D0, 0.2D0, 0.3D0, 0.4D0 /)
  real(fgsl_double) :: c0, c1, cov00, cov01, cov11, chisq
  real(fgsl_double) :: xf, yf, yf_err

  status = fgsl_fit_wlinear (x, 1_fgsl_size_t, w, 1_fgsl_size_t, &
       y, 1_fgsl_size_t, n, & 
       c0, c1, cov00, cov01, cov11, chisq)
       
  
  write(6, '(''# best fit: Y = '',F12.5,'' + '',F12.5, &
       & ''*X'')') c0, c1
  write(6, '(''# covariance matrix: '')')
  write(6, '(''# [ '',F12.5,'', '',F12.5, '' ]'')') cov00, cov01 
  write(6, '(''# [ '',F12.5,'', '',F12.5, '' ]'')') cov01, cov11
  write(6, '(''# chisq = '',F12.5)') chisq

  do i=1, n
     write(6, '(''data: '',2(F6.1,1X),F12.5))') x(i), y(i), 1.0D0/sqrt(w(i))
  end do

  do i=-30,129
     xf = x(1) + dble(i)/100.D0 * (x(n) - x(1))
     status = fgsl_fit_linear_est (xf, c0, c1, cov00, cov01, cov11, &
          yf, yf_err)
     write(6, '(''fit, error: '',F6.1,1X,F10.5,'' +/-'',F10.5)') &
          xf, yf, yf_err 
  end do
end program fitting
