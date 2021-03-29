program fitting2
  use fgsl
  implicit none
  integer(fgsl_int) :: status
  integer(fgsl_size_t) :: i, n, rk
  real(fgsl_double) :: xi, yi, ei, chisq
  type(fgsl_matrix) :: x, cov
  type(fgsl_vector) :: y, w, c
  real(fgsl_double), allocatable :: x_m(:,:), y_v(:), w_v(:)
  real(fgsl_double) :: cov_m(3,3), c_v(3)
  type(fgsl_multifit_linear_workspace) :: work
!
  open(20, file='fitting2.dat', form='formatted', status='old', iostat=status)
  if (status > 0) then
     stop 'Could not open fitting2.dat. You may need to run fitting3.exe'
  end if
  read(20, *) n
  allocate(x_m(3, n), y_v(n), w_v(n))
!
  x = fgsl_matrix_init(x_m)
  cov = fgsl_matrix_init(cov_m)
  y = fgsl_vector_init(y_v)
  w = fgsl_vector_init(w_v)
  c = fgsl_vector_init(c_v)
!
  do i=1, n
     read(20, *) xi, yi, ei
     x_m(1, i) = 1.0D0
     x_m(2, i) = xi
     x_m(3, i) = xi * xi
     y_v(i) = yi
     w_v(i) = 1.0D0/(ei * ei)
  end do
  close(20)
  work = fgsl_multifit_linear_alloc(n, 3_fgsl_size_t)
  status = fgsl_multifit_wlinear(x, w, y, c, cov, chisq, work)
  rk = fgsl_multifit_linear_rank(1.0d-4, work)
  call fgsl_multifit_linear_free(work)
  write(*, '(''# best fit:  Y = '',F12.5,'' + '',F12.5, &
       & ''*X + '',F12.5,''*X^2'')') c_v
  write(*, '(''# covariance matrix: '')') 
  write(*, '(3(F12.5,1X))')  cov_m(1:3,1)
  write(*, '(3(F12.5,1X))')  cov_m(1:3,2)
  write(*, '(3(F12.5,1X))')  cov_m(1:3,3)
  write(*, '(''# chisq = '',F12.5)') chisq
  write(*, '(''# rank = '',I0)') rk 

  call fgsl_vector_free(y)
  call fgsl_vector_free(w)
  call fgsl_vector_free(c)
  call fgsl_matrix_free(x)
  call fgsl_matrix_free(cov)
  deallocate(x_m, y_v, w_v)
end program fitting2
