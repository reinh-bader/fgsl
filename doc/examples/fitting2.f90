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
  x = fgsl_matrix_init(type=1.0D0)
  cov = fgsl_matrix_init(type=1.0D0)
  y = fgsl_vector_init(type=1.0D0)
  w = fgsl_vector_init(type=1.0D0)
  c = fgsl_vector_init(type=1.0D0)
!
  open(20, file='fitting2.dat', form='formatted', status='old', iostat=status)
  if (status > 0) then
     stop 'Could not open fitting2.dat. You may need to run fitting3.exe'
  end if
  read(20, *) n
  allocate(x_m(3, n), y_v(n), w_v(n))
!
  status = fgsl_vector_align(y_v, n, y, n, 0_fgsl_size_t, 1_fgsl_size_t)
  status = fgsl_vector_align(w_v, n, w, n, 0_fgsl_size_t, 1_fgsl_size_t)
  status = fgsl_matrix_align(x_m, 3_fgsl_size_t, 3_fgsl_size_t, n, x)
  status = fgsl_vector_align(c_v, 3_fgsl_size_t, c, &
       3_fgsl_size_t, 0_fgsl_size_t, 1_fgsl_size_t)
  status = fgsl_matrix_align(cov_m, 3_fgsl_size_t, 3_fgsl_size_t, &
       3_fgsl_size_t, cov)
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
