program eigen
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: n = 4_fgsl_size_t
  integer(fgsl_int) :: status, i
  type(fgsl_matrix) :: a, evec
  type(fgsl_vector) :: eval
  real(fgsl_double), target :: af(n, n), evecf(n, n), evalf(4)
  type(fgsl_eigen_symmv_workspace) :: w
!
  a = fgsl_matrix_init(type=1.0_fgsl_double)
  evec = fgsl_matrix_init(type=1.0_fgsl_double)
  eval = fgsl_vector_init(type=1.0_fgsl_double)


  af = reshape((/1.0d0  , 1/2.0d0, 1/3.0d0, 1/4.0d0, &
                 1/2.0d0, 1/3.0d0, 1/4.0d0, 1/5.0d0, &
                 1/3.0d0, 1/4.0d0, 1/5.0d0, 1/6.0d0, &
                 1/4.0d0, 1/5.0d0, 1/6.0d0, 1/7.0d0 /), (/ 4, 4 /))
  status = fgsl_matrix_align(af, n, n, n, a)
  status = fgsl_matrix_align(evecf, n, n, n, evec)
  status = fgsl_vector_align(evalf, n, eval, n, 0_fgsl_size_t, &
       1_fgsl_size_t)

  w = fgsl_eigen_symmv_alloc(n)

  status = fgsl_eigen_symmv(a, eval, evec, w)
  status = fgsl_eigen_symmv_sort (eval, evec, FGSL_EIGEN_SORT_ABS_ASC)

  do i=1, n
     write(*, fmt='(A,I0,A,E15.8)') 'Eigenvalue No. ',i,': ', evalf(i)
     write(*, fmt='(A,4F12.5)') 'Eigenvector    : ',evecf(i,:)
  end do
  
  call fgsl_matrix_free(a)
  call fgsl_matrix_free(evec)
  call fgsl_vector_free(eval)
  call fgsl_eigen_symmv_free(w)
end program eigen
