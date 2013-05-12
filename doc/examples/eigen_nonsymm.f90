program eigen_nonsymm
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: n = 4_fgsl_size_t
  complex(fgsl_double), parameter :: ai = (0.0d0, 1.0d0)
  integer(fgsl_int) :: status, i
  type(fgsl_matrix) :: a
  type(fgsl_matrix_complex) :: evec
  type(fgsl_vector_complex) :: eval
  real(fgsl_double), target :: af(n, n)
  complex(fgsl_double), target :: evecf(n, n), evalf(4)
  type(fgsl_eigen_nonsymmv_workspace) :: w
!
  a = fgsl_matrix_init(type=1.0_fgsl_double)
  evec = fgsl_matrix_init(type=ai)
  eval = fgsl_vector_init(type=ai)


  af =    reshape((/-1.0d0, 1.0d0, -1.0d0, 1.0d0, &
                    -8.0d0, 4.0d0, -2.0d0, 1.0d0, &
                    27.0d0, 9.0d0,  3.0d0, 1.0d0, &
                    64.0d0,16.0d0,  4.0d0, 1.0d0  /), (/ 4, 4 /))
  status = fgsl_matrix_align(af, n, n, n, a)
  status = fgsl_matrix_align(evecf, n, n, n, evec)
  status = fgsl_vector_align(evalf, n, eval, n, 0_fgsl_size_t, &
       1_fgsl_size_t)

  w = fgsl_eigen_nonsymmv_alloc(n)

  status = fgsl_eigen_nonsymmv(a, eval, evec, w)
  status = fgsl_eigen_nonsymmv_sort (eval, evec, FGSL_EIGEN_SORT_ABS_DESC)

  do i=1, n
     write(*, fmt='(A,I0,A,2E15.8)') 'Eigenvalue No. ',i,': ', evalf(i)
     write(*, fmt='(A,4(2F10.5,1X,/,17X))') 'Eigenvector    : ',evecf(i,:)
  end do
  
  call fgsl_matrix_free(a)
  call fgsl_matrix_free(evec)
  call fgsl_vector_free(eval)
  call fgsl_eigen_nonsymmv_free(w)
end program eigen_nonsymm
