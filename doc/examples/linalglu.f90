program linalglu
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: n = 4_fgsl_size_t
  integer(fgsl_int) :: status, signum
  type(fgsl_matrix) :: a
  type(fgsl_vector) :: b, x
  real(fgsl_double), target :: af(n, n), bf(4), xf(4)
  type(fgsl_permutation) :: p
!
  a = fgsl_matrix_init(type=1.0_fgsl_double)
  b = fgsl_vector_init(type=1.0_fgsl_double)
  x = fgsl_vector_init(type=1.0_fgsl_double)
  p = fgsl_permutation_alloc(n)

  af = reshape((/0.18d0, 0.60d0, 0.57d0, 0.96d0, &
                 0.41d0, 0.24d0, 0.99d0, 0.58d0, &
                 0.14d0, 0.30d0, 0.97d0, 0.66d0, &
                 0.51d0, 0.13d0, 0.19d0, 0.85d0/), (/ 4, 4 /))
  bf = (/ 1.0d0, 2.0d0, 3.0d0, 4.0d0 /) 
  status = fgsl_matrix_align(af, n, n, n, a)
  status = fgsl_vector_align(bf, n, b, n, 0_fgsl_size_t, &
       1_fgsl_size_t)
  status = fgsl_vector_align(xf, n, x, n, 0_fgsl_size_t, &
       1_fgsl_size_t)

  status = fgsl_linalg_LU_decomp (a, p, signum)
  status = fgsl_linalg_LU_solve (a, p, b, x)

  write(*, *) 'x = '
  write(*, fmt='(F12.5)') xf
  
  call fgsl_matrix_free(a)
  call fgsl_vector_free(b)
  call fgsl_vector_free(x)
  call fgsl_permutation_free(p)
end program linalglu
