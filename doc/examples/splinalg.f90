PROGRAM splinalg
  use :: fgsl
  use, intrinsic :: iso_fortran_env, only : output_unit, error_unit
  implicit none
  integer(fgsl_size_t), parameter :: bigN = 100           !number of grid points
  integer(fgsl_size_t), parameter :: n = bigN - 2         !subtract 2 to exclude boundaries
  real(fgsl_double), parameter :: h = 1.0 / (bigN - 1.0)  !grid spacing
  type(fgsl_spmatrix) :: A                                !triplet format
  type(fgsl_spmatrix) :: C                                !compressed format
  type(fgsl_vector) :: f                                  !right hand side vector
  type(fgsl_vector) :: u                                  !solution vector
  integer(fgsl_size_t) ::  i
  integer(fgsl_int) :: status
  real(fgsl_double) :: fi, xi
  real(fgsl_double), parameter :: tol = 1.0e-6            !solution relative tolerance
  integer(fgsl_size_t), parameter :: max_iter = 10        !maximum iterations
  type(fgsl_splinalg_itersolve_type), parameter :: T = fgsl_splinalg_itersolve_gmres
  type(fgsl_splinalg_itersolve) :: work
  integer(fgsl_size_t) :: iter = 0
  real(fgsl_double) :: residual, u_gsl, u_exact
  real(fgsl_double),dimension(0:n-1),target :: f_f, u_f

  A = fgsl_spmatrix_alloc(n,n)
  f = fgsl_vector_init(1.0_fgsl_double)
  u = fgsl_vector_init(1.0_fgsl_double)
  status = fgsl_vector_align(f_f, n, f, n, 0_fgsl_size_t, 1_fgsl_size_t)
  status = fgsl_vector_align(u_f, n, u, n, 0_fgsl_size_t, 1_fgsl_size_t)
  work = fgsl_splinalg_itersolve_alloc(T, n, 0_fgsl_size_t)

  ! construct the sparse matrix for the finite difference equation

  ! construct first row
  status = fgsl_spmatrix_set(A, 0_fgsl_size_t, 0_fgsl_size_t, -2.0_fgsl_double)
  status = fgsl_spmatrix_set(A, 0_fgsl_size_t, 1_fgsl_size_t, 1.0_fgsl_double)

  ! construct rows [1:n-2]
  do i=1,n-2
    status = fgsl_spmatrix_set(A, i, i + 1, 1.0_fgsl_double)
    status = fgsl_spmatrix_set(A, i, i, -2.0_fgsl_double)
    status = fgsl_spmatrix_set(A, i, i - 1, 1.0_fgsl_double)
  end do

  ! construct last row
  status = fgsl_spmatrix_set(A, n - 1, n - 1, -2.0_fgsl_double)
  status = fgsl_spmatrix_set(A, n - 1, n - 2, 1.0_fgsl_double)

  ! scale by h^2
  status = fgsl_spmatrix_scale(A, 1.0 / (h * h))

  ! construct right hand side vector
  do i=0,n-1
    xi = (i + 1) * h
    fi = -M_PI * M_PI * sin(M_PI * xi)
    f_f(i) = fi
  end do

  ! convert to compressed column format
  C = fgsl_spmatrix_compcol(A)

  ! now solve the system with the GMRES iterative solver
  ! initial guess u = 0
  u_f = 0

  ! solve the system A u = f
  do
    status = fgsl_splinalg_itersolve_iterate(C, f, tol, u, work)

    ! print out residual norm ||A*u - f||
    residual = fgsl_splinalg_itersolve_normr(work);
    write(output_unit, '(A,I2,A,G15.6)') 'iter ', iter, ' residual = ', residual

    if (status == FGSL_SUCCESS) then
      write(output_unit, '(A)') 'Converged'
    endif
    iter = iter + 1
    if (status /= FGSL_CONTINUE .or. iter >= max_iter) exit
  end do
  ! output solution
  do i=0,n-1
    xi = (i + 1) * h
    u_exact = sin(M_PI * xi)
    u_gsl = u_f(i)

    !printf("%f %.12e %.12e\n", xi, u_gsl, u_exact)
    write(output_unit, '(3G15.6)') xi, u_gsl, u_exact
  end do

  call fgsl_splinalg_itersolve_free(work)

  call fgsl_spmatrix_free(A)
  call fgsl_spmatrix_free(C)
  call fgsl_vector_free(f)
  call fgsl_vector_free(u)
end program splinalg
