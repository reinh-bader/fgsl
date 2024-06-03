module mod_largefit
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: nblock = 5_fgsl_size_t     ! number of blocks to accumulate 
  integer(fgsl_size_t), parameter :: nlcurve = 200_fgsl_size_t
  integer(fgsl_int) :: status
  type(fgsl_error_handler_t) :: errh
contains
  ! function to be fitted
  real(fgsl_double) function func(t)
    real(fgsl_double), intent(in) :: t
    func = exp(sin(10.0d0*t)**3)
  end function func
  ! construct a row of the least squares matrix
  subroutine build_row(t, rv)
    real(fgsl_double), intent(in) :: t
    real(fgsl_double), intent(inout) :: rv(:)
    integer :: j
    rv(1) = 1.0_fgsl_double
    do j=2, size(rv,1)
       rv(j) = rv(j-1) * t
    end do
  end subroutine build_row
  subroutine solve_system(print_data, tf, lambda, n, p, c)
    logical, intent(in) :: print_data
    type(fgsl_multilarge_linear_type), intent(in) :: tf
    real(fgsl_double) :: lambda
    integer(fgsl_size_t) :: i, n, p
    type(fgsl_vector) :: c

    real(fgsl_double), target, allocatable :: xf(:,:), yf(:)
    real(fgsl_double), target :: regparamf(nlcurve), rhof(nlcurve), etaf(nlcurve)
    type(fgsl_matrix) :: X
    type(fgsl_vector) :: regparam, rho, eta, y
    real(fgsl_double) :: dt, fi, ei, rcond, rnorm, snorm, t
    
    integer(fgsl_size_t) :: nleft, nr, nrows, rowidx
    type(fgsl_multilarge_linear_workspace) :: w
    type(fgsl_rng) :: r
    real(fgsl_double), pointer :: qty(:)

    nrows = n / nblock        ! number of rows per block
    
    w = fgsl_multilarge_linear_alloc(tf, p)
    r = fgsl_rng_alloc(fgsl_rng_default)
    allocate(xf(p,nrows), yf(nrows))         ! note transposition for C storage ordering
    regparam = fgsl_vector_init(regparamf)
    rho = fgsl_vector_init(rhof); eta = fgsl_vector_init(etaf)
    rhof = 0._fgsl_double; etaf = 0._fgsl_double

    
    rowidx = 0
    t = 0._fgsl_double
    dt = 1.0_fgsl_double / real(n-1, fgsl_double)
    
    if (print_data) write(*, '("function values:")')
    do while (rowidx < n)
       nleft = n - rowidx     ! number of rows left to accumulate
       nr = min(nrows, nleft) ! number of rows in this block

       ! build (X,y) block with 'nr' rows
       do i = 1, nr
          fi = func(t)
          ei = fgsl_ran_gaussian (r, 0.1_fgsl_double * fi) ! noise
          ! construct this row of LS matrix
          call build_row(t, xf(:,i))
          ! set right hand side value with added noise
          yf(i) = fi + ei

          if (print_data .and. mod(i, 100_fgsl_size_t) == 1) then
             write(*,fmt='(3(F12.5,1X))') t, fi, yf(i)
          end if
          t = t + dt
       end do
       ! accumulate (X,y) block into LS system
       X = fgsl_matrix_init(xf, p, nr)
       y = fgsl_vector_init(yf(:nr))
!       write(*, *) 'Shape: ',shape(fgsl_matrix_to_fptr(X))
!       stop
       status = fgsl_multilarge_linear_accumulate(X, y, w)
       call fgsl_matrix_free(X)
       call fgsl_vector_free(y)
       
       rowidx = rowidx + nr
    end do

    if (print_data) write(*,'(//)')

    ! compute L-curve
    status = fgsl_multilarge_linear_lcurve(regparam, rho, eta, w)

    ! solve large LS system and store solution in c
    status = fgsl_multilarge_linear_solve(lambda, c, rnorm, snorm, w)
    if (print_data) then
       qty => fgsl_multilarge_linear_rhs_ptr(w)
       write(*, '("Qt*y:",I0," elements")') size(qty,1) 
       write(*, '(*(E12.5,1X))') qty
    end if

    ! compute reciprocal condition number 
    status = fgsl_multilarge_linear_rcond(rcond, w)

    write(*, '("=== Method ",A," ===")') trim(fgsl_multilarge_linear_name(w))
    write(*, '("condition number = ",1PE12.5)') 1.0_fgsl_double/rcond
    write(*, '("residual norm    = ",1PE12.5)') rnorm
    write(*, '("solution norm    = ",1PE12.5)') snorm

    write(*,'("=== L-curve ===")')
    do i = 1, nlcurve
       write(*, '(3E19.12)') regparamf(i), rhof(i), etaf(i)
    end do
    write(*,'("=== L-curve complete ===")')

    call fgsl_vector_free(regparam)
    call fgsl_vector_free(rho)
    call fgsl_vector_free(eta)
    call fgsl_multilarge_linear_free(w)
    call fgsl_rng_free(r)
    deallocate(xf, yf)
  end subroutine solve_system
end module mod_largefit
program largefit
  use mod_largefit
  implicit none
  integer(fgsl_size_t), parameter :: n = 50000     ! number of observations
  integer(fgsl_size_t), parameter :: p = 16        ! polynomial order + 1

  real(fgsl_double), target :: c_tsqr_f(p), c_normal_f(p), v(p)
  type(fgsl_vector) :: c_tsqr, c_normal
  real(fgsl_double) :: lambda = 0.0_fgsl_double
  real(fgsl_double) :: f_exact, f_tsqr, f_normal, t
  integer(fgsl_size_t) :: j

  c_tsqr_f = 0.0_fgsl_double; c_normal_f = 0.0_fgsl_double
  c_tsqr = fgsl_vector_init(c_tsqr_f)
  c_normal = fgsl_vector_init(c_normal_f)


  ! solve system with TSQR method
  call solve_system(.true., fgsl_multilarge_linear_tsqr, lambda, n, p, c_tsqr)

  ! turn off error handler so normal equations method won't abort
  errh = fgsl_set_error_handler_off()
  ! solve system with Normal equations method
  call solve_system(.false., fgsl_multilarge_linear_normal, lambda, n, p, c_normal)

  ! output solution
  t = 0._fgsl_double
  do j = 1, 100
     f_exact = func(t)
     call build_row(t, v)
     f_tsqr = dot_product(v, c_tsqr_f)
     f_normal = dot_product(v, c_normal_f)
     write(*, fmt='(F7.2,1X,3(1PE12.5))') t, f_exact, f_tsqr, f_normal

     t = t + 1.0e-2_fgsl_double
  end do

  call fgsl_vector_free(c_tsqr)
  call fgsl_vector_free(c_normal)
end program largefit
