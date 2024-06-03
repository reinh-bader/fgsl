module mod_multifit
! illustrates legacy nonlinear fit interface
! see nlfit*.f90 for the updated interface usage.
  use fgsl
  use, intrinsic :: iso_c_binding
  implicit none
  type data
     integer(fgsl_size_t) :: n
     real(fgsl_double), allocatable :: y(:), sigma(:)
  end type data
contains
  function expb_f(x, cdata, f) bind(c)
    type(c_ptr), value :: x, cdata, f
    integer(c_int) :: expb_f
!
    type(fgsl_vector) :: f_x, f_f
    type(data), pointer :: f_data
    integer(fgsl_size_t) :: i
    real(fgsl_double) :: yy
    real(fgsl_double), pointer :: p_x(:), p_f(:)
!
    call fgsl_obj_c_ptr(f_x, x)
    call fgsl_obj_c_ptr(f_f, f)
    p_x => fgsl_vector_to_fptr(f_x)
    p_f => fgsl_vector_to_fptr(f_f)
    call c_f_pointer(cdata, f_data)
    do i=1,f_data%n
       yy = p_x(1) * exp(- p_x(2) * dble(i-1) ) + p_x(3)
       p_f(i) = (yy - f_data%y(i))/f_data%sigma(i)
    end do
    expb_f = fgsl_success
  end function expb_f
  function expb_df(x, cdata, j) bind(c)
    type(c_ptr), value :: x, cdata, j
    integer(c_int) :: expb_df
!
    type(fgsl_vector) :: f_x
    type(fgsl_matrix) :: f_j
    type(data), pointer :: f_data
    integer(fgsl_size_t) :: i
    real(fgsl_double) :: yy
    real(fgsl_double), pointer :: p_x(:), p_j(:,:)
!
    call fgsl_obj_c_ptr(f_x, x)
    call fgsl_obj_c_ptr(f_j, j)
    p_x => fgsl_vector_to_fptr(f_x)
    p_j => fgsl_matrix_to_fptr(f_j)
    call c_f_pointer(cdata, f_data)
    do i=1,f_data%n
       yy = exp(- p_x(2) * dble(i-1) )
       p_j(1,i) = yy / f_data%sigma(i)
       p_j(2,i) = - dble(i-1) * p_x(1) * yy / f_data%sigma(i)
       p_j(3,i) = 1 / f_data%sigma(i)
    end do
    expb_df = fgsl_success
  end function expb_df
  function expb_fdf(x, cdata, f, j) bind(c)
    type(c_ptr), value :: x, cdata, f, j
    integer(c_int) :: expb_fdf
!
    integer(c_int) :: status
    status = expb_f(x, cdata, f)
    status = expb_df(x, cdata, j)
    expb_fdf = fgsl_success
  end function expb_fdf
end module
program multifit
  use mod_multifit
  implicit none
  real(fgsl_double), parameter :: eps = 1.0E-4_fgsl_double
  integer(fgsl_size_t), parameter :: itmax = 30
  integer(fgsl_size_t), parameter :: nmax = 40, nrt = 3
  integer(fgsl_int) :: status
  type(fgsl_multifit_function_fdf) :: nlfit_fdf
  type(fgsl_multifit_fdfsolver) :: nlfit_slv
  type(fgsl_rng) :: rng
  type(fgsl_vector) :: params
  type(fgsl_matrix) :: cov, jac
  type(data), target :: fitdata
  type(c_ptr) :: ptr
  real(fgsl_double), target :: v_params(3), v_cov(3,3), v_jac(nrt,nmax)
  real(fgsl_double), pointer :: v_fun(:), v_parptr(:)
  real(fgsl_double) :: chi, c
  integer :: i
!
  rng = fgsl_rng_alloc(fgsl_rng_default)
  fitdata%n = nmax
  allocate(fitdata%y(nmax),fitdata%sigma(nmax))
  write(*, fmt='('' No.       Data value       Sigma'')')
  do i=1, nmax
     fitdata%y(i) = 1.0_fgsl_double + &
          5.0_fgsl_double * exp(-0.1_fgsl_double*dble(i-1)) + &
          fgsl_ran_gaussian(rng, 0.1_fgsl_double)
     fitdata%sigma(i) = 0.1_fgsl_double * (1.0_fgsl_double + &
          5.0_fgsl_double * exp(-0.1_fgsl_double*dble(i-1)))
     write(*, fmt='(I3,2X,2(F16.8,1X))') i, fitdata%y(i), fitdata%sigma(i)
  end do
  ptr = c_loc(fitdata)
  nlfit_fdf = fgsl_multifit_function_fdf_init(expb_f, expb_df, &
       expb_fdf, nmax, nrt, ptr)
  nlfit_slv = fgsl_multifit_fdfsolver_alloc(fgsl_multifit_fdfsolver_lmsder, &
       nmax, nrt)
! initial guess for fit parameters
  v_params(1:3) = (/1.0_fgsl_double, 0.0_fgsl_double, 0.0_fgsl_double/)
  params = fgsl_vector_init(v_params(1:nrt))
! alignment of target function and parameter values only needed once
! storage allocated within the nlfit_slv object
  v_fun => fgsl_vector_to_fptr(fgsl_multifit_fdfsolver_f(nlfit_slv))
  v_parptr => fgsl_vector_to_fptr(fgsl_multifit_fdfsolver_position(nlfit_slv))
! storage for cov within Fortran
  cov = fgsl_matrix_init(v_cov)
  jac = fgsl_matrix_init(v_jac)
  if (fgsl_well_defined(nlfit_slv)) then
     status = fgsl_multifit_fdfsolver_set(nlfit_slv, nlfit_fdf, params)
     i = 0
     write(*, fmt= &
          '(''Iteration   <--------- Fit Parameter Values --------->   Targ. function'')')
     do
        write(*, fmt='(2X,I3,2X,3(F15.8,1X),2X,1PD12.5)') i, v_parptr(1:nrt), &
             sqrt(dot_product(v_fun,v_fun))
        i = i + 1
        status = fgsl_multifit_fdfsolver_iterate(nlfit_slv)
        if (status /= fgsl_success .or. i > itmax) then
           write(*, *) 'Iteration or Convergence failure'
           exit
        end if
        status = fgsl_multifit_test_delta( &
             fgsl_multifit_fdfsolver_dx(nlfit_slv), &
             fgsl_multifit_fdfsolver_position(nlfit_slv), eps, eps)
        if (status == fgsl_success) then
           exit
        end if
     end do
     status = fgsl_multifit_fdfsolver_jac(nlfit_slv, jac)
     status = fgsl_multifit_covar(jac, 0.0_fgsl_double, cov)
     chi = sqrt(dot_product(v_fun,v_fun))
     c = max(1.0d0, chi/sqrt(dble(nmax-nrt)))
!  c only relevant for error estimate if chi*chi/dof exceeds 1
     write(*, '(''Iteration complete. Final Results for '')')
     write(*, '(''  fit function A exp (-lambda x) + b: '')')
     write(*, '(''  A      = '',1PE15.8,'' +/- '',1PE12.5)') v_parptr(1), &
          c * v_cov(1,1)
     write(*, '(''  lambda = '',1PE15.8,'' +/- '',1PE12.5)') v_parptr(2), &
          c * v_cov(2,2)
     write(*, '(''  b      = '',1PE15.8,'' +/- '',1PE12.5)') v_parptr(3), &
          c * v_cov(3,3)
     write(*, '(''  chisq / degrees_of_freedom: '',1PE10.3)') chi*chi/dble(nmax-nrt)
  end if
  call fgsl_vector_free(params)
  call fgsl_matrix_free(cov)
  call fgsl_multifit_fdfsolver_free(nlfit_slv)
  call fgsl_multifit_function_fdf_free(nlfit_fdf)
  call fgsl_rng_free(rng)
end program multifit
