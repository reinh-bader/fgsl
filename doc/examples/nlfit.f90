module mod_nlfit
  use fgsl
  use, intrinsic :: iso_c_binding
  implicit none
  type data
     integer(fgsl_size_t) :: n
     real(fgsl_double), allocatable :: y(:)
  end type data
contains
  function expb_f(x, cdata, f) bind(c)
    type(c_ptr), value :: x, cdata, f
    integer(c_int) :: expb_f
!
    type(fgsl_vector) :: f_x, f_f
    type(data), pointer :: f_data
    integer(fgsl_size_t) :: i
    integer(fgsl_int) :: status
    real(fgsl_double) :: yy
    real(fgsl_double), pointer :: p_x(:), p_f(:)
!
!   cast *void to an fgsl_vector
    call fgsl_obj_c_ptr(f_x, x)
    call fgsl_obj_c_ptr(f_f, f)
!
!   align fgsl_vector with Fortran POINTER
    status = fgsl_vector_align(p_x, f_x)
    status = fgsl_vector_align(p_f, f_f)
!
!   cast *void to Fortran data type
    call c_f_pointer(cdata, f_data)
    do i=1,f_data%n
       yy = p_x(1) * exp(- p_x(2) * dble(i-1) ) + p_x(3)
       p_f(i) = (yy - f_data%y(i))
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
    integer(fgsl_int) :: status
    real(fgsl_double) :: yy
    real(fgsl_double), pointer :: p_x(:), p_j(:,:)
!
    call fgsl_obj_c_ptr(f_x, x)
    call fgsl_obj_c_ptr(f_j, j)
    status = fgsl_vector_align(p_x, f_x)
    status = fgsl_matrix_align(p_j, f_j)
    call c_f_pointer(cdata, f_data)
    do i=1,f_data%n
       yy = exp(- p_x(2) * dble(i-1) )
       p_j(1,i) = yy 
       p_j(2,i) = - dble(i-1) * p_x(1) * yy 
       p_j(3,i) = 1.0_fgsl_double
    end do
    expb_df = fgsl_success
  end function expb_df
end module
program nlfit
  use mod_nlfit
  implicit none
  real(fgsl_double), parameter :: eps = 1.0E-8_fgsl_double
  integer(fgsl_size_t), parameter :: itmax = 30
  integer(fgsl_size_t), parameter :: nmax = 40, nrt = 3
  integer(fgsl_int) :: status
  type(fgsl_multifit_nlinear_type) :: t
  type(fgsl_multifit_nlinear_workspace) :: nlfit_w
  type(fgsl_multifit_nlinear_parameters) :: fdf_params
  type(fgsl_multifit_nlinear_fdf) :: nlfit_fdf
  type(fgsl_rng) :: rng
  type(fgsl_vector) :: params, wts
  type(fgsl_matrix) :: cov, jac
  type(data), target :: fitdata
  type(c_ptr) :: ptr
  integer(fgsl_int) :: info
  real(fgsl_double), target :: v_params(nrt), v_cov(nrt,nrt), v_jac(nmax,nrt), v_wts(nmax)
  real(fgsl_double), pointer :: v_fun(:), v_parptr(:)
  real(fgsl_double) :: chi, c, dy, si, xtol, gtol, ftol
  integer :: i
!
  rng = fgsl_rng_alloc(fgsl_rng_default)
  fitdata%n = nmax
  allocate(fitdata%y(nmax))
  write(*, fmt='('' No.       Data value       Sigma'')')
  do i=1, nmax
     fitdata%y(i) = 1.0_fgsl_double + &
          5.0_fgsl_double * exp(-0.1_fgsl_double*dble(i-1)) 
     si = 0.1_fgsl_double * fitdata%y(i)
     dy = fgsl_ran_gaussian(rng, si)
     v_wts(i) = 1.0_fgsl_double / (si * si)
     fitdata%y(i) = fitdata%y(i) + dy
     write(*, fmt='(I3,2X,2(F16.8,1X))') i, fitdata%y(i), si
  end do
  wts = fgsl_vector_init(type = 1.0_fgsl_double)
  status = fgsl_vector_align(v_wts, nmax, wts, nmax, 0_fgsl_size_t, 1_fgsl_size_t)
!
! set up default values for solver parameters, initialize function object
! and solver workspace
  t = fgsl_multifit_nlinear_type('trust')
  fdf_params = fgsl_multifit_nlinear_default_parameters()
  ptr = c_loc(fitdata)
  nlfit_fdf = fgsl_multifit_nlinear_fdf_init(nmax, nrt, ptr, expb_f, expb_df)
  nlfit_w = fgsl_multifit_nlinear_alloc(t, fdf_params, nmax, nrt)
  write(*, '(''Default solver name: '',A)') fgsl_multifit_nlinear_name(nlfit_w)
  write(*, '(''Default trust method: '',A)') fgsl_multifit_nlinear_trs_name(nlfit_w)
!
! initial guess for fit parameters
  v_params(1:3) = (/1.0_fgsl_double, 0.0_fgsl_double, 0.0_fgsl_double/)
  params = fgsl_vector_init(type = 1.0_fgsl_double)
  status = fgsl_vector_align(v_params,nrt,params,nrt, &
       0_fgsl_size_t,1_fgsl_size_t)
! alignment of target function and parameter values only needed once
! storage is persistently allocated within the working spade
  status = fgsl_vector_align(v_fun, &
       fgsl_multifit_nlinear_residual(nlfit_w))
  status = fgsl_vector_align(v_parptr, &
       fgsl_multifit_nlinear_position(nlfit_w))
! storage for cov within Fortran
  cov = fgsl_matrix_init(type = 1.0_fgsl_double)
  jac = fgsl_matrix_init(type = 1.0_fgsl_double)
  status = fgsl_matrix_align(v_cov,nrt,nrt,nrt,cov)
  status = fgsl_matrix_align(v_jac,nrt,nrt,nmax,jac)
  if (fgsl_well_defined(nlfit_w)) then
!
! initialize solver with starting points and weights
     status = fgsl_multifit_nlinear_winit(params, wts, nlfit_fdf, nlfit_w)
! tolerances
     xtol = eps; gtol = eps; ftol = 0.0_fgsl_double
     i = 0
     write(*, fmt= &
          '(''Iteration   <--------- Fit Parameter Values --------->   Targ. function'')')
     do
        write(*, fmt='(2X,I3,2X,3(F15.8,1X),2X,1PD12.5)') i, v_parptr(1:nrt), &
             sqrt(dot_product(v_fun,v_fun))
        i = i + 1
        status = fgsl_multifit_nlinear_iterate(nlfit_w)
        if (status /= fgsl_success .or. i > itmax) then
           write(*, *) 'Iteration or Convergence failure'
           exit
        end if
        status = fgsl_multifit_nlinear_test( xtol, gtol, ftol, info, nlfit_w)
        if (status == fgsl_success) then
           exit
        end if
     end do
     jac = fgsl_multifit_nlinear_jac(nlfit_w)
     status = fgsl_multifit_nlinear_covar(jac, 0.0_fgsl_double, cov)
     chi = sqrt(dot_product(v_fun,v_fun))
     c = max(1.0d0, chi/sqrt(dble(nmax-nrt)))
!  c only relevant for error estimate if chi*chi/dof exceeds 1
     write(*, '(''Iteration complete. Final Results for '')')
     write(*, '(''  fit function A exp (-lambda x) + b: '')')
     write(*, '(''  A      = '',1PE15.8,'' +/- '',1PE12.5)') v_parptr(1), &
          c * sqrt(v_cov(1,1))
     write(*, '(''  lambda = '',1PE15.8,'' +/- '',1PE12.5)') v_parptr(2), &
          c * sqrt(v_cov(2,2))
     write(*, '(''  b      = '',1PE15.8,'' +/- '',1PE12.5)') v_parptr(3), &
          c * sqrt(v_cov(3,3))
     write(*, '(''  chisq / degrees_of_freedom: '',1PE10.3)') chi*chi/dble(nmax-nrt)
  end if
  call fgsl_vector_free(params)
  call fgsl_matrix_free(cov)
  call fgsl_multifit_nlinear_free(nlfit_w)
  call fgsl_multifit_nlinear_fdf_free(nlfit_fdf)
  call fgsl_rng_free(rng)
end program nlfit
