module mod_multimin
  use, intrinsic :: iso_c_binding
  use fgsl
  use mod_unit
  implicit none
contains
  function paraboloid_f(v, p) bind(c)
    type(c_ptr), value :: v, p
    real(c_double) :: paraboloid_f
!
    type(fgsl_vector) :: vec
    real(fgsl_double), pointer :: par(:), pvec(:)
    integer(fgsl_int) :: status
    call fgsl_obj_c_ptr(vec, v)
    call c_f_pointer(p, par, (/ 2 /))
    status = fgsl_vector_align(pvec, vec)
    paraboloid_f = 10.0_fgsl_double * (pvec(1)-par(1))**2 + &
         20.0_fgsl_double * (pvec(2)-par(2))**2 + 30.0_fgsl_double
  end function paraboloid_f
  subroutine paraboloid_df(v, p, df) bind(c)
    type(c_ptr), value :: v, p, df
!
    type(fgsl_vector) :: vec, grad
    real(fgsl_double), pointer :: par(:), pvec(:), pdf(:)
    integer(fgsl_int) :: status
    call fgsl_obj_c_ptr(vec, v)
    call fgsl_obj_c_ptr(grad, df)
    call c_f_pointer(p, par, (/ 2 /))
    status = fgsl_vector_align(pvec, vec)
    status = fgsl_vector_align(pdf, grad)
    pdf(1) = 20.0_fgsl_double * (pvec(1)-par(1))
    pdf(2) = 40.0_fgsl_double * (pvec(2)-par(2))
  end subroutine paraboloid_df
  subroutine paraboloid_fdf(v, p, f, df) bind(c)
    type(c_ptr), value :: v, p, df
    real(c_double) :: f
    f = paraboloid_f(v, p)
    call paraboloid_df(v, p, df)
  end subroutine paraboloid_fdf
end module mod_multimin
program multimin
  use mod_multimin
  implicit none
  real(fgsl_double), parameter :: eps5 = 1.0d-5
  integer(fgsl_int), parameter :: itmax_root = 50
  real(fgsl_double), target :: fpar(2), xv(2), stepv(2)
  real(fgsl_double) :: dmx
  type(c_ptr) :: ptr
  type(fgsl_vector) :: fvec, xvec, step
  type(fgsl_multimin_fminimizer) :: mmin_fmin
  type(fgsl_multimin_function) :: mmin_f
  type(fgsl_multimin_fdfminimizer) :: mmin_fdfmin
  type(fgsl_multimin_function_fdf) :: mmin_fdf
  integer(fgsl_int) :: status, i
  integer(fgsl_size_t) :: nrt
  character(kind=fgsl_char,len=fgsl_strmax) :: name
  real(fgsl_double), pointer :: xptr(:)

! see below
!  std = fgsl_set_error_handler_off()
!  call fgsl_ieee_env_setup()

!
! Test multiminimization routines
!
  call unit_init(20)
!
  nrt = 2
  mmin_fmin = fgsl_multimin_fminimizer_alloc(fgsl_multimin_fminimizer_nmsimplex,nrt)
  name = fgsl_multimin_fminimizer_name(mmin_fmin)
  call unit_assert_equal('fgsl_multiroot_fsolver_name','nmsimplex',trim(name))
! initialize function object
  fpar(1:2) = (/1.0_fgsl_double, 2.0_fgsl_double/)
  ptr = c_loc(fpar)
  mmin_f = fgsl_multimin_function_init(paraboloid_f,nrt,ptr)
!
  xv(1:2) = (/5.0_fgsl_double, 7.0_fgsl_double/)
  xvec = fgsl_vector_init(1.0_fgsl_double)
  status = fgsl_vector_align(xv,nrt,xvec,nrt,0_fgsl_size_t,1_fgsl_size_t)
  stepv(1:2) = (/1.0_fgsl_double, 1.0_fgsl_double/)
  step = fgsl_vector_init(1.0_fgsl_double)
  status = fgsl_vector_align(stepv,nrt,step,nrt,0_fgsl_size_t,1_fgsl_size_t)
  status = fgsl_multimin_fminimizer_set(mmin_fmin, mmin_f, xvec, step)
  call unit_assert_equal('fgsl_multiroot_fminimizer_set:status', &
       fgsl_success,status)
  call fgsl_vector_free(xvec)
  call fgsl_vector_free(step)
  xvec = fgsl_multimin_fminimizer_x(mmin_fmin)
  status = fgsl_vector_align(xptr, xvec)
  call unit_assert_true('fgsl_multiroot_fminimizer_alloc', &
       fgsl_well_defined(mmin_fmin), .true.)
  i = 0
  do
     i = i + 1
     status = fgsl_multimin_fminimizer_iterate(mmin_fmin);
     if (status /= fgsl_success .or. i > itmax_root) then
        exit
     end if
     dmx = fgsl_multimin_fminimizer_size(mmin_fmin);
     status = fgsl_multimin_test_size(dmx, eps5)
     if (status == fgsl_success) then 
        exit
     end if
  end do
  call unit_assert_equal('fgsl_multimin_fminimizer_iterate:status', &
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_multimin_fminimizer_iterate', &
       (/1.0d0, 2.0d0/), &
       xptr,eps5)
  call unit_assert_equal_within('fgsl_multimin_fminimizer_iterate', &
       3.0d1, fgsl_multimin_fminimizer_minimum(mmin_fmin),eps5)
  call fgsl_multimin_fminimizer_free(mmin_fmin)
  call fgsl_multimin_function_free(mmin_f)

! fdfminimizer
  nrt = 2
  mmin_fdfmin = fgsl_multimin_fdfminimizer_alloc(fgsl_multimin_fdfminimizer_conjugate_fr,nrt)
  name = fgsl_multimin_fdfminimizer_name(mmin_fdfmin)
  call unit_assert_equal('fgsl_multiroot_fsolver_name', &
       'conjugate_fr',trim(name))
! initialize function object
  fpar(1:2) = (/1.0_fgsl_double, 2.0_fgsl_double/)
  ptr = c_loc(fpar)
  mmin_fdf = fgsl_multimin_function_fdf_init( &
       paraboloid_f,paraboloid_df, paraboloid_fdf,nrt,ptr)
  xv(1:2) = (/5.0_fgsl_double, 7.0_fgsl_double/)
  xvec = fgsl_vector_init(1.0_fgsl_double)
  status = fgsl_vector_align(xv,nrt,xvec,nrt,0_fgsl_size_t,1_fgsl_size_t)
  status = fgsl_multimin_fdfminimizer_set(mmin_fdfmin, mmin_fdf, xvec, 0.01_fgsl_double, eps5)
  call unit_assert_equal('fgsl_multiroot_fdfminimizer_set:status', &
       fgsl_success,status)
  call fgsl_vector_free(xvec)
  xvec = fgsl_multimin_fdfminimizer_x(mmin_fdfmin)
  status = fgsl_vector_align(xptr, xvec)
  call unit_assert_true('fgsl_multiroot_fdfminimizer_alloc', &
       fgsl_well_defined(mmin_fdfmin), .true.)
  i = 0
  do
     i = i + 1
     status = fgsl_multimin_fdfminimizer_iterate(mmin_fdfmin)
! FIXME: last iteration produces invalid floating point operation. 
! g95 does not check flags, NAG requires -ieee=full to run through.
! try IEEE utilities?
     if (status /= fgsl_success .or. i > itmax_root) then
        exit
     end if
     fvec = fgsl_multimin_fdfminimizer_gradient(mmin_fdfmin)
     status = fgsl_multimin_test_gradient(fvec, eps5)
     if (status == fgsl_success) then 
        exit
     end if
  end do
  call unit_assert_equal('fgsl_multimin_fdfminimizer_iterate:status', &
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_multimin_fdfminimizer_iterate', &
       (/1.0d0, 2.0d0/), &
       xptr,eps5)
  call unit_assert_equal_within('fgsl_multimin_fdfminimizer_iterate', &
       3.0d1, fgsl_multimin_fdfminimizer_minimum(mmin_fdfmin),eps5)
  call fgsl_multimin_fdfminimizer_free(mmin_fdfmin)
  call fgsl_multimin_function_fdf_free(mmin_fdf)
!
! Done
!
  call unit_finalize() 
end program multimin
