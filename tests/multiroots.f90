module mod_multiroots
  use, intrinsic :: iso_c_binding
  use fgsl
  use mod_unit
  implicit none
contains
  function rosenbrock_f(x, params, f) bind(c)
    type(c_ptr), value :: x, params, f
    integer(c_int) :: rosenbrock_f
!
    type(fgsl_vector) :: fx, ff
    real(fgsl_double), pointer :: par(:), xv(:), yv(:)
    call fgsl_obj_c_ptr(fx, x)
    call fgsl_obj_c_ptr(ff, f)
    call c_f_pointer(params, par, (/ 2 /))
    xv => fgsl_vector_to_fptr(fx)
    yv => fgsl_vector_to_fptr(ff)
    yv(1) = par(1) * (1.0_fgsl_double - xv(1))
    yv(2) = par(2) * (xv(2) - xv(1)*xv(1))
    rosenbrock_f = fgsl_success
  end function rosenbrock_f
  function rosenbrock_df(x, params, df) bind(c)
    type(c_ptr), value :: x, params, df
    integer(c_int) :: rosenbrock_df
!
    type(fgsl_vector) :: fx
    type(fgsl_matrix) :: dff
    real(fgsl_double), pointer :: par(:), xv(:), yv(:,:)
    call fgsl_obj_c_ptr(fx, x)
    call fgsl_obj_c_ptr(dff, df)
    call c_f_pointer(params, par, (/ 2 /))
    xv => fgsl_vector_to_fptr(fx)
    yv => fgsl_matrix_to_fptr(dff)
    yv(1:2,1:2) = reshape( source = (/ -par(1), 0.0_fgsl_double , &
         -2.0_fgsl_double*par(2)*xv(1), par(2)  /), &
         shape = (/ 2, 2 /))
    rosenbrock_df = fgsl_success
  end function rosenbrock_df
  function rosenbrock_fdf(x, params, f, df) bind(c)
    type(c_ptr), value :: x, params, f, df
    integer(c_int) :: rosenbrock_fdf
!
    integer(c_int) :: status
    status = rosenbrock_f(x, params, f)
    status = rosenbrock_df(x, params, df)
    rosenbrock_fdf = fgsl_success
  end function rosenbrock_fdf
end module mod_multiroots
program multiroots
  use mod_multiroots
  implicit none
  real(fgsl_double), parameter :: eps5 = 1.0d-5
  integer(fgsl_int), parameter :: itmax_root = 50
  real(fgsl_double), target :: fpar(2), xv(2)
!  real(fgsl_double) :: ri, ra, xlo, xhi
  type(c_ptr) :: ptr
  type(fgsl_multiroot_fsolver) :: mroot_fslv
  type(fgsl_multiroot_fdfsolver) :: mroot_fdfslv
  type(fgsl_multiroot_function) :: mroot_f
  type(fgsl_multiroot_function_fdf) :: mroot_fdf
  type(fgsl_vector) :: xvec, fvec
  integer(fgsl_int) :: status, i
  integer(fgsl_size_t) :: nrt
  character(kind=fgsl_char,len=fgsl_strmax) :: name
  real(fgsl_double), pointer :: xptr(:)
!  real(fgsl_double), pointer :: fptr(:)

!
! Test multiroot finding routines
!
  call unit_init(20)
! fsolver
  nrt = 2
  mroot_fslv = fgsl_multiroot_fsolver_alloc(fgsl_multiroot_fsolver_hybrids,nrt)
  name = fgsl_multiroot_fsolver_name(mroot_fslv)
  call unit_assert_equal('fgsl_multiroot_fsolver_name','hybrids',trim(name))
! initialize function object
  fpar(1:2) = (/1.0_fgsl_double, 10.0_fgsl_double/)
  ptr = c_loc(fpar)
  mroot_f = fgsl_multiroot_function_init(rosenbrock_f,nrt,ptr)
  xv(1:2) = (/-10.0_fgsl_double, -5.0_fgsl_double/)
  xvec = fgsl_vector_init(xv(1:nrt))
  status = fgsl_multiroot_fsolver_set(mroot_fslv, mroot_f, xvec)
  call unit_assert_equal('fgsl_multiroot_fsolver_set:status', &
       fgsl_success,status)
  fvec = fgsl_multiroot_fsolver_f(mroot_fslv)
!  fptr => fgsl_vector_to_fptr(fvec)
  call fgsl_vector_free(xvec)
  xvec = fgsl_multiroot_fsolver_root(mroot_fslv)
  xptr => fgsl_vector_to_fptr(xvec)
  call unit_assert_true('fgsl_multiroot_fsolver_alloc', &
       fgsl_well_defined(mroot_fslv), .true.)
  i = 0
  do
     i = i + 1
     status = fgsl_multiroot_fsolver_iterate(mroot_fslv);
     if (status /= fgsl_success .or. i > itmax_root) then
        exit
     end if
     status = fgsl_multiroot_test_residual(fvec, 1.0d-7)
     if (status == fgsl_success) then 
!       write(6, '(1x,i2,1x,2(1f12.5),1x,2(1f12.5))') i, xptr(1:2), fptr(1:2)
        exit
     end if
  end do
  call unit_assert_equal('fgsl_multiroot_fsolver_iterate:status', &
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_multiroot_fsolver',(/1.0d0, 1.0d0/), &
       xptr,eps5)
  call fgsl_multiroot_fsolver_free(mroot_fslv)
  call fgsl_multiroot_function_free(mroot_f)
! fdfsolver
  nrt = 2
  mroot_fdfslv = fgsl_multiroot_fdfsolver_alloc( &
       fgsl_multiroot_fdfsolver_gnewton,nrt)
  name = fgsl_multiroot_fdfsolver_name(mroot_fdfslv)
  call unit_assert_equal('fgsl_multiroot_fdfsolver_name','gnewton',trim(name))
! initialize function object
  fpar(1:2) = (/1.0_fgsl_double, 10.0_fgsl_double/)
  ptr = c_loc(fpar)
  mroot_fdf = fgsl_multiroot_function_fdf_init(rosenbrock_f,rosenbrock_df, &
       rosenbrock_fdf,nrt,ptr)
!
  xv(1:2) = (/-10.0_fgsl_double, -5.0_fgsl_double/)
  xvec = fgsl_vector_init(xv(1:nrt))
  status = fgsl_multiroot_fdfsolver_set(mroot_fdfslv, mroot_fdf, xvec)
  call unit_assert_equal('fgsl_multiroot_fdfsolver_set:status', &
       fgsl_success,status)
  fvec = fgsl_multiroot_fdfsolver_f(mroot_fdfslv)
!  fptr => fgsl_vector_to_fptr(fvec)
  call fgsl_vector_free(xvec)
  xvec = fgsl_multiroot_fdfsolver_root(mroot_fdfslv)
  xptr => fgsl_vector_to_fptr(xvec)
  call unit_assert_true('fgsl_multiroot_fdfsolver_alloc', &
       fgsl_well_defined(mroot_fdfslv), .true.)
  i = 0
  do
     i = i + 1
     status = fgsl_multiroot_fdfsolver_iterate(mroot_fdfslv);
     if (status /= fgsl_success .or. i > itmax_root) then
        exit
     end if
     status = fgsl_multiroot_test_residual(fvec, 1.0E-7_fgsl_double)
     if (status == fgsl_success) then 
!       write(6, '(1x,i2,1x,2(1f12.5),1x,2(1f12.5))') i, xptr(1:2), fptr(1:2)
        exit
     end if
  end do
  call unit_assert_equal('fgsl_multiroot_fdfsolver_iterate:status', &
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_multiroot_fdfsolver',(/1.0d0, 1.0d0/), &
       xptr,eps5)

  call fgsl_multiroot_fdfsolver_free(mroot_fdfslv)
  call fgsl_multiroot_function_fdf_free(mroot_fdf)


!
! Done
!
  call unit_finalize() 
end program multiroots
