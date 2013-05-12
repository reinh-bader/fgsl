module mod_roots
  use, intrinsic :: iso_c_binding
  use fgsl
  use mod_unit
  implicit none
contains
  function quadratic(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: quadratic
!
    real(fgsl_double), pointer :: p(:)
    call c_f_pointer(params, p, (/3/))
    quadratic = (p(1)*x + p(2))*x + p(3)
  end function quadratic
  function quadratic_deriv(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: quadratic_deriv
!
    real(fgsl_double), pointer :: p(:)
    call c_f_pointer(params, p, (/3/))
    quadratic_deriv = 2.0_fgsl_double*p(1)*x + p(2) 
  end function quadratic_deriv
  subroutine quadratic_fdf(x, params, y, dy) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double), intent(out) :: y, dy
!
    real(fgsl_double), pointer :: p(:)
    call c_f_pointer(params, p, (/3/))
    y = (p(1)*x + p(2))*x + p(3)
    dy = 2.0_fgsl_double*p(1)*x + p(2)
  end subroutine quadratic_fdf
end module mod_roots
program roots
  use mod_roots
  implicit none
  real(fgsl_double), parameter :: eps5 = 1.0d-5
  integer(fgsl_int), parameter :: itmax_root = 50
  real(fgsl_double), target :: fpar(3)
  real(fgsl_double) :: ri, ra, xlo, xhi
  type(c_ptr) :: ptr
  type(fgsl_function) :: stdfunc
  type(fgsl_function_fdf) :: stdfunc_fdf
  type(fgsl_root_fsolver) :: root_fslv
  type(fgsl_root_fdfsolver) :: root_fdfslv
  integer(fgsl_int) :: status, i
  character(kind=fgsl_char,len=fgsl_strmax) :: name
!
! Test root finding routines
!
  call unit_init(20)
!
  fpar = (/1.0_fgsl_double, 0.0_fgsl_double, -5.0_fgsl_double/)
  ptr = c_loc(fpar)
  stdfunc = fgsl_function_init(quadratic, ptr)
  root_fslv = fgsl_root_fsolver_alloc(fgsl_root_fsolver_brent)
  call unit_assert_true('fgsl_root_fsolver_alloc', &
       fgsl_well_defined(root_fslv), .true.)
  stdfunc = fgsl_function_init(quadratic, ptr)
  status = fgsl_root_fsolver_set(root_fslv, stdfunc, 0.0_fgsl_double, &
       5.0_fgsl_double)
  name = fgsl_root_fsolver_name(root_fslv)
  call unit_assert_equal('fgsl_root_fsolver_name','brent',trim(name))
  i = 0
  do
     i = i + 1
     status = fgsl_root_fsolver_iterate(root_fslv)
     if (status /= fgsl_success .or. i > itmax_root) then
        exit
     end if
     ra = fgsl_root_fsolver_root(root_fslv)
     xlo = fgsl_root_fsolver_x_lower(root_fslv)
     xhi = fgsl_root_fsolver_x_upper(root_fslv)
     status = fgsl_root_test_interval (xlo, xhi, 0.0_fgsl_double, eps5)
     if (status == fgsl_success) exit
  end do
  call unit_assert_equal('fgsl_root_fsolver:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_root_fsolver',sqrt(5.0_fgsl_double), &
       ra,eps5)
  call fgsl_root_fsolver_free(root_fslv)
  call fgsl_function_free(stdfunc)
!
  root_fdfslv = fgsl_root_fdfsolver_alloc(fgsl_root_fdfsolver_newton)
  call unit_assert_true('fgsl_root_fdfsolver_alloc', &
       fgsl_well_defined(root_fdfslv), .true.)
  stdfunc_fdf = fgsl_function_fdf_init(quadratic, quadratic_deriv, &
       quadratic_fdf, ptr)
  status = fgsl_root_fdfsolver_set(root_fdfslv, stdfunc_fdf, &
          5.0_fgsl_double)
  name = fgsl_root_fdfsolver_name (root_fdfslv)
  call unit_assert_equal('fgsl_root_fdfsolver_name','newton',trim(name))
  i = 0
  ra = 0
  do
     i = i + 1
     status = fgsl_root_fdfsolver_iterate(root_fdfslv)
     if (status /= fgsl_success .or. i > itmax_root) then
        exit
     end if
     ri = ra
     ra = fgsl_root_fdfsolver_root(root_fdfslv)
     status = fgsl_root_test_delta (ra, ri, 0.0_fgsl_double, eps5)
     if (status == fgsl_success) exit
  end do
  call unit_assert_equal('fgsl_fsolver:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_fsolver',sqrt(5.0_fgsl_double), &
       ra,eps5)
  call fgsl_root_fdfsolver_free(root_fdfslv)
  call fgsl_function_fdf_free(stdfunc_fdf)
!
! Done
!
  call unit_finalize() 
end program roots
