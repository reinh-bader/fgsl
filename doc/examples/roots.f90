module mod_roots
  use fgsl
  use, intrinsic :: iso_c_binding
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
end module mod_roots
program roots
  use mod_roots
  implicit none
  real(fgsl_double), parameter :: eps = 1.0E-8_fgsl_double
  integer(fgsl_int), parameter :: itmax = 30
  real(fgsl_double), target :: fpar(3)
  real(fgsl_double) :: root, xlo, xhi
  character(kind=fgsl_char,len=fgsl_strmax) :: name
  integer :: i
  integer(fgsl_int) :: status
  type(c_ptr) :: ptr
  type(fgsl_root_fsolver) :: root_fslv
  type(fgsl_function) :: func
!
  fpar = (/-1.0_fgsl_double, 0.0_fgsl_double, 5.0_fgsl_double/)
  ptr = c_loc(fpar)
!
  root_fslv = fgsl_root_fsolver_alloc(fgsl_root_fsolver_brent)
  func = fgsl_function_init(quadratic, ptr)
  if (fgsl_well_defined(root_fslv)) then
     status = fgsl_root_fsolver_set(root_fslv, func, 0.0_fgsl_double, &
          5.0_fgsl_double)
     name = fgsl_root_fsolver_name (root_fslv)
     i = 0
     do
        i = i + 1
        status = fgsl_root_fsolver_iterate(root_fslv)
        if (status /= fgsl_success .or. i > itmax) then
           write(*, *) 'Failed to converge or iterate'
           exit
        end if
        root = fgsl_root_fsolver_root(root_fslv)
        xlo = fgsl_root_fsolver_x_lower(root_fslv)
        xhi = fgsl_root_fsolver_x_upper(root_fslv)
        status = fgsl_root_test_interval (xlo, xhi, 0.0_fgsl_double, eps)
        if (status == fgsl_success) exit
     end do
  end if
  write(*, '(''Output for root finding algorithm '',A,'':'')') trim(name)
  write(*, '(''Number of iterations needed: '',i2)') i
  write(*, '(''Root of '',F5.2,''*x*x + '',F5.2,''*x +'',F5.2,'' is: '',1PE18.10)') &
       fpar, root
  write(*, '(''Function value at root is: '',1PE18.10)') quadratic(root,ptr)
  call fgsl_root_fsolver_free(root_fslv)
  call fgsl_function_free(func)
end program roots
