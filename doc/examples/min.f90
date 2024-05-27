module mod_min
  use fgsl
  use, intrinsic :: iso_c_binding
contains
  function cosp(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: cosp
!   
    cosp = cos(x) + 1.0d0
  end function cosp
end module mod_min
program min
  use mod_min
  implicit none
  integer(fgsl_int), parameter :: itmax = 30
  real(fgsl_double), parameter :: eps=1.0E-6_fgsl_double
  integer(fgsl_int) :: status, i
  real(fgsl_double) :: xmin, xlo, xhi
  character(kind=fgsl_char,len=fgsl_strmax) :: name
  type(fgsl_min_fminimizer) :: min_fslv
  type(fgsl_function) :: func
  type(c_ptr) :: ptr
!
  min_fslv = fgsl_min_fminimizer_alloc(fgsl_min_fminimizer_brent)
  func = fgsl_function_init(cosp, ptr)
  if (fgsl_well_defined(min_fslv)) then
     status = fgsl_min_fminimizer_set(min_fslv, func, 3.0_fgsl_double, 0.0_fgsl_double, &
          6.0_fgsl_double)
     name = fgsl_min_fminimizer_name(min_fslv)
     i = 0
     do
        i = i + 1
        status = fgsl_min_fminimizer_iterate(min_fslv)
        if (status /= FGSL_SUCCESS .or. i > itmax) then
           write(*, *) 'Failed to iterate or converge. Aborting.'
           exit
        end if
        xmin = fgsl_min_fminimizer_x_minimum(min_fslv)
        xlo = fgsl_min_fminimizer_x_lower(min_fslv)
        xhi = fgsl_min_fminimizer_x_upper(min_fslv)
        status = fgsl_min_test_interval (xlo, xhi, eps, 0.0_fgsl_double)
        if (status == FGSL_SUCCESS) exit
     end do
  end if
  write(*, '(''Using the '',A,'' algorithm'')') trim(name)
  write(*, '(''Minimum at: '',1PE20.13)') xmin
  write(*, '(''should be : '',1PE20.13)') m_pi
  call fgsl_min_fminimizer_free(min_fslv)
  call fgsl_function_free(func)
end program min
