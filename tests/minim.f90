module mod_minim
  use, intrinsic :: iso_c_binding
  use fgsl
  use mod_unit
  implicit none
contains
  function cosp1(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: cosp1
!   
    cosp1 = cos(x) + 1.0d0
  end function cosp1
end module mod_minim
program minim
  use mod_minim
  implicit none
  real(fgsl_double), parameter :: eps5 = 1.0d-5
  integer(fgsl_int), parameter :: itmax_root = 50
  real(fgsl_double) :: ra, xlo, xhi
  type(c_ptr) :: ptr
  type(fgsl_function) :: stdfunc
  type(fgsl_min_fminimizer) :: min_fslv
  integer(fgsl_int) :: status, i
  character(kind=fgsl_char,len=fgsl_strmax) :: name
!
! Test one-dimensional minimization
!
  call unit_init(20)
!
  min_fslv = fgsl_min_fminimizer_alloc(fgsl_min_fminimizer_brent)
  call unit_assert_true('fgsl_min_fminimizer_alloc', &
       fgsl_well_defined(min_fslv), .true.)
  stdfunc = fgsl_function_init(cosp1, ptr)
  status = fgsl_min_fminimizer_set(min_fslv, stdfunc, &
       3.0_fgsl_double, 0.0_fgsl_double, 6.0_fgsl_double)
  name = fgsl_min_fminimizer_name(min_fslv)
  call unit_assert_equal('fgsl_min_fminimizer_name','brent',trim(name))
  i = 0
  do
     i = i + 1
     status = fgsl_min_fminimizer_iterate(min_fslv)
     if (status /= fgsl_success .or. i > itmax_root) then
        exit
     end if
     ra = fgsl_min_fminimizer_x_minimum(min_fslv)
     xlo = fgsl_min_fminimizer_x_lower(min_fslv)
     xhi = fgsl_min_fminimizer_x_upper(min_fslv)
     status = fgsl_min_test_interval (xlo, xhi, eps5, 0.0_fgsl_double)
     if (status == fgsl_success) exit
  end do
  call unit_assert_equal('fgsl_min_fminimizer:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_min_fminimizer', &
       m_pi,ra,eps5)
  call fgsl_min_fminimizer_free(min_fslv)
  call fgsl_function_free(stdfunc)
!
! Done
!
  call unit_finalize() 
end program minim
