module mod_siman
  use, intrinsic :: iso_c_binding
  use fgsl
  use mod_unit
  implicit none
contains
  function e1(xp) bind(c)
    type(c_ptr), value :: xp
    real(fgsl_double) :: e1
!
    real(fgsl_double), pointer :: x
    call c_f_pointer(xp, x)
    e1 = exp(-(x-1.0_fgsl_double)**2.0_fgsl_double) * &
         sin(8.0_fgsl_double*x)
  end function e1
  function m1(xp, yp) bind(c)
    type(c_ptr), value :: xp, yp
    real(fgsl_double) :: m1
!
    real(fgsl_double), pointer :: x, y
    call c_f_pointer(xp, x)
    call c_f_pointer(yp, y)
    m1 = abs(x - y)
  end function m1
  subroutine s1(r, xp, step_size) bind(c)
    type(c_ptr), value :: r, xp
    real(fgsl_double), value :: step_size
!
    real(fgsl_double), pointer :: x
    real(fgsl_double) :: u
    type(fgsl_rng) :: gr
    call fgsl_obj_c_ptr(gr, r)
    call c_f_pointer(xp, x)
    u = fgsl_rng_uniform(gr)
    x = x + 2.0_fgsl_double * u * step_size - step_size
  end subroutine s1
end module mod_siman
program siman
  use mod_siman
  implicit none
  real(fgsl_double), parameter :: eps6 = 1.0d-6
  real(fgsl_double), target :: xx
  type(c_ptr) :: ptr
  type(fgsl_rng) :: rng
  type(fgsl_siman_params_t) :: siman_params
!
! Test simulated annealing routines
!
  call unit_init(20)
!
  rng = fgsl_rng_alloc(fgsl_rng_default)
  xx = 15.5_fgsl_double
  ptr = c_loc(xx)
  call fgsl_siman_params_init(siman_params, 200, 10, 10.0_fgsl_double, &
       1.0_fgsl_double, 0.002_fgsl_double, 1.005_fgsl_double, 2.0e-6_fgsl_double) 
!  call fgsl_siman_solve(rng, ptr, e1, s1, m1, pp1, &
  call fgsl_siman_solve(rng, ptr, e1, s1, m1, &
       element_size=8_fgsl_size_t, params=siman_params)
!  write(6, *) xx
  call unit_assert_equal_within('fgsl_siman_solve',&
       1.3633498d0,xx,eps6)
  call fgsl_siman_params_free(siman_params)
  call fgsl_rng_free(rng)
!
! Done
!
  call unit_finalize() 
end program siman
