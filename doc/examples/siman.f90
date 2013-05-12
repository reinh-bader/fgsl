module mod_siman
  use fgsl
  use, intrinsic :: iso_c_binding
  implicit none
  interface 
     subroutine pp1(xp) bind(c)
       import :: c_ptr
       type(c_ptr), value :: xp
     end subroutine pp1
  end interface
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
!
end module mod_siman
program siman
  use mod_siman
  implicit none
! how many points do we try before stepping
  integer(fgsl_int), parameter :: N_TRIES = 200
! how many iterations for each T?
  integer(fgsl_int), parameter :: ITERS_FIXED_T = 10 
! max step size in random walk
  real(fgsl_double), parameter :: STEP_SIZE = 10.0_fgsl_double
! Boltzmann constant
  real(fgsl_double), parameter :: KB = 1.0_fgsl_double
! initial temperature
  real(fgsl_double), parameter ::  T_INITIAL = 0.002_fgsl_double
! damping factor for temperature
  real(fgsl_double), parameter :: MU_T = 1.005_fgsl_double
  real(fgsl_double), parameter :: T_MIN = 2.0e-6_fgsl_double
!
  type(fgsl_rng) :: rng
  type(fgsl_rng_type) :: t
  type(fgsl_file) :: stdout
  type(fgsl_siman_params_t) :: params
  integer(fgsl_int) :: status
  real(fgsl_double), target :: tg
  type(c_ptr) :: ptr
!
  t = fgsl_rng_env_setup()
  rng = fgsl_rng_alloc(t)
  tg = 15.5_fgsl_double
  ptr = c_loc(tg)
  call fgsl_siman_params_init(params, N_TRIES, ITERS_FIXED_T, &
       STEP_SIZE, KB, T_INITIAL, MU_T, T_MIN) 
  call fgsl_siman_solve(rng, ptr, e1, s1, m1, pp1, &
       element_size=8_fgsl_size_t, params=params)
! prevent interleaving of Fortran and C output.
  stdout = fgsl_stdout()
  status = fgsl_flush(stdout)
  write(6, '(''Final Result (should be 1.3633498): '',F12.7)') tg
  call fgsl_siman_params_free(params)
  call fgsl_rng_free(rng)
end program siman
