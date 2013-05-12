module mod_ode
  use fgsl
  use, intrinsic :: iso_c_binding
  implicit none
contains
  function func(t, y, dydt, params) bind(c)
    real(c_double), value :: t
    real(c_double), dimension(*), intent(in) :: y
    real(c_double), dimension(*) :: dydt
    type(c_ptr), value :: params
    integer(c_int) :: func
! 
    real(c_double), pointer :: mu
!
    call c_f_pointer(params, mu)
    dydt(1) = y(2)
    dydt(2) = -y(1) - mu*y(2)*(y(1)*y(1) - 1)
    func = fgsl_success
  end function func
  function jac(t, y, dfdy, dfdt, params) bind(c)
    real(c_double), value :: t
    real(c_double), dimension(*), intent(in) :: y
    real(c_double), dimension(*) :: dfdy
    real(c_double), dimension(*) :: dfdt
    type(c_ptr), value :: params
    integer(c_int) :: jac
! 
    real(c_double), pointer :: mu
!
    call c_f_pointer(params, mu)
    dfdy(1) = 0.0_c_double
    dfdy(2) = 1.0_c_double
    dfdy(3) = -2.0_c_double*mu*y(1)*y(2) - 1.0_c_double
    dfdy(4) = -mu*(y(1)*y(1) - 1.0_c_double)
    dfdt(1:2) = 0.0_c_double
    jac = fgsl_success
  end function jac
end module mod_ode
program ode
  use mod_ode
  implicit none
  type(fgsl_odeiv2_system) :: ode_system
  type(fgsl_odeiv2_step) :: ode_step
  type(fgsl_odeiv2_control) :: ode_ctrl
  type(fgsl_odeiv2_evolve) :: ode_evlv
  type(c_ptr) :: ptr
  integer(fgsl_int) :: status
  real(fgsl_double) :: y(2), h, t, t1
  real(fgsl_double), target :: mu
  character(kind=fgsl_char, len=fgsl_strmax) :: name
! Note: bsimp uses the jacobian as opposed to rk8pd
  ode_step = fgsl_odeiv2_step_alloc(fgsl_odeiv2_step_bsimp,2_fgsl_size_t)
  name = fgsl_odeiv2_step_name(ode_step)
  ode_ctrl = fgsl_odeiv2_control_y_new(1.0e-6_fgsl_double, 0.0_fgsl_double)
  ode_evlv = fgsl_odeiv2_evolve_alloc(2_fgsl_size_t)
  mu = 10.0e0_c_double
  ptr = c_loc(mu)
  ode_system = fgsl_odeiv2_system_init(func, 2_c_size_t, ptr, jac)
!
  t = 0.0_fgsl_double; t1 = 100.0_fgsl_double
  h = 1.0e-6_fgsl_double
  y = (/1.0_c_double, 0.0_c_double /)
  write(6, '(''# Using the '',A,'' algorithm'')') trim(name)
  do while (t < t1) 
     status = fgsl_odeiv2_evolve_apply(ode_evlv, ode_ctrl, ode_step, &
          ode_system, t, t1, h, y)
     if (status /= FGSL_SUCCESS) exit
     write(6,fmt='(3(1PE15.8,1X))') t, y(1), y(2)
  end do
  call fgsl_odeiv2_evolve_free (ode_evlv)
  call fgsl_odeiv2_control_free (ode_ctrl)
  call fgsl_odeiv2_step_free (ode_step)
  call fgsl_odeiv2_system_free(ode_system)
end program ode
