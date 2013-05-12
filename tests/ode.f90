module mod_ode
  use, intrinsic :: iso_c_binding
  use fgsl
  use mod_unit
  implicit none
contains
  function func_ode(t, y, dydt, params) bind(c)
    real(c_double), value :: t
    real(c_double), dimension(*), intent(in) :: y
    real(c_double), dimension(*) :: dydt
    type(c_ptr), value :: params
    integer(c_int) :: func_ode
! 
    real(c_double), pointer :: mu
!
!    write(6, *) 'Starting func:'
    call c_f_pointer(params, mu)
!    write(6, *) 'y, mu:', y(1:2), mu
    dydt(1) = y(2)
    dydt(2) = -y(1) - mu*y(2)*(y(1)*y(1) - 1)
    func_ode = fgsl_success
  end function func_ode
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
!    write(6, *) 'Calling jac'
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
  real(fgsl_double), parameter :: eps3 = 1.0d-3
  real(fgsl_double), target :: xx
  real(fgsl_double) :: t, t1, h, y(2)
  character(kind=fgsl_char, len=fgsl_strmax) :: name
  type(c_ptr) :: ptr
  integer(fgsl_int) :: status, step_order
  type(fgsl_odeiv2_step) :: ode_step
  type(fgsl_odeiv2_control) :: ode_ctrl
  type(fgsl_odeiv2_evolve) :: ode_evlv
  type(fgsl_odeiv2_system) :: ode_system
  type(fgsl_odeiv2_driver) :: ode_drv
  type(fgsl_error_handler_t) :: std

!
! Test ODE solvers
!
  std = fgsl_set_error_handler_off()  
  call unit_init(20)
!
  ode_step = fgsl_odeiv2_step_alloc(fgsl_odeiv2_step_bsimp,2_fgsl_size_t)
  call unit_assert_true('fgsl_odeiv2_step_alloc', &
       fgsl_well_defined(ode_step), .true.)
  name = fgsl_odeiv2_step_name(ode_step)
!  write(6, *) 'ode_step_alloc of type ',trim(name),' done'
  call unit_assert_equal('fgsl_odeiv2_step_name', &
       trim(name), 'bsimp')
  step_order = fgsl_odeiv2_step_order(ode_step)
  call unit_assert_equal('fgsl_odeiv2_step_name', &
       step_order, 12_c_int)
  ode_ctrl = fgsl_odeiv2_control_y_new(1.0e-6_fgsl_double, 0.0_fgsl_double)
  name = fgsl_odeiv2_control_name(ode_ctrl)
  call unit_assert_equal('fgsl_odeiv2_control_name', &
       trim(name), 'standard')
  ode_evlv = fgsl_odeiv2_evolve_alloc(2_fgsl_size_t)
  call unit_assert_true('fgsl_odeiv2_evolve_alloc', &
       fgsl_well_defined(ode_evlv), .true.)
  xx = 10.0_fgsl_double
  ptr = c_loc(xx)
  ode_system = fgsl_odeiv2_system_init(func_ode, 2_c_size_t, ptr, jac)
  ode_drv = fgsl_odeiv2_driver_alloc_y_new(ode_system, fgsl_odeiv2_step_bsimp, &
       1.0e-6_fgsl_double, 1.0e-6_fgsl_double, 0.0_fgsl_double)
  call unit_assert_true('fgsl_odeiv2_driver_alloc_y_new', &
       fgsl_well_defined(ode_drv), .true.)
!
! evolution
!
  t = 0.0_fgsl_double; t1 = 100.0_fgsl_double
  h = 1.0e-6_fgsl_double; y = (/1.0_c_double, 0.0_c_double /)
  do while (t < t1) 
     status = fgsl_odeiv2_evolve_apply(ode_evlv, ode_ctrl, ode_step, ode_system, & 
          t, t1, h, y)
     if (status /= fgsl_success) exit
!     write(6,fmt='(3E15.8,1X)') t, y(1), y(2)
  end do
  call unit_assert_equal('fgsl_odeiv2_evolve_apply:status', &
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_odeiv2_evolve_apply',&
       (/-1.758888d0, 0.083643595d0/),y,eps3)
!
! driver
!
  t = 0.0_fgsl_double; t1 = 100.0_fgsl_double
  y = (/1.0_c_double, 0.0_c_double /)
  status = fgsl_odeiv2_driver_apply(ode_drv, t, t1, y)
  call unit_assert_equal('fgsl_odeiv2_driver_apply:status', &
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_driver_evolve_apply',&
       (/-1.758888d0, 0.083643595d0/),y,eps3)
!
  call fgsl_odeiv2_evolve_free (ode_evlv)
  call fgsl_odeiv2_control_free (ode_ctrl)
  call fgsl_odeiv2_step_free (ode_step)
  call fgsl_odeiv2_system_free(ode_system)
  call fgsl_odeiv2_driver_free(ode_drv)
!
! Done
!
  call unit_finalize() 
end program ode
