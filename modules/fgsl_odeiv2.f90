module fgsl_odeiv2
  !> Ordinary Differential Equations
  !> Note: this module contains the new interface
  use fgsl_base

  implicit none
  
  private :: fgsl_odeiv2_system_cinit, fgsl_odeiv2_system_cfree, &
    gsl_odeiv2_step_alloc, gsl_odeiv2_step_reset, gsl_odeiv2_step_free, &
    gsl_odeiv2_step_name, gsl_odeiv2_step_order, gsl_odeiv2_step_set_driver, &
    gsl_odeiv2_step_apply, fgsl_aux_odeiv2_step_alloc, &
    gsl_odeiv2_control_standard_new, gsl_odeiv2_control_y_new, &
    gsl_odeiv2_control_yp_new, gsl_odeiv2_control_scaled_new, &
    gsl_odeiv2_control_alloc, gsl_odeiv2_control_init, gsl_odeiv2_control_free, &
    gsl_odeiv2_control_hadjust, gsl_odeiv2_control_name, &
    gsl_odeiv2_control_errlevel, gsl_odeiv2_control_set_driver, &
    gsl_odeiv2_evolve_alloc, gsl_odeiv2_evolve_apply, &
    gsl_odeiv2_evolve_apply_fixed_step, gsl_odeiv2_evolve_reset, &
    gsl_odeiv2_evolve_free, gsl_odeiv2_evolve_set_driver, &
    gsl_odeiv2_driver_alloc_y_new, gsl_odeiv2_driver_alloc_yp_new, &
    gsl_odeiv2_driver_alloc_standard_new, gsl_odeiv2_driver_alloc_scaled_new, &
    gsl_odeiv2_driver_set_hmin, gsl_odeiv2_driver_set_hmax, &
    gsl_odeiv2_driver_set_nmax, gsl_odeiv2_driver_apply, &
    gsl_odeiv2_driver_apply_fixed_step, gsl_odeiv2_driver_reset, &
    gsl_odeiv2_driver_free, gsl_odeiv2_driver_reset_hstart
  !
  !> Types
  type, public :: fgsl_odeiv2_system
     private
     type(c_ptr) :: gsl_odeiv2_system = c_null_ptr
  end type fgsl_odeiv2_system
  type, public :: fgsl_odeiv2_step_type
     private
     integer(c_int) :: which = 0
  end type fgsl_odeiv2_step_type
  type(fgsl_odeiv2_step_type), parameter, public :: &
       fgsl_odeiv2_step_rk2 = fgsl_odeiv2_step_type(1), &
       fgsl_odeiv2_step_rk4 = fgsl_odeiv2_step_type(2), &
       fgsl_odeiv2_step_rkf45 = fgsl_odeiv2_step_type(3), &
       fgsl_odeiv2_step_rkck = fgsl_odeiv2_step_type(4), &
       fgsl_odeiv2_step_rk8pd = fgsl_odeiv2_step_type(5), &
       fgsl_odeiv2_step_rk1imp = fgsl_odeiv2_step_type(6), &
       fgsl_odeiv2_step_rk2imp = fgsl_odeiv2_step_type(7), &
       fgsl_odeiv2_step_rk4imp = fgsl_odeiv2_step_type(8), &
       fgsl_odeiv2_step_bsimp = fgsl_odeiv2_step_type(9), &
       fgsl_odeiv2_step_msadams = fgsl_odeiv2_step_type(10), &
       fgsl_odeiv2_step_msbdf = fgsl_odeiv2_step_type(11)
  type, public :: fgsl_odeiv2_step
     type(c_ptr) :: gsl_odeiv2_step = c_null_ptr
  end type fgsl_odeiv2_step
  type, public :: fgsl_odeiv2_driver
     private
     type(c_ptr) :: gsl_odeiv2_driver = c_null_ptr
  end type fgsl_odeiv2_driver
  type, public :: fgsl_odeiv2_control_type
     type(c_ptr) :: gsl_odeiv2_control_type = c_null_ptr
  end type fgsl_odeiv2_control_type
  type, public :: fgsl_odeiv2_control
     type(c_ptr) :: gsl_odeiv2_control = c_null_ptr
  end type fgsl_odeiv2_control
  type, public :: fgsl_odeiv2_evolve
     type(c_ptr) :: gsl_odeiv2_evolve
  end type fgsl_odeiv2_evolve
  !
  !> Generics
  interface fgsl_well_defined
     module procedure fgsl_odeiv2_control_status
     module procedure fgsl_odeiv2_evolve_status
     module procedure fgsl_odeiv2_step_status
     module procedure fgsl_odeiv2_system_status
     module procedure fgsl_odeiv2_driver_status
  end interface
  !
  ! C interfaces
  interface
     function fgsl_odeiv2_system_cinit(func, dimension, params, jacobian) bind(c)
       import
       type(c_funptr), value :: func
       integer(c_size_t), value :: dimension
       type(c_ptr), value :: params
       type(c_funptr), value :: jacobian
       type(c_ptr) :: fgsl_odeiv2_system_cinit
     end function fgsl_odeiv2_system_cinit
     subroutine fgsl_odeiv2_system_cfree(system) bind(c)
       import
       type(c_ptr), value :: system
     end subroutine fgsl_odeiv2_system_cfree
     function gsl_odeiv2_step_alloc(t, dim) bind(c)
       import
       type(c_ptr), value :: t
       integer(c_size_t), value :: dim
       type(c_ptr) :: gsl_odeiv2_step_alloc
     end function gsl_odeiv2_step_alloc
     function gsl_odeiv2_step_reset(s) bind(c)
       import
       type(c_ptr), value :: s
       integer(c_int) :: gsl_odeiv2_step_reset
     end function gsl_odeiv2_step_reset
     subroutine gsl_odeiv2_step_free(s) bind(c)
       import
       type(c_ptr), value :: s
     end subroutine gsl_odeiv2_step_free
     function gsl_odeiv2_step_name (s) bind(c)
       import
       type(c_ptr), value :: s
       type(c_ptr) :: gsl_odeiv2_step_name
     end function gsl_odeiv2_step_name
     function gsl_odeiv2_step_order(s) bind(c)
       import
       type(c_ptr), value :: s
       integer(c_int) :: gsl_odeiv2_step_order
     end function gsl_odeiv2_step_order
     function gsl_odeiv2_step_set_driver(s,d) bind(c)
       import
       type(c_ptr), value :: s, d
       integer(c_int) :: gsl_odeiv2_step_set_driver
     end function gsl_odeiv2_step_set_driver
     function gsl_odeiv2_step_apply(s, t, h, y, yerr, dydt_in, dydt_out, dydt) bind(c)
       import
       type(c_ptr), value :: s
       real(c_double), value :: t, h
       type(c_ptr), value :: y, yerr, dydt_out
       type(c_ptr), value :: dydt_in
       type(c_ptr), value :: dydt
       integer(c_int) :: gsl_odeiv2_step_apply
     end function gsl_odeiv2_step_apply
     function fgsl_aux_odeiv2_step_alloc(step_type) bind(c)
       import
       integer(c_int), value :: step_type
       type(c_ptr) :: fgsl_aux_odeiv2_step_alloc
     end function fgsl_aux_odeiv2_step_alloc
     function gsl_odeiv2_control_standard_new(eps_abs, eps_rel, a_y, a_dydt) bind(c)
       import
       real(c_double), value :: eps_abs, eps_rel, a_y, a_dydt
       type(c_ptr) :: gsl_odeiv2_control_standard_new
     end function gsl_odeiv2_control_standard_new
     function gsl_odeiv2_control_y_new(eps_abs, eps_rel) bind(c)
       import
       real(c_double), value :: eps_abs, eps_rel
       type(c_ptr) :: gsl_odeiv2_control_y_new
     end function gsl_odeiv2_control_y_new
     function gsl_odeiv2_control_yp_new(eps_abs, eps_rel) bind(c)
       import
       real(c_double), value :: eps_abs, eps_rel
       type(c_ptr) :: gsl_odeiv2_control_yp_new
     end function gsl_odeiv2_control_yp_new
     function gsl_odeiv2_control_scaled_new(eps_abs, eps_rel, a_y, a_dydt, &
          scale_abs, dim) bind(c)
       import
       real(c_double), value :: eps_abs, eps_rel, a_y, a_dydt
       type(c_ptr), value :: scale_abs
       integer(c_size_t), value :: dim
       type(c_ptr) :: gsl_odeiv2_control_scaled_new
     end function gsl_odeiv2_control_scaled_new
     function gsl_odeiv2_control_alloc(t) bind(c)
       import
       type(c_ptr), value :: t
       type(c_ptr) :: gsl_odeiv2_control_alloc
     end function gsl_odeiv2_control_alloc
     function gsl_odeiv2_control_init(c, eps_abs, eps_rel, a_y, a_dydt) bind(c)
       import
       type(c_ptr), value :: c
       real(c_double), value :: eps_abs, eps_rel, a_y, a_dydt
       integer(c_int) :: gsl_odeiv2_control_init
     end function gsl_odeiv2_control_init
     subroutine gsl_odeiv2_control_free(c) bind(c)
       import
       type(c_ptr), value :: c
     end subroutine gsl_odeiv2_control_free
     function gsl_odeiv2_control_hadjust(c, s, y, yerr, dydt, h) bind(c)
       import
       type(c_ptr), value :: c, s
       type(c_ptr), value :: y, yerr, dydt
       real(c_double) :: h
       integer(c_int) :: gsl_odeiv2_control_hadjust
     end function gsl_odeiv2_control_hadjust
     function gsl_odeiv2_control_name (c) bind(c)
       import
       type(c_ptr), value :: c
       type(c_ptr) :: gsl_odeiv2_control_name
     end function gsl_odeiv2_control_name
     function gsl_odeiv2_control_errlevel(c, y, dydt, h, ind, errlev) bind(c)
       import
       type(c_ptr), value :: c
       real(c_double), value :: y, dydt, h
       integer(c_size_t), value :: ind
       real(c_double) :: errlev
       integer(c_int) :: gsl_odeiv2_control_errlevel
     end function gsl_odeiv2_control_errlevel
     function gsl_odeiv2_control_set_driver(c, d) bind(c)
       import
       type(c_ptr), value :: c, d
       integer(c_int) :: gsl_odeiv2_control_set_driver
     end function gsl_odeiv2_control_set_driver
     function gsl_odeiv2_evolve_alloc(dim) bind(c)
       import
       integer(c_size_t), value :: dim
       type(c_ptr) gsl_odeiv2_evolve_alloc
     end function gsl_odeiv2_evolve_alloc
     function gsl_odeiv2_evolve_apply(e, con, step, sys, t, t1, h, y) bind(c)
       import
       type(c_ptr), value :: e, con, step, sys
       type(c_ptr), value :: y
       real(c_double) :: t, h
       real(c_double), value :: t1
       integer(c_int) :: gsl_odeiv2_evolve_apply
     end function gsl_odeiv2_evolve_apply
     function gsl_odeiv2_evolve_apply_fixed_step(e, con, step, dydt, t, h0, y) bind(c)
       import
       type(c_ptr), value :: e, con, step, dydt,  y
       real(c_double) :: t
       real(c_double), value :: h0
       integer(c_int) :: gsl_odeiv2_evolve_apply_fixed_step
     end function gsl_odeiv2_evolve_apply_fixed_step
     function gsl_odeiv2_evolve_reset(s) bind(c)
       import
       type(c_ptr), value :: s
       integer(c_int) :: gsl_odeiv2_evolve_reset
     end function gsl_odeiv2_evolve_reset
     subroutine gsl_odeiv2_evolve_free(s) bind(c)
       import
       type(c_ptr), value :: s
     end subroutine gsl_odeiv2_evolve_free
     function gsl_odeiv2_evolve_set_driver(c, d) bind(c)
       import
       type(c_ptr), value :: c, d
       integer(c_int) :: gsl_odeiv2_evolve_set_driver
     end function gsl_odeiv2_evolve_set_driver
     function gsl_odeiv2_driver_alloc_y_new(sys, t, hstart, epsabs, epsrel) bind(c)
       import
       type(c_ptr), value :: sys, t
       real(c_double), value :: hstart, epsabs, epsrel
       type(c_ptr) :: gsl_odeiv2_driver_alloc_y_new
     end function gsl_odeiv2_driver_alloc_y_new
     function gsl_odeiv2_driver_alloc_yp_new(sys, t, hstart, epsabs, epsrel) bind(c)
       import
       type(c_ptr), value :: sys, t
       real(c_double), value :: hstart, epsabs, epsrel
       type(c_ptr) :: gsl_odeiv2_driver_alloc_yp_new
     end function gsl_odeiv2_driver_alloc_yp_new
     function gsl_odeiv2_driver_alloc_standard_new(sys, t, hstart, epsabs, epsrel, &
          a_y, a_dydt) bind(c)
       import
       type(c_ptr), value :: sys, t
       real(c_double), value :: hstart, epsabs, epsrel, a_y, a_dydt
       type(c_ptr) :: gsl_odeiv2_driver_alloc_standard_new
     end function gsl_odeiv2_driver_alloc_standard_new
     function gsl_odeiv2_driver_alloc_scaled_new(sys, t, hstart, epsabs, epsrel, &
          a_y, a_dydt, scale_abs) bind(c)
       import
       type(c_ptr), value :: sys, t
       real(c_double), value :: hstart, epsabs, epsrel, a_y, a_dydt
       real(c_double) :: scale_abs(*)
       type(c_ptr) :: gsl_odeiv2_driver_alloc_scaled_new
     end function gsl_odeiv2_driver_alloc_scaled_new
     function gsl_odeiv2_driver_set_hmin(d, hmin) bind(c)
       import
       type(c_ptr), value :: d
       real(c_double), value :: hmin
       integer(c_int) :: gsl_odeiv2_driver_set_hmin
     end function gsl_odeiv2_driver_set_hmin
     function gsl_odeiv2_driver_set_hmax(d, hmax) bind(c)
       import
       type(c_ptr), value :: d
       real(c_double), value :: hmax
       integer(c_int) :: gsl_odeiv2_driver_set_hmax
     end function gsl_odeiv2_driver_set_hmax
     function gsl_odeiv2_driver_set_nmax(d, nmax) bind(c)
       import
       type(c_ptr), value :: d
       integer(c_long), value :: nmax
       integer(c_int) :: gsl_odeiv2_driver_set_nmax
     end function gsl_odeiv2_driver_set_nmax
     function gsl_odeiv2_driver_apply(d, t, t1, y) bind(c)
       import
       type(c_ptr), value :: d
       real(c_double) :: t
       real(c_double), value :: t1
      real(c_double) :: y(*)
       integer(c_int) :: gsl_odeiv2_driver_apply
     end function gsl_odeiv2_driver_apply
     function gsl_odeiv2_driver_apply_fixed_step(d, t, h, n, y) bind(c)
       import
       type(c_ptr), value :: d
       real(c_double) :: t
       real(c_double), value :: h
       integer(c_long), value :: n
       real(c_double) :: y(*)
       integer(c_int) :: gsl_odeiv2_driver_apply_fixed_step
     end function gsl_odeiv2_driver_apply_fixed_step
     function gsl_odeiv2_driver_reset(d) bind(c)
       import
       type(c_ptr), value :: d
       integer(c_int) :: gsl_odeiv2_driver_reset
     end function gsl_odeiv2_driver_reset
     subroutine gsl_odeiv2_driver_free(d) bind(c)
       import
       type(c_ptr), value :: d
     end subroutine gsl_odeiv2_driver_free
     function gsl_odeiv2_driver_reset_hstart(d, hstart) bind(c)
       import
       type(c_ptr), value :: d
       real(c_double), value :: hstart
       integer(c_int) :: gsl_odeiv2_driver_reset_hstart
     end function gsl_odeiv2_driver_reset_hstart  
  end interface
contains
!> Constructor for an ODE system object
!> \param func - interface for a double precision vector valued function with
!> derivatives and a parameter of arbitrary type
!> \param dimension - number of components of the vector function
!> \param params - parameter of arbitrary type
!> \param jacobian - interface for the jacobian of func
!> \result ODE system object.
  function fgsl_odeiv2_system_init(func, dimension, params, jacobian)
    optional :: jacobian
    interface
       function func(t, y, dydt, params) bind(c)
         use, intrinsic :: iso_c_binding
         real(c_double), value :: t
         type(c_ptr), value :: y, dydt, params
         integer(c_int) :: func
       end function func
       function jacobian(t, y, dfdy, dfdt, params) bind(c)
         use, intrinsic :: iso_c_binding
         real(c_double), value :: t
         type(c_ptr), value :: y, dfdy, dfdt, params
         integer(c_int) :: jacobian
       end function jacobian
    end interface
    integer(fgsl_size_t) :: dimension
    type(c_ptr), intent(in), optional :: params
    type(fgsl_odeiv2_system) :: fgsl_odeiv2_system_init
!
    type(c_funptr) :: func_loc
    type(c_funptr) :: jacobian_loc
    type(c_ptr) :: params_loc
    func_loc = c_funloc(func)
    params_loc = c_null_ptr
    jacobian_loc = c_null_funptr
    if (present(jacobian)) jacobian_loc = c_funloc(jacobian)
    if (present(params)) params_loc = params
    fgsl_odeiv2_system_init%gsl_odeiv2_system = &
         fgsl_odeiv2_system_cinit(func_loc, dimension, &
         params_loc, jacobian_loc)
  end function fgsl_odeiv2_system_init
  subroutine fgsl_odeiv2_system_free(system)
    type(fgsl_odeiv2_system), intent(inout) :: system
    call fgsl_odeiv2_system_cfree(system%gsl_odeiv2_system)
  end subroutine fgsl_odeiv2_system_free
  function fgsl_odeiv2_step_alloc(t, dim)
    type(fgsl_odeiv2_step_type), intent(in) :: t
    integer(fgsl_size_t), intent(in) :: dim
    type(fgsl_odeiv2_step) :: fgsl_odeiv2_step_alloc
!
    type(c_ptr) :: step_type
    step_type = fgsl_aux_odeiv2_step_alloc(t%which)
    if (c_associated(step_type)) then
       fgsl_odeiv2_step_alloc%gsl_odeiv2_step = gsl_odeiv2_step_alloc(step_type, dim)
    else
       fgsl_odeiv2_step_alloc%gsl_odeiv2_step = c_null_ptr
    end if
  end function fgsl_odeiv2_step_alloc
  function fgsl_odeiv2_step_reset(s)
    type(fgsl_odeiv2_step), intent(inout) :: s
    integer(fgsl_int) :: fgsl_odeiv2_step_reset
    fgsl_odeiv2_step_reset = gsl_odeiv2_step_reset(s%gsl_odeiv2_step)
  end function fgsl_odeiv2_step_reset
  subroutine fgsl_odeiv2_step_free(s)
    type(fgsl_odeiv2_step), intent(inout) :: s
    call gsl_odeiv2_step_free(s%gsl_odeiv2_step)
  end subroutine fgsl_odeiv2_step_free
  function fgsl_odeiv2_step_name (s)
    type(fgsl_odeiv2_step), intent(in) :: s
    character(kind=fgsl_char, len=fgsl_strmax) :: fgsl_odeiv2_step_name
    type(c_ptr) :: name
    name = gsl_odeiv2_step_name(s%gsl_odeiv2_step)
    fgsl_odeiv2_step_name = fgsl_name(name)
  end function fgsl_odeiv2_step_name
  function fgsl_odeiv2_step_order(s)
    type(fgsl_odeiv2_step), intent(in) :: s
    integer(fgsl_int) :: fgsl_odeiv2_step_order
    fgsl_odeiv2_step_order = gsl_odeiv2_step_order(s%gsl_odeiv2_step)
  end function fgsl_odeiv2_step_order
  function fgsl_odeiv2_step_set_driver(s,d)
    type(fgsl_odeiv2_step) :: s
    type(fgsl_odeiv2_driver), intent(in) :: d
    integer(c_int) :: fgsl_odeiv2_step_set_driver
    fgsl_odeiv2_step_set_driver = gsl_odeiv2_step_set_driver( &
         s%gsl_odeiv2_step, d%gsl_odeiv2_driver)
  end function fgsl_odeiv2_step_set_driver
  function fgsl_odeiv2_step_apply(s, t, h, y, yerr, dydt_in, dydt_out, dydt)
    type(fgsl_odeiv2_step), intent(in) :: s
    real(fgsl_double), intent(in) :: t, h
    real(fgsl_double), intent(inout), target, contiguous :: y(:), yerr(:), dydt_out(:)
    real(fgsl_double), intent(in), target, contiguous :: dydt_in(:)
    type(fgsl_odeiv2_system), intent(in) :: dydt
    integer(fgsl_int) :: fgsl_odeiv2_step_apply
    fgsl_odeiv2_step_apply = gsl_odeiv2_step_apply(s%gsl_odeiv2_step, t, h, &
    c_loc(y), c_loc(yerr), c_loc(dydt_in), c_loc(dydt_out), dydt%gsl_odeiv2_system)
  end function fgsl_odeiv2_step_apply
  function fgsl_odeiv2_control_standard_new(eps_abs, eps_rel, a_y, a_dydt)
    real(fgsl_double), intent(in) :: eps_abs, eps_rel, a_y, a_dydt
    type(fgsl_odeiv2_control) :: fgsl_odeiv2_control_standard_new
    fgsl_odeiv2_control_standard_new%gsl_odeiv2_control = &
         gsl_odeiv2_control_standard_new(eps_abs, eps_rel, a_y, a_dydt)
  end function fgsl_odeiv2_control_standard_new
  function fgsl_odeiv2_control_y_new(eps_abs, eps_rel)
    real(fgsl_double), intent(in) :: eps_abs, eps_rel
    type(fgsl_odeiv2_control) :: fgsl_odeiv2_control_y_new
    fgsl_odeiv2_control_y_new%gsl_odeiv2_control = &
         gsl_odeiv2_control_y_new(eps_abs, eps_rel)
  end function fgsl_odeiv2_control_y_new
  function fgsl_odeiv2_control_yp_new(eps_abs, eps_rel)
    real(fgsl_double), intent(in) :: eps_abs, eps_rel
    type(fgsl_odeiv2_control) :: fgsl_odeiv2_control_yp_new
    fgsl_odeiv2_control_yp_new%gsl_odeiv2_control = &
         gsl_odeiv2_control_yp_new(eps_abs, eps_rel)
  end function fgsl_odeiv2_control_yp_new
  function fgsl_odeiv2_control_scaled_new(eps_abs, eps_rel, a_y, a_dydt, scale_abs)
    real(fgsl_double), intent(in) :: eps_abs, eps_rel, a_y, a_dydt
    real(fgsl_double), intent(in), target, contiguous :: scale_abs(:)
    type(fgsl_odeiv2_control) :: fgsl_odeiv2_control_scaled_new
    fgsl_odeiv2_control_scaled_new%gsl_odeiv2_control = &
         gsl_odeiv2_control_scaled_new(eps_abs, eps_rel, a_y, a_dydt, &
         c_loc(scale_abs), size(scale_abs, kind=fgsl_size_t))
  end function fgsl_odeiv2_control_scaled_new
!> Note: use of fgsl_odeiv2_control_alloc requires an initializer for the t object
!> written in C
  function fgsl_odeiv2_control_alloc(t)
    type(fgsl_odeiv2_control_type), intent(in) :: t
    type(fgsl_odeiv2_control) :: fgsl_odeiv2_control_alloc
    fgsl_odeiv2_control_alloc%gsl_odeiv2_control = gsl_odeiv2_control_alloc( &
         t%gsl_odeiv2_control_type)
  end function fgsl_odeiv2_control_alloc
  function fgsl_odeiv2_control_init(c, eps_abs, eps_rel, a_y, a_dydt)
    type(fgsl_odeiv2_control), intent(in) :: c
    real(fgsl_double), intent(in) :: eps_abs, eps_rel, a_y, a_dydt
    integer(fgsl_int) :: fgsl_odeiv2_control_init
    fgsl_odeiv2_control_init = &
         gsl_odeiv2_control_init(c%gsl_odeiv2_control, eps_abs, eps_rel, a_y, a_dydt)
  end function fgsl_odeiv2_control_init
  subroutine fgsl_odeiv2_control_free(c)
    type(fgsl_odeiv2_control), intent(inout) :: c
    call gsl_odeiv2_control_free(c%gsl_odeiv2_control)
  end subroutine fgsl_odeiv2_control_free
  function fgsl_odeiv2_control_status(s)
    type(fgsl_odeiv2_control), intent(in) :: s
    logical :: fgsl_odeiv2_control_status
    fgsl_odeiv2_control_status = .true.
    if (.not. c_associated(s%gsl_odeiv2_control)) &
         fgsl_odeiv2_control_status = .false.
  end function fgsl_odeiv2_control_status
  function fgsl_odeiv2_control_hadjust(c, s, y, yerr, dydt, h)
    type(fgsl_odeiv2_control), intent(in) :: c
    type(fgsl_odeiv2_step), intent(in) :: s
    real(fgsl_double), intent(in), target, contiguous :: y(:), yerr(:), dydt(:)
    real(fgsl_double), intent(out) :: h
    integer(fgsl_int) :: fgsl_odeiv2_control_hadjust
    fgsl_odeiv2_control_hadjust = gsl_odeiv2_control_hadjust( &
         c%gsl_odeiv2_control, s%gsl_odeiv2_step, c_loc(y), c_loc(yerr), c_loc(dydt), h)
  end function fgsl_odeiv2_control_hadjust
  function fgsl_odeiv2_control_name (c)
    type(fgsl_odeiv2_control), intent(in) :: c
    character(kind=fgsl_char, len=fgsl_strmax) :: fgsl_odeiv2_control_name
!
    type(c_ptr) :: name
    name = gsl_odeiv2_control_name(c%gsl_odeiv2_control)
    fgsl_odeiv2_control_name = fgsl_name(name)
  end function fgsl_odeiv2_control_name
  function fgsl_odeiv2_control_errlevel(c, y, dydt, h, ind, errlev)
    type(fgsl_odeiv2_control), value :: c
    real(fgsl_double), intent(in) :: y, dydt, h
    integer(fgsl_size_t), intent(in) :: ind
    real(fgsl_double), intent(inout) :: errlev
    integer(fgsl_int) :: fgsl_odeiv2_control_errlevel
    fgsl_odeiv2_control_errlevel = gsl_odeiv2_control_errlevel(c%gsl_odeiv2_control, &
         y, dydt, h, ind, errlev)
  end function fgsl_odeiv2_control_errlevel
  function fgsl_odeiv2_control_set_driver(c, d)
    type(fgsl_odeiv2_control), intent(inout) :: c
    type(fgsl_odeiv2_driver), intent(in) :: d
    integer(fgsl_int) :: fgsl_odeiv2_control_set_driver
    fgsl_odeiv2_control_set_driver = gsl_odeiv2_control_set_driver( &
         c%gsl_odeiv2_control, d%gsl_odeiv2_driver)
  end function fgsl_odeiv2_control_set_driver
  function fgsl_odeiv2_evolve_alloc(dim)
    integer(fgsl_size_t), intent(in) :: dim
    type(fgsl_odeiv2_evolve) :: fgsl_odeiv2_evolve_alloc
    fgsl_odeiv2_evolve_alloc%gsl_odeiv2_evolve = &
         gsl_odeiv2_evolve_alloc(dim)
  end function fgsl_odeiv2_evolve_alloc
  function fgsl_odeiv2_evolve_apply(e, con, step, dydt, t, t1, h, y)
    type(fgsl_odeiv2_evolve), intent(inout) :: e
    type(fgsl_odeiv2_control), intent(inout) :: con
    type(fgsl_odeiv2_step), intent(inout) :: step
    type(fgsl_odeiv2_system), intent(in) :: dydt
    real(fgsl_double), intent(inout) :: t, h
    real(fgsl_double), intent(inout), target, contiguous :: y(:)
    real(fgsl_double), intent(in) :: t1
    integer(fgsl_int) :: fgsl_odeiv2_evolve_apply
    fgsl_odeiv2_evolve_apply = gsl_odeiv2_evolve_apply(e%gsl_odeiv2_evolve, &
         con%gsl_odeiv2_control, step%gsl_odeiv2_step, dydt%gsl_odeiv2_system, &
         t, t1, h, c_loc(y))
  end function fgsl_odeiv2_evolve_apply
  function fgsl_odeiv2_evolve_apply_fixed_step(e, con, step, dydt, t, h0, y)
    type(fgsl_odeiv2_evolve), intent(inout) :: e
    type(fgsl_odeiv2_control), intent(inout) :: con
    type(fgsl_odeiv2_step), intent(inout) :: step
    type(fgsl_odeiv2_system), intent(in) :: dydt
    real(fgsl_double), intent(inout) :: t
    real(fgsl_double), intent(in) :: h0
    real(fgsl_double), intent(inout), target, contiguous :: y(:)
    integer(fgsl_int) :: fgsl_odeiv2_evolve_apply_fixed_step
    fgsl_odeiv2_evolve_apply_fixed_step = gsl_odeiv2_evolve_apply_fixed_step( &
         e%gsl_odeiv2_evolve, con%gsl_odeiv2_control, step%gsl_odeiv2_step, &
         dydt%gsl_odeiv2_system, t, h0, c_loc(y))
  end function fgsl_odeiv2_evolve_apply_fixed_step
  function fgsl_odeiv2_evolve_reset(s)
    type(fgsl_odeiv2_evolve), intent(inout) :: s
    integer(c_int) :: fgsl_odeiv2_evolve_reset
    fgsl_odeiv2_evolve_reset = gsl_odeiv2_evolve_reset(s%gsl_odeiv2_evolve)
  end function fgsl_odeiv2_evolve_reset
  subroutine fgsl_odeiv2_evolve_free(s)
    type(fgsl_odeiv2_evolve), intent(inout) :: s
    call gsl_odeiv2_evolve_free(s%gsl_odeiv2_evolve)
  end subroutine fgsl_odeiv2_evolve_free
  function fgsl_odeiv2_evolve_status(s)
    type(fgsl_odeiv2_evolve), intent(in) :: s
    logical :: fgsl_odeiv2_evolve_status
    fgsl_odeiv2_evolve_status = .true.
    if (.not. c_associated(s%gsl_odeiv2_evolve)) &
         fgsl_odeiv2_evolve_status = .false.
  end function fgsl_odeiv2_evolve_status
  function fgsl_odeiv2_step_status(s)
    type(fgsl_odeiv2_step), intent(in) :: s
    logical :: fgsl_odeiv2_step_status
    fgsl_odeiv2_step_status = .true.
    if (.not. c_associated(s%gsl_odeiv2_step)) &
         fgsl_odeiv2_step_status = .false.
  end function fgsl_odeiv2_step_status
  function fgsl_odeiv2_system_status(s)
    type(fgsl_odeiv2_system), intent(in) :: s
    logical :: fgsl_odeiv2_system_status
    fgsl_odeiv2_system_status = .true.
    if (.not. c_associated(s%gsl_odeiv2_system)) &
         fgsl_odeiv2_system_status = .false.
  end function fgsl_odeiv2_system_status
  function fgsl_odeiv2_evolve_set_driver(c, d)
    type(fgsl_odeiv2_evolve), intent(inout) :: c
    type(fgsl_odeiv2_driver), intent(in) :: d
    integer(fgsl_int) :: fgsl_odeiv2_evolve_set_driver
    fgsl_odeiv2_evolve_set_driver = gsl_odeiv2_evolve_set_driver( &
         c%gsl_odeiv2_evolve, d%gsl_odeiv2_driver)
  end function fgsl_odeiv2_evolve_set_driver
  function fgsl_odeiv2_driver_alloc_y_new(sys, t, hstart, epsabs, epsrel)
    type(fgsl_odeiv2_system), intent(in) :: sys
    type(fgsl_odeiv2_step_type), intent(in) :: t
    real(c_double), intent(in) :: hstart, epsabs, epsrel
    type(fgsl_odeiv2_driver) :: fgsl_odeiv2_driver_alloc_y_new
    type(c_ptr) :: step_type
    step_type = fgsl_aux_odeiv2_step_alloc(t%which)
    fgsl_odeiv2_driver_alloc_y_new%gsl_odeiv2_driver = &
         gsl_odeiv2_driver_alloc_y_new(sys%gsl_odeiv2_system, &
         step_type, hstart, epsabs, epsrel)
  end function fgsl_odeiv2_driver_alloc_y_new
  function fgsl_odeiv2_driver_alloc_yp_new(sys, t, hstart, epsabs, epsrel)
    type(fgsl_odeiv2_system), intent(in) :: sys
    type(fgsl_odeiv2_step_type), intent(in) :: t
    real(c_double), intent(in) :: hstart, epsabs, epsrel
    type(fgsl_odeiv2_driver) :: fgsl_odeiv2_driver_alloc_yp_new
    type(c_ptr) :: step_type
    step_type = fgsl_aux_odeiv2_step_alloc(t%which)
    fgsl_odeiv2_driver_alloc_yp_new%gsl_odeiv2_driver = &
         gsl_odeiv2_driver_alloc_yp_new(sys%gsl_odeiv2_system, &
         step_type, hstart, epsabs, epsrel)
  end function fgsl_odeiv2_driver_alloc_yp_new
  function fgsl_odeiv2_driver_alloc_standard_new(sys, t, hstart, epsabs, epsrel, &
       a_y, a_dydt)
    type(fgsl_odeiv2_system), intent(in) :: sys
    type(fgsl_odeiv2_step_type), intent(in) :: t
    real(c_double), intent(in) :: hstart, epsabs, epsrel, a_y, a_dydt
    type(fgsl_odeiv2_driver) :: fgsl_odeiv2_driver_alloc_standard_new
    type(c_ptr) :: step_type
    step_type = fgsl_aux_odeiv2_step_alloc(t%which)
    fgsl_odeiv2_driver_alloc_standard_new%gsl_odeiv2_driver = &
         gsl_odeiv2_driver_alloc_standard_new(sys%gsl_odeiv2_system, &
         step_type, hstart, epsabs, epsrel, a_y, a_dydt)
  end function fgsl_odeiv2_driver_alloc_standard_new
  function fgsl_odeiv2_driver_alloc_scaled_new(sys, t, hstart, epsabs, epsrel, &
       a_y, a_dydt, scale_abs)
    type(fgsl_odeiv2_system), intent(in) :: sys
    type(fgsl_odeiv2_step_type), intent(in) :: t
    real(c_double), intent(in) :: hstart, epsabs, epsrel, a_y, a_dydt
    real(c_double) :: scale_abs(:)
    type(fgsl_odeiv2_driver) :: fgsl_odeiv2_driver_alloc_scaled_new
    type(c_ptr) :: step_type
    step_type = fgsl_aux_odeiv2_step_alloc(t%which)
    fgsl_odeiv2_driver_alloc_scaled_new%gsl_odeiv2_driver = &
         gsl_odeiv2_driver_alloc_scaled_new(sys%gsl_odeiv2_system, &
         step_type, hstart, epsabs, epsrel, a_y, a_dydt, scale_abs)
  end function fgsl_odeiv2_driver_alloc_scaled_new
  function fgsl_odeiv2_driver_set_hmin(d, hmin)
    type(fgsl_odeiv2_driver), intent(inout) :: d
    real(fgsl_double) :: hmin
    integer(fgsl_int) :: fgsl_odeiv2_driver_set_hmin
    fgsl_odeiv2_driver_set_hmin = gsl_odeiv2_driver_set_hmin( &
         d%gsl_odeiv2_driver, hmin)
  end function fgsl_odeiv2_driver_set_hmin
  function fgsl_odeiv2_driver_set_hmax(d, hmax)
    type(fgsl_odeiv2_driver), intent(inout) :: d
    real(fgsl_double) :: hmax
    integer(fgsl_int) :: fgsl_odeiv2_driver_set_hmax
    fgsl_odeiv2_driver_set_hmax = gsl_odeiv2_driver_set_hmax( &
         d%gsl_odeiv2_driver, hmax)
  end function fgsl_odeiv2_driver_set_hmax
  function fgsl_odeiv2_driver_set_nmax(d, nmax)
    type(fgsl_odeiv2_driver), intent(inout) :: d
    integer(fgsl_long) :: nmax
    integer(fgsl_int) :: fgsl_odeiv2_driver_set_nmax
    fgsl_odeiv2_driver_set_nmax = gsl_odeiv2_driver_set_nmax( &
         d%gsl_odeiv2_driver, nmax)
  end function fgsl_odeiv2_driver_set_nmax
  function fgsl_odeiv2_driver_apply(d, t, t1, y)
    type(fgsl_odeiv2_driver), intent(inout) :: d
    real(fgsl_double), intent(inout) :: t
    real(fgsl_double), intent(in) :: t1
    real(fgsl_double), intent(inout) :: y(:)
    integer(fgsl_int) :: fgsl_odeiv2_driver_apply
    fgsl_odeiv2_driver_apply = gsl_odeiv2_driver_apply( &
         d%gsl_odeiv2_driver, t, t1, y)
  end function fgsl_odeiv2_driver_apply
  function fgsl_odeiv2_driver_apply_fixed_step(d, t, h, n, y)
    type(fgsl_odeiv2_driver), intent(inout) :: d
    real(fgsl_double), intent(inout) :: t
    real(fgsl_double), intent(in) :: h
    integer(fgsl_long), intent(in) :: n
    real(fgsl_double), intent(inout) :: y(:)
    integer(fgsl_int) :: fgsl_odeiv2_driver_apply_fixed_step
    fgsl_odeiv2_driver_apply_fixed_step = gsl_odeiv2_driver_apply_fixed_step( &
         d%gsl_odeiv2_driver, t, h, n, y)
  end function fgsl_odeiv2_driver_apply_fixed_step
  function fgsl_odeiv2_driver_reset(d)
    type(fgsl_odeiv2_driver), intent(inout) :: d
    integer(fgsl_int) :: fgsl_odeiv2_driver_reset
    fgsl_odeiv2_driver_reset = gsl_odeiv2_driver_reset(d%gsl_odeiv2_driver)
  end function fgsl_odeiv2_driver_reset
  subroutine fgsl_odeiv2_driver_free(d)
    type(fgsl_odeiv2_driver), intent(inout) :: d
    call gsl_odeiv2_driver_free(d%gsl_odeiv2_driver)
  end subroutine fgsl_odeiv2_driver_free
  function fgsl_odeiv2_driver_status(s)
    type(fgsl_odeiv2_driver), intent(in) :: s
    logical :: fgsl_odeiv2_driver_status
    fgsl_odeiv2_driver_status = .true.
    if (.not. c_associated(s%gsl_odeiv2_driver)) &
         fgsl_odeiv2_driver_status = .false.
  end function fgsl_odeiv2_driver_status
  function fgsl_odeiv2_driver_reset_hstart(d, hstart)
    type(fgsl_odeiv2_driver), intent(inout) :: d
    real(fgsl_double), intent(in) :: hstart
    integer(fgsl_int) :: fgsl_odeiv2_driver_reset_hstart
    fgsl_odeiv2_driver_reset_hstart = gsl_odeiv2_driver_reset_hstart(&
    d%gsl_odeiv2_driver, hstart)
  end function fgsl_odeiv2_driver_reset_hstart
end module fgsl_odeiv2
