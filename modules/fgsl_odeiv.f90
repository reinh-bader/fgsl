module fgsl_odeiv
  !> Ordinary Differential Equations
  !> Note: this module contains the legacy interface
  use fgsl_base

  implicit none
  
  private :: gsl_odeiv_step_alloc, fgsl_aux_odeiv_step_alloc, &
    fgsl_odeiv_system_cinit, fgsl_odeiv_system_cfree, &
    gsl_odeiv_step_reset, gsl_odeiv_step_free, gsl_odeiv_step_name, &
    gsl_odeiv_step_order, gsl_odeiv_step_apply, gsl_odeiv_control_standard_new, &
    gsl_odeiv_control_y_new, gsl_odeiv_control_yp_new, gsl_odeiv_control_scaled_new, &
    gsl_odeiv_control_alloc, gsl_odeiv_control_init, gsl_odeiv_control_free, &
    gsl_odeiv_control_hadjust, gsl_odeiv_control_name, gsl_odeiv_evolve_alloc, &
    gsl_odeiv_evolve_apply, gsl_odeiv_evolve_reset, gsl_odeiv_evolve_free
  
  !
  !> Types
  type, public :: fgsl_odeiv_system
     private
     type(c_ptr) :: gsl_odeiv_system = c_null_ptr
  end type fgsl_odeiv_system
  type, public :: fgsl_odeiv_step_type
     private
     integer(c_int) :: which = 0
  end type fgsl_odeiv_step_type
  type(fgsl_odeiv_step_type), parameter, public :: &
       fgsl_odeiv_step_rk2 = fgsl_odeiv_step_type(1), &
       fgsl_odeiv_step_rk4 = fgsl_odeiv_step_type(2), &
       fgsl_odeiv_step_rkf45 = fgsl_odeiv_step_type(3), &
       fgsl_odeiv_step_rkck = fgsl_odeiv_step_type(4), &
       fgsl_odeiv_step_rk8pd = fgsl_odeiv_step_type(5), &
       fgsl_odeiv_step_rk2imp = fgsl_odeiv_step_type(6), &
       fgsl_odeiv_step_rk2simp = fgsl_odeiv_step_type(7), &
       fgsl_odeiv_step_rk4imp = fgsl_odeiv_step_type(8), &
       fgsl_odeiv_step_bsimp = fgsl_odeiv_step_type(9), &
       fgsl_odeiv_step_gear1 = fgsl_odeiv_step_type(10), &
       fgsl_odeiv_step_gear2 = fgsl_odeiv_step_type(11)
  type, public :: fgsl_odeiv_step
     type(c_ptr) :: gsl_odeiv_step = c_null_ptr
  end type fgsl_odeiv_step
  type, public :: fgsl_odeiv_control
     type(c_ptr) :: gsl_odeiv_control = c_null_ptr
  end type fgsl_odeiv_control
  type, public :: fgsl_odeiv_control_type
     type(c_ptr) :: gsl_odeiv_control_type = c_null_ptr
  end type fgsl_odeiv_control_type
  integer(fgsl_int), parameter, public :: fgsl_odeiv_hadj_inc = 1
  integer(fgsl_int), parameter, public :: fgsl_odeiv_hadj_nil = 0
  integer(fgsl_int), parameter, public :: fgsl_odeiv_hadj_dec = -1
  type, public :: fgsl_odeiv_evolve
     type(c_ptr) :: gsl_odeiv_evolve
  end type fgsl_odeiv_evolve
  !
  !> Generics
  interface fgsl_well_defined
     module procedure fgsl_odeiv_control_status
     module procedure fgsl_odeiv_evolve_status
     module procedure fgsl_odeiv_step_status
     module procedure fgsl_odeiv_system_status
  end interface fgsl_well_defined
  !
  !> C interfaces
  interface
    function gsl_odeiv_step_alloc(t, dim) bind(c)
       import
       type(c_ptr), value :: t
       integer(c_size_t), value :: dim
       type(c_ptr) :: gsl_odeiv_step_alloc
     end function gsl_odeiv_step_alloc
     function fgsl_aux_odeiv_step_alloc(step_type) bind(c)
       import
       integer(c_int), value :: step_type
       type(c_ptr) :: fgsl_aux_odeiv_step_alloc
     end function fgsl_aux_odeiv_step_alloc
     function fgsl_odeiv_system_cinit(func, dimension, params, jacobian) bind(c)
       import
       type(c_funptr), value :: func
       integer(c_size_t), value :: dimension
       type(c_ptr), value :: params
       type(c_funptr), value :: jacobian
       type(c_ptr) :: fgsl_odeiv_system_cinit
     end function fgsl_odeiv_system_cinit
     subroutine fgsl_odeiv_system_cfree(system) bind(c)
       import
       type(c_ptr), value :: system
     end subroutine fgsl_odeiv_system_cfree
     function gsl_odeiv_step_reset(s) bind(c)
       import
       type(c_ptr), value :: s
       integer(c_int) :: gsl_odeiv_step_reset
     end function gsl_odeiv_step_reset
     subroutine gsl_odeiv_step_free(s) bind(c)
       import
       type(c_ptr), value :: s
     end subroutine gsl_odeiv_step_free
     function gsl_odeiv_step_name (s) bind(c)
       import
       type(c_ptr), value :: s
       type(c_ptr) :: gsl_odeiv_step_name
     end function gsl_odeiv_step_name
     function gsl_odeiv_step_order(s) bind(c)
       import
       type(c_ptr), value :: s
       integer(c_int) :: gsl_odeiv_step_order
     end function gsl_odeiv_step_order
     function gsl_odeiv_step_apply(s, t, h, y, yerr, dydt_in, dydt_out, dydt) bind(c)
       import
       type(c_ptr), value :: s
       real(c_double), value :: t, h
       type(c_ptr), value :: y, yerr, dydt_in, dydt_out
       type(c_ptr), value :: dydt
       integer(c_int) :: gsl_odeiv_step_apply
     end function gsl_odeiv_step_apply
     function gsl_odeiv_control_standard_new(eps_abs, eps_rel, a_y, a_dydt) bind(c)
       import
       real(c_double), value :: eps_abs, eps_rel, a_y, a_dydt
       type(c_ptr) :: gsl_odeiv_control_standard_new
     end function gsl_odeiv_control_standard_new
     function gsl_odeiv_control_y_new(eps_abs, eps_rel) bind(c)
       import
       real(c_double), value :: eps_abs, eps_rel
       type(c_ptr) :: gsl_odeiv_control_y_new
     end function gsl_odeiv_control_y_new
     function gsl_odeiv_control_yp_new(eps_abs, eps_rel) bind(c)
       import
       real(c_double), value :: eps_abs, eps_rel
       type(c_ptr) :: gsl_odeiv_control_yp_new
     end function gsl_odeiv_control_yp_new
     function gsl_odeiv_control_scaled_new(eps_abs, eps_rel, a_y, a_dydt, &
          scale_abs, dim) bind(c)
       import
       real(c_double), value :: eps_abs, eps_rel, a_y, a_dydt
       type(c_ptr), value :: scale_abs
       integer(c_size_t), value :: dim
       type(c_ptr) :: gsl_odeiv_control_scaled_new
     end function gsl_odeiv_control_scaled_new
     function gsl_odeiv_control_alloc(t) bind(c)
       import
       type(c_ptr), value :: t
       type(c_ptr) :: gsl_odeiv_control_alloc
     end function gsl_odeiv_control_alloc
     function gsl_odeiv_control_init(c, eps_abs, eps_rel, a_y, a_dydt) bind(c)
       import
       type(c_ptr), value :: c
       real(c_double), value :: eps_abs, eps_rel, a_y, a_dydt
       integer(c_int) :: gsl_odeiv_control_init
     end function gsl_odeiv_control_init
     subroutine gsl_odeiv_control_free(c) bind(c)
       import
       type(c_ptr), value :: c
     end subroutine gsl_odeiv_control_free
     function gsl_odeiv_control_hadjust(c, s, y0, yerr, dydt, h) bind(c)
       import
       type(c_ptr), value :: c, s
       type(c_ptr), value :: y0, yerr, dydt
       type(c_ptr), value :: h
       integer(c_int) :: gsl_odeiv_control_hadjust
     end function gsl_odeiv_control_hadjust
     function gsl_odeiv_control_name (c) bind(c)
       import
       type(c_ptr), value :: c
       type(c_ptr) :: gsl_odeiv_control_name
     end function gsl_odeiv_control_name
     function gsl_odeiv_evolve_alloc(dim) bind(c)
       import
       integer(c_size_t), value :: dim
       type(c_ptr) gsl_odeiv_evolve_alloc
     end function gsl_odeiv_evolve_alloc
     function gsl_odeiv_evolve_apply(e, con, step, dydt, t, t1, h, y) bind(c)
       import
       type(c_ptr), value :: e, con, step, dydt
       type(c_ptr), value :: y
       real(c_double) :: t, h
       real(c_double), value :: t1
       integer(c_int) :: gsl_odeiv_evolve_apply
     end function gsl_odeiv_evolve_apply
     function gsl_odeiv_evolve_reset(s) bind(c)
       import
       type(c_ptr), value :: s
       integer(c_int) :: gsl_odeiv_evolve_reset
     end function gsl_odeiv_evolve_reset
     subroutine gsl_odeiv_evolve_free(s) bind(c)
       import
       type(c_ptr), value :: s
     end subroutine gsl_odeiv_evolve_free
   end interface
contains
!> Constructor for an ODE system object
!> \param func - interface for a double precision vector valued function with
!> derivatives and a parameter of arbitrary type
!> \param dimension - number of components of the vector function
!> \param params - parameter of arbitrary type
!> \param jacobian - interface for the jacobian of func
!> \result ODE system object.
  function fgsl_odeiv_system_init(func, dimension, params, jacobian)
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
    type(fgsl_odeiv_system) :: fgsl_odeiv_system_init
!
    type(c_funptr) :: func_loc
    type(c_funptr) :: jacobian_loc
    type(c_ptr) :: params_loc

    func_loc = c_funloc(func)
    params_loc = c_null_ptr
    jacobian_loc = c_null_funptr
    if (present(jacobian)) jacobian_loc = c_funloc(jacobian)
    if (present(params)) params_loc = params
    fgsl_odeiv_system_init%gsl_odeiv_system = &
         fgsl_odeiv_system_cinit(func_loc, dimension, &
         params_loc, jacobian_loc)
  end function fgsl_odeiv_system_init
  subroutine fgsl_odeiv_system_free(system)
    type(fgsl_odeiv_system), intent(inout) :: system
    call fgsl_odeiv_system_cfree(system%gsl_odeiv_system)
  end subroutine fgsl_odeiv_system_free
  function fgsl_odeiv_step_alloc(t, dim)
    type(fgsl_odeiv_step_type), intent(in) :: t
    integer(fgsl_size_t), intent(in) :: dim
    type(fgsl_odeiv_step) :: fgsl_odeiv_step_alloc
!
    type(c_ptr) :: step_type
    step_type = fgsl_aux_odeiv_step_alloc(t%which)
    if (c_associated(step_type)) then
       fgsl_odeiv_step_alloc%gsl_odeiv_step = gsl_odeiv_step_alloc(step_type, dim)
    else
       fgsl_odeiv_step_alloc%gsl_odeiv_step = c_null_ptr
    end if
  end function fgsl_odeiv_step_alloc
  function fgsl_odeiv_step_reset(s)
    type(fgsl_odeiv_step), intent(inout) :: s
    integer(fgsl_int) :: fgsl_odeiv_step_reset
    fgsl_odeiv_step_reset = gsl_odeiv_step_reset(s%gsl_odeiv_step)
  end function fgsl_odeiv_step_reset
  subroutine fgsl_odeiv_step_free(s)
    type(fgsl_odeiv_step), intent(inout) :: s
    call gsl_odeiv_step_free(s%gsl_odeiv_step)
  end subroutine fgsl_odeiv_step_free
  function fgsl_odeiv_step_name (s)
    type(fgsl_odeiv_step), intent(in) :: s
    character(kind=fgsl_char, len=fgsl_strmax) :: fgsl_odeiv_step_name
!
    type(c_ptr) :: name
    name = gsl_odeiv_step_name(s%gsl_odeiv_step)
    fgsl_odeiv_step_name = fgsl_name(name)
  end function fgsl_odeiv_step_name
  function fgsl_odeiv_step_order(s)
    type(fgsl_odeiv_step), intent(in) :: s
    integer(fgsl_int) :: fgsl_odeiv_step_order
    fgsl_odeiv_step_order = gsl_odeiv_step_order(s%gsl_odeiv_step)
  end function fgsl_odeiv_step_order
  function fgsl_odeiv_step_apply(s, t, h, y, yerr, dydt_in, dydt_out, dydt)
    type(fgsl_odeiv_step), intent(in) :: s
    real(fgsl_double), intent(in) :: t, h
    real(fgsl_double), intent(inout), target, contiguous :: y(:), yerr(:), dydt_in(:), dydt_out(:)
    type(fgsl_odeiv_system), intent(in) :: dydt
    integer(fgsl_int) :: fgsl_odeiv_step_apply
    fgsl_odeiv_step_apply = gsl_odeiv_step_apply(s%gsl_odeiv_step, t, h, &
    c_loc(y), c_loc(yerr), &
    c_loc(dydt_in), c_loc(dydt_out), dydt%gsl_odeiv_system)
  end function fgsl_odeiv_step_apply
  function fgsl_odeiv_control_standard_new(eps_abs, eps_rel, a_y, a_dydt)
    real(fgsl_double), intent(in) :: eps_abs, eps_rel, a_y, a_dydt
    type(fgsl_odeiv_control) :: fgsl_odeiv_control_standard_new
    fgsl_odeiv_control_standard_new%gsl_odeiv_control = &
         gsl_odeiv_control_standard_new(eps_abs, eps_rel, a_y, a_dydt)
  end function fgsl_odeiv_control_standard_new
  function fgsl_odeiv_control_y_new(eps_abs, eps_rel)
    real(fgsl_double), intent(in) :: eps_abs, eps_rel
    type(fgsl_odeiv_control) :: fgsl_odeiv_control_y_new
    fgsl_odeiv_control_y_new%gsl_odeiv_control = &
         gsl_odeiv_control_y_new(eps_abs, eps_rel)
  end function fgsl_odeiv_control_y_new
  function fgsl_odeiv_control_yp_new(eps_abs, eps_rel)
    real(fgsl_double), intent(in) :: eps_abs, eps_rel
    type(fgsl_odeiv_control) :: fgsl_odeiv_control_yp_new
    fgsl_odeiv_control_yp_new%gsl_odeiv_control = &
         gsl_odeiv_control_yp_new(eps_abs, eps_rel)
  end function fgsl_odeiv_control_yp_new
  function fgsl_odeiv_control_scaled_new(eps_abs, eps_rel, a_y, a_dydt, scale_abs)
    real(fgsl_double), intent(in) :: eps_abs, eps_rel, a_y, a_dydt
    real(fgsl_double), intent(in), target, contiguous :: scale_abs(:)
    type(fgsl_odeiv_control) :: fgsl_odeiv_control_scaled_new
    fgsl_odeiv_control_scaled_new%gsl_odeiv_control = &
         gsl_odeiv_control_scaled_new(eps_abs, eps_rel, a_y, a_dydt, &
         c_loc(scale_abs), size(scale_abs, kind=fgsl_size_t))
  end function fgsl_odeiv_control_scaled_new
!> Note: Use of fgsl_odeiv_control_alloc requires an initializer for the t object
!> written in C
  function fgsl_odeiv_control_alloc(t)
    type(fgsl_odeiv_control_type), intent(in) :: t
    type(fgsl_odeiv_control) :: fgsl_odeiv_control_alloc
    fgsl_odeiv_control_alloc%gsl_odeiv_control = gsl_odeiv_control_alloc( &
         t%gsl_odeiv_control_type)
  end function fgsl_odeiv_control_alloc
  function fgsl_odeiv_control_init(c, eps_abs, eps_rel, a_y, a_dydt)
    type(fgsl_odeiv_control), intent(in) :: c
    real(fgsl_double), intent(in) :: eps_abs, eps_rel, a_y, a_dydt
    integer(fgsl_int) :: fgsl_odeiv_control_init
    fgsl_odeiv_control_init = &
         gsl_odeiv_control_init(c%gsl_odeiv_control, eps_abs, eps_rel, a_y, a_dydt)
  end function fgsl_odeiv_control_init
  subroutine fgsl_odeiv_control_free(c)
    type(fgsl_odeiv_control), intent(inout) :: c
    call gsl_odeiv_control_free(c%gsl_odeiv_control)
  end subroutine fgsl_odeiv_control_free
  function fgsl_odeiv_control_hadjust(c, s, y0, yerr, dydt, h)
    type(fgsl_odeiv_control), intent(in) :: c
    type(fgsl_odeiv_step), intent(in) :: s
    real(fgsl_double), intent(in), target, contiguous :: y0(:), yerr(:), dydt(:)
    real(fgsl_double), intent(inout), target, contiguous :: h(:)
    integer(fgsl_int) :: fgsl_odeiv_control_hadjust
    fgsl_odeiv_control_hadjust = gsl_odeiv_control_hadjust(c%gsl_odeiv_control, s%gsl_odeiv_step, &
         c_loc(y0), c_loc(yerr), c_loc(dydt), c_loc(h))
  end function fgsl_odeiv_control_hadjust
  function fgsl_odeiv_control_name (c)
    type(fgsl_odeiv_control), intent(in) :: c
    character(kind=fgsl_char, len=fgsl_strmax) :: fgsl_odeiv_control_name
!
    type(c_ptr) :: name
    name = gsl_odeiv_control_name(c%gsl_odeiv_control)
    fgsl_odeiv_control_name = fgsl_name(name)
  end function fgsl_odeiv_control_name
  function fgsl_odeiv_evolve_alloc(dim)
    integer(fgsl_size_t), intent(in) :: dim
    type(fgsl_odeiv_evolve) :: fgsl_odeiv_evolve_alloc
    fgsl_odeiv_evolve_alloc%gsl_odeiv_evolve = &
         gsl_odeiv_evolve_alloc(dim)
  end function fgsl_odeiv_evolve_alloc
  function fgsl_odeiv_evolve_apply(e, con, step, dydt, t, t1, h, y)
    type(fgsl_odeiv_evolve), intent(inout) :: e
    type(fgsl_odeiv_control), intent(inout) :: con
    type(fgsl_odeiv_step), intent(inout) :: step
    type(fgsl_odeiv_system), intent(in) :: dydt
    real(fgsl_double), intent(inout) :: t, h
    real(fgsl_double), dimension(:), intent(inout), target, contiguous :: y
    real(fgsl_double), intent(in) :: t1
    integer(fgsl_int) :: fgsl_odeiv_evolve_apply
    fgsl_odeiv_evolve_apply = gsl_odeiv_evolve_apply(e%gsl_odeiv_evolve, &
         con%gsl_odeiv_control, step%gsl_odeiv_step, dydt%gsl_odeiv_system, &
         t, t1, h, c_loc(y))
  end function fgsl_odeiv_evolve_apply
  function fgsl_odeiv_evolve_reset(s)
    type(fgsl_odeiv_evolve), intent(inout) :: s
    integer(c_int) :: fgsl_odeiv_evolve_reset
    fgsl_odeiv_evolve_reset = gsl_odeiv_evolve_reset(s%gsl_odeiv_evolve)
  end function fgsl_odeiv_evolve_reset
  subroutine fgsl_odeiv_evolve_free(s)
    type(fgsl_odeiv_evolve), intent(inout) :: s
    call gsl_odeiv_evolve_free(s%gsl_odeiv_evolve)
  end subroutine fgsl_odeiv_evolve_free
  function fgsl_odeiv_evolve_status(s)
    type(fgsl_odeiv_evolve), intent(in) :: s
    logical :: fgsl_odeiv_evolve_status
    fgsl_odeiv_evolve_status = .true.
    if (.not. c_associated(s%gsl_odeiv_evolve)) &
         fgsl_odeiv_evolve_status = .false.
  end function fgsl_odeiv_evolve_status
  function fgsl_odeiv_control_status(s)
    type(fgsl_odeiv_control), intent(in) :: s
    logical :: fgsl_odeiv_control_status
    fgsl_odeiv_control_status = .true.
    if (.not. c_associated(s%gsl_odeiv_control)) &
         fgsl_odeiv_control_status = .false.
  end function fgsl_odeiv_control_status
  function fgsl_odeiv_step_status(s)
    type(fgsl_odeiv_step), intent(in) :: s
    logical :: fgsl_odeiv_step_status
    fgsl_odeiv_step_status = .true.
    if (.not. c_associated(s%gsl_odeiv_step)) &
         fgsl_odeiv_step_status = .false.
  end function fgsl_odeiv_step_status
  function fgsl_odeiv_system_status(s)
    type(fgsl_odeiv_system), intent(in) :: s
    logical :: fgsl_odeiv_system_status
    fgsl_odeiv_system_status = .true.
    if (.not. c_associated(s%gsl_odeiv_system)) &
         fgsl_odeiv_system_status = .false.
  end function fgsl_odeiv_system_status
end module fgsl_odeiv
  
