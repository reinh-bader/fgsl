module fgsl_siman
  !> Simulated Annealing
  !> Note: this module contains the routines for plain, vegas and miser
  use fgsl_base
  
  implicit none
  
  private :: gsl_siman_solve, fgsl_siman_params_t_status
  
  !
  ! Types
  type, bind(c) :: gsl_siman_params_t
     integer(c_int) :: n_tries, iters_fixed_t
     real(c_double) :: step_size, k, t_initial, mu_t, t_min
  end type gsl_siman_params_t
  type, public :: fgsl_siman_params_t
     private
     type(gsl_siman_params_t), pointer :: gsl_siman_params_t => null()
  end type fgsl_siman_params_t
  
  !
  !> Generics
  interface fgsl_well_defined
     module procedure fgsl_siman_params_t_status
  end interface fgsl_well_defined
  !
  !> C interfaces
  interface
	  subroutine gsl_siman_solve(rng, x0_p, ef, take_step, distance, &
	       print_position, copy_func, copy_constructor, destructor, &
	       element_size, params) bind(c)
	    import :: c_size_t, c_ptr, c_funptr, gsl_siman_params_t
	    type(c_funptr), value :: ef, take_step, distance, print_position, &
	         copy_func, copy_constructor, destructor
	    type(c_ptr), value :: rng, x0_p
	    integer(c_size_t), value :: element_size
	    type(gsl_siman_params_t), value :: params
	  end subroutine gsl_siman_solve
  end interface
contains
!> API
  subroutine fgsl_siman_params_init(params, n_tries, iters_fixed_t, &
       step_size, k, t_initial, mu_t, t_min)
    type(fgsl_siman_params_t), intent(inout) :: params
    integer(fgsl_int) :: n_tries, iters_fixed_t
    real(fgsl_double) :: step_size, k, t_initial, mu_t, t_min
    if (.not. associated(params%gsl_siman_params_t)) then
       allocate(params%gsl_siman_params_t)
    end if
    params%gsl_siman_params_t%n_tries = n_tries
    params%gsl_siman_params_t%iters_fixed_t = iters_fixed_t
    params%gsl_siman_params_t%step_size = step_size
    params%gsl_siman_params_t%k = k
    params%gsl_siman_params_t%t_initial = t_initial
    params%gsl_siman_params_t%mu_t = mu_t
    params%gsl_siman_params_t%t_min = t_min
  end subroutine fgsl_siman_params_init
  subroutine fgsl_siman_params_free(params)
    type(fgsl_siman_params_t), intent(inout) :: params
    if (associated(params%gsl_siman_params_t)) then
       deallocate(params%gsl_siman_params_t)
    end if
    params%gsl_siman_params_t => null()
  end subroutine fgsl_siman_params_free
  subroutine fgsl_siman_solve(rng, x0_p, ef, take_step, distance, &
       print_position, copy_func, copy_constructor, destructor, &
       element_size, params) 
    optional :: print_position, copy_func, copy_constructor, destructor, &
         element_size
    type(fgsl_rng), intent(in) :: rng
    type(c_ptr), intent(inout) :: x0_p
    interface
       function ef(xp) bind(c)
         use, intrinsic :: iso_c_binding
         type(c_ptr), value :: xp
         real(c_double) :: ef
       end function ef
       subroutine take_step(rng, xp, step_size) bind(c)
         use, intrinsic :: iso_c_binding
         type(c_ptr), value :: rng, xp
         real(c_double), value :: step_size
       end subroutine take_step
       function distance(xp, yp) bind(c)
         use, intrinsic :: iso_c_binding
         type(c_ptr), value :: xp, yp
         real(c_double) :: distance
       end function distance
       subroutine print_position(xp) bind(c)
         use, intrinsic :: iso_c_binding
         type(c_ptr), value :: xp
       end subroutine print_position
       subroutine copy_func(src, dest) bind(c)
         use, intrinsic :: iso_c_binding
         type(c_ptr), value :: src, dest
       end subroutine copy_func
       function copy_constructor(xp) bind(c)
         use, intrinsic :: iso_c_binding
         type(c_ptr), value :: xp
         type(c_ptr) :: copy_constructor
       end function copy_constructor
       subroutine destructor(xp) bind(c)
         use, intrinsic :: iso_c_binding
         type(c_ptr), value :: xp
       end subroutine destructor
    end interface
    integer(fgsl_size_t), intent(in) :: element_size
    type(fgsl_siman_params_t), intent(in) :: params
!
    type(c_funptr) :: p_ef, p_take_step, p_distance, p_print_position, &
         p_copy_func, p_copy_constructor, p_destructor
    p_ef = c_funloc(ef)
    p_take_step = c_funloc(take_step)
    p_distance = c_funloc(distance)
    if (present(print_position)) then
       p_print_position = c_funloc(print_position)
    else
       p_print_position = c_null_funptr
    end if
    if (present(copy_func) .and. present(copy_constructor) &
         .and. present(destructor)) then
       p_copy_func = c_funloc(copy_func)
       p_copy_constructor = c_funloc(copy_constructor)
       p_destructor = c_funloc(destructor)
       if (element_size /= 0) return
    else
       p_copy_func = c_null_funptr
       p_copy_constructor = c_null_funptr
       p_destructor = c_null_funptr
    end if
    if (associated(params%gsl_siman_params_t)) then
       call gsl_siman_solve(rng%gsl_rng, x0_p, p_ef, p_take_step, p_distance, &
            p_print_position, p_copy_func, p_copy_constructor, p_destructor, &
            element_size, params%gsl_siman_params_t)
    end if
  end subroutine fgsl_siman_solve
  function fgsl_siman_params_t_status(siman_params_t)
    type(fgsl_siman_params_t), intent(in) :: siman_params_t
    logical :: fgsl_siman_params_t_status
    fgsl_siman_params_t_status = .true.
    if (.not. associated(siman_params_t%gsl_siman_params_t)) &
         fgsl_siman_params_t_status = .false.
  end function fgsl_siman_params_t_status

 
end module
