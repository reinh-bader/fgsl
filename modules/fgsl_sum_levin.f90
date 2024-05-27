module fgsl_sum_levin
  !> Series Acceleration
  !> Note: This module maps the functions from gsl_sum.h
  use fgsl_base

  implicit none
  
  private :: gsl_sum_levin_u_alloc, gsl_sum_levin_u_free, &
    gsl_sum_levin_u_accel, gsl_sum_levin_utrunc_alloc, &
    gsl_sum_levin_utrunc_free, gsl_sum_levin_utrunc_accel
  
  !
  !> Types
  type, public :: fgsl_sum_levin_u_workspace
     private
     type(c_ptr) :: gsl_sum_levin_u_workspace = c_null_ptr
  end type fgsl_sum_levin_u_workspace
  type, public :: fgsl_sum_levin_utrunc_workspace
     private
     type(c_ptr) :: gsl_sum_levin_utrunc_workspace = c_null_ptr
  end type fgsl_sum_levin_utrunc_workspace
  !
  !> C interfaces
  interface
	  function gsl_sum_levin_u_alloc(n) bind(c)
	    import
	    integer(c_size_t), value :: n
	    type(c_ptr) :: gsl_sum_levin_u_alloc
	  end function gsl_sum_levin_u_alloc
	  function gsl_sum_levin_u_free(w) bind(c)
	    import
	    type(c_ptr), value :: w
	    integer(c_int) :: gsl_sum_levin_u_free
	  end function gsl_sum_levin_u_free
	  function gsl_sum_levin_u_accel(array, array_size, w, sum_accel, abserr) bind(c)
	    import
	    integer(c_size_t), value :: array_size
	    real(c_double), intent(in) :: array(array_size)
	    type(c_ptr), value :: w
	    real(c_double), intent(out) :: sum_accel, abserr
	    integer(c_int) :: gsl_sum_levin_u_accel
	  end function gsl_sum_levin_u_accel
	  function gsl_sum_levin_utrunc_alloc(n) bind(c)
	    import
	    integer(c_size_t), value :: n
	    type(c_ptr) :: gsl_sum_levin_utrunc_alloc
	  end function gsl_sum_levin_utrunc_alloc
	  function gsl_sum_levin_utrunc_free(w) bind(c)
	    import
	    type(c_ptr), value :: w
	    integer(c_int) :: gsl_sum_levin_utrunc_free
	  end function gsl_sum_levin_utrunc_free
	  function gsl_sum_levin_utrunc_accel(array, array_size, w, sum_accel, abserr) bind(c)
	    import
	    integer(c_size_t), value :: array_size
	    real(c_double), intent(in) :: array(array_size)
	    type(c_ptr), value :: w
	    real(c_double), intent(out) :: sum_accel, abserr
	    integer(c_int) :: gsl_sum_levin_utrunc_accel
	  end function gsl_sum_levin_utrunc_accel
  end interface
contains
  function fgsl_sum_levin_u_alloc(n) 
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_sum_levin_u_workspace) :: fgsl_sum_levin_u_alloc
    fgsl_sum_levin_u_alloc%gsl_sum_levin_u_workspace = &
         gsl_sum_levin_u_alloc(n) 
  end function fgsl_sum_levin_u_alloc
  function fgsl_sum_levin_u_free(w) 
    type(fgsl_sum_levin_u_workspace), intent(inout) :: w
    integer(fgsl_int) :: fgsl_sum_levin_u_free
    fgsl_sum_levin_u_free = &
         gsl_sum_levin_u_free(w%gsl_sum_levin_u_workspace)
  end function fgsl_sum_levin_u_free
  function fgsl_sum_levin_u_accel(array, array_size, w, sum_accel, abserr) 
    integer(fgsl_size_t), intent(in) :: array_size
    real(fgsl_double), intent(in) :: array(array_size)
    type(fgsl_sum_levin_u_workspace), intent(in) :: w
    real(fgsl_double), intent(out) :: sum_accel, abserr
    integer(fgsl_int) :: fgsl_sum_levin_u_accel
    fgsl_sum_levin_u_accel = gsl_sum_levin_u_accel(array, array_size, &
         w%gsl_sum_levin_u_workspace, sum_accel, abserr)
  end function fgsl_sum_levin_u_accel
  function fgsl_sum_levin_utrunc_alloc(n) 
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_sum_levin_utrunc_workspace) :: fgsl_sum_levin_utrunc_alloc
    fgsl_sum_levin_utrunc_alloc%gsl_sum_levin_utrunc_workspace = &
         gsl_sum_levin_utrunc_alloc(n) 
  end function fgsl_sum_levin_utrunc_alloc
  function fgsl_sum_levin_utrunc_free(w) 
    type(fgsl_sum_levin_utrunc_workspace), intent(inout) :: w
    integer(fgsl_int) :: fgsl_sum_levin_utrunc_free
    fgsl_sum_levin_utrunc_free = &
         gsl_sum_levin_utrunc_free(w%gsl_sum_levin_utrunc_workspace)
  end function fgsl_sum_levin_utrunc_free
  function fgsl_sum_levin_utrunc_accel(array, array_size, w, sum_accel, abserr) 
    integer(fgsl_size_t), intent(in) :: array_size
    real(fgsl_double), intent(in) :: array(array_size)
    type(fgsl_sum_levin_utrunc_workspace), intent(in) :: w
    real(fgsl_double), intent(out) :: sum_accel, abserr
    integer(fgsl_int) :: fgsl_sum_levin_utrunc_accel
    fgsl_sum_levin_utrunc_accel = gsl_sum_levin_utrunc_accel(array, array_size, &
         w%gsl_sum_levin_utrunc_workspace, sum_accel, abserr)
  end function fgsl_sum_levin_utrunc_accel
end module fgsl_sum_levin
