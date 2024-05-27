module fgsl_dhtransforms
  !> Discrete Hankel Transforms
  !> Note: This module maps the functions from gsl_dht.h
  use fgsl_base

  implicit none
  
  private :: gsl_dht_alloc, gsl_dht_init, gsl_dht_new, gsl_dht_free, &
    gsl_dht_apply, gsl_dht_x_sample, gsl_dht_k_sample
  
  !
  !> Types
  type, public :: fgsl_dht
     private
     type(c_ptr) :: gsl_dht = c_null_ptr
  end type fgsl_dht
  !
  !> Generics
  interface fgsl_well_defined
     module procedure fgsl_dht_status  
  end interface
  !
  !> C interfaces
  interface
	  function gsl_dht_alloc(size) bind(c)
	    import
	    integer(c_size_t), value :: size
	    type(c_ptr) :: gsl_dht_alloc
	  end function gsl_dht_alloc
	  function gsl_dht_init(t, nu, xmax) bind(c)
	    import
	    type(c_ptr), value :: t
	    real(c_double), value :: nu, xmax
	    integer(c_int) :: gsl_dht_init
	  end function gsl_dht_init
	  function gsl_dht_new(size, nu, xmax) bind(c)
	    import
	    integer(c_size_t), value :: size
	    real(c_double), value :: nu, xmax
	    type(c_ptr) :: gsl_dht_new
	  end function gsl_dht_new
	  subroutine gsl_dht_free(t) bind(c)
	    import
	    type(c_ptr), value :: t
	  end subroutine gsl_dht_free
	  function gsl_dht_apply(t, f_in, f_out) bind(c)
	    import
	    type(c_ptr), value :: t
	    type(c_ptr), value :: f_in
	    type(c_ptr), value :: f_out 
	    integer(c_int) :: gsl_dht_apply
	  end function gsl_dht_apply
	  function gsl_dht_x_sample(t, n) bind(c)
	    import
	    type(c_ptr), value :: t
	    integer(c_int), value :: n
	    real(c_double) :: gsl_dht_x_sample
	  end function gsl_dht_x_sample
	  function gsl_dht_k_sample(t, n) bind(c)
	    import
	    type(c_ptr), value :: t
	    integer(c_int), value :: n
	    real(c_double) :: gsl_dht_k_sample
	  end function gsl_dht_k_sample
  end interface
contains
!> API
  function fgsl_dht_alloc(size)
    integer(fgsl_size_t), intent(in) :: size
    type(fgsl_dht) :: fgsl_dht_alloc
    fgsl_dht_alloc%gsl_dht = gsl_dht_alloc(size)
  end function fgsl_dht_alloc
  function fgsl_dht_init(t, nu, xmax)
    type(fgsl_dht), intent(inout) :: t
    real(fgsl_double), intent(in) :: nu, xmax
    integer(fgsl_int) :: fgsl_dht_init
    fgsl_dht_init = gsl_dht_init(t%gsl_dht, nu, xmax)
  end function fgsl_dht_init
  function fgsl_dht_new(size, nu, xmax)
    integer(fgsl_size_t), intent(in) :: size
    real(fgsl_double), intent(in) :: nu, xmax
    type(fgsl_dht) :: fgsl_dht_new
    fgsl_dht_new%gsl_dht = gsl_dht_new(size, nu, xmax)
  end function fgsl_dht_new
  subroutine fgsl_dht_free(t)
    type(fgsl_dht), intent(inout) :: t
    call gsl_dht_free(t%gsl_dht)
  end subroutine fgsl_dht_free
  function fgsl_dht_apply(t, f_in, f_out)
    type(fgsl_dht), intent(in) :: t
    real(fgsl_double), intent(in), target, contiguous :: f_in(:)
    real(fgsl_double), intent(out), target, contiguous :: f_out(:)
    integer(fgsl_int) :: fgsl_dht_apply
    fgsl_dht_apply = gsl_dht_apply(t%gsl_dht, c_loc(f_in), c_loc(f_out))
  end function fgsl_dht_apply
  function fgsl_dht_x_sample(t, n)
    type(fgsl_dht), intent(in) :: t
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double) :: fgsl_dht_x_sample
    fgsl_dht_x_sample = gsl_dht_x_sample(t%gsl_dht, n)
  end function fgsl_dht_x_sample
  function fgsl_dht_k_sample(t, n)
    type(fgsl_dht), intent(in) :: t
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double) :: fgsl_dht_k_sample
    fgsl_dht_k_sample = gsl_dht_k_sample(t%gsl_dht, n)
  end function fgsl_dht_k_sample
  function fgsl_dht_status(dht)
    type(fgsl_dht), intent(in) :: dht
    logical :: fgsl_dht_status
    fgsl_dht_status = .true.
    if (.not. c_associated(dht%gsl_dht)) fgsl_dht_status = .false.
  end function fgsl_dht_status
end module fgsl_dhtransforms
