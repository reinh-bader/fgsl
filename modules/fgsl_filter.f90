module fgsl_filter
  !> Digital Filtering
  use fgsl_base
  use fgsl_array
  implicit none
  
  private :: gsl_filter_gaussian_alloc, gsl_filter_gaussian_free, &
    gsl_filter_gaussian, gsl_filter_gaussian_kernel, gsl_filter_median_alloc, &
    gsl_filter_median_free, gsl_filter_median, gsl_filter_rmedian_alloc, &
    gsl_filter_rmedian_free, gsl_filter_rmedian, gsl_filter_impulse_alloc, &
    gsl_filter_impulse_free, gsl_filter_impulse
  
  !
  ! Types
  integer(fgsl_int), public, parameter :: &
     fgsl_filter_end_padzero = 0, &
     fgsl_filter_end_padvalue = 1, &
     fgsl_filter_end_truncate = 2, &
     fgsl_filter_scale_mad = 0, &
     fgsl_filter_scale_iqr = 1, &
     fgsl_filter_scale_sn = 2, &
     fgsl_filter_scale_qn = 3

  type, public :: fgsl_filter_gaussian_workspace
    private
    type(c_ptr) :: gsl_filter_gaussian_workspace
  end type fgsl_filter_gaussian_workspace
  type, public :: fgsl_filter_median_workspace
    private
    type(c_ptr) :: gsl_filter_median_workspace
  end type fgsl_filter_median_workspace
  type, public :: fgsl_filter_rmedian_workspace
    private
    type(c_ptr) :: gsl_filter_rmedian_workspace
  end type fgsl_filter_rmedian_workspace
  type, public :: fgsl_filter_impulse_workspace
    private
    type(c_ptr) :: gsl_filter_impulse_workspace
  end type fgsl_filter_impulse_workspace
  !
  ! C interfaces
  interface
	function gsl_filter_gaussian_alloc(k) bind(c)
	  import :: c_ptr, c_size_t
	  integer(c_size_t), value :: k
	  type(c_ptr) :: gsl_filter_gaussian_alloc
	end function gsl_filter_gaussian_alloc
	subroutine gsl_filter_gaussian_free(w) bind(c)
	  import :: c_ptr
	  type(c_ptr), value :: w
	end subroutine gsl_filter_gaussian_free
	function gsl_filter_gaussian(endtype, alpha, order, x, y, w) bind(c)
	  import :: c_int, c_ptr, c_size_t, c_double
	  integer(c_int), value :: endtype
	  real(c_double), value :: alpha
	  integer(c_size_t), value :: order
	  type(c_ptr), value :: x, y, w
	  integer(c_int) :: gsl_filter_gaussian
	end function gsl_filter_gaussian
	function gsl_filter_gaussian_kernel(alpha, order, normalize, kernel) bind(c)
	  import :: c_int, c_ptr, c_size_t, c_double
	  integer(c_int), value :: normalize
	  real(c_double), value :: alpha
	  integer(c_size_t), value :: order
	  type(c_ptr), value :: kernel
	  integer(c_int) :: gsl_filter_gaussian_kernel
	end function gsl_filter_gaussian_kernel
	function gsl_filter_median_alloc(k) bind(c)
	  import :: c_ptr, c_size_t
	  integer(c_size_t), value :: k
	  type(c_ptr) :: gsl_filter_median_alloc
	end function gsl_filter_median_alloc
	subroutine gsl_filter_median_free(w) bind(c)
	  import :: c_ptr
	  type(c_ptr), value :: w
	end subroutine gsl_filter_median_free
	function gsl_filter_median(endtype, alpha, order, x, y, w) bind(c)
	  import :: c_int, c_ptr, c_size_t, c_double
	  integer(c_int), value :: endtype
	  real(c_double), value :: alpha
	  integer(c_size_t), value :: order
	  type(c_ptr), value :: x, y, w
	  integer(c_int) :: gsl_filter_median
	end function gsl_filter_median
	function gsl_filter_rmedian_alloc(k) bind(c)
	  import :: c_ptr, c_size_t
	  integer(c_size_t), value :: k
	  type(c_ptr) :: gsl_filter_rmedian_alloc
	end function gsl_filter_rmedian_alloc
	subroutine gsl_filter_rmedian_free(w) bind(c)
	  import :: c_ptr
	  type(c_ptr), value :: w
	end subroutine gsl_filter_rmedian_free
	function gsl_filter_rmedian(endtype, alpha, order, x, y, w) bind(c)
	  import :: c_int, c_ptr, c_size_t, c_double
	  integer(c_int), value :: endtype
	  real(c_double), value :: alpha
	  integer(c_size_t), value :: order
	  type(c_ptr), value :: x, y, w
	  integer(c_int) :: gsl_filter_rmedian
	end function gsl_filter_rmedian
	function gsl_filter_impulse_alloc(k) bind(c)
	  import :: c_ptr, c_size_t
	  integer(c_size_t), value :: k
	  type(c_ptr) :: gsl_filter_impulse_alloc
	end function gsl_filter_impulse_alloc
	subroutine gsl_filter_impulse_free(w) bind(c)
	  import :: c_ptr
	  type(c_ptr), value :: w
	end subroutine gsl_filter_impulse_free
	function gsl_filter_impulse(endtype, scale_type, t, x, y, xmedian, xsigma, &
	     noutlier, ioutlier, w) bind(c)
	  import :: c_int, c_double, c_ptr, c_size_t
	  integer(c_int), value :: endtype, scale_type
	  real(c_double), value :: t
	  type(c_ptr), value :: x, y, xmedian, xsigma, ioutlier, w
	  integer(c_size_t), intent(inout) :: noutlier
	  integer(c_int) :: gsl_filter_impulse
	end function gsl_filter_impulse
  end interface
contains
	function fgsl_filter_gaussian_alloc(k) 
	  integer(fgsl_size_t), intent(in) :: k
	  type(fgsl_filter_gaussian_workspace) :: fgsl_filter_gaussian_alloc
	
	  fgsl_filter_gaussian_alloc%gsl_filter_gaussian_workspace = &
	       gsl_filter_gaussian_alloc(k)
	end function fgsl_filter_gaussian_alloc
	subroutine fgsl_filter_gaussian_free(w)
	  type(fgsl_filter_gaussian_workspace), intent(inout) :: w
	  call gsl_filter_gaussian_free(w%gsl_filter_gaussian_workspace)
	end subroutine fgsl_filter_gaussian_free
	function fgsl_filter_gaussian(endtype, alpha, order, x, y, w)
	  integer(fgsl_int), intent(in) :: endtype
	  real(fgsl_double), intent(in) :: alpha
	  integer(fgsl_size_t), intent(in) :: order
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) ::  y
	  type(fgsl_filter_gaussian_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_filter_gaussian
	
	  fgsl_filter_gaussian = gsl_filter_gaussian(endtype, alpha, order, &
	       x%gsl_vector, y%gsl_vector, w%gsl_filter_gaussian_workspace)
	end function fgsl_filter_gaussian
	function fgsl_filter_gaussian_kernel(alpha, order, normalize, kernel)
	  integer(fgsl_int), intent(in) :: normalize
	  real(fgsl_double), intent(in) :: alpha
	  integer(fgsl_size_t), intent(in) :: order
	  type(fgsl_vector), intent(inout) :: kernel
	  integer(fgsl_int) :: fgsl_filter_gaussian_kernel
	
	  fgsl_filter_gaussian_kernel = gsl_filter_gaussian_kernel(alpha, order, &
	       normalize, kernel%gsl_vector)
	end function fgsl_filter_gaussian_kernel
	function fgsl_filter_median_alloc(k) 
	  integer(fgsl_size_t), intent(in) :: k
	  type(fgsl_filter_median_workspace) :: fgsl_filter_median_alloc
	
	  fgsl_filter_median_alloc%gsl_filter_median_workspace = &
	       gsl_filter_median_alloc(k)
	end function fgsl_filter_median_alloc
	subroutine fgsl_filter_median_free(w)
	  type(fgsl_filter_median_workspace), intent(inout) :: w
	  call gsl_filter_median_free(w%gsl_filter_median_workspace)
	end subroutine fgsl_filter_median_free
	function fgsl_filter_median(endtype, alpha, order, x, y, w)
	  integer(fgsl_int), intent(in) :: endtype
	  real(fgsl_double), intent(in) :: alpha
	  integer(fgsl_size_t), intent(in) :: order
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) ::  y
	  type(fgsl_filter_median_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_filter_median
	
	  fgsl_filter_median = gsl_filter_median(endtype, alpha, order, &
	       x%gsl_vector, y%gsl_vector, w%gsl_filter_median_workspace)
	end function fgsl_filter_median
	function fgsl_filter_rmedian_alloc(k) 
	  integer(fgsl_size_t), intent(in) :: k
	  type(fgsl_filter_rmedian_workspace) :: fgsl_filter_rmedian_alloc
	
	  fgsl_filter_rmedian_alloc%gsl_filter_rmedian_workspace = &
	       gsl_filter_rmedian_alloc(k)
	end function fgsl_filter_rmedian_alloc
	subroutine fgsl_filter_rmedian_free(w)
	  type(fgsl_filter_rmedian_workspace), intent(inout) :: w
	  call gsl_filter_rmedian_free(w%gsl_filter_rmedian_workspace)
	end subroutine fgsl_filter_rmedian_free
	function fgsl_filter_rmedian(endtype, alpha, order, x, y, w)
	  integer(fgsl_int), intent(in) :: endtype
	  real(fgsl_double), intent(in) :: alpha
	  integer(fgsl_size_t), intent(in) :: order
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) ::  y
	  type(fgsl_filter_rmedian_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_filter_rmedian
	
	  fgsl_filter_rmedian = gsl_filter_rmedian(endtype, alpha, order, &
	       x%gsl_vector, y%gsl_vector, w%gsl_filter_rmedian_workspace)
	end function fgsl_filter_rmedian
	function fgsl_filter_impulse_alloc(k) 
	  integer(fgsl_size_t), intent(in) :: k
	  type(fgsl_filter_impulse_workspace) :: fgsl_filter_impulse_alloc
	
	  fgsl_filter_impulse_alloc%gsl_filter_impulse_workspace = &
	       gsl_filter_impulse_alloc(k)
	end function fgsl_filter_impulse_alloc
	subroutine fgsl_filter_impulse_free(w)
	  type(fgsl_filter_impulse_workspace), intent(inout) :: w
	  call gsl_filter_impulse_free(w%gsl_filter_impulse_workspace)
	end subroutine fgsl_filter_impulse_free
	function fgsl_filter_impulse(endtype, scale_type, t, x, y, xmedian, xsigma, &
	     noutlier, ioutlier, w)
	  integer(fgsl_int), intent(in) :: endtype, scale_type
	  real(fgsl_double), intent(in) :: t
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: y, xmedian, xsigma
	  integer(fgsl_size_t), intent(inout) :: noutlier
	  type(fgsl_vector_int), intent(inout) :: ioutlier
	  type(fgsl_filter_impulse_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_filter_impulse
	
	  fgsl_filter_impulse = gsl_filter_impulse(endtype, scale_type, t, &
	       x%gsl_vector, y%gsl_vector, xmedian%gsl_vector, xsigma%gsl_vector, &
	       noutlier, ioutlier%gsl_vector_int, w%gsl_filter_impulse_workspace)
	end function fgsl_filter_impulse
end module fgsl_filter
