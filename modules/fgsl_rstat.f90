module fgsl_rstat
  !> Running Statistics
  use fgsl_base
  implicit none
  
  private :: gsl_rstat_quantile_alloc, gsl_rstat_quantile_reset, &
    gsl_rstat_quantile_free, gsl_rstat_quantile_add, gsl_rstat_quantile_get, &
    gsl_rstat_alloc, gsl_rstat_free, gsl_rstat_n, gsl_rstat_add, &
    gsl_rstat_min, gsl_rstat_max, gsl_rstat_mean, gsl_rstat_rms, &
    gsl_rstat_variance, gsl_rstat_sd, gsl_rstat_sd_mean, &
    gsl_rstat_median, gsl_rstat_norm, gsl_rstat_skew, gsl_rstat_kurtosis, &
    gsl_rstat_reset
  
  !  
  ! Types: Running Statistics
  type, public :: fgsl_rstat_quantile_workspace
	private
	type(c_ptr) :: gsl_rstat_quantile_workspace
  end type fgsl_rstat_quantile_workspace
  type, public :: fgsl_rstat_workspace
	private
	type(c_ptr) :: gsl_rstat_workspace
  end type fgsl_rstat_workspace
  !
  ! C interfaces
  interface
	function gsl_rstat_quantile_alloc(p) bind(c)
	  import :: c_double, c_ptr
	  real(c_double), value :: p
	  type(c_ptr) :: gsl_rstat_quantile_alloc
	end function gsl_rstat_quantile_alloc
	function gsl_rstat_quantile_reset(w) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value  :: w
	  integer(c_int) :: gsl_rstat_quantile_reset
	end function gsl_rstat_quantile_reset
	subroutine gsl_rstat_quantile_free(w) bind(c)
	  import :: c_ptr
	  type(c_ptr), value :: w
	end subroutine gsl_rstat_quantile_free
	function gsl_rstat_quantile_add(x, w) bind(c)
	  import :: c_double, c_ptr, c_int
	  real(c_double), value :: x
	  type(c_ptr), value :: w
	  integer(c_int) :: gsl_rstat_quantile_add
	end function gsl_rstat_quantile_add
	function gsl_rstat_quantile_get(w) bind(c)
	  import :: c_ptr, c_double
	  type(c_ptr), value :: w
	  real(c_double) :: gsl_rstat_quantile_get
	end function gsl_rstat_quantile_get
	function gsl_rstat_alloc() bind(c)
	  import :: c_ptr
	  type(c_ptr) :: gsl_rstat_alloc
	end function gsl_rstat_alloc
	subroutine gsl_rstat_free(w) bind(c)
	  import :: c_ptr
	  type(c_ptr), value :: w
	end subroutine gsl_rstat_free
	function gsl_rstat_n(w) bind(c)
	  import :: c_ptr, c_size_t
	  type(c_ptr), value :: w
	  integer(c_size_t) :: gsl_rstat_n
	end function gsl_rstat_n
	function gsl_rstat_add(x, w) bind(c)
	  import :: c_double, c_ptr, c_int
	  real(c_double), value :: x
	  type(c_ptr), value :: w
	  integer(c_int) :: gsl_rstat_add
	end function gsl_rstat_add
	function gsl_rstat_min(w) bind(c)
	  import :: c_ptr, c_double
	  type(c_ptr), value :: w
	  real(c_double) :: gsl_rstat_min
	end function gsl_rstat_min
	function gsl_rstat_max(w) bind(c)
	  import :: c_ptr, c_double
	  type(c_ptr), value :: w
	  real(c_double) :: gsl_rstat_max
	end function gsl_rstat_max
	function gsl_rstat_mean(w) bind(c)
	  import :: c_ptr, c_double
	  type(c_ptr), value :: w
	  real(c_double) :: gsl_rstat_mean
	end function gsl_rstat_mean
	function gsl_rstat_rms(w) bind(c)
	  import :: c_ptr, c_double
	  type(c_ptr), value :: w
	  real(c_double) :: gsl_rstat_rms
	end function gsl_rstat_rms
	function gsl_rstat_variance(w) bind(c)
	  import :: c_ptr, c_double
	  type(c_ptr), value :: w
	  real(c_double) :: gsl_rstat_variance
	end function gsl_rstat_variance
	function gsl_rstat_sd(w) bind(c)
	  import :: c_ptr, c_double
	  type(c_ptr), value :: w
	  real(c_double) :: gsl_rstat_sd
	end function gsl_rstat_sd
	function gsl_rstat_sd_mean(w) bind(c)
	  import :: c_ptr, c_double
	  type(c_ptr), value :: w
	  real(c_double) :: gsl_rstat_sd_mean
	end function gsl_rstat_sd_mean
	function gsl_rstat_median(w) bind(c)
	  import :: c_ptr, c_double
	  type(c_ptr), value :: w
	  real(c_double) :: gsl_rstat_median
	end function gsl_rstat_median
	function gsl_rstat_norm(w) bind(c)
	  import :: c_ptr, c_double
	  type(c_ptr), value :: w
	  real(c_double) :: gsl_rstat_norm
	end function gsl_rstat_norm
	function gsl_rstat_skew(w) bind(c)
	  import :: c_ptr, c_double
	  type(c_ptr), value :: w
	  real(c_double) :: gsl_rstat_skew
	end function gsl_rstat_skew
	function gsl_rstat_kurtosis(w) bind(c)
	  import :: c_ptr, c_double
	  type(c_ptr), value :: w
	  real(c_double) :: gsl_rstat_kurtosis
	end function gsl_rstat_kurtosis
	function gsl_rstat_reset(w) bind(c)
	  import :: c_ptr, c_int
	  type(c_ptr), value :: w
	  integer(c_int) :: gsl_rstat_reset
	end function gsl_rstat_reset
  end interface
contains
	function fgsl_rstat_quantile_alloc(p)
	  real(fgsl_double), intent(in) :: p
	  type(fgsl_rstat_quantile_workspace) :: fgsl_rstat_quantile_alloc
	  fgsl_rstat_quantile_alloc%gsl_rstat_quantile_workspace = gsl_rstat_quantile_alloc(p)
	end function fgsl_rstat_quantile_alloc
	subroutine fgsl_rstat_quantile_free(w)
	  type(fgsl_rstat_quantile_workspace), intent(inout) :: w
	  call gsl_rstat_quantile_free(w%gsl_rstat_quantile_workspace)
	end subroutine fgsl_rstat_quantile_free
	integer(fgsl_int) function fgsl_rstat_quantile_reset(w)
	  type(fgsl_rstat_quantile_workspace), intent(inout) :: w
	  fgsl_rstat_quantile_reset = gsl_rstat_quantile_reset(w%gsl_rstat_quantile_workspace)
	end function fgsl_rstat_quantile_reset
	function fgsl_rstat_quantile_add(x, w)
	  real(fgsl_double), intent(in) :: x
	  type(fgsl_rstat_quantile_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_rstat_quantile_add
	  fgsl_rstat_quantile_add = gsl_rstat_quantile_add(x, w%gsl_rstat_quantile_workspace)
	end function fgsl_rstat_quantile_add
	function fgsl_rstat_quantile_get(w)
	  type(fgsl_rstat_quantile_workspace), intent(inout) :: w
	  real(fgsl_double) :: fgsl_rstat_quantile_get
	  fgsl_rstat_quantile_get = gsl_rstat_quantile_get(w%gsl_rstat_quantile_workspace)
	end function fgsl_rstat_quantile_get
	function fgsl_rstat_alloc()
	  type(fgsl_rstat_workspace) :: fgsl_rstat_alloc
	  fgsl_rstat_alloc%gsl_rstat_workspace = gsl_rstat_alloc()
	end function fgsl_rstat_alloc
	subroutine fgsl_rstat_free(w)
	  type(fgsl_rstat_workspace), intent(inout) :: w
	  call gsl_rstat_free(w%gsl_rstat_workspace)
	end subroutine fgsl_rstat_free
	function fgsl_rstat_n(w)
	  type(fgsl_rstat_workspace), intent(inout) :: w
	  integer(fgsl_size_t) :: fgsl_rstat_n
	  fgsl_rstat_n = gsl_rstat_n(w%gsl_rstat_workspace)
	end function fgsl_rstat_n
	function fgsl_rstat_add(x, w)
	  real(fgsl_double), value :: x
	  type(fgsl_rstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_rstat_add
	  fgsl_rstat_add = gsl_rstat_add(x, w%gsl_rstat_workspace)
	end function fgsl_rstat_add
	function fgsl_rstat_min(w)
	  type(fgsl_rstat_workspace), intent(inout) :: w
	  real(fgsl_double) :: fgsl_rstat_min
	  fgsl_rstat_min = gsl_rstat_min(w%gsl_rstat_workspace)
	end function fgsl_rstat_min
	function fgsl_rstat_max(w)
	  type(fgsl_rstat_workspace), intent(inout) :: w
	  real(fgsl_double) :: fgsl_rstat_max
	  fgsl_rstat_max = gsl_rstat_max(w%gsl_rstat_workspace)
	end function fgsl_rstat_max
	function fgsl_rstat_mean(w)
	  type(fgsl_rstat_workspace), intent(inout) :: w
	  real(fgsl_double) :: fgsl_rstat_mean
	  fgsl_rstat_mean = gsl_rstat_mean(w%gsl_rstat_workspace)
	end function fgsl_rstat_mean
	function fgsl_rstat_rms(w)
	  type(fgsl_rstat_workspace), intent(inout) :: w
	  real(fgsl_double) :: fgsl_rstat_rms
	  fgsl_rstat_rms = gsl_rstat_rms(w%gsl_rstat_workspace)
	end function fgsl_rstat_rms
	function fgsl_rstat_variance(w)
	  type(fgsl_rstat_workspace), intent(inout) :: w
	  real(fgsl_double) :: fgsl_rstat_variance
	  fgsl_rstat_variance = gsl_rstat_variance(w%gsl_rstat_workspace)
	end function fgsl_rstat_variance
	function fgsl_rstat_sd(w)
	  type(fgsl_rstat_workspace), intent(inout) :: w
	  real(fgsl_double) :: fgsl_rstat_sd
	  fgsl_rstat_sd = gsl_rstat_sd(w%gsl_rstat_workspace)
	end function fgsl_rstat_sd
	function fgsl_rstat_sd_mean(w)
	  type(fgsl_rstat_workspace), intent(inout) :: w
	  real(fgsl_double) :: fgsl_rstat_sd_mean
	  fgsl_rstat_sd_mean = gsl_rstat_sd_mean(w%gsl_rstat_workspace)
	end function fgsl_rstat_sd_mean
	function fgsl_rstat_median(w)
	  type(fgsl_rstat_workspace), intent(inout) :: w
	  real(fgsl_double) :: fgsl_rstat_median
	  fgsl_rstat_median = gsl_rstat_median(w%gsl_rstat_workspace)
	end function fgsl_rstat_median
		function fgsl_rstat_norm(w)
	  type(fgsl_rstat_workspace), intent(inout) :: w
	  real(fgsl_double) :: fgsl_rstat_norm
	  fgsl_rstat_norm = gsl_rstat_norm(w%gsl_rstat_workspace)
	end function fgsl_rstat_norm
	function fgsl_rstat_skew(w)
	  type(fgsl_rstat_workspace), intent(inout) :: w
	  real(fgsl_double) :: fgsl_rstat_skew
	  fgsl_rstat_skew = gsl_rstat_skew(w%gsl_rstat_workspace)
	end function fgsl_rstat_skew
	function fgsl_rstat_kurtosis(w)
	  type(fgsl_rstat_workspace), intent(inout) :: w
	  real(fgsl_double) :: fgsl_rstat_kurtosis
	  fgsl_rstat_kurtosis = gsl_rstat_kurtosis(w%gsl_rstat_workspace)
	end function fgsl_rstat_kurtosis
	function fgsl_rstat_reset(w)
	  type(fgsl_rstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_rstat_reset
	  fgsl_rstat_reset = gsl_rstat_reset(w%gsl_rstat_workspace)
	end function fgsl_rstat_reset
end module fgsl_rstat
