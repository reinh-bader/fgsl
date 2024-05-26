module fgsl_movstat
  !> Moving Window Statistics
  use fgsl_base
  use fgsl_array
  implicit none
  
  private :: gsl_movstat_alloc, gsl_movstat_alloc2, gsl_movstat_free, &
    gsl_movstat_mean, gsl_movstat_variance, gsl_movstat_sd, gsl_movstat_min, &
    gsl_movstat_max, gsl_movstat_minmax, gsl_movstat_sum, gsl_movstat_median, &
    gsl_movstat_mad0, gsl_movstat_mad, gsl_movstat_qqr, gsl_movstat_sn, &
    gsl_movstat_qn, gsl_movstat_apply, gsl_movstat_fill
  
  !
  ! Types
  integer(fgsl_int), public, parameter :: &
     fgsl_movstat_end_padzero = 0, &
     fgsl_movstat_end_padvalue = 1, &
     fgsl_movstat_end_truncate = 2
  type, public :: fgsl_movstat_workspace
    private
    type(c_ptr) :: gsl_movstat_workspace
  end type fgsl_movstat_workspace
  !> fgsl_movstat_function interoperates with gsl_movstat_function
  type, public, bind(c) :: fgsl_movstat_function
    type(c_funptr) :: function
    type(c_ptr) :: params
  end type fgsl_movstat_function
  !> Note: gsl_movstat_accum is not matched since the publicized
  !> interface does not make explicit use of accumulators
  !
  ! C interfaces
  interface
	function gsl_movstat_alloc(k) bind(c)
	  import :: c_ptr, c_size_t
	  integer(c_size_t), value :: k
	  type(c_ptr) :: gsl_movstat_alloc
	end function gsl_movstat_alloc
	function gsl_movstat_alloc2(k,j) bind(c)
	  import :: c_ptr, c_size_t
	  integer(c_size_t), value :: k, j
	  type(c_ptr) :: gsl_movstat_alloc2
	end function gsl_movstat_alloc2
	subroutine gsl_movstat_free(w) bind(c)
	  import :: c_ptr
	  type(c_ptr), value :: w
	end subroutine gsl_movstat_free
	function gsl_movstat_mean(endtype, x, y, w) bind(c)
	  import :: c_ptr, c_int
	  integer(c_int), value :: endtype
	  type(c_ptr), value :: x, y, w
	  integer(c_int) :: gsl_movstat_mean
	end function gsl_movstat_mean
	function gsl_movstat_variance(endtype, x, y, w) bind(c)
	  import :: c_ptr, c_int
	  integer(c_int), value :: endtype
	  type(c_ptr), value :: x, y, w
	  integer(c_int) :: gsl_movstat_variance
	end function gsl_movstat_variance
	function gsl_movstat_sd(endtype, x, y, w) bind(c)
	  import :: c_ptr, c_int
	  integer(c_int), value :: endtype
	  type(c_ptr), value :: x, y, w
	  integer(c_int) :: gsl_movstat_sd
	end function gsl_movstat_sd
	function gsl_movstat_min(endtype, x, y, w) bind(c)
	  import :: c_ptr, c_int
	  integer(c_int), value :: endtype
	  type(c_ptr), value :: x, y, w
	    integer(c_int) :: gsl_movstat_min
	end function gsl_movstat_min
	function gsl_movstat_max(endtype, x, y, w) bind(c)
	  import :: c_ptr, c_int
	  integer(c_int), value :: endtype
	  type(c_ptr), value :: x, y, w
	  integer(c_int) :: gsl_movstat_max
	end function gsl_movstat_max
	function gsl_movstat_minmax(endtype, x, y_min, y_max, w) bind(c)
	  import :: c_ptr, c_int
	  integer(c_int), value :: endtype
	  type(c_ptr), value :: x, y_min, y_max, w
	    integer(c_int) :: gsl_movstat_minmax
	end function gsl_movstat_minmax
	function gsl_movstat_sum(endtype, x, y, w) bind(c)
	  import :: c_ptr, c_int
	  integer(c_int), value :: endtype
	  type(c_ptr), value :: x, y, w
	  integer(c_int) :: gsl_movstat_sum
	end function gsl_movstat_sum
	function gsl_movstat_median(endtype, x, y, w) bind(c)
	  import :: c_ptr, c_int
	  integer(c_int), value :: endtype
	  type(c_ptr), value :: x, y, w
	  integer(c_int) :: gsl_movstat_median
	end function gsl_movstat_median
	function gsl_movstat_mad0(endtype, x, xmedian, xmad, w) bind(c)
	  import :: c_ptr, c_int
	  integer(c_int), value :: endtype
	  type(c_ptr), value :: x, xmedian, xmad, w
	  integer(c_int) :: gsl_movstat_mad0
	end function gsl_movstat_mad0
	function gsl_movstat_mad(endtype, x, xmedian, xmad, w) bind(c)
	  import :: c_ptr, c_int
	  integer(c_int), value :: endtype
	  type(c_ptr), value :: x, xmedian, xmad, w
	  integer(c_int) :: gsl_movstat_mad
	end function gsl_movstat_mad
	function gsl_movstat_qqr(endtype, x, q, xqqr, w) bind(c)
	  import :: c_ptr, c_int, c_double
	  integer(c_int), value :: endtype
	  type(c_ptr), value :: x, xqqr, w
	  real(c_double), value :: q
	  integer(c_int) :: gsl_movstat_qqr
	end function gsl_movstat_qqr
	function gsl_movstat_sn(endtype, x, xscale, w) bind(c, name='gsl_movstat_Sn')
	  import :: c_ptr, c_int
	  integer(c_int), value :: endtype
	  type(c_ptr), value :: x, xscale, w
	  integer(c_int) :: gsl_movstat_sn
	end function gsl_movstat_sn
	function gsl_movstat_qn(endtype, x, xscale, w) bind(c, name='gsl_movstat_Qn')
	  import :: c_ptr, c_int
	  integer(c_int), value :: endtype
	  type(c_ptr), value :: x, xscale, w
	  integer(c_int) :: gsl_movstat_qn
	end function gsl_movstat_qn
	function gsl_movstat_apply(endtype, f, x, y, w) bind(c)
	  import :: c_ptr, c_int, fgsl_movstat_function
	  integer(c_int), value :: endtype
	  type(fgsl_movstat_function) :: f
	  type(c_ptr), value :: x, y, w
	  integer(c_int) :: gsl_movstat_apply
	end function gsl_movstat_apply
	function gsl_movstat_fill(endtype, x, idx, h, j, window) bind(c)
	  import :: c_ptr, c_int, c_size_t, c_double
	  integer(c_int), value :: endtype
	  type(c_ptr), value :: x
	  integer(c_size_t), value :: idx, h, j
	  real(c_double) :: window
	  integer(c_int) :: gsl_movstat_fill
	end function gsl_movstat_fill
end interface
  
contains
	function fgsl_movstat_alloc(k)
	  integer(fgsl_size_t), intent(in) :: k
	  type(fgsl_movstat_workspace) :: fgsl_movstat_alloc
	  fgsl_movstat_alloc%gsl_movstat_workspace = gsl_movstat_alloc(k)
	end function fgsl_movstat_alloc
	function fgsl_movstat_alloc2(k, j)
	  integer(fgsl_size_t), intent(in) :: k, j
	  type(fgsl_movstat_workspace) :: fgsl_movstat_alloc2
	  fgsl_movstat_alloc2%gsl_movstat_workspace = gsl_movstat_alloc2(k, j)
	end function fgsl_movstat_alloc2
	subroutine fgsl_movstat_free(w)
	  type(fgsl_movstat_workspace), intent(inout) :: w
	  call gsl_movstat_free(w%gsl_movstat_workspace) 
	end subroutine fgsl_movstat_free
	function fgsl_movstat_mean(endtype, x, y, w)
	  integer(fgsl_int), intent(in) :: endtype
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: y
	  type(fgsl_movstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_movstat_mean
	  fgsl_movstat_mean = gsl_movstat_mean(endtype, x%gsl_vector, y%gsl_vector, &
	       w%gsl_movstat_workspace)
	end function fgsl_movstat_mean
	function fgsl_movstat_variance(endtype, x, y, w)
	  integer(fgsl_int), intent(in) :: endtype
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: y
	  type(fgsl_movstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_movstat_variance
	  fgsl_movstat_variance = gsl_movstat_variance(endtype, x%gsl_vector, &
	       y%gsl_vector, w%gsl_movstat_workspace)
	end function fgsl_movstat_variance
	function fgsl_movstat_sd(endtype, x, y, w)
	  integer(fgsl_int), intent(in) :: endtype
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: y
	  type(fgsl_movstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_movstat_sd
	  fgsl_movstat_sd = gsl_movstat_sd(endtype, x%gsl_vector, &
	       y%gsl_vector, w%gsl_movstat_workspace)
	end function fgsl_movstat_sd
	function fgsl_movstat_min(endtype, x, y, w)
	  integer(fgsl_int), intent(in) :: endtype
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: y
	  type(fgsl_movstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_movstat_min
	  fgsl_movstat_min = gsl_movstat_min(endtype, x%gsl_vector, &
	       y%gsl_vector, w%gsl_movstat_workspace)
	end function fgsl_movstat_min
	function fgsl_movstat_max(endtype, x, y, w)
	  integer(fgsl_int), intent(in) :: endtype
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: y
	  type(fgsl_movstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_movstat_max
	  fgsl_movstat_max = gsl_movstat_max(endtype, x%gsl_vector, &
	       y%gsl_vector, w%gsl_movstat_workspace)
	end function fgsl_movstat_max
	function fgsl_movstat_minmax(endtype, x, y_min, y_max, w)
	  integer(fgsl_int), intent(in) :: endtype
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: y_min, y_max
	  type(fgsl_movstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_movstat_minmax
	  fgsl_movstat_minmax = gsl_movstat_minmax(endtype, x%gsl_vector, &
	       y_min%gsl_vector, y_max%gsl_vector, w%gsl_movstat_workspace)
	end function fgsl_movstat_minmax
	function fgsl_movstat_sum(endtype, x, y, w)
	  integer(fgsl_int), intent(in) :: endtype
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: y
	  type(fgsl_movstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_movstat_sum
	  fgsl_movstat_sum = gsl_movstat_sum(endtype, x%gsl_vector, &
	       y%gsl_vector, w%gsl_movstat_workspace)
	end function fgsl_movstat_sum
	function fgsl_movstat_median(endtype, x, y, w)
	  integer(fgsl_int), intent(in) :: endtype
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: y
	  type(fgsl_movstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_movstat_median
	  fgsl_movstat_median = gsl_movstat_median(endtype, x%gsl_vector, &
	       y%gsl_vector, w%gsl_movstat_workspace)
	end function fgsl_movstat_median
	function fgsl_movstat_mad0(endtype, x, xmedian, xmad, w)
	  integer(fgsl_int), intent(in) :: endtype
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: xmedian, xmad
	  type(fgsl_movstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_movstat_mad0
	  fgsl_movstat_mad0 = gsl_movstat_mad0(endtype, x%gsl_vector, &
	       xmedian%gsl_vector, xmad%gsl_vector, w%gsl_movstat_workspace)
	end function fgsl_movstat_mad0
	function fgsl_movstat_mad(endtype, x, xmedian, xmad, w)
	  integer(fgsl_int), intent(in) :: endtype
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: xmedian, xmad
	  type(fgsl_movstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_movstat_mad
	  fgsl_movstat_mad = gsl_movstat_mad(endtype, x%gsl_vector, &
	       xmedian%gsl_vector, xmad%gsl_vector, w%gsl_movstat_workspace)
	end function fgsl_movstat_mad
	function fgsl_movstat_qqr(endtype, x, q, xqqr, w)
	  integer(fgsl_int), intent(in) :: endtype
	  type(fgsl_vector), intent(in) :: x
	  real(fgsl_double), intent(in) :: q
	  type(fgsl_vector), intent(inout) :: xqqr
	  type(fgsl_movstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_movstat_qqr
	  fgsl_movstat_qqr = gsl_movstat_qqr(endtype, x%gsl_vector, &
	       q, xqqr%gsl_vector, w%gsl_movstat_workspace)
	end function fgsl_movstat_qqr
	function fgsl_movstat_sn(endtype, x, xscale, w)
	  integer(fgsl_int), intent(in) :: endtype
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: xscale
	  type(fgsl_movstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_movstat_sn
	  fgsl_movstat_sn = gsl_movstat_sn(endtype, x%gsl_vector, &
	       xscale%gsl_vector, w%gsl_movstat_workspace)
	end function fgsl_movstat_sn
	function fgsl_movstat_qn(endtype, x, xscale, w)
	  integer(fgsl_int), intent(in) :: endtype
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: xscale
	  type(fgsl_movstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_movstat_qn
	  fgsl_movstat_qn = gsl_movstat_qn(endtype, x%gsl_vector, &
	       xscale%gsl_vector, w%gsl_movstat_workspace)
	end function fgsl_movstat_qn
	function fgsl_movstat_apply(endtype, f, x, y, w)
	  integer(fgsl_int), intent(in) :: endtype
	  type(fgsl_movstat_function), intent(in) :: f
	  type(fgsl_vector), intent(in) :: x
	  type(fgsl_vector), intent(inout) :: y
	  type(fgsl_movstat_workspace), intent(inout) :: w
	  integer(fgsl_int) :: fgsl_movstat_apply
	  fgsl_movstat_apply = gsl_movstat_apply(endtype, f, x%gsl_vector, &
	       y%gsl_vector, w%gsl_movstat_workspace)
	end function fgsl_movstat_apply
	function fgsl_movstat_fill(endtype, x, idx, h, j, window)
	  integer(fgsl_int), intent(in) :: endtype
	  type(fgsl_vector), intent(in) :: x
	  integer(fgsl_size_t), intent(in) :: idx, h, j
	  real(fgsl_double), intent(inout) :: window
	  integer(fgsl_int) :: fgsl_movstat_fill
	  fgsl_movstat_fill = gsl_movstat_fill(endtype, x%gsl_vector, &
	       idx, h, j, window)
	end function fgsl_movstat_fill
end module fgsl_movstat
