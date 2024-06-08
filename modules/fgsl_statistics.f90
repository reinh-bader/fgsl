module fgsl_statistics
  !> Statistics
  !> Note: only the double precision specifics are accessible
  use fgsl_base
  implicit none
  
  !
  !> C interfaces
  interface
	function fgsl_stats_mean (data, stride, n) bind(c, name='gsl_stats_mean')
	  import
	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  real(c_double) :: fgsl_stats_mean
	end function fgsl_stats_mean
	function fgsl_stats_variance (data, stride, n) bind(c, name='gsl_stats_variance')
	  import
	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  real(c_double) :: fgsl_stats_variance
	end function fgsl_stats_variance
	function fgsl_stats_variance_m (data, stride, n, mean) bind(c, name='gsl_stats_variance_m')
	  import
   	  real(c_double), intent(in) :: data(*)
      integer(c_size_t), value :: stride, n
	  real(c_double), value :: mean
	  real(c_double) :: fgsl_stats_variance_m
	end function fgsl_stats_variance_m
	  function fgsl_stats_sd (data, stride, n) bind(c, name='gsl_stats_sd')
	  import
   	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  real(c_double) :: fgsl_stats_sd
	end function fgsl_stats_sd
	function fgsl_stats_sd_m (data, stride, n, mean) bind(c, name='gsl_stats_sd_m')
	  import
   	  real(c_double), intent(in) :: data(*)
   	  integer(c_size_t), value :: stride, n
	  real(c_double), value :: mean
	  real(c_double) :: fgsl_stats_sd_m
    end function fgsl_stats_sd_m
    function fgsl_stats_tss (data, stride, n) bind(c, name='gsl_stats_tss')
	  import
   	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  real(c_double) :: fgsl_stats_tss
	end function fgsl_stats_tss
	function fgsl_stats_tss_m (data, stride, n, mean) bind(c, name='gsl_stats_tss_m')
	  import
   	  real(c_double), intent(in) :: data(*)
   	  integer(c_size_t), value :: stride, n
	  real(c_double), value :: mean
	  real(c_double) :: fgsl_stats_tss_m
    end function fgsl_stats_tss_m
	function fgsl_stats_variance_with_fixed_mean (data, stride, n, mean) &
	         bind(c, name='gsl_stats_variance_with_fixed_mean')
	  import
   	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  real(c_double), value :: mean
	  real(c_double) :: fgsl_stats_variance_with_fixed_mean
    end function fgsl_stats_variance_with_fixed_mean
	function fgsl_stats_sd_with_fixed_mean (data, stride, n, mean) &
	         bind(c, name='gsl_stats_sd_with_fixed_mean')
	  import
   	  real(c_double), intent(in) :: data(*)
   	  integer(c_size_t), value :: stride, n
	  real(c_double), value :: mean
	  real(c_double) :: fgsl_stats_sd_with_fixed_mean
	end function fgsl_stats_sd_with_fixed_mean
	!
    function fgsl_stats_absdev (data, stride, n) bind(c, name='gsl_stats_absdev')
	  import
   	  real(c_double), intent(in) :: data(*)
      integer(c_size_t), value :: stride, n
	  real(c_double) :: fgsl_stats_absdev
	end function fgsl_stats_absdev
    function fgsl_stats_absdev_m (data, stride, n, mean) &
	         bind(c, name='gsl_stats_absdev_m')
	  import
   	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  real(c_double), value :: mean
	  real(c_double) :: fgsl_stats_absdev_m
	end function fgsl_stats_absdev_m
    !
	function fgsl_stats_skew (data, stride, n) bind(c, name='gsl_stats_skew')
	  import
   	  real(c_double), intent(in) :: data(*)
      integer(c_size_t), value :: stride, n
	  real(c_double) :: fgsl_stats_skew
	end function fgsl_stats_skew
	function fgsl_stats_skew_m_sd (data, stride, n, mean, sd) &
	         bind(c, name='gsl_stats_skew_m_sd')
	  import
   	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  real(c_double), value :: mean, sd
	  real(c_double) :: fgsl_stats_skew_m_sd
	end function fgsl_stats_skew_m_sd
	function fgsl_stats_kurtosis (data, stride, n) &
	         bind(c, name='gsl_stats_kurtosis')
	  import
   	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  real(c_double) :: fgsl_stats_kurtosis
	end function fgsl_stats_kurtosis
	function fgsl_stats_kurtosis_m_sd (data, stride, n, mean, sd) &
	         bind(c, name='gsl_stats_kurtosis_m_sd')
	  import
   	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  real(c_double), value :: mean, sd
	  real(c_double) :: fgsl_stats_kurtosis_m_sd
	end function fgsl_stats_kurtosis_m_sd
	!
	function fgsl_stats_lag1_autocorrelation (data, stride, n) &
	         bind(c, name='gsl_stats_lag1_autocorrelation')
	  import
   	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  real(c_double) :: fgsl_stats_lag1_autocorrelation
	end function fgsl_stats_lag1_autocorrelation
	function fgsl_stats_lag1_autocorrelation_m (data, stride, n, mean) &
	         bind(c, name='gsl_stats_lag1_autocorrelation_m')
	  import
   	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  real(c_double), value :: mean
	  real(c_double) :: fgsl_stats_lag1_autocorrelation_m
	end function fgsl_stats_lag1_autocorrelation_m
    function fgsl_stats_covariance(data1, stride1, data2, stride2, n) &
	         bind(c, name='gsl_stats_covariance')
	  import
  	  real(c_double), intent(in) :: data1(*), data2(*)
  	  integer(c_size_t), value :: stride1, stride2, n
	  real(c_double) :: fgsl_stats_covariance
    end function fgsl_stats_covariance
	function fgsl_stats_covariance_m(data1, stride1, data2, stride2, n, &
	         mean1, mean2) bind(c, name='gsl_stats_covariance_m')
	  import
   	  real(c_double), intent(in) :: data1(*), data2(*)
   	  integer(c_size_t), value :: stride1, stride2, n
	  real(c_double), value :: mean1, mean2
	  real(c_double) :: fgsl_stats_covariance_m
	end function fgsl_stats_covariance_m
	!
    function fgsl_stats_correlation(data1, stride1, data2, stride2, n) &
	         bind(c, name='gsl_stats_correlation')
	  import
   	  real(c_double), intent(in) :: data1(*), data2(*)
      integer(c_size_t), value :: stride1, stride2, n
	  real(c_double) :: fgsl_stats_correlation
	end function fgsl_stats_correlation
    function fgsl_stats_spearman(data1, stride1, data2, stride2, n, work) &
	         bind(c, name='gsl_stats_spearman')
	  import
   	  real(c_double), intent(in) :: data1(*), data2(*)
      real(c_double), intent(inout) :: work(*)   ! size 2*n
	  integer(c_size_t), value :: stride1, stride2, n
	  real(c_double) :: fgsl_stats_spearman
	end function fgsl_stats_spearman
	!
	function fgsl_stats_wmean(w, wstride, data, stride, n) &
	         bind(c, name='gsl_stats_wmean')
	  import
	  real(c_double), intent(in) :: w(*), data(*)
	  integer(c_size_t), value :: wstride, stride, n
	  real(c_double) :: fgsl_stats_wmean
	end function fgsl_stats_wmean
	function fgsl_stats_wvariance(w, wstride, data, stride, n) &
	          bind(c, name='gsl_stats_wvariance')
	  import
	  real(c_double), intent(in) :: w(*), data(*)
      integer(c_size_t), value :: wstride, stride, n
	  real(c_double) :: fgsl_stats_wvariance
	end function fgsl_stats_wvariance
	function fgsl_stats_wvariance_m(w, wstride, data, stride, n, mean) &
	         bind(c, name='gsl_stats_wvariance_m')
	  import
	  real(c_double), intent(in) :: w(*), data(*)
	  integer(c_size_t), value :: wstride, stride, n
	  real(c_double), value :: mean
	  real(c_double) :: fgsl_stats_wvariance_m
    end function fgsl_stats_wvariance_m
	function fgsl_stats_wsd(w, wstride, data, stride, n) bind(c, name='gsl_stats_wsd')
	  import
	  real(c_double), intent(in) :: w(*), data(*)
      integer(c_size_t), value :: wstride, stride, n
	  real(c_double) :: fgsl_stats_wsd
	end function fgsl_stats_wsd
	function fgsl_stats_wsd_m(w, wstride, data, stride, n, mean) bind(c, name='gsl_stats_wsd_m')
	  import
	  real(c_double), intent(in) :: w(*), data(*)
	  integer(c_size_t), value :: wstride, stride, n
	  real(c_double), value :: mean
	  real(c_double) :: fgsl_stats_wsd_m
	end function fgsl_stats_wsd_m
	function fgsl_stats_wvariance_with_fixed_mean(w, wstride, data, stride, n, mean) &
	         bind(c, name='gsl_stats_wvariance_with_fixed_mean')
	  import
	  real(c_double), intent(in) :: w(*), data(*)
	  integer(c_size_t), value :: wstride, stride, n
	  real(c_double), value :: mean
	  real(c_double) :: fgsl_stats_wvariance_with_fixed_mean
	end function fgsl_stats_wvariance_with_fixed_mean
	function fgsl_stats_wsd_with_fixed_mean(w, wstride, data, stride, n, mean) &
	         bind(c, name='gsl_stats_wsd_with_fixed_mean')
	  import
	  real(c_double), intent(in) :: w(*), data(*)
	  integer(c_size_t), value :: wstride, stride, n
	  real(c_double), value :: mean
	  real(c_double) :: fgsl_stats_wsd_with_fixed_mean
	end function fgsl_stats_wsd_with_fixed_mean
    function fgsl_stats_wabsdev(w, wstride, data, stride, n) bind(c, name='gsl_stats_wabsdev')
	  import
	  real(c_double), intent(in) :: w(*), data(*)
	  integer(c_size_t), value :: wstride, stride, n
	  real(c_double) :: fgsl_stats_wabsdev
    end function fgsl_stats_wabsdev
	function fgsl_stats_wabsdev_m(w, wstride, data, stride, n, mean) &
	         bind(c, name='gsl_stats_wabsdev_m')
	  import
	  real(c_double), intent(in) :: w(*), data(*)
      integer(c_size_t), value :: wstride, stride, n
	  real(c_double), value :: mean
	  real(c_double) :: fgsl_stats_wabsdev_m
	end function fgsl_stats_wabsdev_m
    function fgsl_stats_wskew(w, wstride, data, stride, n) &
             bind(c, name='gsl_stats_wskew')
	  import
	  real(c_double), intent(in) :: w(*), data(*)
	  integer(c_size_t), value :: wstride, stride, n
	  real(c_double) :: fgsl_stats_wskew
    end function fgsl_stats_wskew
    function fgsl_stats_wskew_m_sd(w, wstride, data, stride, n, mean, sd) &
	         bind(c, name='gsl_stats_wskew_m_sd')
	    import
	  real(c_double), intent(in) :: w(*), data(*)
	  integer(c_size_t), value :: wstride, stride, n
	  real(c_double), value :: mean, sd
	  real(c_double) :: fgsl_stats_wskew_m_sd
    end function fgsl_stats_wskew_m_sd
	function fgsl_stats_wkurtosis(w, wstride, data, stride, n) &
	         bind(c, name='gsl_stats_wkurtosis')
	  import
	  real(c_double), intent(in) :: w(*), data(*)
      integer(c_size_t), value :: wstride, stride, n
	  real(c_double) :: fgsl_stats_wkurtosis
	end function fgsl_stats_wkurtosis
	function fgsl_stats_wkurtosis_m_sd(w, wstride, data, stride, n, mean, sd) &
	         bind(c, name='gsl_stats_wkurtosis_m_sd')
	  import
	  real(c_double), intent(in) :: w(*), data(*)
	  integer(c_size_t), value :: wstride, stride, n
	  real(c_double), value :: mean, sd
	  real(c_double) :: fgsl_stats_wkurtosis_m_sd
	end function fgsl_stats_wkurtosis_m_sd
	!
	function fgsl_stats_max(data, stride, n) bind(c, name='gsl_stats_max')
	  import
	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  real(c_double) :: fgsl_stats_max
    end function fgsl_stats_max
	function fgsl_stats_min(data, stride, n) bind(c, name='gsl_stats_min')
	  import
	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  real(c_double) :: fgsl_stats_min
	end function fgsl_stats_min
	subroutine fgsl_stats_minmax(min, max, data, stride, n) bind(c, name='gsl_stats_minmax')
	  import
	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  real(c_double) :: min, max
	end subroutine fgsl_stats_minmax
    function fgsl_stats_max_index(data, stride, n) bind(c, name='gsl_stats_max_index')
	  import
	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  integer(c_size_t) :: fgsl_stats_max_index
	end function fgsl_stats_max_index
    function fgsl_stats_min_index(data, stride, n) bind(c, name='gsl_stats_min_index')
	  import
	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  integer(c_size_t) :: fgsl_stats_min_index
    end function fgsl_stats_min_index
	subroutine fgsl_stats_minmax_index(min_index, max_index, data, stride, n) &
	           bind(c, name='gsl_stats_minmax_index')
	  import
	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  integer(c_size_t) :: min_index, max_index
	end subroutine fgsl_stats_minmax_index
	!
	function fgsl_stats_median_from_sorted_data(data, stride, n) &
	         bind(c, name='gsl_stats_median_from_sorted_data')
	  import
	  real(c_double), intent(in) :: data(*)
	  integer(c_size_t), value :: stride, n
	  real(c_double) :: fgsl_stats_median_from_sorted_data
	end function fgsl_stats_median_from_sorted_data
	function fgsl_stats_median(data, stride, n) bind(c, name='gsl_stats_median')
	  import :: fgsl_size_t, fgsl_double
	  real(fgsl_double), dimension(*), intent(in) :: data
	  integer(fgsl_size_t), value :: stride, n
	  real(fgsl_double) :: fgsl_stats_median
	end function fgsl_stats_median
	function fgsl_stats_quantile_from_sorted_data(data, stride, n, f) &
	         bind(c, name='gsl_stats_quantile_from_sorted_data')
	  import
	  real(c_double), intent(in) :: data(*)
	  real(c_double), value :: f
	  integer(c_size_t), value :: stride, n
	  real(c_double) :: fgsl_stats_quantile_from_sorted_data
	end function fgsl_stats_quantile_from_sorted_data
	!
	subroutine fgsl_stats_select(data, stride, n, k) bind(c, name='gsl_stats_select')
	  import :: fgsl_size_t, fgsl_double
	  real(fgsl_double), dimension(*), intent(inout) :: data
	  integer(fgsl_size_t), value :: stride, n, k
	end subroutine fgsl_stats_select
	!
	function fgsl_stats_trmean_from_sorted_data(alpha, data, stride, n) &
	     bind(c, name='gsl_stats_trmean_from_sorted_data')
	  import :: fgsl_size_t, fgsl_double
	  real(fgsl_double), value :: alpha
	  real(fgsl_double), dimension(*), intent(in) :: data
	  integer(fgsl_size_t), value :: stride, n
	  real(fgsl_double) :: fgsl_stats_trmean_from_sorted_data
	end function fgsl_stats_trmean_from_sorted_data
	function fgsl_stats_gastwirth_from_sorted_data(data, stride, n) &
	         bind(c, name='gsl_stats_gastwirth_from_sorted_data')
	  import :: fgsl_size_t, fgsl_double
	  real(fgsl_double), dimension(*), intent(in) :: data
	  integer(fgsl_size_t), value :: stride, n
	  real(fgsl_double) :: fgsl_stats_gastwirth_from_sorted_data
    end function fgsl_stats_gastwirth_from_sorted_data
	function fgsl_stats_mad0(data, stride, n, work) bind(c, name='gsl_stats_mad0')
	  import :: fgsl_size_t, fgsl_double
	  real(fgsl_double), dimension(*), intent(in) :: data
	  integer(fgsl_size_t), value :: stride, n
	  real(fgsl_double), dimension(*), intent(inout) :: work
	  real(fgsl_double) :: fgsl_stats_mad0
	end function fgsl_stats_mad0
	function fgsl_stats_mad(data, stride, n, work) bind(c, name='gsl_stats_mad')
	  import :: fgsl_size_t, fgsl_double
	  real(fgsl_double), dimension(*), intent(in) :: data
	  integer(fgsl_size_t), value :: stride, n
	  real(fgsl_double), dimension(*), intent(inout) :: work
	  real(fgsl_double) :: fgsl_stats_mad
	end function fgsl_stats_mad
    function fgsl_stats_sn0_from_sorted_data(data, stride, n, work) &
	         bind(c, name='gsl_stats_Sn0_from_sorted_data')
	  import :: fgsl_size_t, fgsl_double
	  real(fgsl_double), dimension(*), intent(in) :: data
	  integer(fgsl_size_t), value :: stride, n
	  real(fgsl_double), dimension(*), intent(inout) :: work
	  real(fgsl_double) :: fgsl_stats_sn0_from_sorted_data
	end function fgsl_stats_sn0_from_sorted_data
	function fgsl_stats_sn_from_sorted_data(data, stride, n, work) &
	         bind(c, name='gsl_stats_Sn_from_sorted_data')
	  import :: fgsl_size_t, fgsl_double
      real(fgsl_double), dimension(*), intent(in) :: data
	  integer(fgsl_size_t), value :: stride, n
	  real(fgsl_double), dimension(*), intent(inout) :: work
	  real(fgsl_double) :: fgsl_stats_sn_from_sorted_data
	end function fgsl_stats_sn_from_sorted_data
	function fgsl_stats_qn0_from_sorted_data(data, stride, n, work, work_int) &
	       bind(c, name='gsl_stats_Qn0_from_sorted_data')
	  import :: fgsl_size_t, fgsl_double, fgsl_int
	  real(fgsl_double), dimension(*), intent(in) :: data
	  integer(fgsl_size_t), value :: stride, n
	  real(fgsl_double), dimension(*), intent(inout) :: work
	  integer(fgsl_int), dimension(*), intent(inout) :: work_int
	  real(fgsl_double) :: fgsl_stats_qn0_from_sorted_data
	end function fgsl_stats_qn0_from_sorted_data
	function fgsl_stats_qn_from_sorted_data(data, stride, n, work, work_int) &
	         bind(c, name='gsl_stats_Qn_from_sorted_data')
	  import :: fgsl_size_t, fgsl_double, fgsl_int
	  real(fgsl_double), dimension(*), intent(in) :: data
	  integer(fgsl_size_t), value :: stride, n
	  real(fgsl_double), dimension(*), intent(inout) :: work
	  integer(fgsl_int), dimension(*), intent(inout) :: work_int
	  real(fgsl_double) :: fgsl_stats_qn_from_sorted_data
    end function fgsl_stats_qn_from_sorted_data
  end interface
end module
