module fgsl_statistics
  !> Statistics
  !> Note: only the double precision specifics are accessible
  use fgsl_base
  implicit none
  
  private :: gsl_stats_mean, gsl_stats_variance, gsl_stats_variance_m, &
     gsl_stats_sd, gsl_stats_sd_m, gsl_stats_variance_with_fixed_mean, &
     gsl_stats_sd_with_fixed_mean, gsl_stats_absdev, gsl_stats_absdev_m, &
     gsl_stats_skew, gsl_stats_skew_m_sd, gsl_stats_kurtosis, &
     gsl_stats_kurtosis_m_sd, gsl_stats_lag1_autocorrelation, &
     gsl_stats_lag1_autocorrelation_m, gsl_stats_covariance, &
     gsl_stats_covariance_m, gsl_stats_correlation, gsl_stats_spearman, &
     gsl_stats_wmean, gsl_stats_wvariance, gsl_stats_wvariance_m, &
     gsl_stats_wsd, gsl_stats_wsd_m, gsl_stats_wvariance_with_fixed_mean, &
     gsl_stats_wsd_with_fixed_mean, gsl_stats_wabsdev, gsl_stats_wabsdev_m, &
     gsl_stats_wskew, gsl_stats_wskew_m_sd, gsl_stats_wkurtosis, &
     gsl_stats_wkurtosis_m_sd, gsl_stats_max, gsl_stats_min, &
     gsl_stats_minmax, gsl_stats_max_index, gsl_stats_min_index, &
     gsl_stats_minmax_index, gsl_stats_median_from_sorted_data, &
     gsl_stats_quantile_from_sorted_data
  
  !
  ! C interfaces
! FIXME: most of the below should be directly mapped
  interface
	  function gsl_stats_mean (data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double) :: gsl_stats_mean
	  end function gsl_stats_mean
	  function gsl_stats_variance (data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double) :: gsl_stats_variance
	  end function gsl_stats_variance
	  function gsl_stats_variance_m (data, stride, n, mean) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double), value :: mean
	    real(c_double) :: gsl_stats_variance_m
	  end function gsl_stats_variance_m
	  function gsl_stats_sd (data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double) :: gsl_stats_sd
	  end function gsl_stats_sd
	  function gsl_stats_sd_m (data, stride, n, mean) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double), value :: mean
	    real(c_double) :: gsl_stats_sd_m
	  end function gsl_stats_sd_m
	  function gsl_stats_variance_with_fixed_mean (data, stride, n, mean) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double), value :: mean
	    real(c_double) :: gsl_stats_variance_with_fixed_mean
	  end function gsl_stats_variance_with_fixed_mean
	  function gsl_stats_sd_with_fixed_mean (data, stride, n, mean) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double), value :: mean
	    real(c_double) :: gsl_stats_sd_with_fixed_mean
	  end function gsl_stats_sd_with_fixed_mean
	  function gsl_stats_absdev (data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double) :: gsl_stats_absdev
	  end function gsl_stats_absdev
	  function gsl_stats_absdev_m (data, stride, n, mean) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double), value :: mean
	    real(c_double) :: gsl_stats_absdev_m
	  end function gsl_stats_absdev_m
	  function gsl_stats_skew (data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double) :: gsl_stats_skew
	  end function gsl_stats_skew
	  function gsl_stats_skew_m_sd (data, stride, n, mean, sd) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double), value :: mean, sd
	    real(c_double) :: gsl_stats_skew_m_sd
	  end function gsl_stats_skew_m_sd
	  function gsl_stats_kurtosis (data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double) :: gsl_stats_kurtosis
	  end function gsl_stats_kurtosis
	  function gsl_stats_kurtosis_m_sd (data, stride, n, mean, sd) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double), value :: mean, sd
	    real(c_double) :: gsl_stats_kurtosis_m_sd
	  end function gsl_stats_kurtosis_m_sd
	  function gsl_stats_lag1_autocorrelation (data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double) :: gsl_stats_lag1_autocorrelation
	  end function gsl_stats_lag1_autocorrelation
	  function gsl_stats_lag1_autocorrelation_m (data, stride, n, mean) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double), value :: mean
	    real(c_double) :: gsl_stats_lag1_autocorrelation_m
	  end function gsl_stats_lag1_autocorrelation_m
	  function gsl_stats_covariance(data1, stride1, data2, stride2, n) bind(c)
	    import
	    type(c_ptr), value :: data1, data2
	    integer(c_size_t), value :: stride1, stride2, n
	    real(c_double) :: gsl_stats_covariance
	  end function gsl_stats_covariance
	  function gsl_stats_covariance_m(data1, stride1, data2, stride2, n, &
	       mean1, mean2) bind(c)
	    import
	    type(c_ptr), value :: data1, data2
	    integer(c_size_t), value :: stride1, stride2, n
	    real(c_double), value :: mean1, mean2
	    real(c_double) :: gsl_stats_covariance_m
	  end function gsl_stats_covariance_m
	  function gsl_stats_correlation(data1, stride1, data2, stride2, n) bind(c)
	    import
	    type(c_ptr), value :: data1, data2
	    integer(c_size_t), value :: stride1, stride2, n
	    real(c_double) :: gsl_stats_correlation
	  end function gsl_stats_correlation
	  function gsl_stats_spearman(data1, stride1, data2, stride2, n, work) bind(c)
	    import
	    type(c_ptr), value :: data1, data2
	    type(c_ptr), value :: work
	    integer(c_size_t), value :: stride1, stride2, n
	    real(c_double) :: gsl_stats_spearman
	  end function gsl_stats_spearman
	  function gsl_stats_wmean(w, wstride, data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: w, data
	    integer(c_size_t), value :: wstride, stride, n
	    real(c_double) :: gsl_stats_wmean
	  end function gsl_stats_wmean
	  function gsl_stats_wvariance(w, wstride, data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: w, data
	    integer(c_size_t), value :: wstride, stride, n
	    real(c_double) :: gsl_stats_wvariance
	  end function gsl_stats_wvariance
	  function gsl_stats_wvariance_m(w, wstride, data, stride, n, mean) bind(c)
	    import
	    type(c_ptr), value :: w, data
	    integer(c_size_t), value :: wstride, stride, n
	    real(c_double), value :: mean
	    real(c_double) :: gsl_stats_wvariance_m
	  end function gsl_stats_wvariance_m
	  function gsl_stats_wsd(w, wstride, data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: w, data
	    integer(c_size_t), value :: wstride, stride, n
	    real(c_double) :: gsl_stats_wsd
	  end function gsl_stats_wsd
	  function gsl_stats_wsd_m(w, wstride, data, stride, n, mean) bind(c)
	    import
	    type(c_ptr), value :: w, data
	    integer(c_size_t), value :: wstride, stride, n
	    real(c_double), value :: mean
	    real(c_double) :: gsl_stats_wsd_m
	  end function gsl_stats_wsd_m
	  function gsl_stats_wvariance_with_fixed_mean(w, wstride, data, stride, n, mean) bind(c)
	    import
	    type(c_ptr), value :: w, data
	    integer(c_size_t), value :: wstride, stride, n
	    real(c_double), value :: mean
	    real(c_double) :: gsl_stats_wvariance_with_fixed_mean
	  end function gsl_stats_wvariance_with_fixed_mean
	  function gsl_stats_wsd_with_fixed_mean(w, wstride, data, stride, n, mean) bind(c)
	    import
	    type(c_ptr), value :: w, data
	    integer(c_size_t), value :: wstride, stride, n
	    real(c_double), value :: mean
	    real(c_double) :: gsl_stats_wsd_with_fixed_mean
	  end function gsl_stats_wsd_with_fixed_mean
	  function gsl_stats_wabsdev(w, wstride, data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: w, data
	    integer(c_size_t), value :: wstride, stride, n
	    real(c_double) :: gsl_stats_wabsdev
	  end function gsl_stats_wabsdev
	  function gsl_stats_wabsdev_m(w, wstride, data, stride, n, mean) bind(c)
	    import
	    type(c_ptr), value :: w, data
	    integer(c_size_t), value :: wstride, stride, n
	    real(c_double), value :: mean
	    real(c_double) :: gsl_stats_wabsdev_m
	  end function gsl_stats_wabsdev_m
	  function gsl_stats_wskew(w, wstride, data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: w, data
	    integer(c_size_t), value :: wstride, stride, n
	    real(c_double) :: gsl_stats_wskew
	  end function gsl_stats_wskew
	  function gsl_stats_wskew_m_sd(w, wstride, data, stride, n, mean, sd) bind(c)
	    import
	    type(c_ptr), value :: w, data
	    integer(c_size_t), value :: wstride, stride, n
	    real(c_double), value :: mean, sd
	    real(c_double) :: gsl_stats_wskew_m_sd
	  end function gsl_stats_wskew_m_sd
	  function gsl_stats_wkurtosis(w, wstride, data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: w, data
	    integer(c_size_t), value :: wstride, stride, n
	    real(c_double) :: gsl_stats_wkurtosis
	  end function gsl_stats_wkurtosis
	  function gsl_stats_wkurtosis_m_sd(w, wstride, data, stride, n, mean, sd) bind(c)
	    import
	    type(c_ptr), value :: w, data
	    integer(c_size_t), value :: wstride, stride, n
	    real(c_double), value :: mean, sd
	    real(c_double) :: gsl_stats_wkurtosis_m_sd
	  end function gsl_stats_wkurtosis_m_sd
	  function gsl_stats_max(data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double) :: gsl_stats_max
	  end function gsl_stats_max
	  function gsl_stats_min(data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double) :: gsl_stats_min
	  end function gsl_stats_min
	  subroutine gsl_stats_minmax(min, max, data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double) :: min, max
	  end subroutine gsl_stats_minmax
	  function gsl_stats_max_index(data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    integer(c_size_t) :: gsl_stats_max_index
	  end function gsl_stats_max_index
	  function gsl_stats_min_index(data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    integer(c_size_t) :: gsl_stats_min_index
	  end function gsl_stats_min_index
	  subroutine gsl_stats_minmax_index(min_index, max_index, data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    integer(c_size_t) :: min_index, max_index
	  end subroutine gsl_stats_minmax_index
	  function gsl_stats_median_from_sorted_data(data, stride, n) bind(c)
	    import
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    real(c_double) :: gsl_stats_median_from_sorted_data
	  end function gsl_stats_median_from_sorted_data
	  function fgsl_stats_median(data, stride, n) bind(c, name='gsl_stats_median')
	    import :: fgsl_size_t, fgsl_double
	    real(fgsl_double), dimension(*), intent(in) :: data
	    integer(fgsl_size_t), value :: stride, n
	    real(fgsl_double) :: fgsl_stats_median
	  end function fgsl_stats_median
	  function gsl_stats_quantile_from_sorted_data(data, stride, n, f) bind(c)
	    import
	    type(c_ptr), value :: data
	    real(c_double), value :: f
	    integer(c_size_t), value :: stride, n
	    real(c_double) :: gsl_stats_quantile_from_sorted_data
	  end function gsl_stats_quantile_from_sorted_data
	  subroutine fgsl_stats_select(data, stride, n, k) bind(c, name='gsl_stats_select')
	    import :: fgsl_size_t, fgsl_double
	    real(fgsl_double), dimension(*), intent(inout) :: data
	    integer(fgsl_size_t), value :: stride, n, k
	  end subroutine fgsl_stats_select
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
contains
  function fgsl_stats_mean(data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double) :: fgsl_stats_mean
    fgsl_stats_mean = gsl_stats_mean(c_loc(data), stride, n)
  end function fgsl_stats_mean
  function fgsl_stats_variance(data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double) :: fgsl_stats_variance
    fgsl_stats_variance = gsl_stats_variance(c_loc(data), stride, n)
  end function fgsl_stats_variance
  function fgsl_stats_variance_m(data, stride, n, mean)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double), intent(in) :: mean
    real(fgsl_double) :: fgsl_stats_variance_m
    fgsl_stats_variance_m = gsl_stats_variance_m(c_loc(data), stride, n, mean)
  end function fgsl_stats_variance_m
  function fgsl_stats_sd(data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double) :: fgsl_stats_sd
    fgsl_stats_sd = gsl_stats_sd(c_loc(data), stride, n)
  end function fgsl_stats_sd
  function fgsl_stats_sd_m(data, stride, n, mean)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double), intent(in) :: mean
    real(fgsl_double) :: fgsl_stats_sd_m
    fgsl_stats_sd_m = gsl_stats_sd_m(c_loc(data), stride, n, mean)
  end function fgsl_stats_sd_m
  function fgsl_stats_variance_with_fixed_mean(data, stride, n, mean)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double), intent(in) :: mean
    real(fgsl_double) :: fgsl_stats_variance_with_fixed_mean
    fgsl_stats_variance_with_fixed_mean = &
         gsl_stats_variance_with_fixed_mean(c_loc(data), stride, n, mean)
  end function fgsl_stats_variance_with_fixed_mean
  function fgsl_stats_sd_with_fixed_mean(data, stride, n, mean)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double), intent(in) :: mean
    real(fgsl_double) :: fgsl_stats_sd_with_fixed_mean
    fgsl_stats_sd_with_fixed_mean = &
         gsl_stats_sd_with_fixed_mean(c_loc(data), stride, n, mean)
  end function fgsl_stats_sd_with_fixed_mean
  function fgsl_stats_absdev(data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double) :: fgsl_stats_absdev
    fgsl_stats_absdev = gsl_stats_absdev(c_loc(data), stride, n)
  end function fgsl_stats_absdev
  function fgsl_stats_absdev_m(data, stride, n, mean)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double), intent(in) :: mean
    real(fgsl_double) :: fgsl_stats_absdev_m
    fgsl_stats_absdev_m = gsl_stats_absdev_m(c_loc(data), stride, n, mean)
  end function fgsl_stats_absdev_m
  function fgsl_stats_skew(data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double) :: fgsl_stats_skew
    fgsl_stats_skew = gsl_stats_skew(c_loc(data), stride, n)
  end function fgsl_stats_skew
  function fgsl_stats_skew_m_sd(data, stride, n, mean, sd)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double), intent(in) :: mean, sd
    real(fgsl_double) :: fgsl_stats_skew_m_sd
    fgsl_stats_skew_m_sd = gsl_stats_skew_m_sd(c_loc(data), stride, n, mean, sd)
  end function fgsl_stats_skew_m_sd
  function fgsl_stats_kurtosis(data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double) :: fgsl_stats_kurtosis
    fgsl_stats_kurtosis = gsl_stats_kurtosis(c_loc(data), stride, n)
  end function fgsl_stats_kurtosis
  function fgsl_stats_kurtosis_m_sd(data, stride, n, mean, sd)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double), intent(in) :: mean, sd
    real(fgsl_double) :: fgsl_stats_kurtosis_m_sd
    fgsl_stats_kurtosis_m_sd = gsl_stats_kurtosis_m_sd(c_loc(data), stride, n, mean, sd)
  end function fgsl_stats_kurtosis_m_sd
  function fgsl_stats_lag1_autocorrelation(data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double) :: fgsl_stats_lag1_autocorrelation
    fgsl_stats_lag1_autocorrelation = gsl_stats_lag1_autocorrelation(c_loc(data), stride, n)
  end function fgsl_stats_lag1_autocorrelation
  function fgsl_stats_lag1_autocorrelation_m(data, stride, n, mean)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double), intent(in) :: mean
    real(fgsl_double) :: fgsl_stats_lag1_autocorrelation_m
    fgsl_stats_lag1_autocorrelation_m = &
         gsl_stats_lag1_autocorrelation_m(c_loc(data), stride, n, mean)
  end function fgsl_stats_lag1_autocorrelation_m
  function fgsl_stats_covariance(data1, stride1, data2, stride2, n)
    real(fgsl_double), intent(in), target, contiguous :: data1(:), data2(:)
    integer(fgsl_size_t), intent(in) :: stride1, stride2, n
    real(fgsl_double) :: fgsl_stats_covariance
    fgsl_stats_covariance = gsl_stats_covariance(c_loc(data1), stride1, c_loc(data2), stride2, n)
  end function fgsl_stats_covariance
  function fgsl_stats_covariance_m(data1, stride1, data2, stride2, n, mean1, mean2)
    real(fgsl_double), intent(in), target, contiguous :: data1(:), data2(:)
    integer(fgsl_size_t), intent(in) :: stride1, stride2, n
    real(fgsl_double), intent(in) :: mean1, mean2
    real(fgsl_double) :: fgsl_stats_covariance_m
    fgsl_stats_covariance_m = &
         gsl_stats_covariance_m(c_loc(data1), stride1, c_loc(data2), stride2, n, mean1, mean2)
  end function fgsl_stats_covariance_m
  function fgsl_stats_correlation(data1, stride1, data2, stride2, n)
    real(fgsl_double), intent(in), target, contiguous :: data1(:), data2(:)
    integer(fgsl_size_t), intent(in) :: stride1, stride2, n
    real(fgsl_double) :: fgsl_stats_correlation
    fgsl_stats_correlation = gsl_stats_correlation(c_loc(data1), stride1, c_loc(data2), stride2, n)
  end function fgsl_stats_correlation
  function fgsl_stats_spearman(data1, stride1, data2, stride2, n, work)
    real(fgsl_double), intent(in), target, contiguous :: data1(:), data2(:)
    real(fgsl_double), intent(inout), target, contiguous :: work(:)
    integer(fgsl_size_t), intent(in) :: stride1, stride2, n
    real(fgsl_double) :: fgsl_stats_spearman
    fgsl_stats_spearman = gsl_stats_spearman(c_loc(data1), stride1, c_loc(data2), stride2, n, c_loc(work))
  end function fgsl_stats_spearman
  function fgsl_stats_wmean(w, wstride, data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: w(:), data(:)
    integer(fgsl_size_t), intent(in) :: wstride, stride, n
    real(fgsl_double) :: fgsl_stats_wmean
    fgsl_stats_wmean = gsl_stats_wmean(c_loc(w), wstride, c_loc(data), stride, n)
  end function fgsl_stats_wmean
  function fgsl_stats_wvariance(w, wstride, data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: w(:), data(:)
    integer(fgsl_size_t), intent(in) :: wstride, stride, n
    real(fgsl_double) :: fgsl_stats_wvariance
    fgsl_stats_wvariance = gsl_stats_wvariance(c_loc(w), wstride, c_loc(data), stride, n)
  end function fgsl_stats_wvariance
  function fgsl_stats_wvariance_m(w, wstride, data, stride, n, mean)
    real(fgsl_double), intent(in), target, contiguous :: w(:), data(:)
    integer(fgsl_size_t), intent(in) :: wstride, stride, n
    real(fgsl_double), intent(in) :: mean
    real(fgsl_double) :: fgsl_stats_wvariance_m
    fgsl_stats_wvariance_m = gsl_stats_wvariance_m(c_loc(w), wstride, c_loc(data), stride, n, mean)
  end function fgsl_stats_wvariance_m
  function fgsl_stats_wsd(w, wstride, data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: w(:), data(:)
    integer(fgsl_size_t), intent(in) :: wstride, stride, n
    real(fgsl_double) :: fgsl_stats_wsd
    fgsl_stats_wsd = gsl_stats_wsd(c_loc(w), wstride, c_loc(data), stride, n)
  end function fgsl_stats_wsd
  function fgsl_stats_wsd_m(w, wstride, data, stride, n, mean)
    real(fgsl_double), intent(in), target, contiguous :: w(:), data(:)
    integer(fgsl_size_t), intent(in) :: wstride, stride, n
    real(fgsl_double), intent(in) :: mean
    real(fgsl_double) :: fgsl_stats_wsd_m
    fgsl_stats_wsd_m = gsl_stats_wsd_m(c_loc(w), wstride, c_loc(data), stride, n, mean)
  end function fgsl_stats_wsd_m
  function fgsl_stats_wvariance_with_fixed_mean(w, wstride, data, stride, n, mean)
    real(fgsl_double), intent(in), target, contiguous :: w(:), data(:)
    integer(fgsl_size_t), intent(in) :: wstride, stride, n
    real(fgsl_double), intent(in) :: mean
    real(fgsl_double) :: fgsl_stats_wvariance_with_fixed_mean
    fgsl_stats_wvariance_with_fixed_mean = &
         gsl_stats_wvariance_with_fixed_mean(c_loc(w), wstride, c_loc(data), stride, n, mean)
  end function fgsl_stats_wvariance_with_fixed_mean
  function fgsl_stats_wsd_with_fixed_mean(w, wstride, data, stride, n, mean)
    real(fgsl_double), intent(in), target, contiguous :: w(:), data(:)
    integer(fgsl_size_t), intent(in) :: wstride, stride, n
    real(fgsl_double), intent(in) :: mean
    real(fgsl_double) :: fgsl_stats_wsd_with_fixed_mean
    fgsl_stats_wsd_with_fixed_mean = &
         gsl_stats_wsd_with_fixed_mean(c_loc(w), wstride, c_loc(data), stride, n, mean)
  end function fgsl_stats_wsd_with_fixed_mean
  function fgsl_stats_wabsdev(w, wstride, data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: w(:), data(:)
    integer(fgsl_size_t), intent(in) :: wstride, stride, n
    real(fgsl_double) :: fgsl_stats_wabsdev
    fgsl_stats_wabsdev = gsl_stats_wabsdev(c_loc(w), wstride, c_loc(data), stride, n)
  end function fgsl_stats_wabsdev
  function fgsl_stats_wabsdev_m(w, wstride, data, stride, n, mean)
    real(fgsl_double), intent(in), target, contiguous :: w(:), data(:)
    integer(fgsl_size_t), intent(in) :: wstride, stride, n
    real(fgsl_double), intent(in) :: mean
    real(fgsl_double) :: fgsl_stats_wabsdev_m
    fgsl_stats_wabsdev_m = gsl_stats_wabsdev_m(c_loc(w), wstride, c_loc(data), stride, n, mean)
  end function fgsl_stats_wabsdev_m
  function fgsl_stats_wskew(w, wstride, data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: w(:), data(:)
    integer(fgsl_size_t), intent(in) :: wstride, stride, n
    real(fgsl_double) :: fgsl_stats_wskew
    fgsl_stats_wskew = gsl_stats_wskew(c_loc(w), wstride, c_loc(data), stride, n)
  end function fgsl_stats_wskew
  function fgsl_stats_wskew_m_sd(w, wstride, data, stride, n, mean, sd)
    real(fgsl_double), intent(in), target, contiguous :: w(:), data(:)
    integer(fgsl_size_t), intent(in) :: wstride, stride, n
    real(fgsl_double), intent(in) :: mean, sd
    real(fgsl_double) :: fgsl_stats_wskew_m_sd
    fgsl_stats_wskew_m_sd = gsl_stats_wskew_m_sd(c_loc(w), wstride, c_loc(data), stride, n, mean, sd)
  end function fgsl_stats_wskew_m_sd
  function fgsl_stats_wkurtosis(w, wstride, data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: w(:), data(:)
    integer(fgsl_size_t), intent(in) :: wstride, stride, n
    real(fgsl_double) :: fgsl_stats_wkurtosis
    fgsl_stats_wkurtosis = gsl_stats_wkurtosis(c_loc(w), wstride, c_loc(data), stride, n)
  end function fgsl_stats_wkurtosis
  function fgsl_stats_wkurtosis_m_sd(w, wstride, data, stride, n, mean, sd)
    real(fgsl_double), intent(in), target, contiguous :: w(:), data(:)
    integer(fgsl_size_t), intent(in) :: wstride, stride, n
    real(fgsl_double), intent(in) :: mean, sd
    real(fgsl_double) :: fgsl_stats_wkurtosis_m_sd
    fgsl_stats_wkurtosis_m_sd = &
         gsl_stats_wkurtosis_m_sd(c_loc(w), wstride, c_loc(data), stride, n, mean, sd)
  end function fgsl_stats_wkurtosis_m_sd
  function fgsl_stats_max(data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double) :: fgsl_stats_max
    fgsl_stats_max = gsl_stats_max (c_loc(data), stride, n)
  end function fgsl_stats_max
  function fgsl_stats_min(data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double) :: fgsl_stats_min
    fgsl_stats_min = gsl_stats_min (c_loc(data), stride, n)
  end function fgsl_stats_min
  subroutine fgsl_stats_minmax(min, max, data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double), intent(out) :: min, max
    call gsl_stats_minmax(min, max, c_loc(data), stride, n)
  end subroutine fgsl_stats_minmax
  function fgsl_stats_max_index(data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_size_t) :: fgsl_stats_max_index
    fgsl_stats_max_index = gsl_stats_max_index(c_loc(data), stride, n)
  end function fgsl_stats_max_index
  function fgsl_stats_min_index(data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_size_t) :: fgsl_stats_min_index
    fgsl_stats_min_index = gsl_stats_min_index(c_loc(data), stride, n)
  end function fgsl_stats_min_index
  subroutine fgsl_stats_minmax_index(min_index, max_index, data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_size_t), intent(out) :: min_index, max_index
    call gsl_stats_minmax_index(min_index, max_index, c_loc(data), stride, n)
  end subroutine fgsl_stats_minmax_index
  function fgsl_stats_median_from_sorted_data(data, stride, n)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double) :: fgsl_stats_median_from_sorted_data
    fgsl_stats_median_from_sorted_data = &
         gsl_stats_median_from_sorted_data(c_loc(data), stride, n)
  end function fgsl_stats_median_from_sorted_data
  function fgsl_stats_quantile_from_sorted_data(data, stride, n, f)
    real(fgsl_double), intent(in), target, contiguous :: data(:)
    real(fgsl_double), intent(in) :: f
    integer(fgsl_size_t), intent(in) :: stride, n
    real(fgsl_double) :: fgsl_stats_quantile_from_sorted_data
    fgsl_stats_quantile_from_sorted_data = &
         gsl_stats_quantile_from_sorted_data(c_loc(data), stride, n, f)
  end function fgsl_stats_quantile_from_sorted_data

end module
