program statistics
  use fgsl
  use mod_unit
  implicit none
  integer(fgsl_size_t), parameter :: nsize = 50
  real(fgsl_double), parameter :: eps10 = 1.0d-10
  real(fgsl_double) :: s_array(nsize), s_arr2(nsize), w(nsize), wk(2*nsize)
  real(fgsl_double) :: s_mean, s_m2, s_var, s_kur, s_sd, s_skew, &
       s_auto, s_cov, s_cor, s_spear, xc, xv
  integer :: i
  integer(fgsl_size_t) :: mx, mn
!
! Test statistical routines
!
  call unit_init(200)
!
  s_array = (/ (dble(i), i=1, nsize) /)
  s_mean = fgsl_stats_mean(s_array,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_mean:1',&
       2.55d1,s_mean,eps10)
  s_mean = fgsl_stats_mean(s_array,2_fgsl_size_t,nsize/2_fgsl_size_t)
  call unit_assert_equal_within('fgsl_stats_mean:2',&
       2.5d1,s_mean,eps10)
  s_var = fgsl_stats_variance(s_array,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_variance',&
       2.125d2,s_var,eps10)
  s_var = fgsl_stats_variance_m(s_array,1_fgsl_size_t,nsize,2.55d1)
  call unit_assert_equal_within('fgsl_stats_variance_m',&
       2.125d2,s_var,eps10)
  s_sd = fgsl_stats_sd(s_array,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_sd',&
       sqrt(2.125d2),s_sd,eps10)
  s_sd = fgsl_stats_sd_m(s_array,1_fgsl_size_t,nsize,2.55d1)
  call unit_assert_equal_within('fgsl_stats_sd_m',&
       sqrt(2.125d2),s_sd,eps10)
  s_var = fgsl_stats_variance_with_fixed_mean(s_array,1_fgsl_size_t,&
       nsize,2.55d1)
  call unit_assert_equal_within('fgsl_stats_variance_with_fixed_mean',&
       2.0825d2,s_var,eps10)
  s_sd = fgsl_stats_sd_with_fixed_mean(s_array,1_fgsl_size_t,&
       nsize,2.55d1)
  call unit_assert_equal_within('fgsl_stats_sd_with_fixed_mean',&
       sqrt(2.0825d2),s_sd,eps10)
  s_var = fgsl_stats_absdev(s_array,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_absdev',&
       1.25d1,s_var,eps10)
  s_var = fgsl_stats_absdev_m(s_array,1_fgsl_size_t,nsize,2.55d1)
  call unit_assert_equal_within('fgsl_stats_absdev',&
       1.25d1,s_var,eps10)
  s_array = (/ (dble(i)**2, i=1, nsize) /)
  s_mean = fgsl_stats_mean(s_array,1_fgsl_size_t,nsize)
  s_sd = fgsl_stats_sd(s_array,1_fgsl_size_t,nsize)
  xc = 0.0d0
  do i=1,nsize
     xc = xc + ((dble(i)**2 - s_mean)/s_sd)**3
  end do
  xc = xc/dble(nsize)
  s_skew = fgsl_stats_skew(s_array,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_skew',&
       xc,s_skew,eps10)
  s_skew = fgsl_stats_skew_m_sd(s_array,1_fgsl_size_t,nsize,s_mean,s_sd)
  call unit_assert_equal_within('fgsl_stats_skew_m_sd',&
       xc,s_skew,eps10)
  xc = 0.0d0
  do i=1,nsize
     xc = xc + ((dble(i)**2 - s_mean)/s_sd)**4
  end do
  xc = xc/dble(nsize) - 3.0d0
  s_kur = fgsl_stats_kurtosis(s_array,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_kurtosis',&
       xc,s_kur,eps10)
  s_kur = fgsl_stats_kurtosis_m_sd(s_array,1_fgsl_size_t,nsize,s_mean,s_sd)
  call unit_assert_equal_within('fgsl_stats_kurtosis_m_sd',&
       xc,s_kur,eps10)
  xc = 0.0d0; xv = 0.0d0
  do i=2,nsize
     xc = xc + (dble(i)**2 - s_mean)*(dble(i-1)**2 - s_mean)
     xv = xv + (dble(i)**2 - s_mean)**2
  end do
  xv = xv + (1.0d0-s_mean)**2
  xc = xc / xv
  s_auto = fgsl_stats_lag1_autocorrelation(s_array,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_lag1_autocorrelation',&
       xc,s_auto,eps10)
  s_auto = fgsl_stats_lag1_autocorrelation_m(s_array,1_fgsl_size_t, &
       nsize,s_mean)
  call unit_assert_equal_within('fgsl_stats_lag1_autocorrelation_m',&
       xc,s_auto,eps10)
  s_arr2 = (/ (dble(i), i=1, nsize) /)
  s_m2 = fgsl_stats_mean(s_arr2,1_fgsl_size_t,nsize)
  xc = 0.0d0
  do i=1,nsize
     xc = xc + (dble(i)**2 - s_mean) * (dble(i) - s_m2)
  end do
  xc = xc / dble(nsize-1)
  s_cov = fgsl_stats_covariance(s_array,1_fgsl_size_t,s_arr2,1_fgsl_size_t, &
       nsize)
  call unit_assert_equal_within('fgsl_stats_covariance',&
       xc,s_cov,eps10)
  s_cov = fgsl_stats_covariance_m(s_array,1_fgsl_size_t,s_arr2, &
       1_fgsl_size_t, nsize, s_mean, s_m2)
  call unit_assert_equal_within('fgsl_stats_covariance_m',&
       xc,s_cov,eps10)
!
  s_cor = fgsl_stats_correlation(s_array,1_fgsl_size_t,s_arr2, &
       1_fgsl_size_t, nsize)
  call unit_assert_equal_within('fgsl_stats_correlation',&
       0.9694696278887304786d0,s_cor,eps10)
!
  s_array = (/ (cos(dble(i)), i=1, nsize) /)
  s_arr2 = (/ (dble(i)**1.2, i=1, nsize) /)
  s_spear = fgsl_stats_spearman(s_array,1_fgsl_size_t,s_arr2,1_fgsl_size_t, &
       nsize, wk)
  call unit_assert_equal_within('fgsl_stats_spearman',&
       4.5858343337334934D-02,s_spear,eps10)

!
  s_array = (/ (dble(i)**2, i=1, nsize) /)
  s_arr2 = (/ (dble(i), i=1, nsize) /)
  w = (/ (0.43d0, i=1, nsize) /)
  s_mean = fgsl_stats_wmean(w,1_fgsl_size_t,s_arr2,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_wmean',&
       2.55d1,s_mean,eps10)
  s_var = fgsl_stats_wvariance(w,1_fgsl_size_t,s_arr2,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_wvariance',&
       2.125d2,s_var,eps10)
  s_var = fgsl_stats_wvariance_m(w,1_fgsl_size_t,s_arr2,1_fgsl_size_t,&
       nsize,s_mean)
  call unit_assert_equal_within('fgsl_stats_wvariance_m',&
       2.125d2,s_var,eps10)
  s_sd = fgsl_stats_wsd(w,1_fgsl_size_t,s_arr2,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_wsd',&
       sqrt(2.125d2),s_sd,eps10)
  s_sd = fgsl_stats_wsd_m(w,1_fgsl_size_t,s_arr2,1_fgsl_size_t,nsize,s_mean)
  call unit_assert_equal_within('fgsl_stats_wsd_m',&
       sqrt(2.125d2),s_sd,eps10)
  s_var = fgsl_stats_wvariance_with_fixed_mean(w,1_fgsl_size_t,s_arr2, &
       1_fgsl_size_t,nsize,2.55d1)
  call unit_assert_equal_within('fgsl_stats_wvariance_with_fixed_mean',&
       2.0825d2,s_var,eps10)
  s_sd = fgsl_stats_wsd_with_fixed_mean(w,1_fgsl_size_t,s_arr2,1_fgsl_size_t,&
       nsize,2.55d1)
  call unit_assert_equal_within('fgsl_stats_wsd_with_fixed_mean',&
       sqrt(2.0825d2),s_sd,eps10)
  s_var = fgsl_stats_wabsdev(w,1_fgsl_size_t,s_arr2,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_wabsdev',&
       1.25d1,s_var,eps10)
  s_var = fgsl_stats_wabsdev_m(w,1_fgsl_size_t,s_arr2,1_fgsl_size_t,&
       nsize,s_mean)
  call unit_assert_equal_within('fgsl_stats_wabsdev_m',&
       1.25d1,s_var,eps10)
  s_mean = fgsl_stats_mean(s_array,1_fgsl_size_t,nsize)
  s_sd = fgsl_stats_sd(s_array,1_fgsl_size_t,nsize)
  xc = 0.0d0
  do i=1,nsize
     xc = xc + ((dble(i)**2 - s_mean)/s_sd)**3
  end do
  xc = xc/dble(nsize)
  s_skew = fgsl_stats_wskew(w,1_fgsl_size_t,s_array,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_wskew',&
       xc,s_skew,eps10)
  s_skew = fgsl_stats_wskew_m_sd(w,1_fgsl_size_t,s_array,1_fgsl_size_t,&
       nsize,s_mean,s_sd)
  call unit_assert_equal_within('fgsl_stats_wskew_m_sd',&
       xc,s_skew,eps10)
    xc = 0.0d0
  do i=1,nsize
     xc = xc + ((dble(i)**2 - s_mean)/s_sd)**4
  end do
  xc = xc/dble(nsize) - 3.0d0
  s_kur = fgsl_stats_wkurtosis(w,1_fgsl_size_t,s_array,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_wkurtosis',&
       xc,s_kur,eps10)
  s_kur = fgsl_stats_wkurtosis_m_sd(w,1_fgsl_size_t,s_array,1_fgsl_size_t, &
       nsize,s_mean,s_sd)
  call unit_assert_equal_within('fgsl_stats_wkurtosis_m_sd',&
       xc,s_kur,eps10)
  xc = fgsl_stats_max(s_array,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_max',&
       maxval(s_array),xc,eps10)
  xc = fgsl_stats_min(s_array,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_min',&
       minval(s_array),xc,eps10)
  call fgsl_stats_minmax(xc, xv, s_array,1_fgsl_size_t,nsize)
  call unit_assert_equal_within('fgsl_stats_minmax',&
       maxval(s_array),xv,eps10)
  call unit_assert_equal_within('fgsl_stats_minmax',&
       minval(s_array),xc,eps10)
! NOTE: zero-based counting
  i = fgsl_stats_max_index(s_array,1_fgsl_size_t,nsize)
  call unit_assert_equal('fgsl_stats_max_index',int(nsize-1),i)
  i = fgsl_stats_min_index(s_array,1_fgsl_size_t,nsize)
  call unit_assert_equal('fgsl_stats_min_index',0,i)
  call fgsl_stats_minmax_index(mn, mx, s_array,1_fgsl_size_t,nsize)
  call unit_assert_equal('fgsl_stats_minmax_index',0,int(mn))
  call unit_assert_equal('fgsl_stats_minmax_index',int(nsize-1),int(mx))
!
  xv = fgsl_stats_median_from_sorted_data(s_array,1_fgsl_size_t,nsize)
  xc = 0.5d0*(s_array(25)+s_array(26))
  call unit_assert_equal_within('fgsl_stats_median_from_sorted_data',&
       xc,xv,eps10)
  xv = fgsl_stats_quantile_from_sorted_data(s_array,1_fgsl_size_t,nsize,0.5d0)
  call unit_assert_equal_within('fgsl_stats_quantile_from_sorted_data',&
       xc,xv,eps10)



!
! Done
!
  call unit_finalize()
end program statistics
