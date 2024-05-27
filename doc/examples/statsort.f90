program statsort
  use fgsl
  implicit none
  real(fgsl_double) :: data(5) = (/17.2D0, 18.1D0, 16.5D0, 18.3D0, 12.6D0 /)
  real(fgsl_double) :: median, upperq, lowerq
!
  write(*, '(''Original dataset: '',5(F9.5))') data
  call fgsl_sort(data, 1_fgsl_size_t, 5_fgsl_size_t)
  write(*, '(''Sorted dataset: '',5(F9.5))') data
!
  median = fgsl_stats_median_from_sorted_data( &
       data, 1_fgsl_size_t, 5_fgsl_size_t)
  upperq = fgsl_stats_quantile_from_sorted_data( &
       data, 1_fgsl_size_t, 5_fgsl_size_t, 0.75D0)
  lowerq = fgsl_stats_quantile_from_sorted_data( &
       data,  1_fgsl_size_t, 5_fgsl_size_t, 0.25D0)
  write(*, '(''The median is '',F9.5)') median
  write(*, '(''The upper quartile is '',F9.5)') upperq
  write(*, '(''The lower quartile is '',F9.5)') lowerq
end program statsort
