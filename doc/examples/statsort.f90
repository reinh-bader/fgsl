program statsort
  use fgsl
  implicit none
  real(fgsl_double) :: data(5) = (/17.2D0, 18.1D0, 16.5D0, 18.3D0, 12.6D0 /)
  real(fgsl_double) :: median, upperq, lowerq
!
  write(6, '(''Original dataset: '',5(F9.5))') data
  call fgsl_sort(data, 1_fgsl_size_t)
  write(6, '(''Sorted dataset: '',5(F9.5))') data
!
  median = fgsl_stats_median_from_sorted_data( &
       data, 1_fgsl_size_t)
  upperq = fgsl_stats_quantile_from_sorted_data( &
       data, 1_fgsl_size_t, 0.75D0)
  lowerq = fgsl_stats_quantile_from_sorted_data( &
       data,  1_fgsl_size_t, 0.25D0)
  write(6, '(''The median is '',F9.5)') median
  write(6, '(''The upper quartile is '',F9.5)') upperq
  write(6, '(''The lower quartile is '',F9.5)') lowerq
end program statsort
