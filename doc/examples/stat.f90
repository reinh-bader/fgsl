program stat
  use fgsl
  implicit none
  real(fgsl_double) :: data(5) = (/17.2D0, 18.1D0, 16.5D0, 18.3D0, 12.6D0 /)
  real(fgsl_double) :: mean, variance, largest, smallest

  mean     = fgsl_stats_mean(data, 1_fgsl_size_t, 5_fgsl_size_t)
  variance = fgsl_stats_variance(data, 1_fgsl_size_t, 5_fgsl_size_t)
  largest  = fgsl_stats_max(data, 1_fgsl_size_t, 5_fgsl_size_t)
  smallest = fgsl_stats_min(data, 1_fgsl_size_t, 5_fgsl_size_t)
  
  write(*, '(''The dataset is '',5(F9.5))') data
  write(*, '(''The sample mean is '',F9.5)') mean
  write(*, '(''The estimated variance is '',F9.5)') variance
  write(*, '(''The largest value is '',F9.5)') largest
  write(*, '(''The smallest value is '',F9.5)') smallest
end program stat
