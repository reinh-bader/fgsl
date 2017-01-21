subroutine rstat1()
  use :: fgsl
  use, intrinsic :: iso_fortran_env
  implicit none
  real(fgsl_double) :: data(5) = [17.2, 18.1, 16.5, 18.3, 12.6]
  real(fgsl_double) :: mean, variance, largest, smallest, rms
  type(fgsl_rstat_workspace) :: rstat_p
  integer(fgsl_size_t) :: i
  integer(fgsl_int) :: status

  rstat_p = fgsl_rstat_alloc()
  !add data to rstat accumulator
  do i=1,5
    status = fgsl_rstat_add(data(i), rstat_p)
  end do

  mean     = fgsl_rstat_mean(rstat_p)
  variance = fgsl_rstat_variance(rstat_p)
  largest  = fgsl_rstat_max(rstat_p)
  smallest = fgsl_rstat_min(rstat_p)
  rms      = fgsl_rstat_rms(rstat_p)

  write(output_unit, '(A,5G11.4)') 'The dataset is ', (data(i), i=1,5)
  write(output_unit, '(A,G11.4)') 'The sample mean is ', mean
  write(output_unit, '(A,G11.4)') 'The estimated variance is ', variance
  write(output_unit, '(A,G11.4)') 'The root mean square is ', rms
  write(output_unit, '(A,G11.4)') 'The largest value is ', largest

  status = fgsl_rstat_reset(rstat_p)
  call fgsl_rstat_free(rstat_p)
end subroutine rstat1

subroutine rstat2
  use :: fgsl
  use, intrinsic :: iso_fortran_env
  implicit none
  integer(fgsl_size_t) :: N = 10000
  real(fgsl_double), allocatable, dimension(:) :: data
  type(fgsl_rstat_quantile_workspace) :: work_25, work_50, work_75
  type(fgsl_rng) :: r
  real(fgsl_double) :: exact_p25, exact_p50, exact_p75
  real(fgsl_double) :: val_p25, val_p50, val_p75
  integer(fgsl_size_t) :: i
  integer(fgsl_int) :: status

  allocate(data(N))
  work_25 = fgsl_rstat_quantile_alloc(0.25_fgsl_double)
  work_50 = fgsl_rstat_quantile_alloc(0.5_fgsl_double)
  work_75 = fgsl_rstat_quantile_alloc(0.75_fgsl_double)
  r = fgsl_rng_alloc(fgsl_rng_default)

  ! add data to quantile accumulators; also store data for exact
  ! comparisons
  do i=1,N
    data(i) = fgsl_ran_rayleigh(r, 1.0_fgsl_double)
    status = fgsl_rstat_quantile_add(data(i), work_25)
    status = fgsl_rstat_quantile_add(data(i), work_50)
    status = fgsl_rstat_quantile_add(data(i), work_75)
  end do

  ! exact values
  call fgsl_sort(data, 1_fgsl_size_t, N)
  exact_p25 = fgsl_stats_quantile_from_sorted_data(data, 1_fgsl_size_t, N, 0.25_fgsl_double)
  exact_p50 = fgsl_stats_quantile_from_sorted_data(data, 1_fgsl_size_t, N, 0.5_fgsl_double)
  exact_p75 = fgsl_stats_quantile_from_sorted_data(data, 1_fgsl_size_t, N, 0.75_fgsl_double)

  ! estimated values
  val_p25 = fgsl_rstat_quantile_get(work_25)
  val_p50 = fgsl_rstat_quantile_get(work_50)
  val_p75 = fgsl_rstat_quantile_get(work_75)

  write(output_unit, '(A,5G12.4,A)') 'The dataset is ', (data(i), i=1,5), ' ...'
  write(output_unit, '(A,G11.4,A,G11.4,A,G11.4)') '0.25 quartile: exact = ', &
    exact_p25, ', estimated = ', val_p25, ', error = ', (val_p25 - exact_p25) / exact_p25
  write(output_unit, '(A,G11.4,A,G11.4,A,G11.4)') '0.50 quartile: exact = ', &
    exact_p50, ', estimated = ', val_p50, ', error = ', (val_p50 - exact_p50) / exact_p50
  write(output_unit, '(A,G11.4,A,G11.4,A,G11.4)') '0.75 quartile: exact = ', &
    exact_p75, ', estimated = ', val_p75, ', error = ', (val_p75 - exact_p75) / exact_p75


  status = fgsl_rstat_quantile_reset(work_25)
  val_p25 = fgsl_rstat_quantile_get(work_25)
  write(output_unit, '(A,G11.4)') 'estimate after reset: ', val_p25

  call fgsl_rstat_quantile_free(work_25)
  call fgsl_rstat_quantile_free(work_50)
  call fgsl_rstat_quantile_free(work_75)
  call fgsl_rng_free(r)
  deallocate(data)
end subroutine rstat2

program rstat
  implicit none
  call rstat1()
  call rstat2()
end program rstat
