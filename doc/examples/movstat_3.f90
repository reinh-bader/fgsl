module mod_movstat_3
  use fgsl
  use, intrinsic :: iso_c_binding, only : c_ptr, c_f_pointer, c_funloc, c_loc
  implicit none
contains
  function func(n, x, params) bind(c)
    integer(fgsl_size_t), value :: n
    real(fgsl_double), dimension(*), intent(inout) :: x
    type(c_ptr), value :: params
    real(fgsl_double) :: func

    real(fgsl_double), pointer :: alpha
    call c_f_pointer(params, alpha)
    call fgsl_sort(x(1:n), 1_fgsl_size_t, n)
    func = fgsl_stats_trmean_from_sorted_data(alpha, x, 1_fgsl_size_t, n)
  end function func
end module mod_movstat_3
program movstat_3
  use mod_movstat_3
  implicit none
  ! length of time series and window size
  integer(fgsl_size_t), parameter :: n = 1000, k = 11
  ! trimmed mean parameter
  real(fgsl_double), target :: alpha = 0.1d0
  ! input vector and filtered output vector
  type(fgsl_vector) :: x, y
  real(fgsl_double), target :: v_x(n), v_y(n)
  type(fgsl_rng) :: r
  type(fgsl_movstat_workspace) :: w
  type(fgsl_movstat_function) :: f
  integer(fgsl_size_t) :: i
  real(fgsl_double) :: sum, ui, outlier
  integer(fgsl_int) :: status


  x = fgsl_vector_init(v_x)
  y = fgsl_vector_init(v_y)
  r = fgsl_rng_alloc(fgsl_rng_default)
  w = fgsl_movstat_alloc(k)
  
  ! generate input signal
  sum = 0.0d0
  setup : do i = 1, n
     ui = fgsl_ran_gaussian(r, 1.0d0)
     if (fgsl_rng_uniform(r) < 0.01d0) then
        outlier = sign(10.0d0, ui)
     else
        outlier = 0.0d0
     end if
     sum = sum + ui
     v_x(i) = sum + outlier
  end do setup
  
  ! apply moving window function
  f%function = c_funloc(func)
  f%params = c_loc(alpha)
  status = fgsl_movstat_apply(FGSL_MOVSTAT_END_PADVALUE, f, x, y, w);

  print : do i = 1, n
     write(*,fmt='(i0, 1x, 2(f11.6,1x))') i, v_x(i), v_y(i)
  end do print

  call fgsl_vector_free(x)
  call fgsl_vector_free(y)
  call fgsl_rng_free(r)
  call fgsl_movstat_free(w)

  
end program movstat_3
