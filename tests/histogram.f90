program histogram
  use fgsl
  use mod_unit
  implicit none
  integer(fgsl_size_t), parameter :: ntmax = 8_fgsl_size_t
  integer(fgsl_size_t) :: ibins, ival
  integer(fgsl_int) :: status, i
  real(fgsl_double), parameter :: eps10 = 1.0d-10
  real(fgsl_double) :: table(3,ntmax), a, b
  type(fgsl_histogram) :: h, h_clone, h_copy
  type(fgsl_file) :: hfile
!
! Test histogram routines
!
  call unit_init(200)
!
  h = fgsl_histogram_alloc(ntmax)
  call unit_assert_true('fgsl_histogram_alloc',fgsl_well_defined(h),.true.)
  status = fgsl_histogram_set_ranges_uniform(h, &
       0.0_fgsl_double, 4.0_fgsl_double)
  call unit_assert_equal('fgsl_histogram_set_ranges_uniform:status', &
       fgsl_success,status)
  status = fgsl_histogram_increment(h, 0.2d0)
  call unit_assert_equal('fgsl_histogram_increment:status',fgsl_success,status)
  status = fgsl_histogram_increment(h, 0.2d0)
  status = fgsl_histogram_increment(h, 0.2d0)
  status = fgsl_histogram_accumulate(h, 2.2d0,2.0d0)
  status = fgsl_histogram_increment(h, 3.2d0)
  status = fgsl_histogram_accumulate(h, 3.2d0, 3.0d0)
  status = fgsl_histogram_increment(h, 3.6d0)
  hfile = fgsl_open('histogram.dat','w')
  status = fgsl_histogram_fprintf(hfile,h,'%16.8f ','%16.8f ')
  status = fgsl_close(hfile)
  open(20, file='histogram.dat',form='FORMATTED',status='OLD')
  do i=1, ntmax
     read(20, fmt=*) table(:,i)
  end do
  call unit_assert_equal_within('fgsl_histogram_fprintf',&
       (/0.0d0,0.5d0,1.0d0,1.5d0,2.0d0,2.5d0,3.0d0,3.5d0/), &
       table(1,:),eps10)
  call unit_assert_equal_within('fgsl_histogram_fprintf',&
       (/0.5d0,1.0d0,1.5d0,2.0d0,2.5d0,3.0d0,3.5d0,4.0d0/), &
       table(2,:),eps10)
  call unit_assert_equal_within('fgsl_histogram_fprintf',&
       (/3.0d0,0.0d0,0.0d0,0.0d0,2.0d0,0.0d0,4.0d0,1.0d0/), &
       table(3,:),eps10)
! NOTE: zero based
  status = fgsl_histogram_get_range(h, 2_fgsl_size_t, a, b) 
  call unit_assert_equal('fgsl_histogram_get_range:status', &
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_histogram_get_range:lower',&
       1.0d0,a,eps10)
  call unit_assert_equal_within('fgsl_histogram_get_range:upper',&
       1.5d0,b,eps10)
  a = fgsl_histogram_max(h)
  call unit_assert_equal_within('fgsl_histogram_max',&
       4.0d0,a,eps10)
  a = fgsl_histogram_min(h)
  call unit_assert_equal_within('fgsl_histogram_min',&
       0.0d0,a,eps10)
  ibins = fgsl_histogram_bins(h)
  call unit_assert_equal('fgsl_histogram_bins',&
       int(ntmax),int(ibins))
  status = fgsl_histogram_find(h, 2.3d0, ival)
  call unit_assert_equal('fgsl_histogram_find',&
       4,int(ival))
  a = fgsl_histogram_max_val(h)
  call unit_assert_equal_within('fgsl_histogram_max_val',&
       4.0d0,a,eps10)
  a = fgsl_histogram_min_val(h)
  call unit_assert_equal_within('fgsl_histogram_min_val',&
       0.0d0,a,eps10)
  ibins = fgsl_histogram_max_bin(h)
  call unit_assert_equal('fgsl_histogram_max_bin',&
       6,int(ibins))
  ibins = fgsl_histogram_min_bin(h)
  call unit_assert_equal('fgsl_histogram_min_bin',&
       1,int(ibins))
! NOTE: sum ( x * f(x) ) / sum (f(x)) is taken at mid point
  a = fgsl_histogram_mean(h)
  call unit_assert_equal_within('fgsl_histogram_mean',&
       2.2d0,a,eps10)
  a = fgsl_histogram_sigma(h)
  call unit_assert_equal_within('fgsl_histogram_sigma',&
       1.35d0,a,eps10)
  a = fgsl_histogram_sum(h)
  call unit_assert_equal_within('fgsl_histogram_sum',&
       1.0d1,a,eps10)
  call unit_assert_true('fgsl_well_defined:false', &
       .not. fgsl_well_defined(h_clone),.true.) 
  h_clone = fgsl_histogram_clone(h)
  call unit_assert_true('fgsl_histogram_well_defined:true', &
       fgsl_well_defined(h_clone),.true.)
  a = fgsl_histogram_mean(h_clone)
  call unit_assert_equal_within('fgsl_histogram_clone',&
       2.2d0,a,eps10)
  h_copy = fgsl_histogram_alloc(ntmax)
  call unit_assert_true('fgsl_well_defined:true', &
       fgsl_well_defined(h_clone),.true.) 
  status = fgsl_histogram_memcpy(h_copy, h)
  call unit_assert_equal('fgsl_histogram_memcpy:status',fgsl_success,status)
  a = fgsl_histogram_mean(h_copy)
  call unit_assert_equal_within('fgsl_histogram_memcpy',&
       2.2d0,a,eps10)


  call fgsl_histogram_free(h)
  call fgsl_histogram_free(h_clone)
  call fgsl_histogram_free(h_copy)



!
! Done
!
  call unit_finalize() 
end program histogram
