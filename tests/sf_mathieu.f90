program sf_mathieu
  use fgsl_sf_mathieu
  use mod_unit
  implicit none

  real(fgsl_double), parameter :: eps10 = 1.0e-10_fgsl_double

  type(fgsl_sf_mathieu_workspace) :: wk
  ! maximum order and q value
  integer(fgsl_size_t) :: nord  
  real(fgsl_double) :: qmax

  !
  type(fgsl_sf_result) :: sfres
  integer(fgsl_int) :: stat

  !
  real(fgsl_double) :: charval, chararr(2), val, valarr(2)

  !
  ! initialize unit testing and work space
  call unit_init(20)
  nord = 4
  qmax = 1.2e2_fgsl_double
  wk = fgsl_sf_mathieu_alloc(nord, qmax)

  charval = fgsl_sf_mathieu_a(1, 2.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_mathieu_a',&
       2.379199880488686247_fgsl_double,charval,eps10)
  
  stat = fgsl_sf_mathieu_a_e(1, 2.0_fgsl_double, sfres)
  call unit_assert_equal_within('fgsl_sf_mathieu_a_e',&
       2.379199880488686247_fgsl_double,sfres%val,sfres%err)

  charval = fgsl_sf_mathieu_b(1, 2.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_mathieu_b',&
       -1.390676501225322825_fgsl_double,charval,eps10)
  
  stat = fgsl_sf_mathieu_b_e(1, 2.0_fgsl_double, sfres)
  call unit_assert_equal_within('fgsl_sf_mathieu_b_e',&
       -1.390676501225322825_fgsl_double,sfres%val,sfres%err)

  stat = fgsl_sf_mathieu_a_array(1, 2, 2.0_fgsl_double, wk, chararr)
  call unit_assert_equal_within('fgsl_sf_mathieu_a_array',&
       2.379199880488686247_fgsl_double,chararr(1),eps10)

  stat = fgsl_sf_mathieu_b_array(1, 2, 2.0_fgsl_double, wk, chararr)
  call unit_assert_equal_within('fgsl_sf_mathieu_b_array',&
       -1.390676501225322825_fgsl_double,chararr(1),eps10)

  val = fgsl_sf_mathieu_ce(1, 2.0_fgsl_double, 1.5_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_mathieu_ce',&
       1.396737008170945982e-1_fgsl_double,val,eps10)

  stat = fgsl_sf_mathieu_ce_e(1, 2.0_fgsl_double, 1.5_fgsl_double, sfres)
  call unit_assert_equal_within('fgsl_sf_mathieu_ce_e',&
       1.396737008170945982e-1_fgsl_double,sfres%val,sfres%err)

  val = fgsl_sf_mathieu_se(1, 2.0_fgsl_double, 1.5_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_mathieu_se',&
       1.180463739819688662_fgsl_double,val,eps10)

  stat = fgsl_sf_mathieu_se_e(1, 2.0_fgsl_double, 1.5_fgsl_double, sfres)
  call unit_assert_equal_within('fgsl_sf_mathieu_se_e',&
       1.180463739819688662_fgsl_double,sfres%val,sfres%err)

  stat = fgsl_sf_mathieu_ce_array(1, 2, 2.0_fgsl_double, 1.5_fgsl_double, &
       wk, valarr)
  call unit_assert_equal_within('fgsl_sf_mathieu_ce_array',&
       1.396737008170945982e-1_fgsl_double,valarr(1),eps10)

  stat = fgsl_sf_mathieu_se_array(1, 2, 2.0_fgsl_double, 1.5_fgsl_double, &
       wk, valarr)
  call unit_assert_equal_within('fgsl_sf_mathieu_se_array',&
       1.180463739819688662_fgsl_double,valarr(1),eps10)

  val = fgsl_sf_mathieu_mc(1, 1, 2.0_fgsl_double, 1.5_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_mathieu_mc',&
       -2.158163819586180754e-1_fgsl_double,val,eps10)

  stat = fgsl_sf_mathieu_mc_e(1, 1, 2.0_fgsl_double, 1.5_fgsl_double, sfres)
  call unit_assert_equal_within('fgsl_sf_mathieu_mc_e',&
       -2.158163819586180754e-1_fgsl_double,sfres%val,sfres%err)

  val = fgsl_sf_mathieu_ms(1, 1, 2.0_fgsl_double, 1.5_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_mathieu_ms',&
       -2.980881195609551559e-01_fgsl_double,val,eps10)

  stat = fgsl_sf_mathieu_ms_e(1, 1, 2.0_fgsl_double, 1.5_fgsl_double, sfres)
  call unit_assert_equal_within('fgsl_sf_mathieu_ms_e',&
       -2.980881195609551559e-01_fgsl_double,sfres%val,sfres%err)

  stat = fgsl_sf_mathieu_mc_array(1, 1, 2, 2.0_fgsl_double, 1.5_fgsl_double, &
       wk, valarr)
  call unit_assert_equal_within('fgsl_sf_mathieu_mc_array',&
       -2.158163819586180754e-1_fgsl_double,valarr(1),eps10)

  stat = fgsl_sf_mathieu_ms_array(1, 1, 2, 2.0_fgsl_double, 1.5_fgsl_double, &
       wk, valarr)
  call unit_assert_equal_within('fgsl_sf_mathieu_ms_array',&
       -2.980881195609551559e-01_fgsl_double,valarr(1),eps10) 
  !
  ! cleanup
  call unit_finalize()
  call fgsl_sf_mathieu_free(wk)

end program
