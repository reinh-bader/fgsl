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
  real(fgsl_double) :: charval

  !
  ! initialize unit testing and work space
  call unit_init(20)
  nord = 4
  qmax = 1.2e2_fgsl_double
  wk = fgsl_sf_mathieu_alloc(nord, qmax)

  charval = fgsl_sf_mathieu_a(1, 2.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_mathieu_a',2.379199880488686247_fgsl_double,&
       charval,eps10)
  
  stat = fgsl_sf_mathieu_a_e(1, 2.0_fgsl_double, sfres)
  call unit_assert_equal_within('fgsl_sf_mathieu_a',2.379199880488686247_fgsl_double,&
       sfres%val,sfres%err)


  !
  ! cleanup
  call unit_finalize()
  call fgsl_sf_mathieu_free(wk)

end program sf_mathieu
