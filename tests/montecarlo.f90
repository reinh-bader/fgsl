module mod_montecarlo
  use fgsl
  use, intrinsic :: iso_c_binding
  implicit none
  real(fgsl_double), parameter :: exact = 1.3932039296856768_fgsl_double
  real(fgsl_double), parameter :: eps6 = 1.0d-6
  integer(fgsl_size_t), parameter :: calls = 500000
contains
  function g(v, n, params) bind(c)
    integer(c_size_t), value :: n
    real(c_double), dimension(*) :: v
    type(c_ptr), value :: params
    real(c_double) :: g
    g = 1.0_c_double/(m_pi**3)/ &
         (1.0_fgsl_double - cos(v(1))*cos(v(2))*cos(v(3)))
  end function g
end module mod_montecarlo
program montecarlo
  use mod_montecarlo
  use mod_unit
  implicit none
  real(fgsl_double) :: chisq, xl(3), xu(3), res, err, y, yy, yyy
  integer(fgsl_int) :: status, stage, mode, verbose
  integer(fgsl_size_t) :: its
  type(fgsl_file) :: file
  type(fgsl_rng) :: r
  type(fgsl_rng_type) :: t
  type(fgsl_monte_plain_state) :: s
  type(fgsl_monte_miser_state) :: m
  type(fgsl_monte_vegas_state) :: v
  type(fgsl_monte_function) :: gfun
  type(c_ptr) :: ptr
!
! Test monte carlo routines
!
  call unit_init(200)
!
  xl = 0.0_fgsl_double ; xu = m_pi
  t = fgsl_rng_env_setup()
  r = fgsl_rng_alloc(t)
  gfun = fgsl_monte_function_init(g, 3_fgsl_size_t, ptr)
  s = fgsl_monte_plain_alloc(3_fgsl_size_t)
  call unit_assert_true('fgsl_monte_plain_alloc',&
       fgsl_well_defined(s),.true.)
  status = fgsl_monte_plain_integrate(gfun, xl, xu, 3_fgsl_size_t, &
       calls, r, s, res, err)
  call unit_assert_equal('fgsl_monte_plain:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_monte_plain:res',1.412209d0,res,eps6)
  call unit_assert_equal_within('fgsl_monte_plain:err',0.013436d0,err,eps6)
  call fgsl_monte_plain_free(s)
!
  m = fgsl_monte_miser_alloc(3_fgsl_size_t)
  call unit_assert_true('fgsl_monte_miser_alloc',&
       fgsl_well_defined(m),.true.)
  status = fgsl_monte_miser_integrate(gfun, xl, xu, 3_fgsl_size_t, &
       calls, r, m, res, err)
  call unit_assert_equal('fgsl_monte_miser_integrate:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_monte_plain:res',1.391322d0,res,eps6)
  call unit_assert_equal_within('fgsl_monte_plain:err',0.003461d0,err,eps6)
  call fgsl_monte_miser_free(m)

!
  v = fgsl_monte_vegas_alloc(3_fgsl_size_t)
  call unit_assert_true('fgsl_monte_vegas_alloc',&
       fgsl_well_defined(v),.true.)
  status = fgsl_monte_vegas_integrate(gfun, xl, xu, 3_fgsl_size_t, &
       10000_fgsl_size_t, r, v, res, err)
  do 
     status = fgsl_monte_vegas_integrate(gfun, xl, xu, 3_fgsl_size_t, &
          calls/5_fgsl_size_t, r, v, res, err)
     call fgsl_monte_vegas_getparams(v, y, yy, chisq, yyy, &
       its, stage, mode, verbose, file)
     if (abs(chisq - 1.0_fgsl_double) <= 0.5_fgsl_double) exit 
  end do
  call unit_assert_equal('fgsl_monte_vegas:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_monte_vegas:res',1.393281d0,res,eps6)
  call unit_assert_equal_within('fgsl_monte_vegas:err',0.000362d0,err,eps6)
  call unit_assert_equal_within('fgsl_monte_vegas_getparams:res',&
       1.393702d0,y,eps6)
  call unit_assert_equal_within('fgsl_monte_vegas_getparams:sigma',&
       0.000630d0,yy,eps6)
  call unit_assert_equal_within('fgsl_monte_vegas_getparams:chisq',&
       1.499468d0,chisq,eps6)
  call unit_assert_equal_within('fgsl_monte_vegas_getparams:alpha',&
       1.5d0,yyy,eps6)
  call unit_assert_equal('fgsl_monte_vegas_getparams:iterations',&
       5,int(its))
  call unit_assert_equal('fgsl_monte_vegas_getparams:stage',&
       1,stage)
  call unit_assert_equal('fgsl_monte_vegas_getparams:mode',&
       fgsl_vegas_mode_stratified,mode)
  call unit_assert_equal('fgsl_monte_vegas_getparams:verbose',&
       -1,verbose)
  call unit_assert_true('fgsl_monte_monte_vegas_getparams',&
       .not. fgsl_well_defined(file),.true.)
! ostream is NULL by default
  call fgsl_monte_vegas_free(v)
  call fgsl_monte_function_free(gfun)
  call fgsl_rng_free(r)
!
! Done
!
  call unit_finalize()
end program montecarlo
