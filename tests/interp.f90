program interp
  use mod_unit
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: nmax = 10
  real(fgsl_double), parameter :: eps10 = 1.0E-10_fgsl_double
  integer(fgsl_int) :: status
  type(fgsl_interp_accel) :: acc
  type(fgsl_interp) :: a_interp
  type(fgsl_spline) :: spline
  integer(fgsl_long) :: i, index
  character(kind=fgsl_char,len=fgsl_strmax) :: name
  real(fgsl_double), dimension(nmax) :: xa, ya, d, d2, di, ra_e, rda_e, rd2a_e, ri_e
  real(fgsl_double) :: dmx, ra, rda, rd2a, ri

!
! Test interpolation API
!
  call unit_init(15)
!
  do i=1,nmax
     xa(i) = dble(i-1)
     ya(i) = dble(i-1)**3.0_fgsl_double
     d(i) =  3.0_fgsl_double * dble(i-1)**2.0_fgsl_double
     d2(i) =  6.0_fgsl_double * dble(i-1)
     di(i) =  1/4.0_fgsl_double * dble(i-1)**4.0_fgsl_double
  end do
  acc = fgsl_interp_accel_alloc()
  call unit_assert_true('fgsl_interp_accel_alloc',fgsl_well_defined(acc),.true.)
  a_interp = fgsl_interp_alloc(fgsl_interp_cspline,nmax)
  call unit_assert_true('fgsl_interp_alloc',fgsl_well_defined(a_interp),.true.)
  name = fgsl_interp_name(a_interp)
  call unit_assert_equal('fgsl_interp_alloc','cspline',name)
  status = fgsl_interp_init(a_interp,xa,ya)
  call unit_assert_equal('fgsl_interp_init',fgsl_success,status)
  dmx = 0.0d0
  do i=2,nmax
     ra = fgsl_interp_eval(a_interp, xa, ya, xa(i), acc)
     status = fgsl_interp_eval_e(a_interp, xa, ya, xa(i), acc, ra_e(i))
     rda = fgsl_interp_eval_deriv(a_interp, xa, ya, xa(i), acc)
     status = fgsl_interp_eval_deriv_e(a_interp, xa, ya, xa(i), acc, rda_e(i))
     rd2a = fgsl_interp_eval_deriv2(a_interp, xa, ya, xa(i), acc)
     status = fgsl_interp_eval_deriv2_e(a_interp, xa, ya, xa(i), acc, rd2a_e(i))
     ri = fgsl_interp_eval_integ(a_interp, xa, ya, 0.0_fgsl_double, xa(i), acc)
     status = fgsl_interp_eval_integ_e(a_interp, xa, ya, 0.0_fgsl_double, xa(i), &
          acc, ri_e(i))
     dmx = max(dmx, abs(ra - ya(i)))
     dmx = max(dmx, abs(rda - rda_e(i)))
     dmx = max(dmx, abs(rd2a - rd2a_e(i)))
     dmx = max(dmx, abs(ri - ri_e(i)))
!     write(6, fmt='(1pd20.13,1x)') ra
!     write(6, fmt='(2(1pd20.13,1x))') xa(i),ya(i)
  end do
  call unit_assert_equal_within('fgsl_interp_eval',0.0d0,dmx,eps10)
  call fgsl_interp_free(a_interp)
!
  spline = fgsl_spline_alloc(fgsl_interp_cspline,nmax)
  call unit_assert_true('fgsl_spline_alloc',fgsl_well_defined(spline),.true.)
  name = fgsl_spline_name(spline)
  call unit_assert_equal('fgsl_spline_alloc','cspline',name)
  status = fgsl_spline_init(spline,xa,ya)
  call unit_assert_equal('fgsl_interp_init',fgsl_success,status)
  dmx = 0.0d0
  do i=2,nmax
     ra = fgsl_spline_eval(spline, xa(i), acc)
     rda = fgsl_spline_eval_deriv(spline, xa(i), acc)
     rd2a = fgsl_spline_eval_deriv2(spline, xa(i), acc)
     ri = fgsl_spline_eval_integ(spline, 0.0_fgsl_double, xa(i), acc)
     dmx = max(dmx, abs(ra - ya(i)))
     dmx = max(dmx, abs(rda - rda_e(i)))
     dmx = max(dmx, abs(rd2a - rd2a_e(i)))
     dmx = max(dmx, abs(ri - ri_e(i)))
  end do
  call unit_assert_equal_within('fgsl_spline_eval',0.0d0,dmx,eps10)
  index = fgsl_spline_min_size(spline)
  call unit_assert_equal('fgsl_spline_min_size',3_fgsl_long,index)

  call fgsl_spline_free(spline)
!
  a_interp = fgsl_interp_alloc(fgsl_interp_polynomial,nmax)
  name = fgsl_interp_name(a_interp)
  call unit_assert_equal('fgsl_interp_alloc','polynomial',name)
  status = fgsl_interp_init(a_interp,xa,ya)
  call unit_assert_equal('fgsl_interp_init',fgsl_success,status)
  dmx = 0.0d0
  do i=2,nmax
     ra = fgsl_interp_eval(a_interp, xa, ya, xa(i), acc)
     rda = fgsl_interp_eval_deriv(a_interp, xa, ya, xa(i), acc)
     rd2a = fgsl_interp_eval_deriv2(a_interp, xa, ya, xa(i), acc)
     ri = fgsl_interp_eval_integ(a_interp, xa, ya, 0.0_fgsl_double, xa(i), acc)
!     write(6, fmt='(4(1pd20.13,1x))') ra, rda, rd2a, ri
!     write(6, fmt='(4(1pd20.13,1x))') ya(i),d(i),d2(i), di(i)
     dmx = max(dmx, abs(ra - ya(i)))
     dmx = max(dmx, abs(rda - d(i)))
     dmx = max(dmx, abs(rd2a - d2(i)))
     dmx = max(dmx, abs(ri - di(i)))
  end do
  call unit_assert_equal_within('fgsl_interp_eval',0.0d0,dmx,eps10)
  call fgsl_interp_free(a_interp)
!
  index = fgsl_interp_bsearch(xa, 1.5_fgsl_double,1_fgsl_size_t,nmax)
  call unit_assert_equal('fgsl_interp_bsearch',2_fgsl_long,index)
  index = fgsl_interp_accel_find(acc, xa, 1.5_fgsl_double)
  call unit_assert_equal('fgsl_interp_accel_find',2_fgsl_long,index)
  call fgsl_interp_accel_free(acc)
!
! Done
!
  call unit_finalize()
end program interp
