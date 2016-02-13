program interp2d
  use mod_unit
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: nmax_x = 10, nmax_y = 20
  real(fgsl_double), parameter :: eps10 = 1.0E-10_fgsl_double
  integer(fgsl_int) :: status
  type(fgsl_interp_accel) :: acc_x, acc_y
  type(fgsl_interp2d) :: a_interp
  type(fgsl_spline2d) :: spline
  integer(fgsl_long) :: i, j, index
  character(kind=fgsl_char,len=fgsl_strmax) :: name
  real(fgsl_double), dimension(nmax_x) :: xa
  real(fgsl_double), dimension(nmax_y) :: ya
  real(fgsl_double), dimension(nmax_x, nmax_y) :: za, dx, dy, dxx, dyy, dxy, &
    ra_e, rdxa_e, rdya_e, rdxxa_e, rdyya_e, rdxya_e
  real(fgsl_double) :: dmx, ra, rdxa, rdya, rdxxa, rdyya, rdxya

  !
  ! Test interpolation API
  !
  call unit_init(15)
  !
  do i=1,nmax_x
     xa(i) = dble(i-1)
  end do
  do j=1,nmax_y
     ya(j) = dble(j-1)
  end do
  do i=1,nmax_x
    do j=1,nmax_y
      za(i,j) = dble(2*i+j-2)**3.0_fgsl_double
      dx(i,j) = 2.0_fgsl_double*3.0_fgsl_double*dble(2*i+j-2)**2.0_fgsl_double
      dy(i,j) = 3.0_fgsl_double*dble(2*i+j-2)**2.0_fgsl_double
      dxy(i,j) = 2.0_fgsl_double*2.0_fgsl_double*3.0_fgsl_double*dble(2*i+j-2)
      dxx(i,j) = 2.0_fgsl_double*2.0_fgsl_double*2.0_fgsl_double*3.0_fgsl_double*dble(2*i+j-2)
      dyy(i,j) = 2.0_fgsl_double*3.0_fgsl_double*dble(2*i+j-2)
   end do
  end do
  acc_x = fgsl_interp_accel_alloc()
  call unit_assert_true('fgsl_interp_accel_alloc',fgsl_well_defined(acc_x),.true.)
  acc_y = fgsl_interp_accel_alloc()
  call unit_assert_true('fgsl_interp_accel_alloc',fgsl_well_defined(acc_y),.true.)
  a_interp = fgsl_interp2d_alloc(fgsl_interp2d_bicubic, nmax_x, nmax_y)
  call unit_assert_true('fgsl_interp2d_alloc',fgsl_well_defined(a_interp),.true.)
  name = fgsl_interp2d_name(a_interp)
  call unit_assert_equal('fgsl_interp2d_alloc','bicubic',name)
  status = fgsl_interp2d_init(a_interp,xa,ya,za)
  call unit_assert_equal('fgsl_interp2d_init',fgsl_success,status)
  dmx = 0.0d0
  do i=2,nmax_x
    do j=1,nmax_y
      ra = fgsl_interp2d_eval(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y)
      status = fgsl_interp2d_eval_e(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y, ra_e(i,j))
      rdxa = fgsl_interp2d_eval_deriv_x(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y)
      status = fgsl_interp2d_eval_deriv_x_e(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y, rdxa_e(i,j))
      rdya = fgsl_interp2d_eval_deriv_y(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y)
      status = fgsl_interp2d_eval_deriv_y_e(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y, rdya_e(i,j))
      rdxxa = fgsl_interp2d_eval_deriv_xx(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y)
      status = fgsl_interp2d_eval_deriv_xx_e(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y, rdxxa_e(i,j))
      rdyya = fgsl_interp2d_eval_deriv_yy(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y)
      status = fgsl_interp2d_eval_deriv_yy_e(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y, rdyya_e(i,j))
      rdxya = fgsl_interp2d_eval_deriv_xy(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y)
      status = fgsl_interp2d_eval_deriv_xy_e(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y, rdxya_e(i,j))
      dmx = max(dmx, abs(ra - za(i,j)))
      dmx = max(dmx, abs(rdxa - rdxa_e(i,j)))
      dmx = max(dmx, abs(rdya - rdya_e(i,j)))
      dmx = max(dmx, abs(rdxxa - rdxxa_e(i,j)))
      dmx = max(dmx, abs(rdyya - rdyya_e(i,j)))
      dmx = max(dmx, abs(rdxya - rdxya_e(i,j)))
    end do
  end do
  call unit_assert_equal_within('fgsl_interp2d_eval',0.0d0,dmx,eps10)
  call fgsl_interp2d_free(a_interp)

  spline = fgsl_spline2d_alloc(fgsl_interp2d_bilinear,nmax_x, nmax_y)
  call unit_assert_true('fgsl_spline2d_alloc',fgsl_well_defined(spline),.true.)
  name = fgsl_spline2d_name(spline)
  call unit_assert_equal('fgsl_spline2d_alloc','bilinear',name)
  status = fgsl_spline2d_init(spline,xa,ya,za)
  call unit_assert_equal('fgsl_interp2d_init',fgsl_success,status)
  dmx = 0.0d0
  do i=2,nmax_x
    do j=2,nmax_y
      ra = fgsl_spline2d_eval(spline, xa(i), ya(j), acc_x, acc_y)
      rdxa = fgsl_spline2d_eval_deriv_x(spline, xa(i), ya(j), acc_x, acc_y)
      rdya = fgsl_spline2d_eval_deriv_y(spline, xa(i), ya(j), acc_x, acc_y)
      rdxxa = fgsl_spline2d_eval_deriv_xx(spline, xa(i), ya(j), acc_x, acc_y)
      rdyya = fgsl_spline2d_eval_deriv_yy(spline, xa(i), ya(j), acc_x, acc_y)
      rdxya = fgsl_spline2d_eval_deriv_xy(spline, xa(i), ya(j), acc_x, acc_y)
      dmx = max(dmx, abs(ra - za(i,j)))
      dmx = max(dmx, abs(rdxa - rdxa_e(i,j)))
      dmx = max(dmx, abs(rdya - rdya_e(i,j)))
      dmx = max(dmx, abs(rdxxa - rdxxa_e(i,j)))
      dmx = max(dmx, abs(rdyya - rdyya_e(i,j)))
      dmx = max(dmx, abs(rdxya - rdxya_e(i,j)))
    end do
  end do
  call unit_assert_equal_within('fgsl_spline2d_eval',0.0d0,dmx,eps10)
  index = fgsl_spline2d_min_size(spline)
  call unit_assert_equal('fgsl_spline2d_min_size',2_fgsl_long,index)

  call fgsl_spline2d_free(spline)

  a_interp = fgsl_interp2d_alloc(fgsl_interp2d_bicubic,nmax_x, nmax_y)
  name = fgsl_interp2d_name(a_interp)
  call unit_assert_equal('fgsl_interp2d_name','bicubic',name)
  status = fgsl_interp2d_init(a_interp,xa,ya,za)
  call unit_assert_equal('fgsl_interp2d_init',fgsl_success,status)
  dmx = 0.0d0
  do i=2,nmax_x
    do j=2,nmax_y
      ra = fgsl_interp2d_eval(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y)
      rdxa = fgsl_interp2d_eval_deriv_x(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y)
      rdya = fgsl_interp2d_eval_deriv_y(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y)
      rdxxa = fgsl_interp2d_eval_deriv_xx(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y)
      rdyya = fgsl_interp2d_eval_deriv_yy(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y)
      rdxya = fgsl_interp2d_eval_deriv_xy(a_interp, xa, ya, za, xa(i), ya(i), acc_x, acc_y)
      dmx = max(dmx, abs(ra - za(i,j)))
      dmx = max(dmx, abs(rdxa - dx(i,j)))
      dmx = max(dmx, abs(rdya - dy(i,j)))
      dmx = max(dmx, abs(rdxxa - dxx(i,j)))
      dmx = max(dmx, abs(rdyya - dyy(i,j)))
      dmx = max(dmx, abs(rdxya - dxy(i,j)))
    end do
  end do
  call unit_assert_equal_within('fgsl_interp2d_eval',0.0d0,dmx,eps10)
  call fgsl_interp2d_free(a_interp)
  !
  ! Done
  !
  call unit_finalize()
end program interp2d
