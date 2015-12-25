program poly
  use mod_unit
  use fgsl
  implicit none
  real(fgsl_double), parameter :: eps10 = 1.0E-10_fgsl_double
  integer(fgsl_int) :: status
  real(fgsl_double) :: ra, ri, ro, &
       d(3), xa(10), ya(10), za(10), dya(10), da(10), di(10)
  complex(fgsl_double) :: z0, z1, z2, &
       z(10)
  type(fgsl_poly_complex_workspace) :: wk
!
! Test polynomial API
!
  call unit_init(50)
!
  d(1:2) = (/1.0_fgsl_double, 2.0_fgsl_double/)
  ra = fgsl_poly_eval(d, 2, 2.0_fgsl_double)
  call unit_assert_equal_within('fgsl_poly_eval',5.0d0,ra,eps10)
  z0 = fgsl_poly_complex_eval(d, 2, (2.0_fgsl_double, 0.0_fgsl_double))
  call unit_assert_equal_within('fgsl_poly_complex_eval',(5.0d0,0.0d0),z0,eps10)
  z(1) = (1.0_fgsl_double, 0.0_fgsl_double)
  z(2) = (2.0_fgsl_double, 0.0_fgsl_double)
  z0 = fgsl_complex_poly_complex_eval(z(1:2), 2, &
       (2.0_fgsl_double, 0.0_fgsl_double))
  call unit_assert_equal_within('fgsl_complex_poly_complex_eval', &
       (5.0d0,0.0d0),z0,eps10)
  xa(1:3) =  (/1.0_fgsl_double, 2.0_fgsl_double, 3.0_fgsl_double/)
  ya(1:3) =  (/1.0_fgsl_double, 8.0_fgsl_double, 3.0_fgsl_double/)
  status = fgsl_poly_dd_init(da, xa, ya, 3_fgsl_size_t)
  ra = fgsl_poly_dd_eval(da, xa, 3_fgsl_size_t, 2.0_fgsl_double)
  call unit_assert_equal('fgsl_poly_dd_init',fgsl_success,status)
  call unit_assert_equal_within('fgsl_poly_dd_eval',8.0d0,ra,eps10)
  status = fgsl_poly_dd_taylor(di, 0.0_fgsl_double, da, xa, 3_fgsl_size_t, d)
  ra = di(1) + 2.0d0 * di(2) + 4.0d0 * di(3)
  call unit_assert_equal('fgsl_poly_dd_taylor:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_poly_dd_taylor',8.0d0,ra,eps10)
  dya(1:3) =  (/4.5_fgsl_double, -11.0_fgsl_double, .6_fgsl_double/)
  status = fgsl_poly_dd_hermite_init(da, za, xa, ya, dya, 3_fgsl_size_t)
  ra = fgsl_poly_dd_eval(da, za, 6_fgsl_size_t, 2.0_fgsl_double)
! FIXME: unclear whether the constant 6 above is correct.
  call unit_assert_equal_within('fgsl_poly_dd_hermite_init',8.0d0,ra,eps10)
  status = fgsl_poly_solve_quadratic(1.0_fgsl_double, -3.0_fgsl_double, &
       2.0_fgsl_double, ra, ri)
  call unit_assert_equal('fgsl_poly_solve_quadratic:status',2,status)
  call unit_assert_equal_within('fgsl_poly_solve_quadratic',1.0d0,ra,eps10)
  call unit_assert_equal_within('fgsl_poly_solve_quadratic',2.0d0,ri,eps10)
  status = fgsl_poly_complex_solve_quadratic(1.0_fgsl_double,0.0_fgsl_double, &
       1.0_fgsl_double, z0, z1)
  call unit_assert_equal('fgsl_poly_complex_solve_quadratic:status',2,status)
  call unit_assert_equal_within('fgsl_poly_complex_solve_quadratic',&
       (0.0d0,-1.0d0),z0,eps10)
  call unit_assert_equal_within('fgsl_poly_complex_solve_quadratic',&
       (0.0d0,1.0d0),z1,eps10)
  status = fgsl_poly_solve_cubic(-6.0_fgsl_double,11.0_fgsl_double, &
       -6.0_fgsl_double, ra, ri, ro)
  call unit_assert_equal('fgsl_poly_solve_cubic:status',3,status)
  call unit_assert_equal_within('fgsl_poly_solve_cubic',1.0d0,ra,eps10)
  call unit_assert_equal_within('fgsl_poly_solve_cubic',2.0d0,ri,eps10)
  call unit_assert_equal_within('fgsl_poly_solve_cubic',3.0d0,ro,eps10)
  status = fgsl_poly_complex_solve_cubic(0.0_fgsl_double,0.0_fgsl_double, &
       -1.0_fgsl_double, z0, z1, z2)
  call unit_assert_equal('fgsl_poly_complex_solve_cubic:status',3,status)
  call unit_assert_equal_within('fgsl_poly_complex_solve_cubic',&
       cos(2.0d0*m_pi/3.0d0) - (0.0d0,1.0d0)*sin(2.0d0*m_pi/3.0d0),z0,eps10)
  call unit_assert_equal_within('fgsl_poly_complex_solve_cubic',&
        cos(2.0d0*m_pi/3.0d0) + (0.0d0,1.0d0)*sin(2.0d0*m_pi/3.0d0),z1,eps10)
  call unit_assert_equal_within('fgsl_poly_complex_solve_cubic',&
        (1.0d0,0.0d0),z2,eps10)
  wk = fgsl_poly_complex_workspace_alloc(4_fgsl_size_t)
  xa(1:4) =  (/-1.0_fgsl_double, 0.0_fgsl_double, 0.0_fgsl_double, 1.0_fgsl_double/)
  call unit_assert_true('fgsl_poly_complex_workspace_alloc',fgsl_well_defined(wk),&
       .true.)
  status = fgsl_poly_complex_solve(xa, 4_fgsl_size_t, wk, z)
  call fgsl_poly_complex_workspace_free(wk)
  call unit_assert_equal_within('fgsl_poly_complex_solve',&
        cos(2.0d0*m_pi/3.0d0)+(0.0d0,1.0d0)*sin(2.0d0*m_pi/3.0d0),z(1),eps10)
  call unit_assert_equal_within('fgsl_poly_complex_solve',&
        cos(2.0d0*m_pi/3.0d0)-(0.0d0,1.0d0)*sin(2.0d0*m_pi/3.0d0),z(2),eps10)
  call unit_assert_equal_within('fgsl_poly_complex_solve',&
        (1.0d0,0.0d0),z(3),eps10)
!
! Done
!
  call unit_finalize()
end program poly
