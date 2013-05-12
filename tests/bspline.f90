program bspline
  use mod_unit
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: nbreak = 10, ncbf = nbreak + 2
  real(fgsl_double), parameter :: eps7 = 1.0D-7
  integer(fgsl_int) :: status, i
  type(fgsl_bspline_workspace) :: sw
  type(fgsl_vector) :: b, k
  real(fgsl_double), target :: bv(ncbf), kv(nbreak)
  real(fgsl_double), pointer :: bp(:)
!
! Test B-Spline API
!
  call unit_init(10)
!
  sw = fgsl_bspline_alloc(4_fgsl_size_t, nbreak)
  kv = (/ (dble(i-1), i=1,nbreak) /)
  b = fgsl_vector_init(1.0d0) ; k = fgsl_vector_init(1.0d0)
  status = fgsl_vector_align(kv, nbreak, k, nbreak, 0_fgsl_size_t, &
       1_fgsl_size_t)
  status = fgsl_vector_align(bv, ncbf, b, ncbf, 0_fgsl_size_t, &
       1_fgsl_size_t)
  status = fgsl_vector_align(bp, b)
!  write(6, *) 'Size = ',size(bp)
  status = fgsl_bspline_knots(k, sw)
  call unit_assert_equal('fgsl_bspline_knots:status',fgsl_success,status)
  status = fgsl_bspline_eval(3.0_fgsl_double, b, sw)
  call unit_assert_equal('fgsl_bspline_eval:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_bspline_eval',(/0.d0,0.d0,0.d0,&
       0.16666666666d0,0.66666666666d0,0.16666666666d0,0.d0,0.d0,0.d0,&
       0.d0,0.d0,0.d0/),bv,eps7)
  status = fgsl_bspline_knots_uniform(0.0d0, dble(nbreak-1), sw)
  call unit_assert_equal('fgsl_bspline_knots_uniform:status',fgsl_success,status)
  status = fgsl_bspline_eval(3.0_fgsl_double, b, sw)
  call unit_assert_equal('fgsl_bspline_eval:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_bspline_eval',(/0.d0,0.d0,0.d0,&
       0.16666666666d0,0.66666666666d0,0.16666666666d0,0.d0,0.d0,0.d0,&
       0.d0,0.d0,0.d0/),bv,eps7)
!FIXME: some tests still missing.

  call fgsl_bspline_free(sw); call fgsl_vector_free(k) ; call fgsl_vector_free(b)
!
! Done
!
  call unit_finalize()
end program bspline
