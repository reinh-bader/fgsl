program math
  use fgsl
  use mod_unit
  implicit none

  real(fgsl_double) :: r

  call unit_init(10)

  r = fgsl_hypot(3.0_fgsl_double, 4.0_fgsl_double)
  call unit_assert_equal_within('fgsl_hypot',5.0d0,r,1.0d-12)
  r = fgsl_hypot3(3.0_fgsl_double, 4.0_fgsl_double, 0.0_fgsl_double)
  call unit_assert_equal_within('fgsl_hypot3',5.0d0,r,1.0d-12)
  



  call unit_finalize()
end program math
