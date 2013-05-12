program ntuplew
  use fgsl
  use, intrinsic :: iso_c_binding
  implicit none
  type, bind(c) :: data
    real(c_double) :: x, y, z
  end type
  type(data), target :: ntuple_row
  type(fgsl_rng) :: r
  type(fgsl_rng_type) :: t
  type(fgsl_ntuple) :: ntuple
  integer(fgsl_int) :: i, status
  integer(fgsl_size_t) :: sz
  type(c_ptr) :: pvoid

  pvoid = c_loc(ntuple_row)
  sz = 3 * fgsl_sizeof(1.0_c_double)
! Note: using c_loc(...) as actual arg fails for g95. Bug?
  ntuple = fgsl_ntuple_create ('test.dat', pvoid, sz)
  t = fgsl_rng_env_setup()
  t = fgsl_rng_default
  r = fgsl_rng_alloc (t)
  do i=1, 10000
     ntuple_row%x = fgsl_ran_ugaussian(r)
     ntuple_row%y = fgsl_ran_ugaussian(r)
     ntuple_row%z = fgsl_ran_ugaussian(r)
     status = fgsl_ntuple_write(ntuple)
  end do
  status = fgsl_ntuple_close(ntuple)
end program ntuplew
