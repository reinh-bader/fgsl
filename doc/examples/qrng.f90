program qrng
  use fgsl
  implicit none
  integer(fgsl_int) :: i, status
  real(fgsl_double) :: v(2)
  type(fgsl_qrng) :: q
!
  q = fgsl_qrng_alloc(fgsl_qrng_sobol, 2)
  do i=1, 1024
     status = fgsl_qrng_get(q, v)
     write(6, '(2(F7.5,1X))') v
  end do
  call fgsl_qrng_free(q)
end program qrng
