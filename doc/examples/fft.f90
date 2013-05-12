program fft
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: nsize = 128_fgsl_size_t
  complex(fgsl_double), target :: data(nsize) 
  integer(fgsl_int) :: status, i
!
  data = (0.0d0, 0.0d0)
  do i=1, 11
     data(i) = (1.0d0, 0.0d0) ; data(nsize - i + 1) = (1.0d0, 0.0d0)
  end do
  data(nsize - 10) = (0.0d0, 0.0d0)
  write(*, *) 'Original data: '
  write(*, fmt='(2F12.5)') data

  status = fgsl_fft_complex_radix2_forward(data, 1_fgsl_size_t, nsize)

  write(*, *) 'Transformed and normalized data: '
  write(*, fmt='(2F12.5)') data/sqrt(dble(nsize))

end program fft
