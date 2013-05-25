program fft
  use fgsl
  use mod_unit
  implicit none
  real(fgsl_double), parameter :: eps10 = 1.0d-10
  complex(fgsl_double), parameter :: ai = (0.0d0, 1.0d0), ui=(1.0d0, 0.0d0)
  complex(fgsl_double) :: data(4)
  real(fgsl_double) :: rdata(4)
  type(fgsl_fft_complex_wavetable) :: wavetable
  type(fgsl_fft_complex_workspace) :: work
  integer(fgsl_int) :: status
  type(fgsl_error_handler_t) :: std
!
  std = fgsl_set_error_handler_off()
  call unit_init(60)
!
! Test FFT routines
!
  data(1) = 1.0d0*ui
  data(2) = 1.0d0*ui
  data(3) = 0.0d0*ui
  data(4) = 1.0d0*ui
  status = fgsl_fft_complex_radix2_forward(data, 1_fgsl_size_t, 4_fgsl_size_t)
  call unit_assert_equal('fgsl_fft_complex_radix2_forward:status', &
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_gsl_fft_complex_radix2_forward:data',&
       (/3.0d0*ui, ui, -ui, ui /),data,eps10)
  data(1) = 1.0d0*ui
  data(2) = 1.0d0*ui
  data(3) = 0.0d0*ui
  data(4) = 1.0d0*ui
  status = fgsl_fft_complex_radix2_backward(data, 1_fgsl_size_t, 4_fgsl_size_t)
  call unit_assert_equal('fgsl_fft_complex_radix2_backward:status', &
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_gsl_fft_complex_radix2_backward:data',&
       (/3.0d0*ui, ui, -ui, ui /),data,eps10)
  data(1) = 1.0d0*ui
  data(2) = 1.0d0*ui
  data(3) = 0.0d0*ui
  data(4) = 1.0d0*ui
  status = fgsl_fft_complex_radix2_inverse(data, 1_fgsl_size_t, 4_fgsl_size_t)
  call unit_assert_equal('fgsl_fft_complex_radix2_inverse:status', &
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_gsl_fft_complex_radix2_inverse:data',&
       (/.75d0*ui, .25d0*ui, -.25d0*ui, .25d0*ui /),data,eps10)
  data(1) = 1.0d0*ui
  data(2) = 1.0d0*ui
  data(3) = 0.0d0*ui
  data(4) = 1.0d0*ui
  status = fgsl_fft_complex_radix2_transform(data, 1_fgsl_size_t, &
       4_fgsl_size_t, 1_fgsl_int ) 
  call unit_assert_equal('fgsl_fft_complex_radix2_transform:status', &
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_gsl_fft_complex_radix2_transform:data',&
       (/3.0d0*ui, ui, -ui, ui /),data,eps10)
  wavetable = fgsl_fft_complex_wavetable_alloc(4_fgsl_size_t)
  work = fgsl_fft_complex_workspace_alloc(4_fgsl_size_t)
  data(1) = 1.0d0*ui
  data(2) = 1.0d0*ui
  data(3) = 0.0d0*ui
  data(4) = 1.0d0*ui
  status = fgsl_fft_complex_forward(data, 1_fgsl_size_t, 4_fgsl_size_t, &
       wavetable, work)
  call unit_assert_equal('fgsl_fft_complex_forward:status', &
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_gsl_fft_complex_forward:data',&
       (/3.0d0*ui, ui, -ui, ui /),data,eps10)
  data(1) = 1.0d0*ui
  data(2) = 1.0d0*ui
  data(3) = 0.0d0*ui
  data(4) = 1.0d0*ui
  status = fgsl_fft_complex_backward(data, 1_fgsl_size_t, 4_fgsl_size_t, &
       wavetable, work)
  call unit_assert_equal('fgsl_fft_complex_backward:status', &
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_gsl_fft_complex_backward:data',&
       (/3.0d0*ui, ui, -ui, ui /),data,eps10)
  data(1) = 1.0d0*ui
  data(2) = 1.0d0*ui
  data(3) = 0.0d0*ui
  data(4) = 1.0d0*ui
  status = fgsl_fft_complex_inverse(data, 1_fgsl_size_t, 4_fgsl_size_t, &
       wavetable, work)
  call unit_assert_equal('fgsl_fft_complex_inverse:status', &
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_gsl_fft_complex_inverse:data',&
       (/.75d0*ui, .25d0*ui, -.25d0*ui, .25d0*ui/),data,eps10)
  data(1) = 1.0d0*ui
  data(2) = 1.0d0*ui
  data(3) = 0.0d0*ui
  data(4) = 1.0d0*ui
  status = fgsl_fft_complex_transform(data, 1_fgsl_size_t, 4_fgsl_size_t, &
       wavetable, work, 1_fgsl_int)
  call unit_assert_equal('fgsl_fft_complex_transform:status', &
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_gsl_fft_complex_transform:data',&
       (/3.0d0*ui, ui, -ui, ui /),data,eps10) 
!
  rdata(1) = 1.0d0
  rdata(2) = 1.0d0
  rdata(3) = 0.0d0
  rdata(4) = 1.0d0
  status = fgsl_fft_real_radix2_transform(rdata, 1_fgsl_size_t, &
       4_fgsl_size_t)
  call unit_assert_equal('fgsl_fft_real_radix2_transform:status', &
       fgsl_success,status)
  status = fgsl_fft_real_unpack(rdata, data, 1_fgsl_size_t, &
       4_fgsl_size_t)
  call unit_assert_equal_within('fgsl_gsl_fft_complex_radix2_forward:data',&
       (/3.0d0*ui, ui, -ui, 0.0d0*ui /),data,eps10)

  
!
  call fgsl_fft_complex_workspace_free(work)
  call fgsl_fft_complex_wavetable_free(wavetable) 
! 
! Done
!
  call unit_finalize()
end program fft
