program dwt
  use fgsl
  integer(fgsl_size_t), parameter :: N=256, NC=20
  integer(fgsl_int) :: i, status
  real(fgsl_double) :: data(N), abscoeff(N)
  integer(fgsl_size_t) :: p(N)
  type(fgsl_wavelet) :: w
  type(fgsl_wavelet_workspace) :: work
!
  w = fgsl_wavelet_alloc(fgsl_wavelet_daubechies, 4_fgsl_size_t)
  work = fgsl_wavelet_workspace_alloc (N)
  open(unit=20, file=DWT_DAT, form='FORMATTED', status='OLD')
  do i=1,N
     read(20, fmt=*) data(i)
  end do
  close(unit=20)
  status = fgsl_wavelet_transform_forward(w, data, 1_fgsl_size_t, N, work)
  do i=1,N
     abscoeff(i) = abs(data(i))
  end do
  call fgsl_sort_index(p, abscoeff, 1_fgsl_size_t, N)
  do i=1,N-NC
     data(p(i)+1) = 0.0_fgsl_double
  end do
  status = fgsl_wavelet_transform_inverse(w, data, 1_fgsl_size_t, N, work)
  do i=1,N
     write(6, '(F15.12)') data(i)
  end do
  call fgsl_wavelet_workspace_free(work)
  call fgsl_wavelet_free(w)
end program dwt
