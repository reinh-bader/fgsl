program vector
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: ndim = 12
  real(fgsl_double) :: v(ndim)
  real(fgsl_double), pointer :: p_v(:)
  integer(fgsl_int) :: i, status
  type(fgsl_vector) :: vec

  v(1:ndim) = (/ (dble(i)+0.1_fgsl_double, i=1,ndim) /)
  vec = fgsl_vector_init(1.0_fgsl_double)
  status = fgsl_vector_align(v, ndim, vec, 3_fgsl_size_t, 2_fgsl_size_t, &
       3_fgsl_size_t)
  if (status == fgsl_success) then
     status = fgsl_vector_align(p_v, vec)
     write(6, '(''Size of pointer is: '',i3)') size(p_v)
     write(6, '(''Components: '',3F12.5)') p_v(1:size(p_v))
     write(6, '(''Should be : '',3F12.5)') (/3.1D0,6.1D0,9.1D0/)
     v(6) = v(6) + 1.0_fgsl_double
     write(6, '(''Increased 2nd component by one: '',F12.5)') p_v(2)
  else
     write(6, *) 'Failed to properly initialize vector object.'
  end if
  call fgsl_vector_free(vec)
end program vector
