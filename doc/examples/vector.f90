program vector
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: ndim = 12
  real(fgsl_double), target :: v(ndim)
  real(fgsl_double), pointer :: p_slice(:), p_vec(:)
  integer(fgsl_int) :: i, status
  type(fgsl_vector) :: vec, slice

  v(1:ndim) = (/ (dble(i)+0.1_fgsl_double, i=1,ndim) /)
  slice = fgsl_vector_init(v(3:), stride=3_fgsl_size_t)
  vec = fgsl_vector_init(v)
  p_slice => fgsl_vector_to_fptr(slice)
  p_vec => fgsl_vector_to_fptr(vec)
  write(*, '(''Size of slice pointer is: '',i3)') size(p_slice)
  write(*, '(''Size of complete pointer is: '',i3)') size(p_vec)
  write(*, '(''Components: '',4F12.5)') p_slice(1:size(p_slice))
  write(*, '(''Should be : '',4F12.5)') v(3::3)
  v(6) = v(6) + 1.0_fgsl_double
  write(*, '(''Increase value of 2nd element of slice: '',2F12.5)') &
       p_slice(2), p_vec(6)
  call fgsl_vector_free(slice)
  call fgsl_vector_free(vec)
end program vector
