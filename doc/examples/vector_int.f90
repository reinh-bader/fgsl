program vector_int
  use fgsl
  implicit none
  integer(fgsl_size_t), parameter :: ndim = 12
  integer(fgsl_int), target :: v(ndim)
  integer(fgsl_int), pointer :: p_slice(:), p_vec(:)
  integer(fgsl_int) :: i
  type(fgsl_vector_int) :: vec, slice

  v(1:ndim) = [ (i, i=1,ndim) ]
  slice = fgsl_vector_init(v(3:), stride=3_fgsl_size_t)
  vec = fgsl_vector_init(v)
  p_slice => fgsl_vector_to_fptr(slice)
  p_vec => fgsl_vector_to_fptr(vec)
  write(*, '(''Size of slice pointer is: '',i3)') size(p_slice)
  write(*, '(''Size of complete pointer is: '',i3)') size(p_vec)
  write(*, '(''Components: '',4i4)') p_slice(1:size(p_slice))
  write(*, '(''Should be : '',4i4)') v(3::3)
  v(6) = v(6) + 1
  write(*, '(''Increase value of 2nd element of slice: '',2i4)') &
       p_slice(2), p_vec(6)
  call fgsl_vector_free(slice)
  call fgsl_vector_free(vec)
end program vector_int
