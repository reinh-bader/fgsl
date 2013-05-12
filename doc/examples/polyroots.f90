program polyroots
  use fgsl
  implicit none
  integer(fgsl_int) :: i, status
  real(fgsl_double) :: a(6) = (/-1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D0 /)
  complex(fgsl_double) :: z(5)
  type(fgsl_poly_complex_workspace) :: w
!
  w = fgsl_poly_complex_workspace_alloc (6_fgsl_size_t)
  status = fgsl_poly_complex_solve (a, 6_fgsl_size_t, w, z)
  call fgsl_poly_complex_workspace_free (w)

  do i=1, 5
     write(6, '(''z'',I0,'' = '',F20.18,'' + i*'',F20.18)') i,z(i) 
  end do
end program polyroots
