program bspline_interp
  !> Transcription of ninth bspline example to Fortran
  !> Interpolation
  use fgsl_bspline
  use fgsl_array
  use fgsl_linalg
  implicit none
  
  integer(fgsl_size_t), parameter :: n = 9          ! number of data points to interpolate
  integer(fgsl_size_t), parameter :: k = 4          ! spline order   

  real(fgsl_double), target :: x_data(n) = &
      [ real(fgsl_double) ::  0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ]
  real(fgsl_double), target :: y_data(n) = &
      [ real(fgsl_double) ::  3.0, 2.9, 2.5, 1.0, 0.9, 0.8, 0.5, 0.2, 0.1 ]
  real(fgsl_double), target :: cf(n)                ! control points for spline    
  real(fgsl_double), target :: xbf(3*(k-1) + 1, n)  ! banded collocation matrix    
  type(fgsl_uint), target :: ipivf
  type(fgsl_vector) :: xv, yv, c
  type(fgsl_matrix) :: xb
  type(fgsl_vector_uint) :: ipiv
      
  type(fgsl_bspline_workspace) :: work
  
  real(fgsl_double) :: result, t
  integer(fgsl_size_t) :: i
  integer(fgsl_int) :: iu, status
  
  ! set up objects
  xv = fgsl_vector_init(x_data)
  yv = fgsl_vector_init(y_data)
  c = fgsl_vector_init(cf)
  xb = fgsl_matrix_init(xbf)
  
  work = fgsl_bspline_alloc_ncontrol(k, n)
  
  allocate(ipivf%array(n))
  ipiv = fgsl_vector_init(ipivf)
  
  ! set up I/O
  open(newunit=iu, file='bspline_interp.txt', form='FORMATTED', &
	   status='REPLACE', action='WRITE') 
  
  ! initialize knots for interpolation
  status = fgsl_bspline_init_interp(xv, work)

  ! compute collocation matrix for interpolation 
  status = fgsl_bspline_col_interp(xv, xb, work)

  ! solve linear system with banded LU
  status = fgsl_linalg_lu_band_decomp(n, k - 1, k - 1, xb, ipiv)
  status = fgsl_linalg_lu_band_solve(k - 1, k - 1, xb, ipiv, yv, c);

  ! write data to external file
  do i = 1, n
    write(iu, fmt='(2(F0.6,1X))') x_data(i), y_data(i)
  end do
  write(iu, fmt='(/)') 
  
  ! write spline to external file
  t = x_data(1)
  do 
    if (t > x_data(n)) exit
    
    status = fgsl_bspline_calc(t, c, result, work)
    write(iu, fmt='(2(F0.6,1X))') t, result
    
    t = t + .005_fgsl_double
  end do
  
  call fgsl_bspline_free(work)
  deallocate(ipivf%array)
  close(iu)
end program bspline_interp
