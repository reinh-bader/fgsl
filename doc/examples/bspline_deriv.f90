program bspline_deriv
    !> Transcription of second bspline example to Fortran
    !> Derivatives of Basis splines
    use fgsl_bspline
    use fgsl_array
    implicit none
  
	integer(c_size_t), parameter :: nbreak = 6       ! break points
	integer(c_size_t), parameter :: spline_order = 4 ! cubic splines
	
	type(fgsl_bspline_workspace) :: w
	integer(c_size_t) :: i, istart, j, k, p, n
	integer(c_int) :: status
	real(fgsl_double) :: a, b, dx, xj
	
	type(fgsl_vector) :: knots
	type(fgsl_matrix) :: db
	real(fgsl_double), allocatable, target :: dbf(:,:)
	real(fgsl_double), pointer, contiguous :: knotsf(:)
	
	integer iu_knots, iu_spline
	
	w = fgsl_bspline_alloc(spline_order, nbreak)
	p = fgsl_bspline_ncontrol(w)
	n = 300
	a = 0._fgsl_double; b = 1._fgsl_double
	dx = (b - a) / (real(n, fgsl_double) - 1.0_fgsl_double)
	
	open(newunit=iu_knots, file='bspline_deriv_knotsvetor.txt', form='FORMATTED', &
		 status='REPLACE', action='WRITE')
	open(newunit=iu_spline, file='bspline_deriv.txt', form='FORMATTED', &
		 status='REPLACE', action='WRITE')
		 
	allocate(dbf(spline_order, p))
	
	db = fgsl_matrix_init(dbf)  
	
	! use uniform breakpoints on [a, b] 
	
	status = fgsl_bspline_init_uniform(a, b, w)
	knots = fgsl_bspline_return_knots_vector(w)
	knotsf => fgsl_vector_to_fptr(knots) 
	
	! output knot vector    
	write(iu_knots, fmt='(F8.6)') knotsf 
	write(iu_knots, fmt='(/)')       
	
	do i=1, spline_order
	  do j = 1, n
		xj = (real(j-1, fgsl_double) * dx)
		status = fgsl_bspline_eval_deriv_basis(xj, i-1, db, w)
		write(iu_spline,  advance='NO', fmt='(F0.6,1X)') xj
		do k=1, p
		  write(iu_spline, advance='NO', fmt='(F0.6,1X)') dbf(i, k)
		end do
		write(iu_spline, fmt=*) 
	  end do
	  write(iu_spline, fmt='(/)') 
	end do
	
	call fgsl_bspline_free(w); nullify(knotsf)
	deallocate(dbf)
	
	close(iu_knots)
	close(iu_spline)
	
end program bspline_deriv
