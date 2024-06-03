program bspline_knots
  !> Transcription of first bspline example to Fortran
  !> Basis splines and uniform knots 
  use fgsl_bspline
  use fgsl_array
  implicit none
  
  integer(fgsl_size_t), parameter :: nbreak = 11 ! number of breakpoints
  integer iu_knots, iu_spline, iu_spline_alt
  
  
  open(newunit=iu_knots, file='bspline_knotsvector.txt', form='FORMATTED', &
       status='REPLACE', action='WRITE')
  open(newunit=iu_spline, file='bspline_knots.txt', form='FORMATTED', &
       status='REPLACE', action='WRITE')
  open(newunit=iu_spline_alt, file='bspline_knots_alt.txt', form='FORMATTED', &
       status='REPLACE', action='WRITE')
       
  call print_basis(2_fgsl_size_t) ! linear splines
  call print_basis(3_fgsl_size_t) ! quadratic splines
  call print_basis(4_fgsl_size_t) ! cubic splines
  call print_basis(5_fgsl_size_t) ! quartic splines
  
  close(iu_knots)
  close(iu_spline)
  close(iu_spline_alt)
contains
  subroutine print_basis(order)
    integer(fgsl_size_t), intent(in) :: order
    
    type(fgsl_bspline_workspace) :: w
    integer(c_size_t) :: i, istart, j, k, p, n
    integer(c_int) :: status
    real(fgsl_double) :: a, b, dx, xi
    
    type(fgsl_vector) :: bv, bvk, knots
    real(fgsl_double), allocatable, target :: bvf(:), bvkf(:)
    real(fgsl_double), pointer, contiguous :: knotsf(:)
    
    w = fgsl_bspline_alloc(order, nbreak)
    p = fgsl_bspline_ncontrol(w)
    n = 300
    a = 0._fgsl_double; b = 1._fgsl_double
    dx = (b - a) / (real(n, fgsl_double) - 1.0_fgsl_double)
    
    allocate(bvf(p))
    bv = fgsl_vector_init(bvf)
    
    k = fgsl_bspline_order(w)
    allocate(bvkf(k))
    bvk = fgsl_vector_init(bvkf)
    
    ! use uniform breakpoints on [a, b] 
    
    status = fgsl_bspline_init_uniform(a, b, w)
    knots = fgsl_bspline_return_knots_vector(w)
    knotsf => fgsl_vector_to_fptr(knots)
   
    ! output knot vector    
    write(iu_knots, fmt='(F8.6)') knotsf 
    write(iu_knots, fmt='(/)')   
   
    do i = 1, n
      xi = (real(i-1, fgsl_double) * dx)
      
      ! variant 1 uses undocumented API call
      status = fgsl_bspline_eval_basis(xi, bv, w)
      write(iu_spline,  advance='NO', fmt='(F8.6,1X)') xi
      do j=1, p
        write(iu_spline, advance='NO', fmt='(F8.6,1X)') bvf(j)
      end do
      write(iu_spline, fmt=*) 
      
      ! variant 2
      status = fgsl_bspline_basis(xi, bvk, istart, w)
      write(iu_spline_alt,  advance='NO', fmt='(F8.6,1X,I2, 1X)') xi, istart
      do j=1, k
        ! bvkf(j) corresponds to bvf(istart+j)
        write(iu_spline_alt, advance='NO', fmt='(F8.6,1X)') bvkf(j)
      end do
      write(iu_spline_alt, fmt=*)      
    end do
    
    
    write(iu_spline, fmt='(/)') 
    write(iu_knots, fmt='(/)') 
    write(iu_spline_alt, fmt='(/)') 
   
    call fgsl_bspline_free(w); nullify(knotsf)
    deallocate(bvf, bvkf)
    
  end subroutine print_basis
end program
