module mod_bspline_proj
  use fgsl_bspline
  use fgsl_linalg
  use fgsl_math
  implicit none
  
  integer(c_size_t), parameter :: k = 4          ! spline order
                                                 ! data interval [a,b] 
  real(fgsl_double), parameter :: a = -2.0_fgsl_double
  real(fgsl_double), parameter :: b =  2.0_fgsl_double
  integer(c_size_t), parameter :: nbreak = 10    ! breakpoints
contains
  function fpol(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: fpol
    
    if (c_associated(params)) continue
    fpol = (x * (-7.0_c_double + x*(-2.0_c_double + x*3.0_c_double)))
  end function fpol
end module mod_bspline_proj
program bspline_proj
  !> Transcription of eighth bspline example to Fortran
  !> Projecting onto the B-spline basis
  use mod_bspline_proj
  implicit none
  
  type(fgsl_bspline_workspace) :: work
  real(fgsl_double), allocatable, target :: cf(:), yf(:), gf(:,:)
  type(fgsl_vector) :: c, y
  type(fgsl_matrix) :: g
  integer(fgsl_size_t) :: n
  integer(fgsl_int) :: iu, status
  real(fgsl_double) :: result, x
  
  type(fgsl_function) :: f
  
  ! set up all objects 
  work = fgsl_bspline_alloc(k,nbreak)
  n = fgsl_bspline_ncontrol(work)  ! number of control points

  allocate(cf(n), yf(n), gf(k, n)) 
  c = fgsl_vector_init(cf)  
  y = fgsl_vector_init(yf)  
  g = fgsl_matrix_init(gf)  
  
  f = fgsl_function_init(fpol, c_null_ptr)
  
  ! set up I/O
  open(newunit=iu, file='bspline_proj.txt', form='FORMATTED', &
	   status='REPLACE', action='WRITE') 
  
  ! use uniform breakpoints on [a, b] 
  status = fgsl_bspline_init_uniform(a, b, work)

  ! compute Gram matrix
  status = fgsl_bspline_gram(0_fgsl_size_t, G, work)

  ! construct rhs vector
  status = fgsl_bspline_proj_rhs(f, y, work)

  ! solve system 
  status = fgsl_linalg_cholesky_band_decomp(g)
  status = fgsl_linalg_cholesky_band_solve(g, y, c)

  ! write results to external file
  x = a
  do 
    if (x >= b) exit
    
    status = fgsl_bspline_calc(x, c, result, work)

    write(iu, fmt='(F0.6,1X, 2(ES0.6E2,1X))') x, fpol(x, c_null_ptr), result

    x = x + .01_fgsl_double
  end do
  
  call fgsl_bspline_free(work)
  deallocate(cf, yf, gf)
  close(iu) 
  
   
end program bspline_proj
