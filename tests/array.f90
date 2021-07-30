program array
  use fgsl
  use mod_unit
  implicit none
  integer(fgsl_size_t), parameter :: nmax = 20
  real(fgsl_double), parameter :: eps10 = 1.0d-10
  integer :: i, j, status
  real(fgsl_double), dimension(nmax), target :: xa
  real(fgsl_double), dimension(nmax) :: ya
  real(fgsl_double), dimension(nmax,nmax), target :: a2d
  real(fgsl_double), dimension(nmax,nmax) :: b2d
  complex(fgsl_double), dimension(nmax), target :: xca
  complex(fgsl_double), dimension(nmax) :: yca
  complex(fgsl_double), dimension(nmax,nmax), target :: ac2d
  complex(fgsl_double), dimension(nmax,nmax) :: bc2d
  real(fgsl_double), pointer, dimension(:) :: fptr => null()
  complex(fgsl_double), pointer, dimension(:) :: fcptr => null()
  real(fgsl_double), pointer, dimension(:,:) :: fmptr => null()
  complex(fgsl_double), pointer, dimension(:,:) :: fcmptr => null()
  type(fgsl_vector) :: fvec
  type(fgsl_vector_complex) :: fcvec
  type(fgsl_matrix) :: fmat
  type(fgsl_matrix_complex) :: fcmat
!
! Test vector and matrix support routines
!
  call unit_init(50)
!
!
! Array processing
!
  do i=1,nmax
     xa(i) = dble(i)
     xca(i) = cmplx(dble(i), dble(2*i), fgsl_double)
  end do
!  write(6, *) xca
  fvec = fgsl_vector_init(xa(3:15),3_fgsl_size_t)
  fcvec = fgsl_vector_init(xca(3:15),3_fgsl_size_t)
  ya(1:5) = fvec
  yca(1:5) = fcvec
  call unit_assert_equal_within('fgsl_vector:assign',&
       (/3.0d0,6.0d0,9.0d0,12.0d0,15.0d0/),ya(1:5),eps10)
!  write(6, *) ya(1:5)
  call unit_assert_equal_within('fgsl_vector:assign',&
       (/(3.0d0,6.0d0),(6.0d0,1.2d1),(9.0d0,1.8d1),(1.20d1,2.4d1),&
       (1.5d1,3.0d1)/),yca(1:5),eps10)
!  write(6, *) yca(1:5)
  fptr => fgsl_vector_to_fptr(fvec)
  fcptr => fgsl_vector_to_fptr(fcvec)
!  write(6, *) fcptr(1:5)
  call unit_assert_equal_within('fgsl_vector:align(out)',&
       (/3.0d0,6.0d0,9.0d0,12.0d0,15.0d0/),fptr,eps10)
  call fgsl_vector_free(fvec)
  call unit_assert_equal_within('fgsl_vector_complex:align(out)',&
       (/(3.0d0,6.0d0),(6.0d0,1.2d1),(9.0d0,1.8d1),(1.20d1,2.4d1),&
       (1.5d1,3.0d1)/),fcptr,eps10)
  call fgsl_vector_free(fcvec)
! 
  do j=1,6
     do i=1,3
        a2d(i, j) = 10*j+i
        ac2d(i, j) = cmplx(10*j+i, 10*j-i)
     end  do
  end do
  fmat = fgsl_matrix_init(a2d, 3_fgsl_size_t,6_fgsl_size_t)
  b2d(1:3,1:6) = fmat
  call unit_assert_equal_within('fgsl_matrix:assign',a2d(1:3,1:6),&
       b2d(1:3,1:6),eps10)
  fmptr => fgsl_matrix_to_fptr(fmat)
  call unit_assert_equal_within('fgsl_matrix:assign',a2d(1:3,1:6),&
       fmptr,eps10)
  call fgsl_matrix_free(fmat)
  fcmat = fgsl_matrix_init(ac2d, 3_fgsl_size_t,6_fgsl_size_t)
  bc2d(1:3,1:6) = fcmat
  call unit_assert_equal_within('fgsl_matrix_complex:assign',ac2d(1:3,1:6),&
       bc2d(1:3,1:6),eps10)
  fcmptr => fgsl_matrix_to_fptr(fcmat)
  call unit_assert_equal_within('fgsl_matrix_complex:assign',ac2d(1:3,1:6),&
       fcmptr,eps10)
  call fgsl_matrix_free(fcmat)
!
! Done
!
  call unit_finalize()
end program array
