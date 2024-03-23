module mod_error
  use, intrinsic :: iso_c_binding
  use fgsl
  use mod_unit
  implicit none
  character(kind=fgsl_char,len=fgsl_strmax) :: test_message
contains
  subroutine mine_sr(reason, file, line, errno) bind(c)
    type(c_ptr), value :: reason, file
    integer(c_int), value :: line, errno
!
    test_message = fgsl_strerror(errno)
    write(6, *) 'Processed error in ',trim(fgsl_name(file)), ' at line ',line
    write(6, *) 'Reason was: ',trim(fgsl_name(reason))
  end subroutine mine_sr
end module mod_error
program error
  use mod_error
  implicit none
!
  integer(fgsl_int), parameter :: num_tests=10
  integer(fgsl_size_t), parameter :: nmax=10
  type(fgsl_error_handler_t) :: std, off, mine
  type(fgsl_interp) :: interp
  integer :: i
  integer(fgsl_int) :: status
  character(kind=fgsl_char, len=fgsl_strmax) :: message
  real(fgsl_double), dimension(10) :: xa, ya
!------------------------------------------------------------------------------
! Testing the Fortran error interface to GSL
!------------------------------------------------------------------------------
  call unit_init(20)
!  write object sizes
! use an interpolation object to test
  do i=1,nmax
     xa(i) = dble(i-1)
     ya(i) = dble(i-1)**3.0_fgsl_double
  end do
!
  std = fgsl_set_error_handler_off()
!
  interp = fgsl_interp_alloc(fgsl_interp_cspline,1_fgsl_size_t)
  call unit_assert_true('fgsl_interp_alloc', &
       .not. fgsl_well_defined(interp), .true.)
  interp = fgsl_interp_alloc(fgsl_interp_cspline,nmax)
  status = fgsl_interp_init(interp,xa(1:1),ya(1:1))
  call unit_assert_equal('fgsl_interp_init:status',fgsl_einval,status)
  message = fgsl_strerror(status)
  call unit_assert_equal('fgsl_interp_init', &
       'invalid argument supplied by user',trim(message))
  status = fgsl_interp_init(interp,xa,ya)
  call unit_assert_equal('fgsl_interp_init:status',fgsl_success,status)
!
! own error handler
!
  mine = fgsl_error_handler_init(mine_sr)
  off = fgsl_set_error_handler(mine)
  status = fgsl_interp_init(interp,xa(1:1),ya(1:1))
!  write(6, *) trim(test_message)
  call unit_assert_equal('fgsl_interp_init:own handler', &
       'invalid argument supplied by user',trim(test_message))
!  need to build with preprocessing for next line
  call fgsl_error("my_own_call",'fgsl_error',__LINE__,2)
!  call fgsl_error("my_own_call","error.f90",80,2)
  write(6, *) trim(test_message)
  call unit_assert_equal('fgsl_interp_init:own handler', &
       'output range error',trim(test_message))
  write(6, *) 'now check GSL-internal abort ...'
!
! Done
!
  call unit_finalize()
  flush(6)
!
! Copy back standard handler
!
  call fgsl_interp_free(interp)
  off = fgsl_set_error_handler(std)
  interp = fgsl_interp_alloc(fgsl_interp_cspline,1_fgsl_size_t)
! the following lines should not be reached
  call unit_init(1)
  call unit_assert_equal('fgsl_interp_alloc:nofail',0,1)
  call unit_finalize()
end program error
