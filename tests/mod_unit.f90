module mod_unit
! utilities for unit testing
  implicit none
  private
  public :: unit_debug, unit_init, unit_finalize, unit_assert_equal, &
       unit_assert_equal_within, unit_assert_true
  integer, parameter :: ik = selected_int_kind(6)
  integer, parameter :: lk = selected_int_kind(14)
  integer, parameter :: dk = selected_real_kind(15)
!FIXME - PGI does not accept  logical, protected :: unit_debug = .false.
  logical :: unit_debug = .false.
  logical, allocatable :: unit_ok(:)
  integer :: index_ok
  interface unit_assert_equal
     module procedure unit_assert_equal_integer
     module procedure unit_assert_equal_long
     module procedure unit_assert_equal_integer_array
     module procedure unit_assert_equal_string
  end interface
  interface unit_assert_equal_within
     module procedure unit_assert_equal_within_double
     module procedure unit_assert_equal_within_double_array
     module procedure unit_assert_equal_within_double_2d_array
     module procedure unit_assert_equal_within_double_complex
     module procedure unit_assert_equal_within_double_complex_array
     module procedure unit_assert_equal_within_double_complex_2d_array
   end interface
contains
  subroutine unit_init(num_tests)
    integer, intent(in) :: num_tests
    if (allocated(unit_ok)) then
       write(6, *) 'unit_init: Please call unit_finalize before reinitializing.'
       stop
    else
       allocate(unit_ok(num_tests))
       index_ok = 1
       unit_ok = .true.
    end if
    inquire(file='unit_debug',exist=unit_debug)
! FIXME debug read-in
  end subroutine unit_init
  subroutine unit_assert_equal_integer(description, ival_target, ival_check)
    character(len=*), intent(in) :: description
    integer(kind=ik), intent(in) :: ival_target, ival_check
    call unit_toggle(description, ival_target == ival_check)
  end subroutine unit_assert_equal_integer
  subroutine unit_assert_equal_long(description, ival_target, ival_check)
    character(len=*), intent(in) :: description
    integer(kind=lk), intent(in) :: ival_target, ival_check
    call unit_toggle(description, ival_target == ival_check)
  end subroutine unit_assert_equal_long
  subroutine unit_assert_equal_integer_array(description, &
       ival_target, ival_check)
    character(len=*), intent(in) :: description
    integer(kind=ik), intent(in) :: ival_target(:), ival_check(:)
    integer(kind=ik) :: i
    logical :: ok
    if (size(ival_target) /= size(ival_check)) then
       call unit_toggle(description, .false.)
    end if
    ok = .true.
    do i=1,size(ival_target)
       if (ival_target(i) /= ival_check(i)) ok= .false.
    end do
    call unit_toggle(description, ok)
    if (unit_debug .and. .not. ok) then
       do i=1,min(size(ival_target),size(ival_check))
          write(6,fmt='(a,4x,2(I0,2X))') description,ival_target(i),ival_check(i)
       end do
    end if
  end subroutine unit_assert_equal_integer_array
  subroutine unit_assert_equal_string(description, ival_target, ival_check)
    character(len=*), intent(in) :: description
    character(len=*), intent(in) :: ival_target, ival_check
    call unit_toggle(description, lge(ival_target,ival_check) .and. &
         lle(ival_target,ival_check))
  end subroutine unit_assert_equal_string
  subroutine unit_assert_equal_within_double(description, &
       val_target, val_check, eps)
    character(len=*), intent(in) :: description
    real(kind=dk), intent(in) :: val_target, val_check, eps
    logical :: ok
    ok = .true.
    if (abs(val_target - val_check) > eps) ok = .false.
    call unit_toggle(description, ok)
    if (unit_debug .and. .not. ok) then
       write(6,fmt='(a,4x,2(1PD25.18),1PD8.1)') description,val_target,val_check,eps
    end if
  end subroutine unit_assert_equal_within_double
  subroutine unit_assert_equal_within_double_array(description, &
       val_target, val_check, eps)
    character(len=*), intent(in) :: description
    real(kind=dk), intent(in) :: val_target(:), val_check(:), eps
    integer(kind=ik) :: i
    logical :: ok
    if (size(val_target) /= size(val_check)) then
       call unit_toggle(description, .false.)
    end if
    ok = .true.
    do i=1,size(val_target)
       if (abs(val_target(i) - val_check(i)) > eps) ok= .false.
    end do
    call unit_toggle(description, ok)
    if (unit_debug .and. .not. ok) then
       do i=1,min(size(val_target),size(val_check))
          write(6,fmt='(a,4x,2(1PD25.18),1PD8.1)') description,val_target(i),val_check(i),eps
       end do
    end if
  end subroutine unit_assert_equal_within_double_array
  subroutine unit_assert_equal_within_double_2d_array(description, &
       val_target, val_check, eps)
    character(len=*), intent(in) :: description
    real(kind=dk), intent(in) :: val_target(:,:), val_check(:,:), eps
    integer(kind=ik) :: i, j
    logical :: ok
    if (size(val_target,1) /= size(val_check,1) .or. &
         size(val_target,1) /= size(val_check,1)) then
       call unit_toggle(description, .false.)
       return
    end if
    ok = .true.
    do j=1,size(val_target,2)
       do i=1,size(val_target,1)
          if (abs(val_target(i,j) - val_check(i,j)) > eps) ok= .false.
       end do
    end do
    call unit_toggle(description, ok)
    if (unit_debug .and. .not. ok) then
       do j=1,min(size(val_target, 2),size(val_check, 2))
          do i=1,min(size(val_target, 1),size(val_check, 1))
             write(6,fmt='(a,4x,2(1PD25.18),1PD8.1)') description,val_target(i,j),val_check(i,j),eps
          end do
       end do
    end if
  end subroutine unit_assert_equal_within_double_2d_array
  subroutine unit_assert_equal_within_double_complex(description, &
       val_target, val_check, eps)
    character(len=*), intent(in) :: description
    complex(kind=dk), intent(in) :: val_target, val_check
    real(kind=dk), intent(in) :: eps
    call unit_toggle(description, abs(val_target - val_check) <= eps)
    if (unit_debug) then
       write(6,fmt='(a,/,4x,2(1PD25.18),/,4x,2(1PD25.18),1PD8.1)') &
            description,val_target,val_check,eps
    end if
  end subroutine unit_assert_equal_within_double_complex
  subroutine unit_assert_equal_within_double_complex_array(&
       description, val_target, val_check, eps)
    character(len=*), intent(in) :: description
    complex(kind=dk), dimension(:), intent(in) :: val_target, val_check
    real(kind=dk), intent(in) :: eps
    integer :: i
    logical :: ok
    if (size(val_target) /= size(val_check)) then
       call unit_toggle(description, .false.)
    end if
    ok = .true.
    do i=1,size(val_target)
       if (abs(val_target(i) - val_check(i)) > eps) ok= .false.
    end do
    call unit_toggle(description, ok)
  end subroutine unit_assert_equal_within_double_complex_array
  subroutine unit_assert_equal_within_double_complex_2d_array(&
       description, val_target, val_check, eps)
    character(len=*), intent(in) :: description
    complex(kind=dk), dimension(:,:), intent(in) :: val_target, val_check
    real(kind=dk), intent(in) :: eps
    integer :: i, j
    logical :: ok
    if (size(val_target) /= size(val_check)) then
       call unit_toggle(description, .false.)
    end if
    ok = .true.
    do j=1,size(val_target,2)
       do i=1,size(val_target,1)
          if (abs(val_target(i,j) - val_check(i,j)) > eps) ok= .false.
       end do
    end do
    call unit_toggle(description, ok)
  end subroutine unit_assert_equal_within_double_complex_2d_array
  subroutine unit_assert_true(description, check, abort)
    character(len=*), intent(in) :: description
    logical, intent(in) :: check, abort
    call unit_toggle(description, check)
    if (.not. check .and. abort) then
       call unit_finalize()
       write(6, *) description,': aborting Execution'
       stop
    end if
  end subroutine unit_assert_true
  subroutine unit_toggle(description,test_ok)
    character(len=*), intent(in) :: description
    logical, intent(in) :: test_ok
    if (.not. test_ok) then
       write(6, *) 'FAIL: ', description
    elseif (unit_debug) then
       write(6, *) 'PASS: ', description
    end if
    if (.not. allocated(unit_ok)) then
       write(6, *) 'Please in insert call to unit_init(). Aborting. '
       stop
    end if
    if (index_ok > size(unit_ok)) then
       write(6, *) 'Please initialize unit tests with num_tests > ', &
            size(unit_ok)
       stop
    end if
    unit_ok(index_ok) = test_ok
    index_ok = index_ok + 1
 end subroutine unit_toggle
  subroutine unit_finalize()
    if (all(unit_ok)) then
       write(6, fmt='(''OK: All '',i0,'' tests passed'')') index_ok-1
    else
       write(6, fmt='(''FAIL: '',i0,'' of '',i0,'' tests failed'')') &
            count(.not. unit_ok),index_ok-1
    end if
    deallocate(unit_ok)
    index_ok = 0
  end subroutine unit_finalize
end module mod_unit
