module mod_ntuple
  use fgsl
  use mod_unit
  use, intrinsic :: iso_c_binding
  integer(fgsl_size_t), parameter :: nsize = 50
  real(fgsl_double), parameter :: eps10 = 1.0d-10
  type, bind(c) :: tuple_data
     integer(c_int) :: len
     real(c_double) :: stuff(nsize) 
  end type tuple_data
end module mod_ntuple
program ntuple
  use mod_ntuple
  implicit none
  integer :: i
  integer(fgsl_size_t) :: isz, iisz
  integer(fgsl_int) :: status
  type(tuple_data), target :: obj_data
  type(tuple_data), pointer :: ptr_data
  type(c_ptr) :: obj_ptr
  type(fgsl_ntuple) :: tuple
!
! Test tuple routines
!
  call unit_init(200)
!
  obj_ptr = c_loc(obj_data)
  isz = fgsl_sizeof(1_fgsl_int) + nsize * fgsl_sizeof(1.0_fgsl_double) + 4
! FIXME: I don't know why the +4 above is needed; if it's not there the last
! Element in the structure is read incorrectly.
! This needs to be re-checked!
  tuple = fgsl_ntuple_create ('ntuple.dat', obj_ptr, isz)
  call unit_assert_true('fgsl_ntuple_create',fgsl_well_defined(tuple),.true.)
  obj_data%len = nsize
  obj_data%stuff = (/(dble(i),i=1,nsize)/)
  status = fgsl_ntuple_write(tuple)
  call unit_assert_equal('fgsl_ntuple_write',fgsl_success,status)
  obj_data%len = nsize
  obj_data%stuff = (/(dble(i)*10,i=1,nsize)/)
  status = fgsl_ntuple_bookdata(tuple)
  call unit_assert_equal('fgsl_ntuple_bookdata',fgsl_success,status)
  status = fgsl_ntuple_close(tuple)
  call unit_assert_equal('fgsl_ntuple_close',fgsl_success,status)
!
  tuple = fgsl_ntuple_open('ntuple.dat',obj_ptr,isz)
  call unit_assert_true('fgsl_ntuple_open',fgsl_well_defined(tuple),.true.)
  status = fgsl_ntuple_read(tuple)
  obj_ptr = fgsl_ntuple_data(tuple)
  call c_f_pointer(obj_ptr, ptr_data)
  call unit_assert_equal('fgsl_ntuple_read',int(nsize),ptr_data%len)
  call unit_assert_equal_within('fgsl_ntuple_read',(/(dble(i),i=1,nsize)/), &
       ptr_data%stuff,eps10)
  obj_data%stuff = 0.0d0
  status = fgsl_ntuple_read(tuple)
  obj_ptr = fgsl_ntuple_data(tuple)
  call c_f_pointer(obj_ptr, ptr_data)
  iisz = fgsl_ntuple_size(tuple)
  call unit_assert_equal('fgsl_ntuple_read',int(nsize),ptr_data%len)
  call unit_assert_equal('fgsl_ntuple_read',int(isz),int(iisz))
  call unit_assert_equal_within('fgsl_ntuple_read',(/(dble(i)*10,i=1,nsize)/), &
       ptr_data%stuff,eps10)
!
! FIXME: project call and histogramming is not in the test suite presently
!
!
! Done
!
  call unit_finalize() 
end program ntuple
