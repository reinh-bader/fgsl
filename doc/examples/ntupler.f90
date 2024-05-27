module mod_ntupler
  use fgsl
  use, intrinsic :: iso_c_binding
  implicit none
  type, bind(c) :: data
    real(c_double) :: x, y, z
  end type
contains
  function sel_func(ntuple_data, params) bind(c)
    type(c_ptr), value :: ntuple_data, params
    integer(c_int) :: sel_func
    type(data), pointer :: data_ptr
    real(fgsl_double) :: x, y, z, e2
    real(c_double), pointer :: scale
    call c_f_pointer(ntuple_data, data_ptr)
    call c_f_pointer(params, scale)
    x = data_ptr%x
    y = data_ptr%y
    z = data_ptr%z
    e2 = x*x + y*y + z*z
    sel_func = 0
    if (e2 > scale) then
       sel_func = 1
    end if
  end function sel_func
  function val_func(ntuple_data, params) bind(c)
    type(c_ptr), value :: ntuple_data, params
    real(c_double) :: val_func
    type(data), pointer :: data_ptr
    real(fgsl_double) :: x, y, z
    call c_f_pointer(ntuple_data, data_ptr)
    x = data_ptr%x
    y = data_ptr%y
    z = data_ptr%z
    val_func = x*x + y*y + z*z
  end function val_func
end module mod_ntupler
program ntupler
  use mod_ntupler
  implicit none
  type(data), target :: ntuple_row
  type(fgsl_ntuple) :: ntuple
  type(fgsl_ntuple_select_fn) :: s
  type(fgsl_ntuple_value_fn) :: v
  type(fgsl_histogram) :: h
  type(fgsl_file) :: stdout
  type(fgsl_error_handler_t) :: std
  real(fgsl_double), target :: lower
  integer(fgsl_int) :: status
  integer(fgsl_size_t) :: sz
  type(c_ptr) :: ptr_lower, ptr_ntuple
!
  std = fgsl_set_error_handler_off()
! Note: with standard error handler on fgsl_ntuple_open will abort
!       if ntuple file not found.
  ptr_ntuple = c_loc(ntuple_row)
! FIXME sizeof for scalars
  sz = 3 * fgsl_sizeof(1.0_c_double)
  ntuple = fgsl_ntuple_open('test.dat', ptr_ntuple, sz)
  if (fgsl_well_defined(ntuple)) then
     lower = 1.5D0
     h = fgsl_histogram_alloc (100_fgsl_size_t)
     status = fgsl_histogram_set_ranges_uniform(h, &
          0.0_fgsl_double, 10.0_fgsl_double)
     ptr_lower = c_loc(lower)
     s = fgsl_ntuple_select_fn_init(sel_func, ptr_lower)
     v = fgsl_ntuple_value_fn_init(val_func, c_null_ptr)
     status = fgsl_ntuple_project (h, ntuple, v, s)
     stdout = fgsl_stdout()
     status = fgsl_histogram_fprintf(stdout, h, '%f', '%f')
     call fgsl_histogram_free(h)
     status = fgsl_ntuple_close (ntuple)
     call fgsl_ntuple_select_fn_free(s)
     call fgsl_ntuple_value_fn_free(v)
  else
     write(*, '('' Could not open ntuple. Please run ntuplew.exe first. '')')
  end if
end program ntupler
