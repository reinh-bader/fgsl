module fgsl_multisets
  !> Multisets
  use fgsl_array
  use fgsl_io
  implicit none

  private :: gsl_multiset_alloc, gsl_multiset_calloc, &
       gsl_multiset_init_first, gsl_multiset_init_last, &
       gsl_multiset_free, gsl_multiset_memcpy, &
       gsl_multiset_get, gsl_multiset_n, gsl_multiset_k, &
       gsl_multiset_data, gsl_multiset_valid, gsl_multiset_next, &
       gsl_multiset_prev, gsl_multiset_fwrite, gsl_multiset_fread, &
       gsl_multiset_fprintf, gsl_multiset_fscanf, &
       gsl_aux_sizeof_multiset, fgsl_multiset_status, &
       fgsl_sizeof_multiset
  !
  ! Type definition
  type, public :: fgsl_multiset
     private
     type(c_ptr) :: gsl_multiset = c_null_ptr
  end type fgsl_multiset

  !
  ! Generic interfaces
  interface fgsl_well_defined
     module procedure fgsl_multiset_status
  end interface fgsl_well_defined
  interface fgsl_sizeof
     module procedure fgsl_sizeof_multiset
  end interface fgsl_sizeof

  !
  ! C interfaces
  interface
     function gsl_multiset_alloc(n, k) bind(c)
       import
       integer(c_size_t), value :: n, k
       type(c_ptr) :: gsl_multiset_alloc
     end function gsl_multiset_alloc
     function gsl_multiset_calloc(n, k) bind(c)
       import
       integer(c_size_t), value :: n, k
       type(c_ptr) :: gsl_multiset_calloc
     end function gsl_multiset_calloc
     subroutine gsl_multiset_init_first(c) bind(c)
       import
       type(c_ptr), value :: c
     end subroutine gsl_multiset_init_first
     subroutine gsl_multiset_init_last(c) bind(c)
       import
       type(c_ptr), value :: c
     end subroutine gsl_multiset_init_last
     subroutine gsl_multiset_free(c) bind(c)
       import
       type(c_ptr), value :: c
     end subroutine gsl_multiset_free
     function gsl_multiset_memcpy(dest, src) bind(c)
       import
       type(c_ptr), value :: dest
       type(c_ptr), value :: src
       integer(c_int) :: gsl_multiset_memcpy
     end function gsl_multiset_memcpy
     function gsl_multiset_get(c, i) bind(c)
       import
       type(c_ptr), value :: c
       integer(c_size_t), value :: i
       integer(c_size_t) :: gsl_multiset_get
     end function gsl_multiset_get
     function gsl_multiset_n(c) bind(c)
       import
       type(c_ptr), value :: c
       integer(c_size_t) :: gsl_multiset_n
     end function gsl_multiset_n
     function gsl_multiset_k(c) bind(c)
       import
       type(c_ptr), value :: c
       integer(c_size_t) :: gsl_multiset_k
     end function gsl_multiset_k
     function gsl_multiset_data(c) bind(c)
       import
       type(c_ptr), value :: c
       type(c_ptr) :: gsl_multiset_data
     end function gsl_multiset_data
     function gsl_multiset_valid(c) bind(c)
       import
       type(c_ptr), value :: c
       integer(c_int) :: gsl_multiset_valid
     end function gsl_multiset_valid
     function gsl_multiset_next(c) bind(c)
       import
       type(c_ptr), value :: c
       integer(c_int) :: gsl_multiset_next
     end function gsl_multiset_next
     function gsl_multiset_prev(c) bind(c)
       import
       type(c_ptr), value :: c
       integer(c_int) :: gsl_multiset_prev
     end function gsl_multiset_prev
     function gsl_multiset_fwrite(stream, c) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: stream, c
       integer(c_int) :: gsl_multiset_fwrite
     end function gsl_multiset_fwrite
     function gsl_multiset_fread(stream, c) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: stream, c
       integer(c_int) :: gsl_multiset_fread
     end function gsl_multiset_fread
     function gsl_multiset_fprintf(stream, c, format) bind(c)
       import :: c_ptr, c_int, c_char
       type(c_ptr), value :: stream, c
       character(kind=c_char) :: format
       integer(c_int) :: gsl_multiset_fprintf
     end function gsl_multiset_fprintf
     function gsl_multiset_fscanf(stream, c) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: stream, c
       integer(c_int) :: gsl_multiset_fscanf
     end function gsl_multiset_fscanf
     function gsl_aux_sizeof_multiset() bind(c)
       import :: c_size_t
       integer(c_size_t) :: gsl_aux_sizeof_multiset
     end function gsl_aux_sizeof_multiset
  end interface

contains
  function fgsl_multiset_alloc(n, k)
    integer(fgsl_size_t), intent(in) :: n, k
    type(fgsl_multiset) :: fgsl_multiset_alloc
    fgsl_multiset_alloc%gsl_multiset = gsl_multiset_alloc(n, k)
  end function fgsl_multiset_alloc
  function fgsl_multiset_calloc(n, k)
    integer(fgsl_size_t), intent(in) :: n, k
    type(fgsl_multiset) :: fgsl_multiset_calloc
    fgsl_multiset_calloc%gsl_multiset = gsl_multiset_calloc(n, k)
  end function fgsl_multiset_calloc
  subroutine fgsl_multiset_init_first(c)
    type(fgsl_multiset), intent(inout) :: c
    call gsl_multiset_init_first(c%gsl_multiset)
  end subroutine fgsl_multiset_init_first
  subroutine fgsl_multiset_init_last(c)
    type(fgsl_multiset), intent(inout) :: c
    call gsl_multiset_init_last(c%gsl_multiset)
  end subroutine fgsl_multiset_init_last
  subroutine fgsl_multiset_free(c)
    type(fgsl_multiset), intent(inout) :: c
    call gsl_multiset_free(c%gsl_multiset)
  end subroutine fgsl_multiset_free
  function fgsl_multiset_memcpy(dest, src)
    type(fgsl_multiset), intent(inout) :: dest
    type(fgsl_multiset),  intent(in) :: src
    integer(fgsl_int) :: fgsl_multiset_memcpy
    fgsl_multiset_memcpy = gsl_multiset_memcpy(dest%gsl_multiset, &
         src%gsl_multiset)
  end function fgsl_multiset_memcpy
  function fgsl_multiset_get(c, i)
    type(fgsl_multiset), intent(inout) :: c
    integer(fgsl_size_t), intent(in) :: i
    integer(fgsl_size_t) :: fgsl_multiset_get
    fgsl_multiset_get = gsl_multiset_get(c%gsl_multiset, i)
  end function fgsl_multiset_get
  function fgsl_multiset_n(c)
    type(fgsl_multiset), intent(in) :: c
    integer(fgsl_size_t) :: fgsl_multiset_n
    fgsl_multiset_n = gsl_multiset_n(c%gsl_multiset)
  end function fgsl_multiset_n
  function fgsl_multiset_k(c)
    type(fgsl_multiset), intent(in) :: c
    integer(fgsl_size_t) :: fgsl_multiset_k
    fgsl_multiset_k = gsl_multiset_k(c%gsl_multiset)
  end function fgsl_multiset_k
  function fgsl_multiset_data(c)
    type(fgsl_multiset), intent(in) :: c
    integer(fgsl_size_t), pointer :: fgsl_multiset_data(:)
!
    integer(fgsl_size_t) :: size
    type(c_ptr) :: cdata
    size = gsl_multiset_k(c%gsl_multiset)
    cdata = gsl_multiset_data(c%gsl_multiset)
    if (c_associated(cdata)) then
       call c_f_pointer(cdata,fgsl_multiset_data,(/size/))
    else
       nullify(fgsl_multiset_data)
    end if
  end function fgsl_multiset_data
  function fgsl_multiset_valid(c)
    type(fgsl_multiset), intent(in) :: c
    integer(fgsl_int) :: fgsl_multiset_valid
    fgsl_multiset_valid = gsl_multiset_valid(c%gsl_multiset)
  end function fgsl_multiset_valid
  function fgsl_multiset_next(c)
    type(fgsl_multiset), intent(in) :: c
    integer(fgsl_int) :: fgsl_multiset_next
    fgsl_multiset_next = gsl_multiset_next(c%gsl_multiset)
  end function fgsl_multiset_next
  function fgsl_multiset_prev(c)
    type(fgsl_multiset), intent(in) :: c
    integer(fgsl_int) :: fgsl_multiset_prev
    fgsl_multiset_prev = gsl_multiset_prev(c%gsl_multiset)
  end function fgsl_multiset_prev
  function fgsl_multiset_fwrite(stream, c)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_multiset), intent(in) :: c
    integer(fgsl_int) :: fgsl_multiset_fwrite
    fgsl_multiset_fwrite = gsl_multiset_fwrite(stream%gsl_file, &
         c%gsl_multiset)
  end function fgsl_multiset_fwrite
  function fgsl_multiset_fread(stream, c)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_multiset), intent(inout) :: c
    integer(fgsl_int) :: fgsl_multiset_fread
    fgsl_multiset_fread = gsl_multiset_fread(stream%gsl_file, &
         c%gsl_multiset)
  end function fgsl_multiset_fread
  function fgsl_multiset_fprintf(stream, c, format)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_multiset), intent(in) :: c
    character(kind=fgsl_char, len=*), intent(in) :: format
    integer(fgsl_int) :: fgsl_multiset_fprintf
!
    fgsl_multiset_fprintf = gsl_multiset_fprintf(stream%gsl_file, &
         c%gsl_multiset, format // c_null_char)
  end function fgsl_multiset_fprintf
  function fgsl_multiset_fscanf(stream, c)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_multiset), intent(inout) :: c
    integer(fgsl_int) :: fgsl_multiset_fscanf
    fgsl_multiset_fscanf = gsl_multiset_fscanf(stream%gsl_file, &
         c%gsl_multiset)
  end function fgsl_multiset_fscanf
  function fgsl_multiset_status(multiset)
    type(fgsl_multiset), intent(in) :: multiset
    logical :: fgsl_multiset_status
    fgsl_multiset_status = .true.
    if (.not. c_associated(multiset%gsl_multiset)) &
         fgsl_multiset_status = .false.
  end function fgsl_multiset_status
  function fgsl_sizeof_multiset(c)
    type(fgsl_multiset), intent(in) :: c
    integer(fgsl_size_t) :: fgsl_sizeof_multiset
    fgsl_sizeof_multiset = gsl_aux_sizeof_multiset()
  end function fgsl_sizeof_multiset
end module fgsl_multisets
