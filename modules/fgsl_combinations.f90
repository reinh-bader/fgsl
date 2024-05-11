module fgsl_combinations
  !> Combinations
  use fgsl_array
  use fgsl_io
  implicit none

  private :: gsl_combination_alloc, gsl_combination_calloc, &
       gsl_combination_init_first, gsl_combination_init_last, &
       gsl_combination_free, gsl_combination_memcpy, &
       gsl_combination_get, gsl_combination_n, gsl_combination_k, &
       gsl_combination_data, gsl_combination_valid, &
       gsl_combination_next, gsl_combination_prev, &
       gsl_combination_fwrite, gsl_combination_fread, &
       gsl_combination_fprintf, gsl_combination_fscanf, &
       gsl_aux_sizeof_combination, &
       fgsl_combination_status, fgsl_sizeof_combination

  !
  ! Type definition
  type, public :: fgsl_combination
     private
     type(c_ptr) :: gsl_combination = c_null_ptr
  end type fgsl_combination

  !
  ! Generic interfaces
  interface fgsl_well_defined
     module procedure fgsl_combination_status
  end interface fgsl_well_defined
  interface fgsl_sizeof
     module procedure fgsl_sizeof_combination
  end interface fgsl_sizeof

  !
  ! C interfaces
  interface
     function gsl_combination_alloc(n, k) bind(c)
       import
       integer(c_size_t), value :: n, k
       type(c_ptr) :: gsl_combination_alloc
     end function gsl_combination_alloc
     function gsl_combination_calloc(n, k) bind(c)
       import
       integer(c_size_t), value :: n, k
       type(c_ptr) :: gsl_combination_calloc
     end function gsl_combination_calloc
     subroutine gsl_combination_init_first(c) bind(c)
       import
       type(c_ptr), value :: c
     end subroutine gsl_combination_init_first
     subroutine gsl_combination_init_last(c) bind(c)
       import
       type(c_ptr), value :: c
     end subroutine gsl_combination_init_last
     subroutine gsl_combination_free(c) bind(c)
       import
       type(c_ptr), value :: c
     end subroutine gsl_combination_free
     function gsl_combination_memcpy(dest, src) bind(c)
       import
       type(c_ptr), value :: dest
       type(c_ptr), value :: src
       integer(c_int) :: gsl_combination_memcpy
     end function gsl_combination_memcpy
     function gsl_combination_get(c, i) bind(c)
       import
       type(c_ptr), value :: c
       integer(c_size_t), value :: i
       integer(c_size_t) :: gsl_combination_get
     end function gsl_combination_get
     function gsl_combination_n(c) bind(c)
       import
       type(c_ptr), value :: c
       integer(c_size_t) :: gsl_combination_n
     end function gsl_combination_n
     function gsl_combination_k(c) bind(c)
       import
       type(c_ptr), value :: c
       integer(c_size_t) :: gsl_combination_k
     end function gsl_combination_k
     function gsl_combination_data(c) bind(c)
       import
       type(c_ptr), value :: c
       type(c_ptr) :: gsl_combination_data
     end function gsl_combination_data
     function gsl_combination_valid(c) bind(c)
       import
       type(c_ptr), value :: c
       integer(c_int) :: gsl_combination_valid
     end function gsl_combination_valid
     function gsl_combination_next(c) bind(c)
       import
       type(c_ptr), value :: c
       integer(c_int) :: gsl_combination_next
     end function gsl_combination_next
     function gsl_combination_prev(c) bind(c)
       import
       type(c_ptr), value :: c
       integer(c_int) :: gsl_combination_prev
     end function gsl_combination_prev
     function gsl_combination_fwrite(stream, c) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: stream, c
       integer(c_int) :: gsl_combination_fwrite
     end function gsl_combination_fwrite
     function gsl_combination_fread(stream, c) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: stream, c
       integer(c_int) :: gsl_combination_fread
     end function gsl_combination_fread
     function gsl_combination_fprintf(stream, c, format) bind(c)
       import :: c_ptr, c_int, c_char
       type(c_ptr), value :: stream, c
       character(kind=c_char) :: format
       integer(c_int) :: gsl_combination_fprintf
     end function gsl_combination_fprintf
     function gsl_combination_fscanf(stream, c) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: stream, c
       integer(c_int) :: gsl_combination_fscanf
     end function gsl_combination_fscanf
     function gsl_aux_sizeof_combination() bind(c)
       import :: c_size_t
       integer(c_size_t) :: gsl_aux_sizeof_combination
     end function gsl_aux_sizeof_combination
  end interface

contains
  function fgsl_combination_alloc(n, k)
    integer(fgsl_size_t), intent(in) :: n, k
    type(fgsl_combination) :: fgsl_combination_alloc
    fgsl_combination_alloc%gsl_combination = gsl_combination_alloc(n, k)
  end function fgsl_combination_alloc
  function fgsl_combination_calloc(n, k)
    integer(fgsl_size_t), intent(in) :: n, k
    type(fgsl_combination) :: fgsl_combination_calloc
    fgsl_combination_calloc%gsl_combination = gsl_combination_calloc(n, k)
  end function fgsl_combination_calloc
  subroutine fgsl_combination_init_first(c)
    type(fgsl_combination), intent(inout) :: c
    call gsl_combination_init_first(c%gsl_combination)
  end subroutine fgsl_combination_init_first
  subroutine fgsl_combination_init_last(c)
    type(fgsl_combination), intent(inout) :: c
    call gsl_combination_init_last(c%gsl_combination)
  end subroutine fgsl_combination_init_last
  subroutine fgsl_combination_free(c)
    type(fgsl_combination), intent(inout) :: c
    call gsl_combination_free(c%gsl_combination)
  end subroutine fgsl_combination_free
  function fgsl_combination_memcpy(dest, src)
    type(fgsl_combination), intent(inout) :: dest
    type(fgsl_combination),  intent(in) :: src
    integer(fgsl_int) :: fgsl_combination_memcpy
    fgsl_combination_memcpy = gsl_combination_memcpy(dest%gsl_combination, &
         src%gsl_combination)
  end function fgsl_combination_memcpy
  function fgsl_combination_get(c, i)
    type(fgsl_combination), intent(inout) :: c
    integer(fgsl_size_t), intent(in) :: i
    integer(fgsl_size_t) :: fgsl_combination_get
    fgsl_combination_get = gsl_combination_get(c%gsl_combination, i)
  end function fgsl_combination_get
  function fgsl_combination_n(c)
    type(fgsl_combination), intent(in) :: c
    integer(fgsl_size_t) :: fgsl_combination_n
    fgsl_combination_n = gsl_combination_n(c%gsl_combination)
  end function fgsl_combination_n
  function fgsl_combination_k(c)
    type(fgsl_combination), intent(in) :: c
    integer(fgsl_size_t) :: fgsl_combination_k
    fgsl_combination_k = gsl_combination_k(c%gsl_combination)
  end function fgsl_combination_k
  function fgsl_combination_data(c)
    type(fgsl_combination), intent(in) :: c
    integer(fgsl_size_t), pointer :: fgsl_combination_data(:)
!
    integer(fgsl_size_t) :: size
    type(c_ptr) :: cdata
    size = gsl_combination_k(c%gsl_combination)
    cdata = gsl_combination_data(c%gsl_combination)
    if (c_associated(cdata)) then
       call c_f_pointer(cdata,fgsl_combination_data,(/size/))
    else
       nullify(fgsl_combination_data)
    end if
  end function fgsl_combination_data
  function fgsl_combination_valid(c)
    type(fgsl_combination), intent(in) :: c
    integer(fgsl_int) :: fgsl_combination_valid
    fgsl_combination_valid = gsl_combination_valid(c%gsl_combination)
  end function fgsl_combination_valid
  function fgsl_combination_next(c)
    type(fgsl_combination), intent(in) :: c
    integer(fgsl_int) :: fgsl_combination_next
    fgsl_combination_next = gsl_combination_next(c%gsl_combination)
  end function fgsl_combination_next
  function fgsl_combination_prev(c)
    type(fgsl_combination), intent(in) :: c
    integer(fgsl_int) :: fgsl_combination_prev
    fgsl_combination_prev = gsl_combination_prev(c%gsl_combination)
  end function fgsl_combination_prev
  function fgsl_combination_fwrite(stream, c)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_combination), intent(in) :: c
    integer(fgsl_int) :: fgsl_combination_fwrite
    fgsl_combination_fwrite = gsl_combination_fwrite(stream%gsl_file, &
         c%gsl_combination)
  end function fgsl_combination_fwrite
  function fgsl_combination_fread(stream, c)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_combination), intent(inout) :: c
    integer(fgsl_int) :: fgsl_combination_fread
    fgsl_combination_fread = gsl_combination_fread(stream%gsl_file, &
         c%gsl_combination)
  end function fgsl_combination_fread
  function fgsl_combination_fprintf(stream, c, format)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_combination), intent(in) :: c
    character(kind=fgsl_char, len=*), intent(in) :: format
    integer(fgsl_int) :: fgsl_combination_fprintf
!
    fgsl_combination_fprintf = gsl_combination_fprintf(stream%gsl_file, &
         c%gsl_combination, format // c_null_char)
  end function fgsl_combination_fprintf
  function fgsl_combination_fscanf(stream, c)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_combination), intent(inout) :: c
    integer(fgsl_int) :: fgsl_combination_fscanf
    fgsl_combination_fscanf = gsl_combination_fscanf(stream%gsl_file, &
         c%gsl_combination)
  end function fgsl_combination_fscanf
  function fgsl_combination_status(combination)
    type(fgsl_combination), intent(in) :: combination
    logical :: fgsl_combination_status
    fgsl_combination_status = .true.
    if (.not. c_associated(combination%gsl_combination)) &
         fgsl_combination_status = .false.
  end function fgsl_combination_status  
  function fgsl_sizeof_combination(c)
    type(fgsl_combination), intent(in) :: c
    integer(fgsl_size_t) :: fgsl_sizeof_combination
    fgsl_sizeof_combination = gsl_aux_sizeof_combination()
  end function fgsl_sizeof_combination
end module fgsl_combinations
