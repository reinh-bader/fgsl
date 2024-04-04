!-*-f90-*-
module fgsl_base
  !> definition of kind parameters and inquiry functions
  use, intrinsic :: iso_c_binding
  implicit none

  private :: gsl_aux_sizeof_double, gsl_aux_sizeof_float, gsl_aux_sizeof_int, &
       gsl_aux_sizeof_long, gsl_aux_sizeof_size_t, gsl_aux_sizeof_char

  !> Generic interfaces
  interface fgsl_sizeof
     module procedure fgsl_sizeof_double
     module procedure fgsl_sizeof_float
     module procedure fgsl_sizeof_int
     module procedure fgsl_sizeof_size_t
     module procedure fgsl_sizeof_char
  end interface fgsl_sizeof
  
  !  
  !> Kind and length parameters are default integer
  !
  integer, parameter, public :: fgsl_double = c_double
  integer, parameter, public :: fgsl_double_complex = c_double_complex
  !  integer, parameter, public :: fgsl_extended = selected_real_kind(18)
  integer, parameter, public :: fgsl_extended = selected_real_kind(13)
  ! possibly FIXME - c_long_double unsupported, selected_real_kind(30) unsupported 
  integer, parameter, public :: fgsl_float = c_float
  integer, parameter, public :: fgsl_int = c_int
  integer, parameter, public :: fgsl_long = c_long
  integer, parameter, public :: fgsl_size_t = c_size_t
  integer, parameter, public :: fgsl_char = c_char
  integer, parameter, public :: fgsl_strmax = 128
  integer, parameter, public :: fgsl_pathmax = 2048
  !  
  !>  private interfaces: miscellaneous additions
  interface
     function gsl_aux_sizeof_double() bind(c)
       import :: c_size_t
       integer(c_size_t) :: gsl_aux_sizeof_double
     end function gsl_aux_sizeof_double
     function gsl_aux_sizeof_float() bind(c)
       import :: c_size_t
       integer(c_size_t) :: gsl_aux_sizeof_float
     end function gsl_aux_sizeof_float
     function gsl_aux_sizeof_int() bind(c)
       import :: c_size_t
       integer(c_size_t) :: gsl_aux_sizeof_int
     end function gsl_aux_sizeof_int
     function gsl_aux_sizeof_long() bind(c)
       import :: c_size_t
       integer(c_size_t) :: gsl_aux_sizeof_long
     end function gsl_aux_sizeof_long
     function gsl_aux_sizeof_size_t() bind(c)
       import :: c_size_t
       integer(c_size_t) :: gsl_aux_sizeof_size_t
     end function gsl_aux_sizeof_size_t
     function gsl_aux_sizeof_char() bind(c)
       import :: c_size_t
       integer(c_size_t) :: gsl_aux_sizeof_char
     end function gsl_aux_sizeof_char
  end interface
contains
  !
  !  API: miscellaneous additions
  !
  !> C string to Fortran string conversion
  function fgsl_name(c_name)
    type(c_ptr), intent(in) :: c_name
    character(kind=fgsl_char, len=fgsl_strmax) :: fgsl_name
    !
    character(kind=fgsl_char,len=fgsl_strmax) :: result
    character(fgsl_char), pointer :: fc(:)
    integer :: len
    !
    call c_f_pointer(c_name,fc,(/fgsl_strmax/))
    len = 1
    do 
       if (fc(len) == c_null_char .or. len > fgsl_strmax) exit
       result(len:len) = fc(len)
       len = len + 1
    end do
    if (fc(len) == c_null_char) len = len - 1
    fgsl_name = result(1:len)
  end function fgsl_name
  !> size of intrinsic double precision type
  function fgsl_sizeof_double(x)
    real(fgsl_double), intent(in) :: x
    integer(fgsl_size_t) :: fgsl_sizeof_double
    fgsl_sizeof_double = gsl_aux_sizeof_double()
  end function fgsl_sizeof_double
  !> size of intrinsic single precision type
  function fgsl_sizeof_float(x)
    real(fgsl_float), intent(in) :: x
    integer(fgsl_size_t) :: fgsl_sizeof_float
    fgsl_sizeof_float = gsl_aux_sizeof_float()
  end function fgsl_sizeof_float
  !> size of intrinsic integer type
  function fgsl_sizeof_int(x)
    integer(fgsl_int), intent(in) :: x
    integer(fgsl_size_t) :: fgsl_sizeof_int
    fgsl_sizeof_int = gsl_aux_sizeof_int()
  end function fgsl_sizeof_int
  !> size of intrinsic long integer type
  function fgsl_sizeof_long(x)
    integer(fgsl_long), intent(in) :: x
    integer(fgsl_size_t) :: fgsl_sizeof_long
    fgsl_sizeof_long = gsl_aux_sizeof_long()
  end function fgsl_sizeof_long
  !> size of intrinsic size_t integer type
  function fgsl_sizeof_size_t(x)
    integer(fgsl_size_t), intent(in) :: x
    integer(fgsl_size_t) :: fgsl_sizeof_size_t
    fgsl_sizeof_size_t = gsl_aux_sizeof_size_t()
  end function fgsl_sizeof_size_t
  !> size of intrinsic character type
  function fgsl_sizeof_char(x)
    character(fgsl_char), intent(in) :: x
    integer(fgsl_size_t) :: fgsl_sizeof_char
    fgsl_sizeof_char = gsl_aux_sizeof_char()
  end function fgsl_sizeof_char

end module fgsl_base
