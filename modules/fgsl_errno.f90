!-*-f90-*-
module fgsl_errno
!> Error treatment
!
!>
!> The error handling subroutines are available from Fortran, with
!> exception of the macros <CODE>GSL_ERROR</CODE> and  <CODE>GSL_ERROR_VAL</CODE>.
!> A user-defined error handler can be defined either in C or using a
!> Fortran function with the <CODE>bind(c)</CODE> attribute. Here is the
!> description of the required interface:
!> <PRE>
!> subroutine errhand(reason, file, line, errno) bind(c)
!>    type(c_ptr), value :: reason, file
!>    integer(c_int), value :: line, errno
!> end subroutine errhand
!> </PRE>
!> An object of type <CODE>fgsl_error_handler_t</CODE> is returned by the
!> constructor <CODE>fgsl_error_handler_init(errhand)</CODE>, which takes
!> a subroutine with the interface described above as its argument.
!> The subroutine <CODE>fgsl_error(reason, file, line, errno)</CODE> works
!> in an analogous manner as the C version. If the Fortran preprocessor is
!> supported, it should be possible to use the macros <CODE>__FILE__</CODE>
!> and <CODE>__LINE__</CODE> in the above call. Once not needed any more, the
!> error handler object can be deallocated by calling the subroutine
!> <CODE>fgsl_error_handler_free</CODE> with itself as its only argument.
!> Note that the function <CODE>fgsl_strerror</CODE> returns a string
!> of length <CODE>fgsl_strmax</CODE>.
  use fgsl_base
  implicit none

  private :: gsl_set_error_handler, gsl_set_error_handler_off, gsl_strerror, &
       gsl_error, fgsl_error_handler_status
  !
  !> generic interfaces
  interface fgsl_well_defined
     module procedure fgsl_error_handler_status
  end interface fgsl_well_defined
  !
  !> types
  type :: fgsl_error_handler_t
     private
     type(c_funptr) :: gsl_error_handler_t = c_null_funptr
  end type fgsl_error_handler_t

  !
  !> error codes
  integer(fgsl_int), parameter, public :: fgsl_success = 0
  integer(fgsl_int), parameter, public :: fgsl_failure = -1
  integer(fgsl_int), parameter, public :: fgsl_continue = -2 ! iteration has not converged
  integer(fgsl_int), parameter, public :: fgsl_edom = 1      ! input domain error, e.g. sqrt(-1)
  integer(fgsl_int), parameter, public :: fgsl_erange = 2    ! output range error, e.g. exp(1e100)
  integer(fgsl_int), parameter, public :: fgsl_efault = 3    ! invalid pointer
  integer(fgsl_int), parameter, public :: fgsl_einval = 4    ! invalid argument supplied by user
  integer(fgsl_int), parameter, public :: fgsl_efactor = 6   ! generic failure
  integer(fgsl_int), parameter, public :: fgsl_esanity = 7   ! sanity check failed - shouldn't happen
  integer(fgsl_int), parameter, public :: fgsl_enomem = 8    ! malloc failed
  integer(fgsl_int), parameter, public :: fgsl_ebadfunc = 9  ! problem with user-supplied function
  integer(fgsl_int), parameter, public :: fgsl_erunaway = 10 ! iterative process is out of control
  integer(fgsl_int), parameter, public :: fgsl_emaxiter = 11 ! exceeded max number of iterations
  integer(fgsl_int), parameter, public :: fgsl_ezerodiv = 12 ! tried to divide by zero
  integer(fgsl_int), parameter, public :: fgsl_ebadtol = 13  ! user specified an invalid tolerance
  integer(fgsl_int), parameter, public :: fgsl_etol = 14     ! failed to reach the specified tolerance
  integer(fgsl_int), parameter, public :: fgsl_eundrflw = 15 ! underflow
  integer(fgsl_int), parameter, public :: fgsl_eovrflw = 16  ! overflow
  integer(fgsl_int), parameter, public :: fgsl_eloss = 17    ! loss of accuracy
  integer(fgsl_int), parameter, public :: fgsl_eround = 18   ! failed because of roundoff error
  integer(fgsl_int), parameter, public :: fgsl_ebadlen = 19  ! matrix, vector lengths are not conformant
  integer(fgsl_int), parameter, public :: fgsl_enotsqr = 20  ! matrix not square
  integer(fgsl_int), parameter, public :: fgsl_esing = 21    ! apparent singularity detected
  integer(fgsl_int), parameter, public :: fgsl_ediverge = 22 ! integral or series is divergent
  integer(fgsl_int), parameter, public :: fgsl_eunsup = 23   ! no hw support for requested feature
  integer(fgsl_int), parameter, public :: fgsl_eunimpl = 24  ! requested feature not (yet) implemented
  integer(fgsl_int), parameter, public :: fgsl_ecache = 25   ! cache limit exceeded
  integer(fgsl_int), parameter, public :: fgsl_etable = 26   ! table limit exceeded
  integer(fgsl_int), parameter, public :: fgsl_enoprog = 27  ! iteration: no progress towards solution
  integer(fgsl_int), parameter, public :: fgsl_enoprogj = 28 ! jacobian evals not improving the solution
  integer(fgsl_int), parameter, public :: fgsl_etolf = 29    ! can't reach specified tolerance in F
  integer(fgsl_int), parameter, public :: fgsl_etolx = 30    ! can't reach specified tolerance in X
  integer(fgsl_int), parameter, public :: fgsl_etolg = 31    ! can't reach specified tolerance in gradient
  integer(fgsl_int), parameter, public :: fgsl_eof = 32      ! end of file

!  FIXME: missing still
!  interface fgsl_error
!     module procedure fgsl_err_info
!     module procedure fgsl_err_noinfo
!  end interface

  !
  !> private C interfaces 
  interface
     function gsl_set_error_handler(new_handler) bind(c)
       import
       type(c_funptr), value :: new_handler
       type(c_funptr) :: gsl_set_error_handler
     end function gsl_set_error_handler
     function gsl_set_error_handler_off() bind(c)
       import
       type(c_funptr) :: gsl_set_error_handler_off
     end function gsl_set_error_handler_off
     function gsl_strerror(errno) bind(c)
       import
       integer(c_int), value :: errno
       type(c_ptr) :: gsl_strerror
     end function gsl_strerror
     subroutine gsl_error(reason, file, line, gsl_errno) bind(c)
       import
       type(c_ptr), value :: reason, file
       integer(c_int), value :: line
       integer(c_int), value :: gsl_errno
     end subroutine gsl_error
  end interface
contains
  !> API
  function fgsl_set_error_handler(new_handler)
    type(fgsl_error_handler_t), intent(in) :: new_handler
    type(fgsl_error_handler_t) :: fgsl_set_error_handler
    fgsl_set_error_handler%gsl_error_handler_t = &
         gsl_set_error_handler(new_handler%gsl_error_handler_t)
  end function fgsl_set_error_handler
  function fgsl_set_error_handler_off()
    type(fgsl_error_handler_t) :: fgsl_set_error_handler_off
    fgsl_set_error_handler_off%gsl_error_handler_t = &
         gsl_set_error_handler_off()
  end function fgsl_set_error_handler_off
  function fgsl_strerror(errno)
    integer(fgsl_int), intent(in) :: errno
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_strerror
!
    type(c_ptr) :: name
!
    name = gsl_strerror(errno)
    fgsl_strerror = fgsl_name(name)
  end function fgsl_strerror
  subroutine fgsl_error(reason, file, line, errno)
    character(kind=fgsl_char,len=*), intent(in) :: &
         reason, file
    integer(fgsl_int), intent(in) :: line, errno
    character(kind=fgsl_char,len=fgsl_strmax), target :: reason_null, file_null
    if (len(trim(reason)) < fgsl_strmax .and. &
         len(trim(file)) < fgsl_strmax) then
       reason_null = trim(reason) // c_null_char
       file_null = trim(file) // c_null_char
       call gsl_error(c_loc(reason_null), c_loc(file_null), line, errno)
    end if
  end subroutine fgsl_error
  function fgsl_error_handler_status(error_handler_t)
    type(fgsl_error_handler_t), intent(in) :: error_handler_t
    logical :: fgsl_error_handler_status
    fgsl_error_handler_status = .true.
    if (.not. c_associated(error_handler_t%gsl_error_handler_t)) &
         fgsl_error_handler_status = .false.
  end function fgsl_error_handler_status
!
! initialize own error handler
!
  function fgsl_error_handler_init(handler_sr)
    interface
       subroutine handler_sr(reason, file, line, errno) bind(c)
         import :: c_ptr, c_int
         type(c_ptr), value :: reason, file
         integer(c_int), value :: line, errno
       end subroutine handler_sr
    end interface
    type(fgsl_error_handler_t) :: fgsl_error_handler_init
!
    type(c_funptr) :: fptr
    fptr = c_funloc(handler_sr)
    fgsl_error_handler_init%gsl_error_handler_t = fptr
  end function fgsl_error_handler_init

END MODULE fgsl_errno
