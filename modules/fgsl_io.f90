!-*-f90-*-
module fgsl_io
  !> Auxiliary I/O routines
  use fgsl_base
  use fgsl_errno
  implicit none

  private :: fopen, fclose, fgsl_cstdin, fgsl_cstdout, fgsl_cstderr, fflush

  !
  !> Types
  !
  type :: fgsl_file
     ! type component is now public, since other modules need access.
     type(c_ptr) :: gsl_file = c_null_ptr
  end type fgsl_file
  !
  !>  Interfaces
  !
  interface
     function fopen(path, mode) bind(c)
       import :: c_char, c_ptr
       type(c_ptr), value :: path, mode
       type(c_ptr) :: fopen
     end function fopen
     function fclose(fd) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: fd
       integer(c_int) :: fclose
     end function fclose
     function fgsl_cstdin() bind(c)
       import :: c_ptr
       type(c_ptr) :: fgsl_cstdin
     end function fgsl_cstdin
     function fgsl_cstdout() bind(c)
       import :: c_ptr
       type(c_ptr) :: fgsl_cstdout
     end function fgsl_cstdout
     function fgsl_cstderr() bind(c)
       import :: c_ptr
       type(c_ptr) :: fgsl_cstderr
     end function fgsl_cstderr
     function fflush(stream) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: stream
       integer(c_int) :: fflush
     end function fflush
  end interface
contains
  !
  !>  API
  !
  !
  !> fgsl_open maps the POSIX call fopen() to Fortran
  !> \param path - string specifying the path name of the file to be opened
  !> \param mode - string containing the opening mode
  !> \return object of type fgsl_file which can be used in other I/O calls.
  function fgsl_open(path, mode)
    character(kind=fgsl_char, len=*), intent(in) :: path, mode
    type(fgsl_file) :: fgsl_open
    character(kind=fgsl_char,len=fgsl_pathmax), target :: lpath
    character(kind=fgsl_char,len=fgsl_strmax), target :: lmode
    if (len(trim(path)) < fgsl_pathmax .and. len(trim(mode)) < fgsl_strmax) then
       lpath = trim(path) // c_null_char
       lmode = trim(mode) // c_null_char
       fgsl_open%gsl_file = fopen(c_loc(lpath), c_loc(lmode))
    else
       fgsl_open%gsl_file = c_null_ptr
    end if
  end function fgsl_open
  !> fgsl_open maps the POSIX call fclose() to Fortran
  !> \param fd - on entry: open file object
  !> \return Status.
  function fgsl_close(fd)
    type(fgsl_file), intent(inout) :: fd
    integer(fgsl_int) :: fgsl_close
    !
    integer(c_int) :: status
    status = fclose(fd%gsl_file)
    fgsl_close = fgsl_success
    if (status /= 0) fgsl_close = fgsl_efault
  end function fgsl_close
  !> fgsl_stdin produces a fgsl_file object corresponding to C standard input
  function fgsl_stdin()
    type(fgsl_file) :: fgsl_stdin
    fgsl_stdin%gsl_file = fgsl_cstdin()
  end function fgsl_stdin
  !> fgsl_stdout produces a fgsl_file object corresponding to C standard output
  function fgsl_stdout()
    type(fgsl_file) :: fgsl_stdout
    fgsl_stdout%gsl_file = fgsl_cstdout()
  end function fgsl_stdout
  !> fgsl_stderr produces a fgsl_file object corresponding to C standard error
  function fgsl_stderr()
    type(fgsl_file) :: fgsl_stderr
    fgsl_stderr%gsl_file = fgsl_cstderr()
  end function fgsl_stderr
  !> fgsl_flush flushes a fgsl_file object
  function fgsl_flush(file)
    type(fgsl_file), intent(in) :: file
    integer(fgsl_int) :: fgsl_flush
    fgsl_flush = fflush(file%gsl_file)
  end function fgsl_flush
  function fgsl_file_status(file)
    type(fgsl_file), intent(in) :: file
    logical :: fgsl_file_status
    fgsl_file_status = .true.
    if (.not. c_associated(file%gsl_file)) fgsl_file_status = .false.
  end function fgsl_file_status

end module fgsl_io
