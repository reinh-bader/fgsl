  subroutine errhand(reason, file, line, errno) bind(c)
    type(c_ptr), value :: reason, file
    integer(c_int), value :: line, errno
  end subroutine errhand
