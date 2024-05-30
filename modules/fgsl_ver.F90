#include "config.h"
module fgsl_ver
  use fgsl_base
  implicit none
  !
  !> Version strings
  character(kind=fgsl_char, len=*), public, parameter :: &
       fgsl_version=PACKAGE_VERSION
  character(kind=fgsl_char, len=*), public, parameter :: &
       fgsl_gslbase=GSL_VERSION
end module fgsl_ver
