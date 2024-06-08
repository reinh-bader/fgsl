module fgsl_const_num
  !> \page constnum Dimensionless numerical constants
  !> See \ref fgsl_const_num for details.
  use fgsl_base
  
  implicit none
  
  real(fgsl_double), parameter, public :: fgsl_const_num_fine_structure = 7.297352533E-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_avogadro = 6.02214199E23_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_yotta = 1e24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_zetta = 1e21_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_exa = 1e18_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_peta = 1e15_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_tera = 1e12_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_giga = 1e9_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_mega = 1e6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_kilo = 1e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_milli = 1e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_micro = 1e-6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_nano = 1e-9_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_pico = 1e-12_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_femto = 1e-15_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_atto = 1e-18_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_zepto = 1e-21_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_num_yocto = 1e-24_fgsl_double
  
end module fgsl_const_num
