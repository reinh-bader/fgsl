!-*-f90-*-
module fgsl
#include "config.h"
!-------------------------------------------------------------------------------
!>  \mainpage
!>  \brief Interface module for use of GSL from Fortran
!>  \author R. Bader, T. Schoonjans
!>  \details
!>  Please see the <a href="pages.html">Related Pages</a> section
!>  for the information about the conventions used in the interface.
!>  Examples on how to use the interface are available in the
!>  <p><b>doc/examples</b><p> subdirectory of the source package.
!>  \page "Introduction"
!>  <OL>
!>  <LI> Introductory notes:
!>      <UL>
!>      <LI> In Fortran code, GSL_* must be replaced by FGSL_* for each API
!>           call, abstract data type, module variables and parameters (with
!>           exception of the M_* mathematical constants)
!>      <LI> Some names were changed due to UC/LC aliasing. See the documentation
!>           chapter on special functions for details.
!>      <LI> Intrinsic type matching (see modules/fgsl_base.f90)
!>        -# real(fgsl_double) is used for double precision values
!>        -# real(fgsl_float) is used for single precision values
!>        -# integer(fgsl_int) for integer
!>        -# integer(fgsl_long) for long integer
!>        -# integer(fgsl_size_t) for size_t integer
!>        -# complex(fgsl_double_complex) for gsl_complex
!>        -# character(fgsl_char) for characters
!>        -# no value attributes and mostly no pointers in Fortran calls
!>        -# unsigned int must be converted to integer(fgsl_long).
!>        -# char * results are converted to fixed length strings. Use TRIM.
!>       </UL>
!>  <LI> Additional routines:
!>      <UL>
!>      <LI> Generic interface fgsl_well_defined for checking status of FGSL
!>           objects (which are typically opaque).
!>      <LI> See api/array.finc for array alignment routines.
!>      <LI> See api/math.finc for function object constructors.
!>      <LI> See modules/fgsl_io.f90 for I/O related add-ons.
!>      </UL>
!>  <LI> Structure of the documentation:
!>      <UL>
!>      <LI> type definitions are in the fgsl section of the Modules menu item
!>      <LI> all API routines are available via the Files menu item
!>      <LI> additional remarks on the various files are available via the Related
!>           Pages menu item
!>      </UL>
!>  <LI> Only interfaces from the GSL manual are implemented. The
!>       C include files may contain more stuff which may only be meant
!>       for internal use, or is not officially documented.
!>  <LI> Inlining of GSL routines is not possible.
!>  <LI> Macros are not supported:
!>      <UL>
!>      <LI> macro values are replicated as parameters
!>      <LI> Inf/Nan need to use IEEE_VALUE (if available)
!>      </UL>
!>  </OL>
!>
!-------------------------------------------------------------------------------
  use, intrinsic :: iso_c_binding
  use fgsl_base
  use fgsl_errno
  use fgsl_io
  use fgsl_math
  use fgsl_complex_math
  use fgsl_poly
  use fgsl_sf
  implicit none

!
! Version strings
!
  character(kind=fgsl_char, len=*), public, parameter :: &
       fgsl_version=PACKAGE_VERSION
  character(kind=fgsl_char, len=*), public, parameter :: &
       fgsl_gslbase=GSL_VERSION

! Numerical constants
!
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
!
! MKSA physical units
!
  real(fgsl_double), parameter, public :: fgsl_const_mksa_speed_of_light = 2.99792458e8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_gravitational_constant = 6.673e-11_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_plancks_constant_h = 6.62606896e-34_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_plancks_constant_hbar = 1.05457162825e-34_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_astronomical_unit = 1.49597870691e11_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_light_year = 9.46053620707e15_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_parsec = 3.08567758135e16_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_grav_accel = 9.80665e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_electron_volt = 1.602176487e-19_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_mass_electron = 9.10938188e-31_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_mass_muon = 1.88353109e-28_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_mass_proton = 1.67262158e-27_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_mass_neutron = 1.67492716e-27_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_rydberg = 2.17987196968e-18_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_boltzmann = 1.3806504e-23_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_bohr_magneton = 9.27400899e-24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_nuclear_magneton = 5.05078317e-27_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_electron_magnetic_moment = 9.28476362e-24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_proton_magnetic_moment = 1.410606633e-26_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_molar_gas = 8.314472e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_standard_gas_volume = 2.2710981e-2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_minute = 6e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_hour = 3.6e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_day = 8.64e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_week = 6.048e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_inch = 2.54e-2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_foot = 3.048e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_yard = 9.144e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_mile = 1.609344e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_nautical_mile = 1.852e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_fathom = 1.8288e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_mil = 2.54e-5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_point = 3.52777777778e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_texpoint = 3.51459803515e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_micron = 1e-6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_angstrom = 1e-10_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_hectare = 1e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_acre = 4.04685642241e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_barn = 1e-28_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_liter = 1e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_us_gallon = 3.78541178402e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_quart = 9.46352946004e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_pint = 4.73176473002e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_cup = 2.36588236501e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_fluid_ounce = 2.95735295626e-5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_tablespoon = 1.47867647813e-5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_teaspoon = 4.92892159375e-6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_canadian_gallon = 4.54609e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_uk_gallon = 4.546092e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_miles_per_hour = 4.4704e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_kilometers_per_hour = 2.77777777778e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_knot = 5.14444444444e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_pound_mass = 4.5359237e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_ounce_mass = 2.8349523125e-2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_ton = 9.0718474e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_metric_ton = 1e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_uk_ton = 1.0160469088e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_troy_ounce = 3.1103475e-2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_carat = 2e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_unified_atomic_mass = 1.660538782e-27_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_gram_force = 9.80665e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_pound_force = 4.44822161526e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_kilopound_force = 4.44822161526e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_poundal = 1.38255e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_calorie = 4.1868e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_btu = 1.05505585262e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_therm = 1.05506e8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_horsepower = 7.457e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_bar = 1e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_std_atmosphere = 1.01325e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_torr = 1.33322368421e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_meter_of_mercury = 1.33322368421e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_inch_of_mercury = 3.38638815789e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_inch_of_water = 2.490889e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_psi = 6.89475729317e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_poise = 1e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_stokes = 1e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_faraday = 9.64853429775e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_electron_charge = 1.602176487e-19_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_gauss = 1e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_stilb = 1e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_lumen = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_lux = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_phot = 1e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_footcandle = 1.076e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_lambert = 1e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_footlambert = 1.07639104e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_curie = 3.7e10_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_roentgen = 2.58e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_rad = 1e-2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_solar_mass = 1.98892e30_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_bohr_radius = 5.291772083e-11_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_newton = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_dyne = 1e-5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_joule = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_erg = 1e-7_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_stefan_boltzmann_constant = 5.67040047374e-8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_thomson_cross_section = 6.65245893699e-29_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_vacuum_permittivity = 8.854187817e-12_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_vacuum_permeability = 1.25663706144e-6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_mksa_debye = 3.33564095198e-30_fgsl_double
!
! CGSM physical constants
!
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_speed_of_light = 2.99792458e10_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_gravitational_constant = 6.673e-8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_plancks_constant_h = 6.62606896e-27_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_plancks_constant_hbar = 1.05457162825e-27_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_astronomical_unit = 1.49597870691e13_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_light_year = 9.46053620707e17_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_parsec = 3.08567758135e18_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_grav_accel = 9.80665e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_electron_volt = 1.602176487e-12_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_mass_electron = 9.10938188e-28_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_mass_muon = 1.88353109e-25_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_mass_proton = 1.67262158e-24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_mass_neutron = 1.67492716e-24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_rydberg = 2.17987196968e-11_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_boltzmann = 1.3806504e-16_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_bohr_magneton = 9.27400899e-21_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_nuclear_magneton = 5.05078317e-24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_electron_magnetic_moment = 9.28476362e-21_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_proton_magnetic_moment = 1.410606633e-23_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_molar_gas = 8.314472e7_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_standard_gas_volume = 2.2710981e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_minute = 6e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_hour = 3.6e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_day = 8.64e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_week = 6.048e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_inch = 2.54e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_foot = 3.048e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_yard = 9.144e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_mile = 1.609344e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_nautical_mile = 1.852e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_fathom = 1.8288e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_mil = 2.54e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_point = 3.52777777778e-2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_texpoint = 3.51459803515e-2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_micron = 1e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_angstrom = 1e-8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_hectare = 1e8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_acre = 4.04685642241e7_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_barn = 1e-24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_liter = 1e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_us_gallon = 3.78541178402e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_quart = 9.46352946004e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_pint = 4.73176473002e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_cup = 2.36588236501e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_fluid_ounce = 2.95735295626e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_tablespoon = 1.47867647813e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_teaspoon = 4.92892159375e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_canadian_gallon = 4.54609e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_uk_gallon = 4.546092e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_miles_per_hour = 4.4704e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_kilometers_per_hour = 2.77777777778e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_knot = 5.14444444444e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_pound_mass = 4.5359237e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_ounce_mass = 2.8349523125e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_ton = 9.0718474e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_metric_ton = 1e6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_uk_ton = 1.0160469088e6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_troy_ounce = 3.1103475e1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_carat = 2e-1_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_unified_atomic_mass = 1.660538782e-24_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_gram_force = 9.80665e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_pound_force = 4.44822161526e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_kilopound_force = 4.44822161526e8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_poundal = 1.38255e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_calorie = 4.1868e7_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_btu = 1.05505585262e10_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_therm = 1.05506e15_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_horsepower = 7.457e9_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_bar = 1e6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_std_atmosphere = 1.01325e6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_torr = 1.33322368421e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_meter_of_mercury = 1.33322368421e6_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_inch_of_mercury = 3.38638815789e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_inch_of_water = 2.490889e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_psi = 6.89475729317e4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_poise = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_stokes = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_faraday = 9.64853429775e3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_electron_charge = 1.602176487e-20_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_gauss = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_stilb = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_lumen = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_lux = 1e-4_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_phot = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_footcandle = 1.076e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_lambert = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_footlambert = 1.07639104e-3_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_curie = 3.7e10_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_roentgen = 2.58e-8_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_rad = 1e2_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_solar_mass = 1.98892e33_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_bohr_radius = 5.291772083e-9_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_newton = 1e5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_dyne = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_joule = 1e7_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_erg = 1e0_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_stefan_boltzmann_constant = 5.67040047374e-5_fgsl_double
  real(fgsl_double), parameter, public :: fgsl_const_cgsm_thomson_cross_section = 6.65245893699e-25_fgsl_double



!
! Enum: legendre special Functions
! The below probably are not needed any more
integer(fgsl_int), public, parameter :: gsl_sf_legendre_schmidt = 0
integer(fgsl_int), public, parameter :: gsl_sf_legendre_spharm = 1
integer(fgsl_int), public, parameter :: gsl_sf_legendre_full = 2
integer(fgsl_int), public, parameter :: gsl_sf_legendre_none = 3


!
! Types : Array support
!
  type, public :: fgsl_vector
     type(c_ptr) :: gsl_vector = c_null_ptr
  end type fgsl_vector
   type, public :: fgsl_vector_int
     type(c_ptr) :: gsl_vector_int = c_null_ptr
  end type fgsl_vector_int
  type, public :: fgsl_matrix
     type(c_ptr) :: gsl_matrix = c_null_ptr
  end type fgsl_matrix
  type, public :: fgsl_vector_complex
     type(c_ptr) :: gsl_vector_complex = c_null_ptr
  end type fgsl_vector_complex
  type, public :: fgsl_matrix_complex
     type(c_ptr) :: gsl_matrix_complex = c_null_ptr
  end type fgsl_matrix_complex
!
! Types: large linear least squares systems
!
  type, public :: fgsl_multilarge_linear_type
    private
    integer(fgsl_int) :: which = 0
  end type fgsl_multilarge_linear_type
  type(fgsl_multilarge_linear_type), parameter, public :: &
    fgsl_multilarge_linear_normal = fgsl_multilarge_linear_type(1), &
    fgsl_multilarge_linear_tsqr = fgsl_multilarge_linear_type(2)

  type, public :: fgsl_multilarge_linear_workspace
    private
    type(c_ptr) :: gsl_multilarge_linear_workspace
  end type fgsl_multilarge_linear_workspace
!
! Types : Interpolation
!
  type, public :: fgsl_interp_type
     private
     integer(fgsl_int) :: which = 0
  end type fgsl_interp_type
  type(fgsl_interp_type), parameter, public :: &
       fgsl_interp_linear = fgsl_interp_type(1), &
       fgsl_interp_polynomial = fgsl_interp_type(2), &
       fgsl_interp_cspline = fgsl_interp_type(3), &
       fgsl_interp_cspline_periodic = fgsl_interp_type(4), &
       fgsl_interp_akima = fgsl_interp_type(5), &
       fgsl_interp_akima_periodic = fgsl_interp_type(6), &
       fgsl_interp_steffen = fgsl_interp_type(7)
  type, public :: fgsl_interp
     private
     type(c_ptr) :: gsl_interp = c_null_ptr
  end type fgsl_interp
  type, public :: fgsl_interp_accel
     private
     type(c_ptr) :: gsl_interp_accel = c_null_ptr
  end type fgsl_interp_accel
  type, public :: fgsl_spline
     private
     type(c_ptr) :: gsl_spline = c_null_ptr
  end type fgsl_spline
  type, public :: fgsl_spline2d
     private
     type(c_ptr) :: gsl_spline2d = c_null_ptr
  end type fgsl_spline2d
  type, public :: fgsl_interp2d_type
    private
    integer(fgsl_int) :: which = 0
  end type fgsl_interp2d_type
  type(fgsl_interp2d_type), parameter, public :: &
       fgsl_interp2d_bilinear = fgsl_interp2d_type(1), &
       fgsl_interp2d_bicubic = fgsl_interp2d_type(2)
  type, public :: fgsl_interp2d
     private
     type(c_ptr) :: gsl_interp2d = c_null_ptr
  end type fgsl_interp2d
!
! Types: Permutations, Combinations and Multisets
!
  type, public :: fgsl_permutation
     private
     type(c_ptr) :: gsl_permutation = c_null_ptr
  end type fgsl_permutation
  type, public :: fgsl_combination
     private
     type(c_ptr) :: gsl_combination = c_null_ptr
  end type fgsl_combination
  type, public :: fgsl_multiset
     private
     type(c_ptr) :: gsl_multiset = c_null_ptr
  end type fgsl_multiset
!
! Types: Robust multifit
!
  type, public :: fgsl_multifit_robust_type
     private
     integer(fgsl_int) :: which = 0
  end type fgsl_multifit_robust_type
  type(fgsl_multifit_robust_type), parameter, public :: &
       fgsl_multifit_robust_default = fgsl_multifit_robust_type(1), &
       fgsl_multifit_robust_bisquare = fgsl_multifit_robust_type(2), &
       fgsl_multifit_robust_cauchy = fgsl_multifit_robust_type(3), &
       fgsl_multifit_robust_fair = fgsl_multifit_robust_type(4), &
       fgsl_multifit_robust_huber = fgsl_multifit_robust_type(5), &
       fgsl_multifit_robust_ols = fgsl_multifit_robust_type(6), &
       fgsl_multifit_robust_welsch = fgsl_multifit_robust_type(7)
  type, public :: fgsl_multifit_robust_workspace
       private
       type(c_ptr) :: gsl_multifit_robust_workspace
  end type fgsl_multifit_robust_workspace
  type, public :: fgsl_multifit_robust_stats
       real(fgsl_double) :: sigma_ols
       real(fgsl_double) :: sigma_mad
       real(fgsl_double) :: sigma_rob
       real(fgsl_double) :: sigma
       real(fgsl_double) :: Rsq
       real(fgsl_double) :: adj_Rsq
       real(fgsl_double) :: rmse
       real(fgsl_double) :: sse
       real(fgsl_double) :: dof
       real(fgsl_double) :: numit
       type(fgsl_vector) :: weights
       type(fgsl_vector) :: r
  end type fgsl_multifit_robust_stats
  type, bind(c) :: gsl_multifit_robust_stats
       real(c_double) :: sigma_ols
       real(c_double) :: sigma_mad
       real(c_double) :: sigma_rob
       real(c_double) :: sigma
       real(c_double) :: Rsq
       real(c_double) :: adj_Rsq
       real(c_double) :: rmse
       real(c_double) :: sse
       real(c_double) :: dof
       real(c_double) :: numit
       type(c_ptr) :: weights
       type(c_ptr) :: r
  end type gsl_multifit_robust_stats
  !
  ! Types: Linear Algebra
  !
  integer(fgsl_int), parameter, public :: &
       cblasrowmajor = 101, &
       cblascolmajor = 102, &
       cblasnotrans = 111, &
       cblastrans = 112, &
       cblasconjtrans = 113, &
       cblasupper = 121, &
       cblaslower = 122, &
       cblasnonunit = 131, &
       cblasunit = 132, &
       cblasleft = 141, &
       cblasright = 142
  !
  ! Types: Eigensystems
  !
  type, public :: fgsl_eigen_symm_workspace
     private
     type(c_ptr) :: gsl_eigen_symm_workspace = c_null_ptr
  end type fgsl_eigen_symm_workspace
  type, public :: fgsl_eigen_symmv_workspace
     private
     type(c_ptr) :: gsl_eigen_symmv_workspace = c_null_ptr
  end type fgsl_eigen_symmv_workspace
  type, public :: fgsl_eigen_herm_workspace
     private
     type(c_ptr) :: gsl_eigen_herm_workspace = c_null_ptr
  end type fgsl_eigen_herm_workspace
  type, public :: fgsl_eigen_hermv_workspace
     private
     type(c_ptr) :: gsl_eigen_hermv_workspace = c_null_ptr
  end type fgsl_eigen_hermv_workspace
  type, public :: fgsl_eigen_nonsymm_workspace
     private
     type(c_ptr) :: gsl_eigen_nonsymm_workspace = c_null_ptr
  end type fgsl_eigen_nonsymm_workspace
  type, public :: fgsl_eigen_nonsymmv_workspace
     private
     type(c_ptr) :: gsl_eigen_nonsymmv_workspace = c_null_ptr
  end type fgsl_eigen_nonsymmv_workspace
  type, public :: fgsl_eigen_gensymm_workspace
     private
     type(c_ptr) :: gsl_eigen_gensymm_workspace = c_null_ptr
  end type fgsl_eigen_gensymm_workspace
  type, public :: fgsl_eigen_gensymmv_workspace
     private
     type(c_ptr) :: gsl_eigen_gensymmv_workspace = c_null_ptr
  end type fgsl_eigen_gensymmv_workspace
  type, public :: fgsl_eigen_genherm_workspace
     private
     type(c_ptr) :: gsl_eigen_genherm_workspace = c_null_ptr
  end type fgsl_eigen_genherm_workspace
  type, public :: fgsl_eigen_genhermv_workspace
     private
     type(c_ptr) :: gsl_eigen_genhermv_workspace = c_null_ptr
  end type fgsl_eigen_genhermv_workspace
  type, public :: fgsl_eigen_gen_workspace
     private
     type(c_ptr) :: gsl_eigen_gen_workspace = c_null_ptr
  end type fgsl_eigen_gen_workspace
  type, public :: fgsl_eigen_genv_workspace
     private
     type(c_ptr) :: gsl_eigen_genv_workspace = c_null_ptr
  end type fgsl_eigen_genv_workspace
  integer(c_int), parameter, public :: fgsl_eigen_sort_val_asc = 0
  integer(c_int), parameter, public :: fgsl_eigen_sort_val_desc = 1
  integer(c_int), parameter, public :: fgsl_eigen_sort_abs_asc = 2
  integer(c_int), parameter, public :: fgsl_eigen_sort_abs_desc = 3
!
! Types: FFT
!
  type, public :: fgsl_fft_complex_wavetable
     private
     type(c_ptr) :: gsl_fft_complex_wavetable = c_null_ptr
  end type fgsl_fft_complex_wavetable
  type, public :: fgsl_fft_real_wavetable
     private
     type(c_ptr) :: gsl_fft_real_wavetable = c_null_ptr
  end type fgsl_fft_real_wavetable
  type, public :: fgsl_fft_halfcomplex_wavetable
     private
     type(c_ptr) :: gsl_fft_halfcomplex_wavetable = c_null_ptr
  end type fgsl_fft_halfcomplex_wavetable
  type, public :: fgsl_fft_complex_workspace
     private
     type(c_ptr) :: gsl_fft_complex_workspace = c_null_ptr
  end type fgsl_fft_complex_workspace
  type, public :: fgsl_fft_real_workspace
     private
     type(c_ptr) :: gsl_fft_real_workspace = c_null_ptr
  end type fgsl_fft_real_workspace
!
! Types: Numerical Integration
!
  type, public :: fgsl_integration_workspace
     private
     type(c_ptr) :: gsl_integration_workspace = c_null_ptr
  end type fgsl_integration_workspace
  integer(fgsl_int), parameter, public :: fgsl_integ_gauss15 = 1
  integer(fgsl_int), parameter, public :: fgsl_integ_gauss21 = 2
  integer(fgsl_int), parameter, public :: fgsl_integ_gauss31 = 3
  integer(fgsl_int), parameter, public :: fgsl_integ_gauss41 = 4
  integer(fgsl_int), parameter, public :: fgsl_integ_gauss51 = 5
  integer(fgsl_int), parameter, public :: fgsl_integ_gauss61 = 6
  type, public :: fgsl_integration_qaws_table
     private
     type(c_ptr) :: gsl_integration_qaws_table = c_null_ptr
  end type fgsl_integration_qaws_table
  type, public :: fgsl_integration_qawo_table
     private
     type(c_ptr) :: gsl_integration_qawo_table = c_null_ptr
  end type fgsl_integration_qawo_table
  integer(fgsl_int), parameter, public :: fgsl_integ_cosine = 0
  integer(fgsl_int), parameter, public :: fgsl_integ_sine = 1
  type, public :: fgsl_integration_cquad_workspace
     private
     type(c_ptr) :: gsl_integration_cquad_workspace = c_null_ptr
  end type fgsl_integration_cquad_workspace
  type, public :: fgsl_integration_romberg_workspace
     private
     type(c_ptr) :: gsl_integration_romberg_workspace  = c_null_ptr
  end type fgsl_integration_romberg_workspace
  type, public :: fgsl_integration_glfixed_table
     private
     type(c_ptr) :: gsl_integration_glfixed_table = c_null_ptr
  end type fgsl_integration_glfixed_table
  type, public :: fgsl_integration_fixed_workspace
     private
     type(c_ptr) :: gsl_integration_fixed_workspace = c_null_ptr
  end type fgsl_integration_fixed_workspace
  integer(fgsl_int), parameter, public :: fgsl_integration_fixed_legendre = 1
  integer(fgsl_int), parameter, public :: fgsl_integration_fixed_chebyshev = 2
  integer(fgsl_int), parameter, public :: fgsl_integration_fixed_gegenbauer = 3
  integer(fgsl_int), parameter, public :: fgsl_integration_fixed_jacobi = 4
  integer(fgsl_int), parameter, public :: fgsl_integration_fixed_laguerre = 5
  integer(fgsl_int), parameter, public :: fgsl_integration_fixed_hermite = 6
  integer(fgsl_int), parameter, public :: fgsl_integration_fixed_exponential = 7
  integer(fgsl_int), parameter, public :: fgsl_integration_fixed_rational = 8
  integer(fgsl_int), parameter, public :: fgsl_integration_fixed_chebyshev2 = 9


!
! Types: Random and Quasi-random numbers
!
  type, public :: fgsl_rng
     private
     type(c_ptr) :: gsl_rng = c_null_ptr
  end type fgsl_rng
  type, public :: fgsl_rng_type
     private
     type(c_ptr) :: gsl_rng_type = c_null_ptr
     integer(fgsl_int) :: type = 0
  end type fgsl_rng_type
! Note: we need a dynamic component here, since
! fgsl_rng_default needs to change at run time.
! fgsl_rng_default will be set by fgsl_rng_env_setup,
! and the static objects will be all set at the
! first call of fgsl_rng_alloc
!  integer, parameter :: rngmax = 61
! check new g95  type(fgsl_rng_type) :: fgsl_rng_allgen(rngmax) = (/(fgsl_rng_type(c_null_ptr, i)), i=1, rngmax/)
! cannot have protected attribute since modified by fgsl_rng_alloc
  type(fgsl_rng_type), public :: &
       fgsl_rng_default = fgsl_rng_type(c_null_ptr, -1), &
       fgsl_rng_borosh13 = fgsl_rng_type(c_null_ptr, 1), &
       fgsl_rng_coveyou = fgsl_rng_type(c_null_ptr, 2), &
       fgsl_rng_cmrg = fgsl_rng_type(c_null_ptr, 3), &
       fgsl_rng_fishman18 = fgsl_rng_type(c_null_ptr, 4), &
       fgsl_rng_fishman20 = fgsl_rng_type(c_null_ptr, 5), &
       fgsl_rng_fishman2x = fgsl_rng_type(c_null_ptr, 6), &
       fgsl_rng_gfsr4 = fgsl_rng_type(c_null_ptr, 7), &
       fgsl_rng_knuthran = fgsl_rng_type(c_null_ptr, 8), &
       fgsl_rng_knuthran2 = fgsl_rng_type(c_null_ptr, 9), &
       fgsl_rng_lecuyer21 = fgsl_rng_type(c_null_ptr, 10), &
       fgsl_rng_minstd = fgsl_rng_type(c_null_ptr, 11), &
       fgsl_rng_mrg = fgsl_rng_type(c_null_ptr, 12), &
       fgsl_rng_mt19937 = fgsl_rng_type(c_null_ptr, 13), &
       fgsl_rng_mt19937_1999 = fgsl_rng_type(c_null_ptr, 14), &
       fgsl_rng_mt19937_1998 = fgsl_rng_type(c_null_ptr, 15), &
       fgsl_rng_r250 = fgsl_rng_type(c_null_ptr, 16), &
       fgsl_rng_ran0 = fgsl_rng_type(c_null_ptr, 17), &
       fgsl_rng_ran1 = fgsl_rng_type(c_null_ptr, 18), &
       fgsl_rng_ran2 = fgsl_rng_type(c_null_ptr, 19), &
       fgsl_rng_ran3 = fgsl_rng_type(c_null_ptr, 20), &
       fgsl_rng_rand = fgsl_rng_type(c_null_ptr, 21), &
       fgsl_rng_rand48 = fgsl_rng_type(c_null_ptr, 22), &
       fgsl_rng_random128_bsd = fgsl_rng_type(c_null_ptr, 23), &
       fgsl_rng_random128_glibc2 = fgsl_rng_type(c_null_ptr, 24), &
       fgsl_rng_random128_libc5 = fgsl_rng_type(c_null_ptr, 25), &
       fgsl_rng_random256_bsd = fgsl_rng_type(c_null_ptr, 26), &
       fgsl_rng_random256_glibc2 = fgsl_rng_type(c_null_ptr, 27), &
       fgsl_rng_random256_libc5 = fgsl_rng_type(c_null_ptr, 28), &
       fgsl_rng_random32_bsd = fgsl_rng_type(c_null_ptr, 29), &
       fgsl_rng_random32_glibc2 = fgsl_rng_type(c_null_ptr, 30), &
       fgsl_rng_random32_libc5 = fgsl_rng_type(c_null_ptr, 31), &
       fgsl_rng_random64_bsd = fgsl_rng_type(c_null_ptr, 32), &
       fgsl_rng_random64_glibc2 = fgsl_rng_type(c_null_ptr, 33), &
       fgsl_rng_random64_libc5 = fgsl_rng_type(c_null_ptr, 34), &
       fgsl_rng_random8_bsd = fgsl_rng_type(c_null_ptr, 35)
  type(fgsl_rng_type), public ::  &
       fgsl_rng_random8_glibc2 = fgsl_rng_type(c_null_ptr, 36), &
       fgsl_rng_random8_libc5 = fgsl_rng_type(c_null_ptr, 37), &
       fgsl_rng_random_bsd = fgsl_rng_type(c_null_ptr, 38), &
       fgsl_rng_random_glibc2 = fgsl_rng_type(c_null_ptr, 39), &
       fgsl_rng_random_libc5 = fgsl_rng_type(c_null_ptr, 40), &
       fgsl_rng_randu = fgsl_rng_type(c_null_ptr, 41), &
       fgsl_rng_ranf = fgsl_rng_type(c_null_ptr, 42), &
       fgsl_rng_ranlux = fgsl_rng_type(c_null_ptr, 43), &
       fgsl_rng_ranlux389 = fgsl_rng_type(c_null_ptr, 44), &
       fgsl_rng_ranlxd1 = fgsl_rng_type(c_null_ptr, 45), &
       fgsl_rng_ranlxd2 = fgsl_rng_type(c_null_ptr, 46), &
       fgsl_rng_ranlxs0 = fgsl_rng_type(c_null_ptr, 47), &
       fgsl_rng_ranlxs1 = fgsl_rng_type(c_null_ptr, 48), &
       fgsl_rng_ranlxs2 = fgsl_rng_type(c_null_ptr, 49), &
       fgsl_rng_ranmar = fgsl_rng_type(c_null_ptr, 50), &
       fgsl_rng_slatec = fgsl_rng_type(c_null_ptr, 51), &
       fgsl_rng_taus = fgsl_rng_type(c_null_ptr, 52), &
       fgsl_rng_taus2 = fgsl_rng_type(c_null_ptr, 53), &
       fgsl_rng_taus113 = fgsl_rng_type(c_null_ptr, 54), &
       fgsl_rng_transputer = fgsl_rng_type(c_null_ptr, 55), &
       fgsl_rng_tt800 = fgsl_rng_type(c_null_ptr, 56), &
       fgsl_rng_uni = fgsl_rng_type(c_null_ptr, 57), &
       fgsl_rng_uni32 = fgsl_rng_type(c_null_ptr, 58), &
       fgsl_rng_vax = fgsl_rng_type(c_null_ptr, 59), &
       fgsl_rng_waterman14 = fgsl_rng_type(c_null_ptr, 60), &
       fgsl_rng_zuf = fgsl_rng_type(c_null_ptr, 61), &
       fgsl_rng_knuthran2002 = fgsl_rng_type(c_null_ptr, 62)
  integer(fgsl_long), public, bind(c, name='gsl_rng_default_seed') :: fgsl_rng_default_seed
  type, public :: fgsl_qrng
     private
     type(c_ptr) :: gsl_qrng
  end type fgsl_qrng
  type, public :: fgsl_qrng_type
     private
     integer(fgsl_int) :: type = 0
  end type fgsl_qrng_type
  type(fgsl_qrng_type), parameter, public :: &
       fgsl_qrng_niederreiter_2 = fgsl_qrng_type(1), &
       fgsl_qrng_sobol = fgsl_qrng_type(2), &
       fgsl_qrng_halton =  fgsl_qrng_type(3), &
       fgsl_qrng_reversehalton =  fgsl_qrng_type(4)
  type, public :: fgsl_ran_discrete_t
     private
     type(c_ptr) :: gsl_ran_discrete_t
  end type fgsl_ran_discrete_t
!
! Types: Histograms
!
  type, public :: fgsl_histogram
     private
     type(c_ptr) :: gsl_histogram = c_null_ptr
  end type fgsl_histogram
  type, public :: fgsl_histogram_pdf
     private
     type(c_ptr) :: gsl_histogram_pdf = c_null_ptr
  end type fgsl_histogram_pdf
  type, public :: fgsl_histogram2d
     private
     type(c_ptr) :: gsl_histogram2d = c_null_ptr
  end type fgsl_histogram2d
  type, public :: fgsl_histogram2d_pdf
     private
     type(c_ptr) :: gsl_histogram2d_pdf = c_null_ptr
  end type fgsl_histogram2d_pdf
!
! Types: Ntuples
!
  type, public :: fgsl_ntuple
     private
     type(c_ptr) :: gsl_ntuple = c_null_ptr
  end type fgsl_ntuple
  type, public :: fgsl_ntuple_select_fn
     private
     type(c_ptr) :: gsl_ntuple_select_fn = c_null_ptr
  end type fgsl_ntuple_select_fn
  type, public :: fgsl_ntuple_value_fn
     private
     type(c_ptr) :: gsl_ntuple_value_fn = c_null_ptr
  end type fgsl_ntuple_value_fn
!
! Types: Monte Carlo integration
!
  type, public :: fgsl_monte_function
     private
     type(c_ptr) :: gsl_monte_function = c_null_ptr
  end type fgsl_monte_function
  type, public :: fgsl_monte_plain_state
     private
     type(c_ptr) :: gsl_monte_plain_state = c_null_ptr
  end type fgsl_monte_plain_state
  type, public :: fgsl_monte_miser_state
     private
     type(c_ptr) :: gsl_monte_miser_state = c_null_ptr
  end type fgsl_monte_miser_state
  type, public :: fgsl_monte_vegas_state
     private
     type(c_ptr) :: gsl_monte_vegas_state = c_null_ptr
  end type fgsl_monte_vegas_state
! NOTE: not all compilers support enum yet
  integer(c_int), parameter, public :: fgsl_vegas_mode_importance = 1
  integer(c_int), parameter, public :: fgsl_vegas_mode_importance_only = 0
  integer(c_int), parameter, public :: fgsl_vegas_mode_stratified = -1
!
! Types: Simulated Annealing
!
  type, bind(c) :: gsl_siman_params_t
     integer(c_int) :: n_tries, iters_fixed_t
     real(c_double) :: step_size, k, t_initial, mu_t, t_min
  end type gsl_siman_params_t
  type, public :: fgsl_siman_params_t
     private
     type(gsl_siman_params_t), pointer :: gsl_siman_params_t => null()
  end type fgsl_siman_params_t
!
! Types: Ordinary Differential Equations
!
  type, public :: fgsl_odeiv2_system
     private
     type(c_ptr) :: gsl_odeiv2_system = c_null_ptr
  end type fgsl_odeiv2_system
  type, public :: fgsl_odeiv2_step_type
     private
     integer(c_int) :: which = 0
  end type fgsl_odeiv2_step_type
  type(fgsl_odeiv2_step_type), parameter, public :: &
       fgsl_odeiv2_step_rk2 = fgsl_odeiv2_step_type(1), &
       fgsl_odeiv2_step_rk4 = fgsl_odeiv2_step_type(2), &
       fgsl_odeiv2_step_rkf45 = fgsl_odeiv2_step_type(3), &
       fgsl_odeiv2_step_rkck = fgsl_odeiv2_step_type(4), &
       fgsl_odeiv2_step_rk8pd = fgsl_odeiv2_step_type(5), &
       fgsl_odeiv2_step_rk1imp = fgsl_odeiv2_step_type(6), &
       fgsl_odeiv2_step_rk2imp = fgsl_odeiv2_step_type(7), &
       fgsl_odeiv2_step_rk4imp = fgsl_odeiv2_step_type(8), &
       fgsl_odeiv2_step_bsimp = fgsl_odeiv2_step_type(9), &
       fgsl_odeiv2_step_msadams = fgsl_odeiv2_step_type(10), &
       fgsl_odeiv2_step_msbdf = fgsl_odeiv2_step_type(11)
  type, public :: fgsl_odeiv2_step
     type(c_ptr) :: gsl_odeiv2_step = c_null_ptr
  end type fgsl_odeiv2_step
  type, public :: fgsl_odeiv2_driver
     private
     type(c_ptr) :: gsl_odeiv2_driver = c_null_ptr
  end type fgsl_odeiv2_driver
  type, public :: fgsl_odeiv2_control_type
     type(c_ptr) :: gsl_odeiv2_control_type = c_null_ptr
  end type fgsl_odeiv2_control_type
  type, public :: fgsl_odeiv2_control
     type(c_ptr) :: gsl_odeiv2_control = c_null_ptr
  end type fgsl_odeiv2_control
  type, public :: fgsl_odeiv2_evolve
     type(c_ptr) :: gsl_odeiv2_evolve
  end type fgsl_odeiv2_evolve

! obsolescent legacy interface
  type, public :: fgsl_odeiv_system
     private
     type(c_ptr) :: gsl_odeiv_system = c_null_ptr
  end type fgsl_odeiv_system
  type, public :: fgsl_odeiv_step_type
     private
     integer(c_int) :: which = 0
  end type fgsl_odeiv_step_type
  type(fgsl_odeiv_step_type), parameter, public :: &
       fgsl_odeiv_step_rk2 = fgsl_odeiv_step_type(1), &
       fgsl_odeiv_step_rk4 = fgsl_odeiv_step_type(2), &
       fgsl_odeiv_step_rkf45 = fgsl_odeiv_step_type(3), &
       fgsl_odeiv_step_rkck = fgsl_odeiv_step_type(4), &
       fgsl_odeiv_step_rk8pd = fgsl_odeiv_step_type(5), &
       fgsl_odeiv_step_rk2imp = fgsl_odeiv_step_type(6), &
       fgsl_odeiv_step_rk2simp = fgsl_odeiv_step_type(7), &
       fgsl_odeiv_step_rk4imp = fgsl_odeiv_step_type(8), &
       fgsl_odeiv_step_bsimp = fgsl_odeiv_step_type(9), &
       fgsl_odeiv_step_gear1 = fgsl_odeiv_step_type(10), &
       fgsl_odeiv_step_gear2 = fgsl_odeiv_step_type(11)
  type, public :: fgsl_odeiv_step
     type(c_ptr) :: gsl_odeiv_step = c_null_ptr
  end type fgsl_odeiv_step
  type, public :: fgsl_odeiv_control
     type(c_ptr) :: gsl_odeiv_control = c_null_ptr
  end type fgsl_odeiv_control
  type, public :: fgsl_odeiv_control_type
     type(c_ptr) :: gsl_odeiv_control_type = c_null_ptr
  end type fgsl_odeiv_control_type
  integer(fgsl_int), parameter, public :: fgsl_odeiv_hadj_inc = 1
  integer(fgsl_int), parameter, public :: fgsl_odeiv_hadj_nil = 0
  integer(fgsl_int), parameter, public :: fgsl_odeiv_hadj_dec = -1
  type, public :: fgsl_odeiv_evolve
     type(c_ptr) :: gsl_odeiv_evolve
  end type fgsl_odeiv_evolve
!
! Types: Chebyshev approximation
!
  type, public :: fgsl_cheb_series
     private
     type(c_ptr) :: gsl_cheb_series = c_null_ptr
  end type fgsl_cheb_series
!
! Types: Series acceleration
!
  type, public :: fgsl_sum_levin_u_workspace
     private
     type(c_ptr) :: gsl_sum_levin_u_workspace = c_null_ptr
  end type fgsl_sum_levin_u_workspace
  type, public :: fgsl_sum_levin_utrunc_workspace
     private
     type(c_ptr) :: gsl_sum_levin_utrunc_workspace = c_null_ptr
  end type fgsl_sum_levin_utrunc_workspace
!
! Types: Wavelet transforms
!
  type, public :: fgsl_wavelet
     private
     type(c_ptr) :: gsl_wavelet = c_null_ptr
  end type fgsl_wavelet
  type, public :: fgsl_wavelet_type
     private
     integer(c_int) :: which = 0
  end type fgsl_wavelet_type
  type(fgsl_wavelet_type), public, parameter :: &
       fgsl_wavelet_daubechies = fgsl_wavelet_type(1), &
       fgsl_wavelet_daubechies_centered = fgsl_wavelet_type(2), &
       fgsl_wavelet_haar = fgsl_wavelet_type(3), &
       fgsl_wavelet_haar_centered = fgsl_wavelet_type(4), &
       fgsl_wavelet_bspline = fgsl_wavelet_type(5), &
       fgsl_wavelet_bspline_centered = fgsl_wavelet_type(6)
  type, public :: fgsl_wavelet_workspace
     private
     type(c_ptr) :: gsl_wavelet_workspace
  end type fgsl_wavelet_workspace
!
! Types: Hankel transforms
!
  type, public :: fgsl_dht
     private
     type(c_ptr) :: gsl_dht = c_null_ptr
  end type fgsl_dht
!
! Types: Root finding
!
  type, public :: fgsl_root_fsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_root_fsolver_type
  type(fgsl_root_fsolver_type), public, parameter :: &
       fgsl_root_fsolver_bisection = fgsl_root_fsolver_type(1), &
       fgsl_root_fsolver_brent = fgsl_root_fsolver_type(2), &
       fgsl_root_fsolver_falsepos = fgsl_root_fsolver_type(3)
  type, public :: fgsl_root_fdfsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_root_fdfsolver_type
  type(fgsl_root_fdfsolver_type), public, parameter :: &
       fgsl_root_fdfsolver_newton = fgsl_root_fdfsolver_type(1), &
       fgsl_root_fdfsolver_secant = fgsl_root_fdfsolver_type(2), &
       fgsl_root_fdfsolver_steffenson = fgsl_root_fdfsolver_type(3)
  type, public :: fgsl_root_fsolver
     private
     type(c_ptr) :: gsl_root_fsolver = c_null_ptr
  end type fgsl_root_fsolver
  type, public :: fgsl_root_fdfsolver
     private
     type(c_ptr) :: gsl_root_fdfsolver = c_null_ptr
  end type fgsl_root_fdfsolver
!
! Types: Minimization
!
  type, public :: fgsl_min_fminimizer_type
     private
     integer(c_int) :: which = 0
  end type fgsl_min_fminimizer_type
  type(fgsl_min_fminimizer_type), public, parameter :: &
       fgsl_min_fminimizer_goldensection = fgsl_min_fminimizer_type(1), &
       fgsl_min_fminimizer_brent = fgsl_min_fminimizer_type(2), &
       fgsl_min_fminimizer_quad_golden = fgsl_min_fminimizer_type(3)
  type, public :: fgsl_min_fminimizer
     private
     type(c_ptr) :: gsl_min_fminimizer = c_null_ptr
  end type fgsl_min_fminimizer
!
! Types: Multi-Root
!
  type, public :: fgsl_multiroot_function
     private
     type(c_ptr) :: gsl_multiroot_function = c_null_ptr
  end type fgsl_multiroot_function
  type, public :: fgsl_multiroot_function_fdf
     private
     type(c_ptr) :: gsl_multiroot_function_fdf = c_null_ptr
  end type fgsl_multiroot_function_fdf
  type, public :: fgsl_multiroot_fsolver
     private
     type(c_ptr) :: gsl_multiroot_fsolver = c_null_ptr
  end type fgsl_multiroot_fsolver
  type, public :: fgsl_multiroot_fsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multiroot_fsolver_type
  type(fgsl_multiroot_fsolver_type), public, parameter :: &
       fgsl_multiroot_fsolver_dnewton = fgsl_multiroot_fsolver_type(1), &
       fgsl_multiroot_fsolver_broyden = fgsl_multiroot_fsolver_type(2), &
       fgsl_multiroot_fsolver_hybrid = fgsl_multiroot_fsolver_type(3), &
       fgsl_multiroot_fsolver_hybrids = fgsl_multiroot_fsolver_type(4)
  type, public :: fgsl_multiroot_fdfsolver
     private
     type(c_ptr) :: gsl_multiroot_fdfsolver = c_null_ptr
  end type fgsl_multiroot_fdfsolver
  type, public :: fgsl_multiroot_fdfsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multiroot_fdfsolver_type
  type(fgsl_multiroot_fdfsolver_type), public, parameter :: &
       fgsl_multiroot_fdfsolver_newton = fgsl_multiroot_fdfsolver_type(1), &
       fgsl_multiroot_fdfsolver_gnewton = fgsl_multiroot_fdfsolver_type(2), &
       fgsl_multiroot_fdfsolver_hybridj = fgsl_multiroot_fdfsolver_type(3), &
       fgsl_multiroot_fdfsolver_hybridsj = fgsl_multiroot_fdfsolver_type(4)
!
! Types: Multi-Min
!
  type, public :: fgsl_multimin_function
     private
     type(c_ptr) :: gsl_multimin_function = c_null_ptr
  end type fgsl_multimin_function
  type, public :: fgsl_multimin_function_fdf
     private
     type(c_ptr) :: gsl_multimin_function_fdf = c_null_ptr
  end type fgsl_multimin_function_fdf
  type, public :: fgsl_multimin_fminimizer
     private
     type(c_ptr) :: gsl_multimin_fminimizer = c_null_ptr
  end type fgsl_multimin_fminimizer
  type, public :: fgsl_multimin_fminimizer_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multimin_fminimizer_type
  type(fgsl_multimin_fminimizer_type), public, parameter :: &
       fgsl_multimin_fminimizer_nmsimplex = fgsl_multimin_fminimizer_type(1), &
       fgsl_multimin_fminimizer_nmsimplex2 = fgsl_multimin_fminimizer_type(2), &
       fgsl_multimin_fminimizer_nmsimplex2rand = fgsl_multimin_fminimizer_type(3)
  type, public :: fgsl_multimin_fdfminimizer
     private
     type(c_ptr) :: gsl_multimin_fdfminimizer = c_null_ptr
  end type fgsl_multimin_fdfminimizer
  type, public :: fgsl_multimin_fdfminimizer_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multimin_fdfminimizer_type
  type(fgsl_multimin_fdfminimizer_type), public, parameter :: &
       fgsl_multimin_fdfminimizer_steepest_descent = fgsl_multimin_fdfminimizer_type(1), &
       fgsl_multimin_fdfminimizer_conjugate_pr = fgsl_multimin_fdfminimizer_type(2), &
       fgsl_multimin_fdfminimizer_conjugate_fr = fgsl_multimin_fdfminimizer_type(3), &
       fgsl_multimin_fdfminimizer_vector_bfgs = fgsl_multimin_fdfminimizer_type(4), &
       fgsl_multimin_fdfminimizer_vector_bfgs2 = fgsl_multimin_fdfminimizer_type(5)
!
! Types and constants: Fitting
!
  type, public :: fgsl_multifit_linear_workspace
     private
     type(c_ptr) :: gsl_multifit_linear_workspace = c_null_ptr
  end type fgsl_multifit_linear_workspace
!
! new nonlinear interfaces for both small and large problems
  type, public :: fgsl_multifit_nlinear_type
     private
     type(c_ptr) :: gsl_multifit_nlinear_type = c_null_ptr
  end type fgsl_multifit_nlinear_type
  type, public :: fgsl_multifit_nlinear_workspace
     type(c_ptr) :: gsl_multifit_nlinear_workspace = c_null_ptr
  end type fgsl_multifit_nlinear_workspace
  type, BIND(C) :: gsl_multifit_nlinear_parameters
     type(c_ptr) :: trs, scale, solver
     integer(c_int) :: fdtype
     real(c_double) :: factor_up, factor_down, avmax, h_df, h_fvv
  end type
  type, public :: fgsl_multifit_nlinear_parameters
     private
     type(gsl_multifit_nlinear_parameters) :: gsl_multifit_nlinear_parameters 
  end type fgsl_multifit_nlinear_parameters
  type, public :: fgsl_multilarge_nlinear_type
     private
     type(c_ptr) :: gsl_multilarge_nlinear_type = c_null_ptr
  end type fgsl_multilarge_nlinear_type
  type, public :: fgsl_multilarge_nlinear_workspace
     type(c_ptr) :: gsl_multilarge_nlinear_workspace = c_null_ptr
  end type fgsl_multilarge_nlinear_workspace
  type, BIND(C) :: gsl_multilarge_nlinear_parameters
     type(c_ptr) :: trs, scale, solver
     integer(c_int) :: fdtype
     real(c_double) :: factor_up, factor_down, avmax, h_df, h_fvv
     integer(c_size_t) :: max_iter    
     real(c_double) :: tol
  end type
  type, public :: fgsl_multilarge_nlinear_parameters
     private
     type(gsl_multilarge_nlinear_parameters) :: gsl_multilarge_nlinear_parameters
  end type fgsl_multilarge_nlinear_parameters
  type, public :: fgsl_multifit_nlinear_fdf
     private
     type(c_ptr) :: gsl_multifit_nlinear_fdf = c_null_ptr
  end type fgsl_multifit_nlinear_fdf
  type, public :: fgsl_multilarge_nlinear_fdf
     private
     type(c_ptr) :: gsl_multilarge_nlinear_fdf = c_null_ptr
  end type fgsl_multilarge_nlinear_fdf
  abstract interface
    subroutine fgsl_nlinear_callback(iter, params, w) BIND(C)
      import :: fgsl_size_t, c_ptr, c_funptr
      integer(fgsl_size_t), value :: iter
      type(c_ptr), value :: params, w
    end subroutine
    integer(c_int) function fgsl_nlinear_fdf_func(x, params, f) bind(c)
      import :: c_ptr, c_int
      type(c_ptr), value :: x, params, f
    end function 
    integer(c_int) function fgsl_nlinear_fdf_dfunc(x, params, df) bind(c)
      import :: c_ptr, c_int
      type(c_ptr), value :: x, params, df
    end function 
    integer(c_int) function fgsl_nlinear_fdf_dlfunc(t, x, u, params, v, jtj) bind(c)
      import :: c_ptr, c_int
!     assuming int is the correct integer for enum type
      integer(c_int), value :: t
      type(c_ptr), value :: x, u, params, v, jtj
    end function 
    integer(c_int) function fgsl_nlinear_fdf_fvv(x, v, params, vv) bind(c)
      import :: c_ptr, c_int
      type(c_ptr), value :: x, v, params, vv
    end function
  end interface
!
! trust region subproblem methods
  type, private :: fgsl_multifit_nlinear_trs
    integer(c_int) :: which = 0
  end type
  type(fgsl_multifit_nlinear_trs), public, parameter :: &
          fgsl_multifit_nlinear_trs_lm = fgsl_multifit_nlinear_trs(1), &
          fgsl_multifit_nlinear_trs_lmaccel = fgsl_multifit_nlinear_trs(2), &
          fgsl_multifit_nlinear_trs_dogleg = fgsl_multifit_nlinear_trs(3), &
          fgsl_multifit_nlinear_trs_ddogleg = fgsl_multifit_nlinear_trs(4), &
          fgsl_multifit_nlinear_trs_subspace2d = fgsl_multifit_nlinear_trs(5)
  type, private :: fgsl_multilarge_nlinear_trs
    integer(c_int) :: which = 0
  end type
  type(fgsl_multilarge_nlinear_trs), public, parameter :: &
          fgsl_multilarge_nlinear_trs_lm = fgsl_multilarge_nlinear_trs(1), &
          fgsl_multilarge_nlinear_trs_lmaccel = fgsl_multilarge_nlinear_trs(2), &
          fgsl_multilarge_nlinear_trs_dogleg = fgsl_multilarge_nlinear_trs(3), &
          fgsl_multilarge_nlinear_trs_ddogleg = fgsl_multilarge_nlinear_trs(4), &
          fgsl_multilarge_nlinear_trs_subspace2d = fgsl_multilarge_nlinear_trs(5), &
          fgsl_multilarge_nlinear_trs_cgst = fgsl_multilarge_nlinear_trs(6)
! 
! scaling matrix strategies
  type, private :: fgsl_multifit_nlinear_scale
    integer(c_int) :: which = 0
  end type
  type(fgsl_multifit_nlinear_scale), public, parameter :: &
          fgsl_multifit_nlinear_scale_levenberg = fgsl_multifit_nlinear_scale(1), &
          fgsl_multifit_nlinear_scale_marquardt = fgsl_multifit_nlinear_scale(2), &
          fgsl_multifit_nlinear_scale_more = fgsl_multifit_nlinear_scale(3)
  type, private :: fgsl_multilarge_nlinear_scale
    integer(c_int) :: which = 0
  end type
  type(fgsl_multilarge_nlinear_scale), public, parameter :: &
          fgsl_multilarge_nlinear_scale_levenberg = fgsl_multilarge_nlinear_scale(1), &
          fgsl_multilarge_nlinear_scale_marquardt = fgsl_multilarge_nlinear_scale(2), &
          fgsl_multilarge_nlinear_scale_more = fgsl_multilarge_nlinear_scale(3)
!
! linear solvers
  type, private :: fgsl_multifit_nlinear_solver
    integer(c_int) :: which = 0
  end type
  type(fgsl_multifit_nlinear_solver), public, parameter :: &
          fgsl_multifit_nlinear_solver_cholesky = fgsl_multifit_nlinear_solver(1), &
          fgsl_multifit_nlinear_solver_qr = fgsl_multifit_nlinear_solver(2), &
          fgsl_multifit_nlinear_solver_svd = fgsl_multifit_nlinear_solver(3)
  integer(fgsl_int), parameter, public :: FGSL_MULTIFIT_NLINEAR_FWDIFF = 0, & 
                                  FGSL_MULTIFIT_NLINEAR_CTRDIFF = 1
  type, private :: fgsl_multilarge_nlinear_solver
    integer(c_int) :: which = 0
  end type
  type(fgsl_multilarge_nlinear_solver), public, parameter :: &
          fgsl_multilarge_nlinear_solver_cholesky = fgsl_multilarge_nlinear_solver(1)
!
! nonlinear fitting legacy interface
  type, public :: fgsl_multifit_function
     private
     type(c_ptr) :: gsl_multifit_function = c_null_ptr
  end type fgsl_multifit_function
  type, public :: fgsl_multifit_function_fdf
     private
     type(c_ptr) :: gsl_multifit_function_fdf = c_null_ptr
  end type fgsl_multifit_function_fdf
  type, public :: fgsl_multifit_fsolver
     private
     type(c_ptr) :: gsl_multifit_fsolver = c_null_ptr
  end type fgsl_multifit_fsolver
  type, public :: fgsl_multifit_fsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multifit_fsolver_type
  type, public :: fgsl_multifit_fdfsolver
     private
     type(c_ptr) :: gsl_multifit_fdfsolver = c_null_ptr
  end type fgsl_multifit_fdfsolver
  type, public :: fgsl_multifit_fdfsolver_type
     private
     integer(c_int) :: which = 0
  end type fgsl_multifit_fdfsolver_type
  type(fgsl_multifit_fdfsolver_type), public, parameter :: &
       fgsl_multifit_fdfsolver_lmder = fgsl_multifit_fdfsolver_type(1), &
       fgsl_multifit_fdfsolver_lmsder = fgsl_multifit_fdfsolver_type(2), &
       fgsl_multifit_fdfsolver_lmniel = fgsl_multifit_fdfsolver_type(3)
  type, public:: fgsl_multifit_fdfridge
    private
    type(c_ptr) :: gsl_multifit_fdfridge = c_null_ptr
  end type fgsl_multifit_fdfridge
!
! Types: B-Splines
!
  type, public :: fgsl_bspline_workspace
     private
     type(c_ptr) :: gsl_bspline_workspace = c_null_ptr
  end type fgsl_bspline_workspace
!
! Types: sparse matrices
!
  integer(fgsl_size_t), public, parameter :: fgsl_spmatrix_triplet = 0 
  integer(fgsl_size_t), public, parameter :: fgsl_spmatrix_ccs = 1     
  integer(fgsl_size_t), public, parameter :: fgsl_spmatrix_crs = 2     
  integer(fgsl_size_t), public, parameter :: fgsl_spmatrix_type_coo = fgsl_spmatrix_triplet 
  integer(fgsl_size_t), public, parameter :: fgsl_spmatrix_type_csc = fgsl_spmatrix_ccs
  integer(fgsl_size_t), public, parameter :: fgsl_spmatrix_type_csr = fgsl_spmatrix_crs
  type, public :: fgsl_spmatrix
    private
    type(c_ptr) :: gsl_spmatrix = c_null_ptr
  end type fgsl_spmatrix
!
! Types: sparse matrix linear algebra
!
type, public :: fgsl_splinalg_itersolve_type
   private
   integer(c_int) :: which = 0
end type fgsl_splinalg_itersolve_type
type(fgsl_splinalg_itersolve_type), public, parameter :: &
     fgsl_splinalg_itersolve_gmres = fgsl_splinalg_itersolve_type(1)
type, public :: fgsl_splinalg_itersolve
  private
  type(c_ptr) :: gsl_splinalg_itersolve
end type fgsl_splinalg_itersolve
!
! Types: Running Statistics
!
type, public :: fgsl_rstat_quantile_workspace
  private
  type(c_ptr) :: gsl_rstat_quantile_workspace
end type fgsl_rstat_quantile_workspace
type, public :: fgsl_rstat_workspace
  private
  type(c_ptr) :: gsl_rstat_workspace
end type fgsl_rstat_workspace
!
! Types: Moving Window Statistics
!
integer(fgsl_int), public, parameter :: &
     fgsl_movstat_end_padzero = 0, &
     fgsl_movstat_end_padvalue = 1, &
     fgsl_movstat_end_truncate = 2
type, public :: fgsl_movstat_workspace
   private
   type(c_ptr) :: gsl_movstat_workspace
end type fgsl_movstat_workspace
!> fgsl_movstat_function interoperates with gsl_movstat_function
type, public, bind(c) :: fgsl_movstat_function
   type(c_funptr) :: function
   type(c_ptr) :: params
end type fgsl_movstat_function
!> Note: gsl_movstat_accum is not matched since the publicized
!! interface does not make explicit use of accumulators

!
! Types: Digital Filtering
!
integer(fgsl_int), public, parameter :: &
     fgsl_filter_end_padzero = 0, &
     fgsl_filter_end_padvalue = 1, &
     fgsl_filter_end_truncate = 2, &
     fgsl_filter_scale_mad = 0, &
     fgsl_filter_scale_iqr = 1, &
     fgsl_filter_scale_sn = 2, &
     fgsl_filter_scale_qn = 3
     

type, public :: fgsl_filter_gaussian_workspace
   private
   type(c_ptr) :: gsl_filter_gaussian_workspace
end type fgsl_filter_gaussian_workspace
type, public :: fgsl_filter_median_workspace
   private
   type(c_ptr) :: gsl_filter_median_workspace
end type fgsl_filter_median_workspace
type, public :: fgsl_filter_rmedian_workspace
   private
   type(c_ptr) :: gsl_filter_rmedian_workspace
end type fgsl_filter_rmedian_workspace
type, public :: fgsl_filter_impulse_workspace
   private
   type(c_ptr) :: gsl_filter_impulse_workspace
end type fgsl_filter_impulse_workspace
! required C interfaces
! FGSL names occurring here are auxiliary routines
! needed to transfer static C information to the Fortran subsystem
  interface
#include "interface/array.finc"
#include "interface/interp.finc"
#include "interface/permutation.finc"
#include "interface/sort.finc"
#include "interface/linalg.finc"
#include "interface/eigen.finc"
#include "interface/fft.finc"
#include "interface/integration.finc"
#include "interface/rng.finc"
#include "interface/statistics.finc"
#include "interface/histogram.finc"
#include "interface/ntuple.finc"
#include "interface/montecarlo.finc"
#include "interface/siman.finc"
#include "interface/ode.finc"
#include "interface/deriv.finc"
#include "interface/chebyshev.finc"
#include "interface/sum_levin.finc"
#include "interface/wavelet.finc"
#include "interface/dht.finc"
#include "interface/roots.finc"
#include "interface/min.finc"
#include "interface/multiroots.finc"
#include "interface/multimin.finc"
#include "interface/fit.finc"
#include "interface/nlfit.finc"
#include "interface/multifit.finc"
#include "interface/bspline.finc"
#include "interface/ieee.finc"
#include "interface/multilarge.finc"
#include "interface/spmatrix.finc"
#include "interface/splinalg.finc"
#include "interface/rstat.finc"
#include "interface/movstat.finc"
#include "interface/filter.finc"
  end interface
#include "interface/generics.finc"
contains
#include "api/array.finc"
#include "api/interp.finc"
#include "api/permutation.finc"
#include "api/sort.finc"
#include "api/linalg.finc"
#include "api/eigen.finc"
#include "api/fft.finc"
#include "api/integration.finc"
#include "api/rng.finc"
#include "api/statistics.finc"
#include "api/histogram.finc"
#include "api/ntuple.finc"
#include "api/montecarlo.finc"
#include "api/siman.finc"
#include "api/ode.finc"
#include "api/deriv.finc"
#include "api/chebyshev.finc"
#include "api/sum_levin.finc"
#include "api/wavelet.finc"
#include "api/dht.finc"
#include "api/roots.finc"
#include "api/min.finc"
#include "api/multiroots.finc"
#include "api/multimin.finc"
#include "api/fit.finc"
#include "api/nlfit.finc"
#include "api/multifit.finc"
#include "api/bspline.finc"
#include "api/ieee.finc"
#include "api/multilarge.finc"
#include "api/spmatrix.finc"
#include "api/splinalg.finc"
#include "api/rstat.finc"
#include "api/movstat.finc"
#include "api/filter.finc"
end module fgsl
