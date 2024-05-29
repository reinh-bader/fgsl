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
  use fgsl_array
  use fgsl_permutations
  use fgsl_combinations
  use fgsl_multisets
  use fgsl_sorting
  use fgsl_linalg
  use fgsl_eigen
  use fgsl_fft
  use fgsl_integration
  use fgsl_rngen
  use fgsl_qrngen
  use fgsl_cdf
  use fgsl_statistics
  use fgsl_rstat
  use fgsl_movstat
  use fgsl_filter
  use fgsl_histograms
  use fgsl_ntuples
  use fgsl_montecarlo
  use fgsl_siman
  use fgsl_odeiv2 !> Note: legacy fgsl_odeiv not associated here
  use fgsl_odeiv  ! FIXME test case ode needs update
  use fgsl_interpolation
  use fgsl_deriv
  use fgsl_chebyshev
  use fgsl_sum_levin
  use fgsl_wavelets
  use fgsl_dhtransforms
  use fgsl_roots
  use fgsl_min
  use fgsl_multiroots
  use fgsl_multimin
  use fgsl_fit
  use fgsl_multifit
  use fgsl_multilarge
  use fgsl_multi_nlinear
  
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
!
  
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




! required C interfaces
! FGSL names occurring here are auxiliary routines
! needed to transfer static C information to the Fortran subsystem
  interface
#include "interface/bspline.finc"
#include "interface/ieee.finc"
#include "interface/spmatrix.finc"
#include "interface/splinalg.finc"
  end interface
#include "interface/generics.finc"
contains
#include "api/bspline.finc"
#include "api/ieee.finc"
#include "api/spmatrix.finc"
#include "api/splinalg.finc"
end module fgsl
