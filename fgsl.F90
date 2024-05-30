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
  
  use fgsl_ver
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
  use fgsl_bspline
  use fgsl_sparse_matrix
  use fgsl_spblas
  use fgsl_splinalg
  
  use fgsl_const_num
  use fgsl_const_mksa
  use fgsl_const_cgsm
  
  use fgsl_ieee_utils
  
  implicit none



!
! Enum: legendre special Functions
! The below probably are not needed any more
integer(fgsl_int), public, parameter :: gsl_sf_legendre_schmidt = 0
integer(fgsl_int), public, parameter :: gsl_sf_legendre_spharm = 1
integer(fgsl_int), public, parameter :: gsl_sf_legendre_full = 2
integer(fgsl_int), public, parameter :: gsl_sf_legendre_none = 3


end module fgsl
