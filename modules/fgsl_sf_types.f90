!-*-f90-*-
module fgsl_sf_types
  !> Types for special functions
  use fgsl_base
  implicit none

  !
  ! both interoperable and non-interoperable types are
  ! made available. Overloaded assignment permits direct copying
  ! when necessary
  type, public :: fgsl_sf_result
     real(fgsl_double) :: val, err
  end type fgsl_sf_result
  type, public, bind(c) :: gsl_sf_result
     real(c_double) :: val, err
  end type gsl_sf_result
  type, public :: fgsl_sf_result_e10
     real(fgsl_double) :: val, err
     integer(fgsl_int) :: e10
  end type fgsl_sf_result_e10
  type, public, bind(c) :: gsl_sf_result_e10
     real(c_double) :: val, err
     integer(c_int) :: e10
  end type gsl_sf_result_e10

  interface assignment(=)
     module procedure gsl_sf_to_fgsl_sf
     module procedure gsl_sfe10_to_fgsl_sfe10
  end interface assignment(=)

  !
  ! mode type and constants
  type, public :: fgsl_mode_t
     integer(c_int) :: gsl_mode = 0
  end type fgsl_mode_t

  type(fgsl_mode_t), parameter, public :: &
       fgsl_prec_double = fgsl_mode_t(0), &
       fgsl_prec_single = fgsl_mode_t(1), &
       fgsl_prec_approx = fgsl_mode_t(2)
contains
  elemental subroutine gsl_sf_to_fgsl_sf(result, source)
    type(fgsl_sf_result), intent(out) :: result
    type(gsl_sf_result), intent(in) :: source
    result%val = source%val
    result%err = source%err
  end subroutine gsl_sf_to_fgsl_sf
  elemental subroutine gsl_sfe10_to_fgsl_sfe10(result, source)
    type(fgsl_sf_result_e10), intent(out) :: result
    type(gsl_sf_result_e10), intent(in) :: source
    result%val = source%val
    result%err = source%err
    result%e10 = source%e10
  end subroutine gsl_sfe10_to_fgsl_sfe10
end module fgsl_sf_types
