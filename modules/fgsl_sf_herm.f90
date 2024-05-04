!-*-f90-*-
module fgsl_sf_herm
  !>  Special functions - Hermite polynomials and functions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_hermite_deriv_e, gsl_sf_hermite_prob_e, gsl_sf_hermite_prob_deriv_e, &
       gsl_sf_hermite_prob_series_e, gsl_sf_hermite_e, gsl_sf_hermite_phys_e, &
       gsl_sf_hermite_phys_series_e, gsl_sf_hermite_series_e, gsl_sf_hermite_func_e, &
       gsl_sf_hermite_func_fast_e, gsl_sf_hermite_func_series_e

  interface
     function fgsl_sf_hermite(n, x) bind(c, name='gsl_sf_hermite') 
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_hermite
     end function fgsl_sf_hermite
     function fgsl_sf_hermite_deriv(m, n, x) &
          bind(c, name='gsl_sf_hermite_deriv') 
       import
       integer(c_int), value :: m, n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_hermite_deriv
     end function fgsl_sf_hermite_deriv
     function gsl_sf_hermite_deriv_e(m, n, x, result) bind(c)
       import
       integer(c_int), value :: m, n
       real(c_double), value :: x
       type(gsl_sf_result), intent(inout) :: result
       integer(c_int) :: gsl_sf_hermite_deriv_e
     end function gsl_sf_hermite_deriv_e
     function fgsl_sf_hermite_prob(n, x) bind(c, name='gsl_sf_hermite_prob') 
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_hermite_prob
     end function fgsl_sf_hermite_prob
     function fgsl_sf_hermite_prob_deriv(m, n, x) &
          bind(c, name='gsl_sf_hermite_prob_deriv') 
       import
       integer(c_int), value :: m, n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_hermite_prob_deriv
     end function fgsl_sf_hermite_prob_deriv
     function gsl_sf_hermite_prob_e(n, x, result) bind(c)
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result), intent(inout) :: result
       integer(c_int) :: gsl_sf_hermite_prob_e
     end function gsl_sf_hermite_prob_e
     function gsl_sf_hermite_prob_deriv_e(m, n, x, result) bind(c)
       import
       integer(c_int), value :: m, n
       real(c_double), value :: x
       type(gsl_sf_result), intent(inout) :: result
       integer(c_int) :: gsl_sf_hermite_prob_deriv_e
     end function gsl_sf_hermite_prob_deriv_e
     function fgsl_sf_hermite_prob_array(nmax, x, result_array) &
          bind(c, name='gsl_sf_hermite_prob_array') 
       import
       integer(c_int), value :: nmax
       real(c_double), value :: x
       real(c_double), intent(inout), dimension(*) :: result_array
       integer(c_int) :: fgsl_sf_hermite_prob_array
     end function fgsl_sf_hermite_prob_array
     function fgsl_sf_hermite_prob_series(n, x, a) &
          bind(c, name='gsl_sf_hermite_prob_series') 
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double), dimension(*), intent(in)  :: a
       real(c_double) :: fgsl_sf_hermite_prob_series
     end function fgsl_sf_hermite_prob_series
     function gsl_sf_hermite_prob_series_e(n, x, a, result) bind(c)
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double), dimension(*), intent(in) :: a
       type(gsl_sf_result), intent(inout) :: result
       integer(c_int) :: gsl_sf_hermite_prob_series_e
     end function gsl_sf_hermite_prob_series_e
     function fgsl_sf_hermite_phys(n, x) bind(c, name='gsl_sf_hermite_phys') 
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_hermite_phys
     end function fgsl_sf_hermite_phys
     function gsl_sf_hermite_e(n, x, result) bind(c)
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result), intent(inout) :: result
       integer(c_int) :: gsl_sf_hermite_e
     end function gsl_sf_hermite_e
     function gsl_sf_hermite_phys_e(n, x, result) bind(c)
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result), intent(inout) :: result
       integer(c_int) :: gsl_sf_hermite_phys_e
     end function gsl_sf_hermite_phys_e
     function fgsl_sf_hermite_array(nmax, x, result_array) &
          bind(c, name='gsl_sf_hermite_array') 
       import
       integer(c_int), value :: nmax
       real(c_double), value :: x
       real(c_double), intent(inout), dimension(*) :: result_array
       integer(c_int) :: fgsl_sf_hermite_array
     end function fgsl_sf_hermite_array
     function fgsl_sf_hermite_array_deriv(m, nmax, x, result_array) &
          bind(c, name='gsl_sf_hermite_array_deriv') 
       import
       integer(c_int), value :: m, nmax
       real(c_double), value :: x
       real(c_double), intent(inout), dimension(*) :: result_array
       integer(c_int) :: fgsl_sf_hermite_array_deriv
     end function fgsl_sf_hermite_array_deriv
     function fgsl_sf_hermite_deriv_array(mmax, n, x, result_array) &
          bind(c, name='gsl_sf_hermite_deriv_array') 
       import
       integer(c_int), value :: mmax, n
       real(c_double), value :: x
       real(c_double), intent(inout), dimension(*) :: result_array
       integer(c_int) :: fgsl_sf_hermite_deriv_array
     end function fgsl_sf_hermite_deriv_array
     function fgsl_sf_hermite_prob_array_deriv(m, nmax, x, result_array) &
          bind(c, name='gsl_sf_hermite_prob_array_deriv') 
       import
       integer(c_int), value :: m, nmax
       real(c_double), value :: x
       real(c_double), intent(inout), dimension(*) :: result_array
       integer(c_int) :: fgsl_sf_hermite_prob_array_deriv
     end function fgsl_sf_hermite_prob_array_deriv
     function fgsl_sf_hermite_prob_deriv_array(mmax, n, x, result_array) &
          bind(c, name='gsl_sf_hermite_prob_deriv_array') 
       import
       integer(c_int), value :: mmax, n
       real(c_double), value :: x
       real(c_double), intent(inout), dimension(*) :: result_array
       integer(c_int) :: fgsl_sf_hermite_prob_deriv_array
     end function fgsl_sf_hermite_prob_deriv_array
     function fgsl_sf_hermite_zero(n, s) bind(c, name='gsl_sf_hermite_zero') 
       import
       integer(c_int), value :: n, s
       real(c_double) :: fgsl_sf_hermite_zero
     end function fgsl_sf_hermite_zero
	 function gsl_sf_hermite_zero_e(n, s, result) bind(c)
       import
       integer(c_int), value :: n, s
       type(gsl_sf_result), intent(inout) :: result
       integer(c_int) :: gsl_sf_hermite_zero_e
     end function gsl_sf_hermite_zero_e
     function fgsl_sf_hermite_prob_zero(n, s) bind(c, name='gsl_sf_hermite_prob_zero') 
       import
       integer(c_int), value :: n, s
       real(c_double) :: fgsl_sf_hermite_prob_zero
     end function fgsl_sf_hermite_prob_zero
	 function gsl_sf_hermite_prob_zero_e(n, s, result) bind(c)
       import
       integer(c_int), value :: n, s
       type(gsl_sf_result), intent(inout) :: result
       integer(c_int) :: gsl_sf_hermite_prob_zero_e
     end function gsl_sf_hermite_prob_zero_e

     function fgsl_sf_hermite_phys_array(nmax, x, result_array) &
          bind(c, name='gsl_sf_hermite_phys_array') 
       import
       integer(c_int), value :: nmax
       real(c_double), value :: x
       real(c_double), intent(inout), dimension(*) :: result_array
       integer(c_int) :: fgsl_sf_hermite_phys_array
     end function fgsl_sf_hermite_phys_array
     function fgsl_sf_hermite_series(n, x, a) &
          bind(c, name='gsl_sf_hermite_series') 
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double), dimension(*), intent(in)  :: a
       real(c_double) :: fgsl_sf_hermite_series
     end function fgsl_sf_hermite_series
     function fgsl_sf_hermite_phys_series(n, x, a) &
          bind(c, name='gsl_sf_hermite_phys_series') 
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double), dimension(*), intent(in)  :: a
       real(c_double) :: fgsl_sf_hermite_phys_series
     end function fgsl_sf_hermite_phys_series
     function gsl_sf_hermite_phys_series_e(n, x, a, result) bind(c)
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double), dimension(*), intent(in) :: a
       type(gsl_sf_result), intent(inout) :: result
       integer(c_int) :: gsl_sf_hermite_phys_series_e
     end function gsl_sf_hermite_phys_series_e
     function gsl_sf_hermite_series_e(n, x, a, result) bind(c)
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double), dimension(*), intent(in) :: a
       type(gsl_sf_result), intent(inout) :: result
       integer(c_int) :: gsl_sf_hermite_series_e
     end function gsl_sf_hermite_series_e
     function fgsl_sf_hermite_func(n, x) bind(c, name='gsl_sf_hermite_func') 
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_hermite_func
     end function fgsl_sf_hermite_func
     function gsl_sf_hermite_func_e(n, x, result) bind(c)
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result), intent(inout) :: result
       integer(c_int) :: gsl_sf_hermite_func_e
     end function gsl_sf_hermite_func_e
     function fgsl_sf_hermite_func_fast(n, x) bind(c, name='gsl_sf_hermite_func_fast') 
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_hermite_func_fast
     end function fgsl_sf_hermite_func_fast
     function gsl_sf_hermite_func_fast_e(n, x, result) bind(c)
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       type(gsl_sf_result), intent(inout) :: result
       integer(c_int) :: gsl_sf_hermite_func_fast_e
     end function gsl_sf_hermite_func_fast_e
     function fgsl_sf_hermite_func_array(nmax, x, result_array) &
          bind(c, name='gsl_sf_hermite_func_array') 
       import
       integer(c_int), value :: nmax
       real(c_double), value :: x
       real(c_double), intent(inout), dimension(*) :: result_array
       integer(c_int) :: fgsl_sf_hermite_func_array
     end function fgsl_sf_hermite_func_array
     function fgsl_sf_hermite_func_series(n, x, a) &
          bind(c, name='gsl_sf_hermite_func_series') 
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double), dimension(*), intent(in)  :: a
       real(c_double) :: fgsl_sf_hermite_func_series
     end function fgsl_sf_hermite_func_series
     function gsl_sf_hermite_func_series_e(n, x, a, result) bind(c)
       import
       integer(c_int), value :: n
       real(c_double), value :: x
       real(c_double), dimension(*), intent(in) :: a
       type(gsl_sf_result), intent(inout) :: result
       integer(c_int) :: gsl_sf_hermite_func_series_e
     end function gsl_sf_hermite_func_series_e
  end interface
contains
  function fgsl_sf_hermite_deriv_e(m, n, x, result) 
    integer(fgsl_int), intent(in) :: m, n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hermite_deriv_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hermite_deriv_e = gsl_sf_hermite_deriv_e(m, n, x, res)
    result = res
  end function fgsl_sf_hermite_deriv_e
  function fgsl_sf_hermite_prob_e(n, x, result) 
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hermite_prob_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hermite_prob_e = gsl_sf_hermite_prob_e(n, x, res)
    result = res
  end function fgsl_sf_hermite_prob_e
  function fgsl_sf_hermite_prob_deriv_e(m, n, x, result) 
    integer(fgsl_int), intent(in) :: m, n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hermite_prob_deriv_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hermite_prob_deriv_e = gsl_sf_hermite_prob_deriv_e(m, n, x, res)
    result = res
  end function fgsl_sf_hermite_prob_deriv_e
  function fgsl_sf_hermite_prob_series_e(n, x, a, result) 
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), dimension(*), intent(in) :: a
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hermite_prob_series_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hermite_prob_series_e = gsl_sf_hermite_prob_series_e(n, x, a, res)
    result = res
  end function fgsl_sf_hermite_prob_series_e
  function fgsl_sf_hermite_phys_e(n, x, result) 
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hermite_phys_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hermite_phys_e = gsl_sf_hermite_phys_e(n, x, res)
    result = res
  end function fgsl_sf_hermite_phys_e
  function fgsl_sf_hermite_e(n, x, result) 
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hermite_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hermite_e = gsl_sf_hermite_e(n, x, res)
    result = res
  end function fgsl_sf_hermite_e
  function fgsl_sf_hermite_zero_e(n, s, result) 
    integer(fgsl_int), intent(in) :: n, s
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hermite_zero_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hermite_zero_e = gsl_sf_hermite_zero_e(n, s, res)
    result = res
  end function fgsl_sf_hermite_zero_e
  function fgsl_sf_hermite_prob_zero_e(n, s, result) 
    integer(fgsl_int), intent(in) :: n, s
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hermite_prob_zero_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hermite_prob_zero_e = gsl_sf_hermite_prob_zero_e(n, s, res)
    result = res
  end function fgsl_sf_hermite_prob_zero_e
  
  function fgsl_sf_hermite_phys_series_e(n, x, a, result) 
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), dimension(*), intent(in) :: a
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hermite_phys_series_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hermite_phys_series_e = gsl_sf_hermite_phys_series_e(n, x, a, res)
    result = res
  end function fgsl_sf_hermite_phys_series_e
  function fgsl_sf_hermite_series_e(n, x, a, result) 
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), dimension(*), intent(in) :: a
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hermite_series_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hermite_series_e = gsl_sf_hermite_series_e(n, x, a, res)
    result = res
  end function fgsl_sf_hermite_series_e
  function fgsl_sf_hermite_func_e(n, x, result) 
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hermite_func_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hermite_func_e = gsl_sf_hermite_func_e(n, x, res)
    result = res
  end function fgsl_sf_hermite_func_e
  function fgsl_sf_hermite_func_fast_e(n, x, result) 
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hermite_func_fast_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hermite_func_fast_e = gsl_sf_hermite_func_fast_e(n, x, res)
    result = res
  end function fgsl_sf_hermite_func_fast_e
  function fgsl_sf_hermite_func_series_e(n, x, a, result) 
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), dimension(*), intent(in) :: a
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hermite_func_series_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hermite_func_series_e = gsl_sf_hermite_func_series_e(n, x, a, res)
    result = res
  end function fgsl_sf_hermite_func_series_e
end module fgsl_sf_herm
