module fgsl_fit
  !> Linear least-squeares fitting - linear regression
  use fgsl_base
  implicit none

  private :: gsl_fit_linear, gsl_fit_wlinear, gsl_fit_linear_est, &
    gsl_fit_mul, gsl_fit_wmul, gsl_fit_mul_est
  
  !
  !> C interfaces
  interface
	  function gsl_fit_linear(x, xstride, y, ystride, n, c0, c1, &
	       cov00, cov01, cov11, sumsq) bind(c)
	    import
	    type(c_ptr), value :: x, y
	    integer(c_size_t), value :: xstride, ystride, n
	    real(c_double) :: c0, c1, cov00, cov01, cov11, sumsq
	    integer(c_int) :: gsl_fit_linear
	  end function gsl_fit_linear
	  function gsl_fit_wlinear(x, xstride, w, wstride, y, ystride, n, c0, c1, &
	       cov00, cov01, cov11, chisq) bind(c)
	    import
	    type(c_ptr), value :: x, y, w
	    integer(c_size_t), value :: xstride, ystride, wstride, n
	    real(c_double) :: c0, c1, cov00, cov01, cov11, chisq
	    integer(c_int) :: gsl_fit_wlinear
	  end function gsl_fit_wlinear
	  function gsl_fit_linear_est(x, c0, c1, cov00, cov01, cov11, y, y_err) bind(c)
	    import
	    real(c_double), value :: x, c0, c1, cov00, cov01, cov11
	    real(c_double) ::  y, y_err
	    integer(c_int) :: gsl_fit_linear_est
	  end function gsl_fit_linear_est
	  function gsl_fit_mul(x, xstride, y, ystride, n, c1, &
	        cov11, sumsq) bind(c)
	    import
	    type(c_ptr), value :: x, y
	    integer(c_size_t), value :: xstride, ystride, n
	    real(c_double) :: c1, cov11, sumsq
	    integer(c_int) :: gsl_fit_mul
	  end function gsl_fit_mul
	  function gsl_fit_wmul(x, xstride, w, wstride, y, ystride, n, c1, &
	        cov11, chisq) bind(c)
	    import
	    type(c_ptr), value :: x, y, w
	    integer(c_size_t), value :: xstride, ystride, wstride, n
	    real(c_double) :: c1, cov11, chisq
	    integer(c_int) :: gsl_fit_wmul
	  end function gsl_fit_wmul
	  function gsl_fit_mul_est(x, c1, cov11, y, y_err) bind(c)
	    import
	    real(c_double), value :: x, c1, cov11
	    real(c_double) ::  y, y_err
	    integer(c_int) :: gsl_fit_mul_est
	  end function gsl_fit_mul_est
  end interface
contains
!
!>  API
  function fgsl_fit_linear(x, xstride, y, ystride, n, c0, c1, &
       cov00, cov01, cov11, sumsq)
    real(fgsl_double), intent(in), target, contiguous :: x(:), y(:)
    integer(fgsl_size_t), intent(in) :: xstride, ystride, n
    real(fgsl_double), intent(out) :: c0, c1, cov00, cov01, cov11, sumsq
    integer(fgsl_int) :: fgsl_fit_linear
    fgsl_fit_linear = gsl_fit_linear(c_loc(x), xstride, c_loc(y), ystride, n, c0, c1, &
       cov00, cov01, cov11, sumsq)
  end function fgsl_fit_linear
  function fgsl_fit_wlinear(x, xstride, w, wstride, y, ystride, n, c0, c1, &
       cov00, cov01, cov11, chisq)
    real(fgsl_double), intent(in), target, contiguous :: x(:), y(:), w(:)
    integer(fgsl_size_t), intent(in) :: xstride, ystride, wstride, n
    real(fgsl_double), intent(out) :: c0, c1, cov00, cov01, cov11, chisq
    integer(fgsl_int) :: fgsl_fit_wlinear
    fgsl_fit_wlinear = gsl_fit_wlinear(c_loc(x), xstride, c_loc(w), wstride, &
    c_loc(y), ystride, n, c0, c1, cov00, cov01, cov11, chisq)
  end function fgsl_fit_wlinear
  function fgsl_fit_linear_est(x, c0, c1, cov00, cov01, cov11, y, y_err)
    real(fgsl_double), intent(in) :: x, c0, c1, cov00, cov01, cov11
    real(fgsl_double), intent(out) ::  y, y_err
    integer(fgsl_int) :: fgsl_fit_linear_est
    fgsl_fit_linear_est = gsl_fit_linear_est(x, c0, c1, cov00, cov01, cov11, y, y_err)
  end function fgsl_fit_linear_est
  function fgsl_fit_mul(x, xstride, y, ystride, n, c1, &
        cov11, sumsq)
    real(fgsl_double), intent(in), target, contiguous :: x(:), y(:)
    integer(fgsl_size_t), intent(in) :: xstride, ystride, n
    real(fgsl_double), intent(out) :: c1,  cov11, sumsq
    integer(fgsl_int) :: fgsl_fit_mul
    fgsl_fit_mul = gsl_fit_mul(c_loc(x), xstride, c_loc(y), ystride, n, c1, &
        cov11, sumsq)
  end function fgsl_fit_mul
  function fgsl_fit_wmul(x, xstride, w, wstride, y, ystride, n, c1, &
        cov11, chisq)
    real(fgsl_double), intent(in), target, contiguous :: x(:), y(:), w(:)
    integer(fgsl_size_t), intent(in) :: xstride, ystride, wstride, n
    real(fgsl_double), intent(out) :: c1, cov11, chisq
    integer(fgsl_int) :: fgsl_fit_wmul
    fgsl_fit_wmul = gsl_fit_wmul(c_loc(x), xstride, c_loc(w), wstride, &
    c_loc(y), ystride, n, c1, cov11, chisq)
  end function fgsl_fit_wmul
  function fgsl_fit_mul_est(x, c1, cov11, y, y_err)
    real(fgsl_double), intent(in) :: x, c1, cov11
    real(fgsl_double), intent(out) ::  y, y_err
    integer(fgsl_int) :: fgsl_fit_mul_est
    fgsl_fit_mul_est = gsl_fit_mul_est(x, c1, cov11, y, y_err)
  end function fgsl_fit_mul_est
end module fgsl_fit
