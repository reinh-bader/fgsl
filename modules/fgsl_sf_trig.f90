module fgsl_sf_trig
  !>  Special functions - Trigonometric Functions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_hypot_e, gsl_sf_sinc_e, gsl_sf_complex_sin_e, gsl_sf_complex_cos_e, &
       gsl_sf_complex_logsin_e, gsl_sf_lnsinh_e, gsl_sf_lncosh_e, gsl_sf_polar_to_rect, &
       gsl_sf_rect_to_polar, gsl_sf_angle_restrict_symm_e, gsl_sf_angle_restrict_pos_e, &
       gsl_sf_sin_err_e, gsl_sf_cos_err_e 

  interface
     function fgsl_sf_hypot(x, y) bind(c, name='gsl_sf_hypot')
       import
       real(c_double), value :: x, y
       real(c_double) :: fgsl_sf_hypot
     end function fgsl_sf_hypot
     function gsl_sf_hypot_e(x, y, result) bind(c)
       import
       real(c_double), value :: x, y
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_hypot_e
     end function gsl_sf_hypot_e
     function fgsl_sf_sinc(x) bind(c, name='gsl_sf_sinc')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_sinc
     end function fgsl_sf_sinc
     function gsl_sf_sinc_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_sinc_e
     end function gsl_sf_sinc_e
     function gsl_sf_complex_sin_e(zr, zi, szr, szi) bind(c)
       import
       real(c_double), value :: zr, zi
       type(gsl_sf_result) :: szr, szi
       integer(c_int) :: gsl_sf_complex_sin_e
     end function gsl_sf_complex_sin_e
     function gsl_sf_complex_cos_e(zr, zi, czr, czi) bind(c)
       import
       real(c_double), value :: zr, zi
       type(gsl_sf_result) :: czr, czi
       integer(c_int) :: gsl_sf_complex_cos_e
     end function gsl_sf_complex_cos_e
     function gsl_sf_complex_logsin_e(zr, zi, lszr, lszi) bind(c)
       import
       real(c_double), value :: zr, zi
       type(gsl_sf_result) :: lszr, lszi
       integer(c_int) :: gsl_sf_complex_logsin_e
     end function gsl_sf_complex_logsin_e
     function fgsl_sf_lnsinh(x) bind(c, name='gsl_sf_lnsinh')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_lnsinh
     end function fgsl_sf_lnsinh
     function gsl_sf_lnsinh_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_lnsinh_e
     end function gsl_sf_lnsinh_e
     function fgsl_sf_lncosh(x) bind(c, name='gsl_sf_lncosh')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_lncosh
     end function fgsl_sf_lncosh
     function gsl_sf_lncosh_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_lncosh_e
     end function gsl_sf_lncosh_e
     function gsl_sf_polar_to_rect(r, theta, x, y) bind(c)
       import
       real(c_double), value :: r, theta
       type(gsl_sf_result) :: x, y
       integer(c_int) :: gsl_sf_polar_to_rect
     end function gsl_sf_polar_to_rect
     function gsl_sf_rect_to_polar(x, y, r, theta) bind(c)
       import
       real(c_double), value :: x, y
       type(gsl_sf_result) :: r, theta
       integer(c_int) :: gsl_sf_rect_to_polar
     end function gsl_sf_rect_to_polar
     function fgsl_sf_angle_restrict_symm(theta) bind(c, &
          name='gsl_sf_angle_restrict_symm')
       import
       real(c_double), value :: theta
       real(c_double) :: fgsl_sf_angle_restrict_symm
     end function fgsl_sf_angle_restrict_symm
     function gsl_sf_angle_restrict_symm_e(theta) bind(c)
       import
       real(c_double), intent(inout) :: theta
       integer(c_int) :: gsl_sf_angle_restrict_symm_e
     end function gsl_sf_angle_restrict_symm_e
     function fgsl_sf_angle_restrict_pos(theta) bind(c, &
          name='gsl_sf_angle_restrict_pos')
       import
       real(c_double), value :: theta
       real(c_double) :: fgsl_sf_angle_restrict_pos
     end function fgsl_sf_angle_restrict_pos
     function gsl_sf_angle_restrict_pos_e(theta) bind(c)
       import
       real(c_double), intent(inout) :: theta
       integer(c_int) :: gsl_sf_angle_restrict_pos_e
     end function gsl_sf_angle_restrict_pos_e
     function gsl_sf_sin_err_e(x, dx, result) bind(c)
       import
       real(c_double), value :: x, dx
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_sin_err_e
     end function gsl_sf_sin_err_e
     function gsl_sf_cos_err_e(x, dx, result) bind(c)
       import
       real(c_double), value :: x, dx
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_cos_err_e
     end function gsl_sf_cos_err_e     
  end interface
contains
!  fgsl_sf_hypot --> interface
  function fgsl_sf_hypot_e(x, y, result)
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_hypot_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_hypot_e = gsl_sf_hypot_e(x, y, res)
    result = res
  end function fgsl_sf_hypot_e
!  fgsl_sf_sinc --> interface
  function fgsl_sf_sinc_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_sinc_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_sinc_e = gsl_sf_sinc_e(x, res)
    result = res
  end function fgsl_sf_sinc_e
  function fgsl_sf_complex_sin_e(zr, zi, szr, szi)
    real(fgsl_double), intent(in) :: zr, zi
    type(fgsl_sf_result), intent(out) :: szr, szi
    integer(fgsl_int) :: fgsl_sf_complex_sin_e
!
    type(gsl_sf_result) :: r_loc, i_loc
    fgsl_sf_complex_sin_e = gsl_sf_complex_sin_e(zr, zi, r_loc, i_loc)
    szr = r_loc
    szi = i_loc
  end function fgsl_sf_complex_sin_e
  function fgsl_sf_complex_cos_e(zr, zi, czr, czi)
    real(fgsl_double), intent(in) :: zr, zi
    type(fgsl_sf_result), intent(out) :: czr, czi
    integer(fgsl_int) :: fgsl_sf_complex_cos_e
!
    type(gsl_sf_result) :: r_loc, i_loc
    fgsl_sf_complex_cos_e = gsl_sf_complex_cos_e(zr, zi, r_loc, i_loc)
    czr = r_loc
    czi = i_loc
  end function fgsl_sf_complex_cos_e
  function fgsl_sf_complex_logsin_e(zr, zi, lszr, lszi)
    real(fgsl_double), intent(in) :: zr, zi
    type(fgsl_sf_result), intent(out) :: lszr, lszi
    integer(fgsl_int) :: fgsl_sf_complex_logsin_e
!
    type(gsl_sf_result) :: r_loc, i_loc
    fgsl_sf_complex_logsin_e = gsl_sf_complex_logsin_e(zr, zi, r_loc, i_loc)
    lszr = r_loc
    lszi = i_loc
  end function fgsl_sf_complex_logsin_e
!  fgsl_sf_lnsinh --> interface
  function fgsl_sf_lnsinh_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_lnsinh_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_lnsinh_e = gsl_sf_lnsinh_e(x, res)
    result = res
  end function fgsl_sf_lnsinh_e
!  fgsl_sf_lncosh --> interface
  function fgsl_sf_lncosh_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_lncosh_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_lncosh_e = gsl_sf_lncosh_e(x, res)
    result = res
  end function fgsl_sf_lncosh_e
  function fgsl_sf_polar_to_rect(r, theta, x, y)
    real(fgsl_double), intent(in) :: r, theta
    type(fgsl_sf_result), intent(out) :: x, y
    integer(fgsl_int) :: fgsl_sf_polar_to_rect
!
    type(gsl_sf_result) :: x_loc, y_loc
    fgsl_sf_polar_to_rect = gsl_sf_polar_to_rect(r, theta, x_loc, y_loc)
    x = x_loc
    y = y_loc
  end function fgsl_sf_polar_to_rect
  function fgsl_sf_rect_to_polar(x, y, r, theta)
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_sf_result), intent(out) :: r, theta
    integer(fgsl_int) :: fgsl_sf_rect_to_polar
!
    type(gsl_sf_result) :: r_loc, th_loc
    fgsl_sf_rect_to_polar = gsl_sf_rect_to_polar(x, y, r_loc, th_loc)
    r = r_loc
    theta = th_loc
  end function fgsl_sf_rect_to_polar
!  fgsl_sf_angle_restrict_symm --> interface
  function fgsl_sf_angle_restrict_symm_e(theta)
    real(fgsl_double), intent(inout) :: theta
    integer(fgsl_int) :: fgsl_sf_angle_restrict_symm_e
!
    fgsl_sf_angle_restrict_symm_e = gsl_sf_angle_restrict_symm_e(theta)
  end function fgsl_sf_angle_restrict_symm_e
!  fgsl_sf_angle_restrict_pos --> interface
  function fgsl_sf_angle_restrict_pos_e(theta)
    real(fgsl_double), intent(inout) :: theta
    integer(fgsl_int) :: fgsl_sf_angle_restrict_pos_e
!
    fgsl_sf_angle_restrict_pos_e = gsl_sf_angle_restrict_pos_e(theta)
  end function fgsl_sf_angle_restrict_pos_e
  function fgsl_sf_sin_err_e(x, dx, result)
    real(fgsl_double), intent(in) :: x, dx
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_sin_err_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_sin_err_e = gsl_sf_sin_err_e(x, dx, res)
    result = res
  end function fgsl_sf_sin_err_e
  function fgsl_sf_cos_err_e(x, dx, result)
    real(fgsl_double), intent(in) :: x, dx
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_cos_err_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_cos_err_e = gsl_sf_cos_err_e(x, dx, res)
    result = res
  end function fgsl_sf_cos_err_e  
end module fgsl_sf_trig
