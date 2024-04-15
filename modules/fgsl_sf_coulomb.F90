!-*-f90-*-
module fgsl_sf_coulomb
  !>  Special functions - Coulomb functions
  use fgsl_errno
  use fgsl_sf_types
  implicit none

  !> C interfaces
  interface
     function fgsl_sf_hydrogenicr_1(z, r) bind(c, name='gsl_sf_hydrogenicR_1')
       import
       real(c_double), value :: z, r
       real(c_double) :: fgsl_sf_hydrogenicr_1
     end function fgsl_sf_hydrogenicr_1
     function gsl_sf_hydrogenicr_1_e(z, r, result) bind(c, name='gsl_sf_hydrogenicR_1_e')
       import
       real(c_double), value :: z, r
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_hydrogenicr_1_e
     end function gsl_sf_hydrogenicr_1_e
     function fgsl_sf_hydrogenicr(n, l, z, r) bind(c, name='gsl_sf_hydrogenicR')
       import
       integer(c_int), value :: n, l
       real(c_double), value :: z, r
       real(c_double) :: fgsl_sf_hydrogenicr
     end function fgsl_sf_hydrogenicr
     function gsl_sf_hydrogenicr_e(n, l, z, r, result) &
          bind(c, name='gsl_sf_hydrogenicR_e')
       import
       integer(c_int), value :: n, l
       real(c_double), value :: z, r
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_hydrogenicr_e
     end function gsl_sf_hydrogenicr_e
     function gsl_sf_coulomb_wave_fg_e(eta, x, l_f, k, f, fp, g, gp, exp_f, exp_g) &
          bind(c, name='gsl_sf_coulomb_wave_FG_e')
       import
       real(c_double), value :: eta, x, l_f
       integer(c_int), value :: k
       type(gsl_sf_result) :: f, fp, g, gp
       real(c_double) :: exp_f, exp_g
       integer(c_int) :: gsl_sf_coulomb_wave_fg_e
     end function gsl_sf_coulomb_wave_fg_e
     function gsl_sf_coulomb_wave_f_array (l_min, kmax, eta, x, fc_array, &
          f_exponent) bind(c, name='gsl_sf_coulomb_wave_F_array')
       import
       real(c_double), value :: l_min, eta, x
       integer(c_int), value :: kmax
       type(c_ptr), value :: fc_array
       real(c_double) :: f_exponent
       integer(c_int) :: gsl_sf_coulomb_wave_f_array
     end function gsl_sf_coulomb_wave_f_array
     function gsl_sf_coulomb_wave_fg_array (l_min, kmax, eta, x, fc_array, &
          gc_array, f_exponent, g_exponent) bind(c, name='gsl_sf_coulomb_wave_FG_array')
       import
       real(c_double), value :: l_min, eta, x
       integer(c_int), value :: kmax
       type(c_ptr), value :: fc_array, gc_array
       real(c_double) :: f_exponent, g_exponent
       integer(c_int) :: gsl_sf_coulomb_wave_fg_array
     end function gsl_sf_coulomb_wave_fg_array
     function gsl_sf_coulomb_wave_fgp_array (l_min, kmax, eta, x, fc_array, fcp_array, &
          gc_array, gcp_array, f_exponent, g_exponent) &
          bind(c, name='gsl_sf_coulomb_wave_FGp_array')
       import
       real(c_double), value :: l_min, eta, x
       integer(c_int), value :: kmax
       type(c_ptr), value :: fc_array, gc_array, &
            fcp_array, gcp_array
       real(c_double) :: f_exponent, g_exponent
       integer(c_int) :: gsl_sf_coulomb_wave_fgp_array
     end function gsl_sf_coulomb_wave_fgp_array
     function gsl_sf_coulomb_wave_sphf_array (l_min, kmax, eta, x, fc_array, &
          f_exponent) bind(c, name='gsl_sf_coulomb_wave_sphF_array')
       import
       real(c_double), value :: l_min, eta, x
       integer(c_int), value :: kmax
       type(c_ptr), value :: fc_array
       real(c_double) :: f_exponent
       integer(c_int) :: gsl_sf_coulomb_wave_sphf_array
     end function gsl_sf_coulomb_wave_sphf_array
     function gsl_sf_coulomb_cl_e(l, eta, result) bind(c, name='gsl_sf_coulomb_CL_e')
       import
       real(c_double), value :: eta, l
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_coulomb_cl_e
     end function gsl_sf_coulomb_cl_e
     function gsl_sf_coulomb_cl_array (l_min, kmax, eta, cl) &
          bind(c, name='gsl_sf_coulomb_CL_array')
       import
       real(c_double), value :: l_min, eta
       integer(c_int), value :: kmax
       type(c_ptr), value :: cl
       integer(c_int) :: gsl_sf_coulomb_cl_array
     end function gsl_sf_coulomb_cl_array
  end interface
contains
  !> API
  !  fgsl_sf_hydrogenicr_1 --> interface
  function fgsl_sf_hydrogenicr_1_e(z, r, result)
    real(fgsl_double), intent(in) :: z, r
    integer(fgsl_int) :: fgsl_sf_hydrogenicr_1_e
    type(fgsl_sf_result), intent(out) :: result
!
    type(gsl_sf_result) :: res
    fgsl_sf_hydrogenicr_1_e = gsl_sf_hydrogenicr_1_e(z, r, res)
    result = res
  end function fgsl_sf_hydrogenicr_1_e
!  fgsl_sf_hydrogenicr --> interface
  function fgsl_sf_hydrogenicr_e(n, l, z, r, result)
    integer(fgsl_int), intent(in) :: n, l
    real(fgsl_double), intent(in) :: z, r
    integer(fgsl_int) :: fgsl_sf_hydrogenicr_e
    type(fgsl_sf_result), intent(out) :: result
!
    type(gsl_sf_result) :: res
    fgsl_sf_hydrogenicr_e = gsl_sf_hydrogenicr_e(n, l, z, r, res)
    result = res
  end function fgsl_sf_hydrogenicr_e
  function fgsl_sf_coulomb_wave_fg_e(eta, x, l_f, k, f, fp, g, gp, exp_f, exp_g)
    real(fgsl_double), intent(in) :: eta, x, l_f
    integer(fgsl_int), intent(in) :: k
    type(fgsl_sf_result), intent(out) :: f, fp, g, gp
    real(fgsl_double), intent(out) :: exp_f, exp_g
    integer(fgsl_int) :: fgsl_sf_coulomb_wave_fg_e
!
    type(gsl_sf_result) :: fl, fpl, gl, gpl
    fgsl_sf_coulomb_wave_fg_e = gsl_sf_coulomb_wave_fg_e(eta, x, l_f, k, &
         fl, fpl, gl, gpl, exp_f, exp_g)
    f = fl
    fp = fpl
    g = gl
    gp = gpl
  end function fgsl_sf_coulomb_wave_fg_e
  function fgsl_sf_coulomb_wave_f_array (l_min, eta, x, fc_array, &
          f_exponent)
    real(fgsl_double), intent(in) :: l_min, eta, x
    real(fgsl_double), intent(inout), contiguous, target :: fc_array(:)
    real(fgsl_double), intent(out) :: f_exponent
    integer(fgsl_int) :: fgsl_sf_coulomb_wave_f_array
    fgsl_sf_coulomb_wave_f_array = gsl_sf_coulomb_wave_f_array (l_min, &
    size(fc_array, kind=fgsl_int)-1, &
    eta, x, c_loc(fc_array), f_exponent)
  end function fgsl_sf_coulomb_wave_f_array
  function fgsl_sf_coulomb_wave_fg_array(l_min, eta, x, fc_array, &
          gc_array, f_exponent, g_exponent)
    real(fgsl_double), intent(in) :: l_min, eta, x
    real(fgsl_double), intent(out), contiguous, target :: &
         fc_array(:), gc_array(:)
    real(fgsl_double), intent(out) :: f_exponent, g_exponent
    integer(fgsl_int) :: fgsl_sf_coulomb_wave_fg_array
    if (size(fc_array) /= size(gc_array)) then
      call fgsl_error('fc_array and gc_array dimensions do not match', 'fgsl_specfunc', __LINE__, fgsl_ebadlen)
      fgsl_sf_coulomb_wave_fg_array = fgsl_ebadlen
      return
    endif
    fgsl_sf_coulomb_wave_fg_array = gsl_sf_coulomb_wave_fg_array (l_min, &
    size(fc_array, kind=fgsl_int)-1, &
    eta, x, c_loc(fc_array), c_loc(gc_array), f_exponent, g_exponent)
  end function fgsl_sf_coulomb_wave_fg_array
  function fgsl_sf_coulomb_wave_fgp_array(l_min, eta, x, fc_array, fcp_array, &
          gc_array, gcp_array, f_exponent, g_exponent)
    real(fgsl_double), intent(in) :: l_min, eta, x
    real(fgsl_double), intent(inout), contiguous, target :: fc_array(:), &
         gc_array(:), fcp_array(:), gcp_array(:)
    real(fgsl_double), intent(out) :: f_exponent, g_exponent
    integer(fgsl_int) :: fgsl_sf_coulomb_wave_fgp_array
    if (size(fc_array) /= size(gc_array) .or.&
        size(fc_array) /= size(fcp_array) .or.&
        size(fc_array) /= size(gcp_array)) then
      call fgsl_error('fc_array, fcp_array, gc_array and gcp_array dimensions do not match',&
       'fgsl_specfunc', __LINE__, fgsl_ebadlen)
      fgsl_sf_coulomb_wave_fgp_array = fgsl_ebadlen
      return
    endif
    fgsl_sf_coulomb_wave_fgp_array = gsl_sf_coulomb_wave_fgp_array (l_min, &
    size(fc_array, kind=fgsl_int)-1, &
    eta, x, c_loc(fc_array), c_loc(fcp_array), c_loc(gc_array), c_loc(gcp_array),&
     f_exponent, g_exponent)
  end function fgsl_sf_coulomb_wave_fgp_array
  function fgsl_sf_coulomb_wave_sphf_array(l_min, eta, x, fc_array, &
          f_exponent)
    real(fgsl_double), intent(in) :: l_min, eta, x
    real(fgsl_double), intent(inout), contiguous, target :: fc_array(:)
    real(fgsl_double), intent(out) :: f_exponent
    integer(fgsl_int) :: fgsl_sf_coulomb_wave_sphf_array
    fgsl_sf_coulomb_wave_sphf_array = gsl_sf_coulomb_wave_sphf_array (l_min, &
    size(fc_array, kind=fgsl_int)-1, &
    eta, x, c_loc(fc_array), f_exponent)
  end function fgsl_sf_coulomb_wave_sphf_array
  function fgsl_sf_coulomb_cl_e(l, eta, result)
    real(fgsl_double), intent(in) :: eta, l
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_coulomb_cl_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_coulomb_cl_e = gsl_sf_coulomb_cl_e(l, eta, res)
    result = res
  end function fgsl_sf_coulomb_cl_e
  function fgsl_sf_coulomb_cl_array(l_min, eta, cl)
    real(fgsl_double), intent(in) :: l_min, eta
    real(fgsl_double), intent(inout), contiguous, target :: cl(:)
    integer(fgsl_int) :: fgsl_sf_coulomb_cl_array
    fgsl_sf_coulomb_cl_array = gsl_sf_coulomb_cl_array (l_min, &
         size(cl, kind=fgsl_int)-1, eta, c_loc(cl))
  end function fgsl_sf_coulomb_cl_array
end module fgsl_sf_coulomb
