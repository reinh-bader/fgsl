!-*-f90-*-
module fgsl_sf_coupling
  !>  Special functions - Coupling coefficients
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_coupling_3j_e, gsl_sf_coupling_6j_e, gsl_sf_coupling_9j_e

  !> C interfaces
  interface
     function fgsl_sf_coupling_3j(two_ja, two_jb, two_jc, two_ma, two_mb, two_mc) &
          bind(c, name='gsl_sf_coupling_3j')
       import
       integer(c_int), value :: two_ja, two_jb, two_jc, two_ma, two_mb, two_mc
       real(c_double) :: fgsl_sf_coupling_3j
     end function fgsl_sf_coupling_3j
     function gsl_sf_coupling_3j_e(two_ja, two_jb, two_jc, two_ma, two_mb, two_mc, result) &
          bind(c)
       import
       integer(c_int), value :: two_ja, two_jb, two_jc, two_ma, two_mb, two_mc
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_coupling_3j_e
     end function gsl_sf_coupling_3j_e
     function fgsl_sf_coupling_6j(two_ja, two_jb, two_jc, two_jd, two_je, two_jf) &
          bind(c, name='gsl_sf_coupling_6j')
       import
       integer(c_int), value :: two_ja, two_jb, two_jc, two_jd, two_je, two_jf
       real(c_double) :: fgsl_sf_coupling_6j
     end function fgsl_sf_coupling_6j
     function gsl_sf_coupling_6j_e(two_ja, two_jb, two_jc, two_jd, two_je, two_jf, result) &
          bind(c)
       import
       integer(c_int), value :: two_ja, two_jb, two_jc, two_jd, two_je, two_jf
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_coupling_6j_e
     end function gsl_sf_coupling_6j_e
     function fgsl_sf_coupling_9j(two_ja, two_jb, two_jc, two_jd, two_je, two_jf, &
          two_jg, two_jh, two_ji) bind(c, name='gsl_sf_coupling_9j')
       import
       integer(c_int), value :: two_ja, two_jb, two_jc, two_jd, two_je, two_jf, &
            two_jg, two_jh, two_ji
       real(c_double) :: fgsl_sf_coupling_9j
     end function fgsl_sf_coupling_9j
     function gsl_sf_coupling_9j_e(two_ja, two_jb, two_jc, two_jd, two_je, two_jf, &
          two_jg, two_jh, two_ji, result) bind(c)
       import
       integer(c_int), value :: two_ja, two_jb, two_jc, two_jd, two_je, two_jf, &
            two_jg, two_jh, two_ji
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_coupling_9j_e
     end function gsl_sf_coupling_9j_e
  end interface
contains
  !   fgsl_sf_coupling_3j --> interface
  function fgsl_sf_coupling_3j_e(two_ja, two_jb, two_jc, two_ma, two_mb, two_mc, result)
    integer(fgsl_int), intent(in) :: two_ja, two_jb, two_jc, two_ma, two_mb, two_mc
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_coupling_3j_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_coupling_3j_e = gsl_sf_coupling_3j_e(two_ja, two_jb, two_jc, two_ma, &
         two_mb, two_mc, res)
    result = res
  end function fgsl_sf_coupling_3j_e
!  fgsl_sf_coupling_6j --> interface
  function fgsl_sf_coupling_6j_e(two_ja, two_jb, two_jc, two_jd, two_je, two_jf, result)
    integer(fgsl_int), intent(in) :: two_ja, two_jb, two_jc, two_jd, two_je, two_jf
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_coupling_6j_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_coupling_6j_e = gsl_sf_coupling_6j_e(two_ja, two_jb, two_jc, two_jd, &
         two_je, two_jf, res)
    result = res
  end function fgsl_sf_coupling_6j_e
!  fgsl_sf_coupling_9j --> interface
  function fgsl_sf_coupling_9j_e(two_ja, two_jb, two_jc, two_jd, two_je, two_jf, &
          two_jg, two_jh, two_ji, result)
    integer(fgsl_int), intent(in) :: two_ja, two_jb, two_jc, two_jd, two_je, two_jf, &
          two_jg, two_jh, two_ji
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_coupling_9j_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_coupling_9j_e = gsl_sf_coupling_9j_e(two_ja, two_jb, two_jc, two_jd, &
         two_je, two_jf, two_jg, two_jh, two_ji, res)
    result = res
  end function fgsl_sf_coupling_9j_e
end module fgsl_sf_coupling
