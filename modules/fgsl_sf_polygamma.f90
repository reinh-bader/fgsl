!-*-f90-*-
module fgsl_sf_polygamma
  !>  Special functions - Psi / Polygamma Functions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_psi_int_e, gsl_sf_psi_e, gsl_sf_psi_1_int_e, &
       gsl_sf_psi_1_e, gsl_sf_psi_n_e, gsl_sf_psi_1piy_e 

  interface
     function fgsl_sf_psi_int(n) bind(c, name='gsl_sf_psi_int')
       import
       integer(c_int), value :: n
       real(c_double) :: fgsl_sf_psi_int
     end function fgsl_sf_psi_int
     function gsl_sf_psi_int_e(n, result) bind(c)
       import
       integer(c_int), value :: n
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_psi_int_e
     end function gsl_sf_psi_int_e
     function fgsl_sf_psi(x) bind(c, name='gsl_sf_psi')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_psi
     end function fgsl_sf_psi
     function gsl_sf_psi_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_psi_e
     end function gsl_sf_psi_e
     function fgsl_sf_psi_1_int(n) bind(c, name='gsl_sf_psi_1_int')
       import
       integer(c_int), value :: n
       real(c_double) :: fgsl_sf_psi_1_int
     end function fgsl_sf_psi_1_int
     function gsl_sf_psi_1_int_e(n, result) bind(c)
       import
       integer(c_int), value :: n
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_psi_1_int_e
     end function gsl_sf_psi_1_int_e
     function fgsl_sf_psi_1(x) bind(c, name='gsl_sf_psi_1')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_psi_1
     end function fgsl_sf_psi_1
     function gsl_sf_psi_1_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_psi_1_e
     end function gsl_sf_psi_1_e
     function fgsl_sf_psi_n(m, x) bind(c, name='gsl_sf_psi_n')
       import
       integer(c_int), value :: m
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_psi_n
     end function fgsl_sf_psi_n
     function gsl_sf_psi_n_e(m, x, result) bind(c)
       import
       integer(c_int), value :: m
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_psi_n_e
     end function gsl_sf_psi_n_e
     function fgsl_sf_psi_1piy(x) bind(c, name='gsl_sf_psi_1piy')
       import
       real(c_double), value :: x
       real(c_double) :: fgsl_sf_psi_1piy
     end function fgsl_sf_psi_1piy
     function gsl_sf_psi_1piy_e(x, result) bind(c)
       import
       real(c_double), value :: x
       type(gsl_sf_result) :: result
       integer(c_int) :: gsl_sf_psi_1piy_e
     end function gsl_sf_psi_1piy_e
  end interface
contains
!  fgsl_sf_psi_int --> interface
  function fgsl_sf_psi_int_e(n, result)
    integer(c_int), intent(in) :: n
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_psi_int_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_psi_int_e = gsl_sf_psi_int_e(n, res)
    result = res
  end function fgsl_sf_psi_int_e
!  fgsl_sf_psi --> interface
  function fgsl_sf_psi_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_psi_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_psi_e = gsl_sf_psi_e(x, res)
    result = res
  end function fgsl_sf_psi_e
!  fgsl_sf_psi_1_int --> interface
  function fgsl_sf_psi_1_int_e(n, result)
    integer(c_int), intent(in) :: n
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_psi_1_int_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_psi_1_int_e = gsl_sf_psi_1_int_e(n, res)
    result = res
  end function fgsl_sf_psi_1_int_e
!  fgsl_sf_psi_1 --> interface
  function fgsl_sf_psi_1_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_psi_1_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_psi_1_e = gsl_sf_psi_1_e(x, res)
    result = res
  end function fgsl_sf_psi_1_e
!  fgsl_sf_psi_n --> interface
  function fgsl_sf_psi_n_e(m, x, result)
    integer(fgsl_int), intent(in) :: m
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_psi_n_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_psi_n_e = gsl_sf_psi_n_e(m, x, res)
    result = res
  end function fgsl_sf_psi_n_e
!  fgsl_sf_psi_1piy --> interface
  function fgsl_sf_psi_1piy_e(x, result)
    real(fgsl_double), intent(in) :: x
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_psi_1piy_e
!
    type(gsl_sf_result) :: res
    fgsl_sf_psi_1piy_e = gsl_sf_psi_1piy_e(x, res)
    result = res
  end function fgsl_sf_psi_1piy_e
end module fgsl_sf_polygamma
