!-*-f90-*-
module fgsl_sf_mathieu
  !>  Special functions - Mathieu Functions
  use fgsl_sf_types
  implicit none

  private :: gsl_sf_mathieu_alloc, gsl_sf_mathieu_free, &
       gsl_sf_mathieu_a_array, gsl_sf_mathieu_b_array, gsl_sf_mathieu_a_e, &
       gsl_sf_mathieu_b_e, gsl_sf_mathieu_ce_e, gsl_sf_mathieu_se_e, &
       gsl_sf_mathieu_se_array, gsl_sf_mathieu_mc_e, gsl_sf_mathieu_ms_e, &
       gsl_sf_mathieu_mc_array, gsl_sf_mathieu_ms_array

  !> workspace type 
  type, public :: fgsl_sf_mathieu_workspace
     private
     type(c_ptr) :: gsl_sf_mathieu_workspace
  end type fgsl_sf_mathieu_workspace

  !> C interfaces
  interface
     function gsl_sf_mathieu_a_array(order_min, order_max, qq, work, result_array) bind(c)
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: order_min, order_max
       real(c_double), value :: qq
       type(c_ptr), value :: work, result_array
       integer(c_int) :: gsl_sf_mathieu_a_array
     end function gsl_sf_mathieu_a_array
     function gsl_sf_mathieu_b_array(order_min, order_max, qq, work, result_array) bind(c)
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: order_min, order_max
       real(c_double), value :: qq
       type(c_ptr), value :: work, result_array
       integer(c_int) :: gsl_sf_mathieu_b_array
     end function gsl_sf_mathieu_b_array
     function gsl_sf_mathieu_a_e(order, qq, result) bind(c)
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: order
       real(c_double), value :: qq
       type(c_ptr), value :: result
       integer(c_int) :: gsl_sf_mathieu_a_e
     end function gsl_sf_mathieu_a_e
     function fgsl_sf_mathieu_a(order, qq) bind(c, name='gsl_sf_mathieu_a')
       import :: c_int, c_double
       integer(c_int), value :: order
       real(c_double), value :: qq
       real(c_double) :: fgsl_sf_mathieu_a
     end function fgsl_sf_mathieu_a
     function gsl_sf_mathieu_b_e(order, qq, result) bind(c)
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: order
       real(c_double), value :: qq
       type(c_ptr), value :: result
       integer(c_int) :: gsl_sf_mathieu_b_e
     end function gsl_sf_mathieu_b_e
     function fgsl_sf_mathieu_b(order, qq) bind(c, name='gsl_sf_mathieu_b')
       import :: c_int, c_double
       integer(c_int), value :: order
       real(c_double), value :: qq
       real(c_double) :: fgsl_sf_mathieu_b
     end function fgsl_sf_mathieu_b
     function fgsl_sf_mathieu_a_coeff(order, qq, aa, coeff) &
          bind(c, name='gsl_sf_mathieu_a_coeff')
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: order
       real(c_double), value :: qq, aa
       real(c_double), intent(inout), dimension(*) :: coeff
       integer(c_int) :: fgsl_sf_mathieu_a_coeff
     end function fgsl_sf_mathieu_a_coeff
     function fgsl_sf_mathieu_b_coeff(order, qq, aa, coeff) &
          bind(c, name='gsl_sf_mathieu_b_coeff')
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: order
       real(c_double), value :: qq, aa
       real(c_double), intent(inout), dimension(*) :: coeff
       integer(c_int) :: fgsl_sf_mathieu_b_coeff
     end function fgsl_sf_mathieu_b_coeff
     function gsl_sf_mathieu_alloc(nn, qq) bind(c)
       import :: c_size_t, c_double, c_ptr
       integer(c_size_t), value :: nn
       real(c_double), value :: qq
       type(c_ptr) :: gsl_sf_mathieu_alloc
     end function gsl_sf_mathieu_alloc
     subroutine gsl_sf_mathieu_free(workspace) bind(c)
       import :: c_ptr
       type(c_ptr), value :: workspace
     end subroutine gsl_sf_mathieu_free
     function gsl_sf_mathieu_ce_e(order, qq, zz, result) bind(c)
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: order
       real(c_double), value :: qq, zz
       type(c_ptr), value :: result
       integer(c_int) :: gsl_sf_mathieu_ce_e
     end function gsl_sf_mathieu_ce_e
     function fgsl_sf_mathieu_ce(order, qq, zz) bind(c, name='gsl_sf_mathieu_ce')
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: order
       real(c_double), value :: qq, zz
       real(c_double) :: fgsl_sf_mathieu_ce
     end function fgsl_sf_mathieu_ce
     function gsl_sf_mathieu_se_e(order, qq, zz, result) bind(c)
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: order
       real(c_double), value :: qq, zz
       type(c_ptr), value :: result
       integer(c_int) :: gsl_sf_mathieu_se_e
     end function gsl_sf_mathieu_se_e
     function fgsl_sf_mathieu_se(order, qq, zz) bind(c, name='gsl_sf_mathieu_se')
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: order
       real(c_double), value :: qq, zz
       real(c_double) :: fgsl_sf_mathieu_se
     end function fgsl_sf_mathieu_se
     function gsl_sf_mathieu_ce_array(nmin, nmax, qq, zz, work, result_array) bind(c)
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: nmin, nmax
       real(c_double), value :: qq, zz
       type(c_ptr), value :: work, result_array
       integer(c_int) :: gsl_sf_mathieu_ce_array
     end function gsl_sf_mathieu_ce_array
     function gsl_sf_mathieu_se_array(nmin, nmax, qq, zz, work, result_array) bind(c)
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: nmin, nmax
       real(c_double), value :: qq, zz
       type(c_ptr), value :: work, result_array
       integer(c_int) :: gsl_sf_mathieu_se_array
     end function gsl_sf_mathieu_se_array
     function gsl_sf_mathieu_mc_e(kind, order, qq, zz, result) &
       bind(c, name='gsl_sf_mathieu_Mc_e')
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: kind, order
       real(c_double), value :: qq, zz
       type(c_ptr), value :: result
       integer(c_int) :: gsl_sf_mathieu_mc_e
     end function gsl_sf_mathieu_mc_e
     function fgsl_sf_mathieu_mc(kind, order, qq, zz) &
       bind(c, name='gsl_sf_mathieu_Mc')
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: kind, order
       real(c_double), value :: qq, zz
       real(c_double) :: fgsl_sf_mathieu_mc
     end function fgsl_sf_mathieu_mc
     function gsl_sf_mathieu_ms_e(kind, order, qq, zz, result) &
       bind(c, name='gsl_sf_mathieu_Ms_e')
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: kind, order
       real(c_double), value :: qq, zz
       type(c_ptr), value :: result
       integer(c_int) :: gsl_sf_mathieu_ms_e
     end function gsl_sf_mathieu_ms_e
     function fgsl_sf_mathieu_ms(kind, order, qq, zz) &
       bind(c, name='gsl_sf_mathieu_Ms')
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: kind, order
       real(c_double), value :: qq, zz
       real(c_double) :: fgsl_sf_mathieu_ms
     end function fgsl_sf_mathieu_ms
     function gsl_sf_mathieu_mc_array(kind, nmin, nmax, qq, zz, work, result_array) &
       bind(c, name='gsl_sf_mathieu_Mc_array')
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: kind, nmin, nmax
       real(c_double), value :: qq, zz
       type(c_ptr), value :: work, result_array
       integer(c_int) :: gsl_sf_mathieu_mc_array
     end function gsl_sf_mathieu_mc_array
     function gsl_sf_mathieu_ms_array(kind, nmin, nmax, qq, zz, work, result_array) &
       bind(c, name='gsl_sf_mathieu_Ms_array')
       import :: c_int, c_double, c_ptr
       integer(c_int), value :: kind, nmin, nmax
       real(c_double), value :: qq, zz
       type(c_ptr), value :: work, result_array
       integer(c_int) :: gsl_sf_mathieu_ms_array
     end function gsl_sf_mathieu_ms_array
  end interface
contains
  function fgsl_sf_mathieu_a_array(order_min, order_max, qq, work, result_array)
    integer(fgsl_int), intent(in) :: order_min, order_max
    real(fgsl_double), intent(in) :: qq
    type(fgsl_sf_mathieu_workspace), intent(inout) :: work
    real(fgsl_double), dimension(:), intent(inout), contiguous, target :: &
         result_array
    integer(fgsl_int) :: fgsl_sf_mathieu_a_array
    fgsl_sf_mathieu_a_array = gsl_sf_mathieu_a_array(order_min, order_max, qq, &
    work%gsl_sf_mathieu_workspace, c_loc(result_array))
  end function fgsl_sf_mathieu_a_array
  function fgsl_sf_mathieu_b_array(order_min, order_max, qq, work, result_array)
    integer(fgsl_int), intent(in) :: order_min, order_max
    real(fgsl_double), intent(in) :: qq
    type(fgsl_sf_mathieu_workspace), intent(inout) :: work
    real(fgsl_double), dimension(:), intent(inout), contiguous, target :: &
         result_array
    integer(fgsl_int) :: fgsl_sf_mathieu_b_array
    fgsl_sf_mathieu_b_array = gsl_sf_mathieu_b_array(order_min, order_max, qq, &
    work%gsl_sf_mathieu_workspace, c_loc(result_array))
  end function fgsl_sf_mathieu_b_array
  function fgsl_sf_mathieu_a_e(order, qq, result)
    integer(c_int), intent(in) :: order
    real(c_double), intent(in) :: qq
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_mathieu_a_e
    type(gsl_sf_result), target :: res
    fgsl_sf_mathieu_a_e = gsl_sf_mathieu_a_e(order, qq, c_loc(res))
    result = res
  end function fgsl_sf_mathieu_a_e
!  fgsl_sf_mathieu_a --> interface
  function fgsl_sf_mathieu_b_e(order, qq, result)
    integer(c_int), intent(in) :: order
    real(c_double), intent(in) :: qq
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_mathieu_b_e
    type(gsl_sf_result), target :: res
    fgsl_sf_mathieu_b_e = gsl_sf_mathieu_b_e(order, qq, c_loc(res))
    result = res
  end function fgsl_sf_mathieu_b_e
!  fgsl_sf_mathieu_b --> interface
!  fgsl_sf_mathieu_a_coeff --> interface
!  fgsl_sf_mathieu_b_coeff --> interface
  function fgsl_sf_mathieu_alloc(n, qmax)
    integer(fgsl_size_t), intent(in) :: n
    real(fgsl_double), intent(in) :: qmax
    type(fgsl_sf_mathieu_workspace) :: fgsl_sf_mathieu_alloc
    fgsl_sf_mathieu_alloc%gsl_sf_mathieu_workspace = &
    gsl_sf_mathieu_alloc(n, qmax)
  end function fgsl_sf_mathieu_alloc
  subroutine fgsl_sf_mathieu_free(workspace)
    type(fgsl_sf_mathieu_workspace), intent(inout) :: workspace
    call gsl_sf_mathieu_free(workspace%gsl_sf_mathieu_workspace)
  end subroutine fgsl_sf_mathieu_free
  function fgsl_sf_mathieu_ce_e(order, qq, zz, result)
    integer(fgsl_int), intent(in) :: order
    real(fgsl_double), intent(in) :: qq, zz
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_mathieu_ce_e
    type(gsl_sf_result), target :: res
    fgsl_sf_mathieu_ce_e = gsl_sf_mathieu_ce_e(order, qq, zz, c_loc(res))
    result = res
  end function fgsl_sf_mathieu_ce_e
!  fgsl_sf_mathieu_ce --> interface
  function fgsl_sf_mathieu_se_e(order, qq, zz, result)
    integer(fgsl_int), intent(in) :: order
    real(fgsl_double), intent(in) :: qq, zz
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_mathieu_se_e
    type(gsl_sf_result), target :: res
    fgsl_sf_mathieu_se_e = gsl_sf_mathieu_se_e(order, qq, zz, c_loc(res))
    result = res
  end function fgsl_sf_mathieu_se_e
!  fgsl_sf_mathieu_se --> interface
  function fgsl_sf_mathieu_ce_array(nmin, nmax, qq, zz, work, result_array)
    integer(fgsl_int), intent(in) :: nmin, nmax
    real(fgsl_double), intent(in) :: qq, zz
    type(fgsl_sf_mathieu_workspace), intent(inout) :: work
    real(fgsl_double), dimension(:), intent(inout), contiguous, target :: &
         result_array
    integer(fgsl_int) :: fgsl_sf_mathieu_ce_array
    fgsl_sf_mathieu_ce_array = gsl_sf_mathieu_ce_array(nmin, nmax, qq, zz, &
    work%gsl_sf_mathieu_workspace, c_loc(result_array))
  end function fgsl_sf_mathieu_ce_array
  function fgsl_sf_mathieu_se_array(nmin, nmax, qq, zz, work, result_array)
    integer(fgsl_int), intent(in) :: nmin, nmax
    real(fgsl_double), intent(in) :: qq, zz
    type(fgsl_sf_mathieu_workspace), intent(inout) :: work
    real(fgsl_double), dimension(:), intent(inout), contiguous, target :: &
         result_array
    integer(fgsl_int) :: fgsl_sf_mathieu_se_array
    fgsl_sf_mathieu_se_array = gsl_sf_mathieu_se_array(nmin, nmax, qq, zz, &
    work%gsl_sf_mathieu_workspace, c_loc(result_array))
  end function fgsl_sf_mathieu_se_array
  function fgsl_sf_mathieu_mc_e(kind, order, qq, zz, result)
    integer(fgsl_int), intent(in) :: kind, order
    real(fgsl_double), intent(in) :: qq, zz
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_mathieu_mc_e
    type(gsl_sf_result), target :: res
    fgsl_sf_mathieu_mc_e = gsl_sf_mathieu_mc_e(kind, order, qq, zz, c_loc(res))
    result = res
  end function fgsl_sf_mathieu_mc_e
!  fgsl_sf_mathieu_mc --> interface
  function fgsl_sf_mathieu_ms_e(kind, order, qq, zz, result)
    integer(fgsl_int), intent(in) :: kind, order
    real(fgsl_double), intent(in) :: qq, zz
    type(fgsl_sf_result), intent(out) :: result
    integer(fgsl_int) :: fgsl_sf_mathieu_ms_e
    type(gsl_sf_result), target :: res
    fgsl_sf_mathieu_ms_e = gsl_sf_mathieu_ms_e(kind, order, qq, zz, c_loc(res))
    result = res
  end function fgsl_sf_mathieu_ms_e
!  fgsl_sf_mathieu_ms --> interface
  function fgsl_sf_mathieu_mc_array(kind, nmin, nmax, qq, zz, work, result_array)
    integer(fgsl_int), intent(in) :: kind, nmin, nmax
    real(fgsl_double), intent(in) :: qq, zz
    type(fgsl_sf_mathieu_workspace), intent(inout) :: work
    real(fgsl_double), dimension(:), intent(inout), contiguous, target :: &
         result_array
    integer(fgsl_int) :: fgsl_sf_mathieu_mc_array
    fgsl_sf_mathieu_mc_array = gsl_sf_mathieu_mc_array(kind, nmin, nmax, qq, zz, &
    work%gsl_sf_mathieu_workspace, c_loc(result_array))
  end function fgsl_sf_mathieu_mc_array
  function fgsl_sf_mathieu_ms_array(kind, nmin, nmax, qq, zz, work, result_array)
    integer(fgsl_int), intent(in) :: kind, nmin, nmax
    real(fgsl_double), intent(in) :: qq, zz
    type(fgsl_sf_mathieu_workspace), intent(inout) :: work
    real(fgsl_double), dimension(:), intent(inout), contiguous, target :: &
         result_array
    integer(fgsl_int) :: fgsl_sf_mathieu_ms_array
    fgsl_sf_mathieu_ms_array = gsl_sf_mathieu_ms_array(kind, nmin, nmax, qq, zz, &
    work%gsl_sf_mathieu_workspace, c_loc(result_array))
  end function fgsl_sf_mathieu_ms_array
end module fgsl_sf_mathieu
