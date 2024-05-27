module fgsl_interpolation
  !> Interpolation
  !> Note: this module contains the bindings from gsl_interp.h and gsl_spline.h
  use fgsl_base
  use fgsl_errno

  implicit none
  
  private :: gsl_interp_eval, gsl_interp_eval_e, gsl_interp_eval_integ, &
    gsl_interp_eval_integ_e, gsl_interp_eval_deriv, gsl_interp_eval_deriv_e, &
    gsl_interp_eval_deriv2, gsl_interp_eval_deriv2_e, fgsl_aux_interp_alloc, &
    gsl_interp_alloc, gsl_interp_free, gsl_interp_init, gsl_interp_accel_alloc, &
    gsl_interp_accel_free, gsl_interp_name, gsl_interp_min_size, &
    gsl_interp_type_min_size, gsl_interp_bsearch, gsl_interp_accel_find
  private :: gsl_spline_alloc, gsl_spline_init, gsl_spline_name, &
    gsl_spline_min_size, gsl_spline_eval, gsl_spline_eval_e, gsl_spline_eval_deriv, &
    gsl_spline_eval_deriv_e, gsl_spline_eval_deriv2, gsl_spline_eval_deriv2_e, &
    gsl_spline_eval_integ, gsl_spline_eval_integ_e, gsl_spline_free
  private :: gsl_aux_sizeof_interp, fgsl_aux_interp2d_alloc, gsl_interp2d_alloc, &
    gsl_interp2d_name, gsl_interp2d_min_size, gsl_interp2d_type_min_size, &
    gsl_interp2d_init, gsl_interp2d_free, gsl_interp2d_eval, &
    gsl_interp2d_eval_extrap, gsl_interp2d_eval_e, gsl_interp2d_eval_e_extrap, &
    gsl_interp2d_eval_extrap_e, gsl_interp2d_eval_deriv_x, &
    gsl_interp2d_eval_deriv_x_e, gsl_interp2d_eval_deriv_y, &
    gsl_interp2d_eval_deriv_y_e, gsl_interp2d_eval_deriv_xx, &
    gsl_interp2d_eval_deriv_xx_e, gsl_interp2d_eval_deriv_yy, &
    gsl_interp2d_eval_deriv_yy_e, gsl_interp2d_eval_deriv_xy, &
    gsl_interp2d_eval_deriv_xy_e
  private :: gsl_spline2d_alloc, gsl_spline2d_init, gsl_spline2d_free, &
    gsl_spline2d_eval, gsl_spline2d_eval_e, gsl_spline2d_eval_extrap, &
    gsl_spline2d_eval_extrap_e, gsl_spline2d_eval_deriv_x, &
    gsl_spline2d_eval_deriv_x_e, gsl_spline2d_eval_deriv_y, &
    gsl_spline2d_eval_deriv_y_e, gsl_spline2d_eval_deriv_xx, &
    gsl_spline2d_eval_deriv_xx_e, gsl_spline2d_eval_deriv_yy, &
    gsl_spline2d_eval_deriv_yy_e, gsl_spline2d_eval_deriv_xy, &
    gsl_spline2d_eval_deriv_xy_e, gsl_spline2d_min_size, &
    gsl_spline2d_name, gsl_spline2d_set, gsl_spline2d_get

  
  !
  !> Types
  type, public :: fgsl_interp_type
     private
     integer(fgsl_int) :: which = 0
  end type fgsl_interp_type
  type(fgsl_interp_type), parameter, public :: &
       fgsl_interp_linear = fgsl_interp_type(1), &
       fgsl_interp_polynomial = fgsl_interp_type(2), &
       fgsl_interp_cspline = fgsl_interp_type(3), &
       fgsl_interp_cspline_periodic = fgsl_interp_type(4), &
       fgsl_interp_akima = fgsl_interp_type(5), &
       fgsl_interp_akima_periodic = fgsl_interp_type(6), &
       fgsl_interp_steffen = fgsl_interp_type(7)
  type, public :: fgsl_interp
     private
     type(c_ptr) :: gsl_interp = c_null_ptr
  end type fgsl_interp
  type, public :: fgsl_interp_accel
     private
     type(c_ptr) :: gsl_interp_accel = c_null_ptr
  end type fgsl_interp_accel
  type, public :: fgsl_spline
     private
     type(c_ptr) :: gsl_spline = c_null_ptr
  end type fgsl_spline
  type, public :: fgsl_spline2d
     private
     type(c_ptr) :: gsl_spline2d = c_null_ptr
  end type fgsl_spline2d
  type, public :: fgsl_interp2d_type
    private
    integer(fgsl_int) :: which = 0
  end type fgsl_interp2d_type
  type(fgsl_interp2d_type), parameter, public :: &
       fgsl_interp2d_bilinear = fgsl_interp2d_type(1), &
       fgsl_interp2d_bicubic = fgsl_interp2d_type(2)
  type, public :: fgsl_interp2d
     private
     type(c_ptr) :: gsl_interp2d = c_null_ptr
  end type fgsl_interp2d
  
  !
  !> Generics
  interface fgsl_well_defined
     module procedure fgsl_interp_status
     module procedure fgsl_interp2d_status
     module procedure fgsl_spline_status
     module procedure fgsl_spline2d_status
     module procedure fgsl_interp_accel_status
  end interface fgsl_well_defined
  interface fgsl_sizeof
     module procedure fgsl_sizeof_interp
  end interface fgsl_sizeof
  !
  !> C interfaces
  interface
     function gsl_interp_eval(interp, xa, ya, x, acc) bind(c)
       import
       type(c_ptr), value :: interp
       type(c_ptr), value :: xa, ya
       real(c_double), value :: x
       type(c_ptr), value :: acc
       real(c_double) :: gsl_interp_eval
     end function gsl_interp_eval
     function gsl_interp_eval_e(interp, xa, ya, x, acc, y) bind(c)
       import
       type(c_ptr), value :: interp
       type(c_ptr), value :: xa, ya
       real(c_double), value :: x
       real(c_double) :: y
       type(c_ptr), value :: acc
       integer(c_int) :: gsl_interp_eval_e
     end function gsl_interp_eval_e
     function gsl_interp_eval_integ(interp, xa, ya, a, b, acc) bind(c)
       import
       type(c_ptr), value :: interp
       type(c_ptr), value :: xa, ya
       real(c_double), value :: a, b
       type(c_ptr), value :: acc
       real(c_double) :: gsl_interp_eval_integ
     end function gsl_interp_eval_integ
     function gsl_interp_eval_integ_e(interp, xa, ya, a, b, acc, result) bind(c)
       import
       type(c_ptr), value :: interp
       type(c_ptr), value :: xa, ya
       real(c_double), value :: a, b
       real(c_double), intent(out) :: result
       type(c_ptr), value :: acc
       integer(c_int) :: gsl_interp_eval_integ_e
     end function gsl_interp_eval_integ_e
     function gsl_interp_eval_deriv(interp, xa, ya, x, acc) bind(c)
       import
       type(c_ptr), value :: interp
       type(c_ptr), value :: xa, ya
       real(c_double), value :: x
       type(c_ptr), value :: acc
       real(c_double) :: gsl_interp_eval_deriv
     end function gsl_interp_eval_deriv
     function gsl_interp_eval_deriv_e(interp, xa, ya, x, acc, y) bind(c)
       import
       type(c_ptr), value :: interp
       type(c_ptr), value :: xa, ya
       real(c_double), value :: x
       real(c_double), intent(out) :: y
       type(c_ptr), value :: acc
       integer(c_int) :: gsl_interp_eval_deriv_e
     end function gsl_interp_eval_deriv_e
     function gsl_interp_eval_deriv2(interp, xa, ya, x, acc) bind(c)
       import
       type(c_ptr), value :: interp
       type(c_ptr), value :: xa, ya
       real(c_double), value :: x
       type(c_ptr), value :: acc
       real(c_double) :: gsl_interp_eval_deriv2
     end function gsl_interp_eval_deriv2
     function gsl_interp_eval_deriv2_e(interp, xa, ya, x, acc, y) bind(c)
       import
       type(c_ptr), value :: interp
       type(c_ptr), value :: xa, ya
       real(c_double), value :: x
       real(c_double), intent(out) :: y
       type(c_ptr), value :: acc
       integer(c_int) :: gsl_interp_eval_deriv2_e
     end function gsl_interp_eval_deriv2_e
     function fgsl_aux_interp_alloc(int_interp) bind(c)
       import
       integer(fgsl_int), value :: int_interp
       type(c_ptr) :: fgsl_aux_interp_alloc
     end function fgsl_aux_interp_alloc
     function gsl_interp_alloc(interp_type, size) bind(c)
       import
       type(c_ptr), value :: interp_type
       integer(c_size_t), value :: size
       type(c_ptr) :: gsl_interp_alloc
     end function gsl_interp_alloc
     subroutine gsl_interp_free(interp) bind(c)
       import
       type(c_ptr), value :: interp
     end subroutine gsl_interp_free
     function gsl_interp_init(interp, xa, ya, size) bind(c)
       import
       type(c_ptr), value :: interp
       type(c_ptr), value :: xa, ya
       integer(c_size_t), value :: size
       integer(c_int) gsl_interp_init
     end function gsl_interp_init
     function gsl_interp_accel_alloc() bind(c)
       import
       type(c_ptr) :: gsl_interp_accel_alloc
     end function gsl_interp_accel_alloc
     subroutine gsl_interp_accel_free(acc) bind(c)
       import
       type(c_ptr), value :: acc
     end subroutine gsl_interp_accel_free
     function gsl_interp_name(interp) bind(c)
       import
       type(c_ptr), value :: interp
       type(c_ptr) :: gsl_interp_name
     end function gsl_interp_name
     function gsl_interp_min_size(interp) bind(c)
       import
       type(c_ptr), value :: interp
       integer(c_int) :: gsl_interp_min_size
     end function gsl_interp_min_size
     function gsl_interp_type_min_size(interp) bind(c)
       import
       type(c_ptr), value :: interp
       integer(c_int) :: gsl_interp_type_min_size
     end function gsl_interp_type_min_size
     function gsl_interp_bsearch(xa, x, index_lo, index_hi) bind(c)
       import
       type(c_ptr), value :: xa
       real(c_double), value :: x
       integer(c_size_t), value :: index_lo, index_hi
       integer(c_size_t) ::  gsl_interp_bsearch
     end function gsl_interp_bsearch
     function gsl_interp_accel_find(acc, xa, size, x) bind(c)
       import
       type(c_ptr), value :: acc
       type(c_ptr), value :: xa
       integer(c_size_t), value :: size
       real(c_double), value :: x
       integer(c_size_t) ::  gsl_interp_accel_find
     end function gsl_interp_accel_find
     function gsl_spline_alloc(interp_type, size) bind(c)
       import
       type(c_ptr), value :: interp_type
       integer(c_size_t), value :: size
       type(c_ptr) :: gsl_spline_alloc
     end function gsl_spline_alloc
     function gsl_spline_init(spline, xa, ya, size) bind(c)
       import
       type(c_ptr), value :: spline
       type(c_ptr), value :: xa, ya
       integer(c_size_t), value :: size
       integer(c_int) gsl_spline_init
     end function gsl_spline_init
     function gsl_spline_name(spline) bind(c)
       import
       type(c_ptr), value :: spline
       type(c_ptr) :: gsl_spline_name
     end function gsl_spline_name
     function gsl_spline_min_size(spline) bind(c)
       import
       type(c_ptr), value :: spline
       integer(c_int) :: gsl_spline_min_size
     end function gsl_spline_min_size
     function gsl_spline_eval(spline, x, acc) bind(c)
       import
       type(c_ptr), value :: spline
       real(c_double), value :: x
       type(c_ptr), value :: acc
       real(c_double) :: gsl_spline_eval
     end function gsl_spline_eval
     function gsl_spline_eval_e(spline, x, acc, y) bind(c)
       import
       type(c_ptr), value :: spline
       real(c_double), value :: x
       real(c_double), intent(out) :: y
       type(c_ptr), value :: acc
       integer(c_int) :: gsl_spline_eval_e
     end function gsl_spline_eval_e
     function gsl_spline_eval_deriv(spline, x, acc) bind(c)
       import
       type(c_ptr), value :: spline
       real(c_double), value :: x
       type(c_ptr), value :: acc
       real(c_double) :: gsl_spline_eval_deriv
     end function gsl_spline_eval_deriv
     function gsl_spline_eval_deriv_e(spline, x, acc, y) bind(c)
       import
       type(c_ptr), value :: spline
       real(c_double), value :: x
       real(c_double), intent(out) :: y
       type(c_ptr), value :: acc
       integer(c_int) :: gsl_spline_eval_deriv_e
     end function gsl_spline_eval_deriv_e
     function gsl_spline_eval_deriv2(spline, x, acc) bind(c)
       import
       type(c_ptr), value :: spline
       real(c_double), value :: x
       type(c_ptr), value :: acc
       real(c_double) :: gsl_spline_eval_deriv2
     end function gsl_spline_eval_deriv2
     function gsl_spline_eval_deriv2_e(spline, x, acc, y) bind(c)
       import
       type(c_ptr), value :: spline
       real(c_double), value :: x
       real(c_double), intent(out) :: y
       type(c_ptr), value :: acc
       integer(c_int) :: gsl_spline_eval_deriv2_e
     end function gsl_spline_eval_deriv2_e
     function gsl_spline_eval_integ(spline, a, b, acc) bind(c)
       import
       type(c_ptr), value :: spline
       real(c_double), value :: a, b
       type(c_ptr), value :: acc
       real(c_double) :: gsl_spline_eval_integ
     end function gsl_spline_eval_integ
     function gsl_spline_eval_integ_e(spline, a, b, acc, y) bind(c)
       import
       type(c_ptr), value :: spline
       real(c_double), value :: a, b
       real(c_double), intent(out) :: y
       type(c_ptr), value :: acc
       integer(c_int) :: gsl_spline_eval_integ_e
     end function gsl_spline_eval_integ_e
     subroutine gsl_spline_free(spline) bind(c)
       import
       type(c_ptr), value :: spline
     end subroutine gsl_spline_free
     function gsl_aux_sizeof_interp() bind(c)
       import :: c_size_t
       integer(c_size_t) :: gsl_aux_sizeof_interp
     end function gsl_aux_sizeof_interp
     function fgsl_aux_interp2d_alloc(i) bind(c)
       import :: c_ptr, c_int
       integer(c_int), value :: i
       type(c_ptr) :: fgsl_aux_interp2d_alloc
     end function fgsl_aux_interp2d_alloc
     function gsl_interp2d_alloc(T, xsize, ysize) bind(c)
       import :: c_ptr, c_size_t
       type(c_ptr), value :: T
       integer(c_size_t), value :: xsize, ysize
       type(c_ptr) :: gsl_interp2d_alloc
     end function gsl_interp2d_alloc
     function gsl_interp2d_name(interp) bind(c)
       import :: c_ptr
       type(c_ptr), value :: interp
       type(c_ptr) :: gsl_interp2d_name
     end function gsl_interp2d_name
     function gsl_interp2d_min_size(interp) bind(c)
       import :: c_ptr, c_size_t
       type(c_ptr), value :: interp
       integer(c_size_t) :: gsl_interp2d_min_size
     end function gsl_interp2d_min_size
     function gsl_interp2d_type_min_size(T) bind(c)
       import :: c_ptr, c_size_t
       type(c_ptr), value :: T
       integer(c_size_t) :: gsl_interp2d_type_min_size
     end function gsl_interp2d_type_min_size
     function gsl_interp2d_init(interp, xa, ya, za, xsize, ysize) bind(c)
       import :: c_int, c_ptr, c_size_t, c_double
       type(c_ptr), value :: interp
       type(c_ptr), value :: xa, ya, za
       integer(c_size_t), value :: xsize, ysize
       integer(c_int) :: gsl_interp2d_init
     end function gsl_interp2d_init
     subroutine gsl_interp2d_free(interp) bind(c)
       import :: c_ptr
       type(c_ptr), value :: interp
     end subroutine gsl_interp2d_free
     function gsl_interp2d_eval(interp, xarr, yarr, zarr, x, y, xa, ya) bind(c)
       import :: c_ptr, c_double
       type(c_ptr), value :: interp
       type(c_ptr), value :: xarr, yarr, zarr
       real(c_double), value :: x, y
       type(c_ptr), value :: xa, ya
       real(c_double) :: gsl_interp2d_eval
     end function gsl_interp2d_eval
     function gsl_interp2d_eval_extrap(interp, xarr, yarr, zarr, x, y, xa, ya) bind(c)
       import :: c_ptr, c_double
       type(c_ptr), value :: interp
       type(c_ptr), value :: xarr, yarr, zarr
       real(c_double), value :: x, y
       type(c_ptr), value :: xa, ya
       real(c_double) :: gsl_interp2d_eval_extrap
     end function gsl_interp2d_eval_extrap
     function gsl_interp2d_eval_e(interp, xarr, yarr, zarr, x, y, xa, ya, z) bind(c)
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: interp
       type(c_ptr), value :: xarr, yarr, zarr
       real(c_double), value :: x, y
       type(c_ptr), value :: xa, ya
       real(c_double) :: z
       integer(c_int) :: gsl_interp2d_eval_e
     end function gsl_interp2d_eval_e
     function gsl_interp2d_eval_e_extrap(interp, xarr, yarr, zarr, x, y, xa, ya, z) bind(c)
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: interp
       type(c_ptr), value :: xarr, yarr, zarr
       real(c_double), value :: x, y
       type(c_ptr), value :: xa, ya
       real(c_double) :: z
       integer(c_int) :: gsl_interp2d_eval_e_extrap
     end function gsl_interp2d_eval_e_extrap
     function gsl_interp2d_eval_extrap_e(interp, xarr, yarr, zarr, x, y, xa, ya, z) bind(c)
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: interp
       type(c_ptr), value :: xarr, yarr, zarr
       real(c_double), value :: x, y
       type(c_ptr), value :: xa, ya
       real(c_double) :: z
       integer(c_int) :: gsl_interp2d_eval_extrap_e
     end function gsl_interp2d_eval_extrap_e
     function gsl_interp2d_eval_deriv_x(interp, xarr, yarr, zarr, x, y, xa, ya) bind(c)
       import :: c_ptr, c_double
       type(c_ptr), value :: interp
       type(c_ptr), value :: xarr, yarr, zarr
       real(c_double), value :: x, y
       type(c_ptr), value :: xa, ya
       real(c_double) :: gsl_interp2d_eval_deriv_x
     end function gsl_interp2d_eval_deriv_x
     function gsl_interp2d_eval_deriv_x_e(interp, xarr, yarr, zarr, x, y, xa, ya, z) bind(c)
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: interp
       type(c_ptr), value :: xarr, yarr, zarr
       real(c_double), value :: x, y
       type(c_ptr), value :: xa, ya
       real(c_double) :: z
       integer(c_int) :: gsl_interp2d_eval_deriv_x_e
     end function gsl_interp2d_eval_deriv_x_e
     function gsl_interp2d_eval_deriv_y(interp, xarr, yarr, zarr, x, y, xa, ya) bind(c)
       import :: c_ptr, c_double
       type(c_ptr), value :: interp
       type(c_ptr), value :: xarr, yarr, zarr
       real(c_double), value :: x, y
       type(c_ptr), value :: xa, ya
       real(c_double) :: gsl_interp2d_eval_deriv_y
     end function gsl_interp2d_eval_deriv_y
     function gsl_interp2d_eval_deriv_y_e(interp, xarr, yarr, zarr, x, y, xa, ya, z) bind(c)
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: interp
       type(c_ptr), value :: xarr, yarr, zarr
       real(c_double), value :: x, y
       type(c_ptr), value :: xa, ya
       real(c_double) :: z
       integer(c_int) :: gsl_interp2d_eval_deriv_y_e
     end function gsl_interp2d_eval_deriv_y_e
     function gsl_interp2d_eval_deriv_xx(interp, xarr, yarr, zarr, x, y, xa, ya) bind(c)
       import :: c_ptr, c_double
       type(c_ptr), value :: interp
       type(c_ptr), value :: xarr, yarr, zarr
       real(c_double), value :: x, y
       type(c_ptr), value :: xa, ya
       real(c_double) :: gsl_interp2d_eval_deriv_xx
     end function gsl_interp2d_eval_deriv_xx
     function gsl_interp2d_eval_deriv_xx_e(interp, xarr, yarr, zarr, x, y, xa, ya, z) bind(c)
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: interp
       type(c_ptr), value :: xarr, yarr, zarr
       real(c_double), value :: x, y
       type(c_ptr), value :: xa, ya
       real(c_double) :: z
       integer(c_int) :: gsl_interp2d_eval_deriv_xx_e
     end function gsl_interp2d_eval_deriv_xx_e
     function gsl_interp2d_eval_deriv_yy(interp, xarr, yarr, zarr, x, y, xa, ya) bind(c)
       import :: c_ptr, c_double
       type(c_ptr), value :: interp
       type(c_ptr), value :: xarr, yarr, zarr
       real(c_double), value :: x, y
       type(c_ptr), value :: xa, ya
       real(c_double) :: gsl_interp2d_eval_deriv_yy
     end function gsl_interp2d_eval_deriv_yy
     function gsl_interp2d_eval_deriv_yy_e(interp, xarr, yarr, zarr, x, y, xa, ya, z) bind(c)
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: interp
       type(c_ptr), value :: xarr, yarr, zarr
       real(c_double), value :: x, y
       type(c_ptr), value :: xa, ya
       real(c_double) :: z
       integer(c_int) :: gsl_interp2d_eval_deriv_yy_e
     end function gsl_interp2d_eval_deriv_yy_e
     function gsl_interp2d_eval_deriv_xy(interp, xarr, yarr, zarr, x, y, xa, ya) bind(c)
       import :: c_ptr, c_double
       type(c_ptr), value :: interp
       type(c_ptr), value :: xarr, yarr, zarr
       real(c_double), value :: x, y
       type(c_ptr), value :: xa, ya
       real(c_double) :: gsl_interp2d_eval_deriv_xy
     end function gsl_interp2d_eval_deriv_xy
     function gsl_interp2d_eval_deriv_xy_e(interp, xarr, yarr, zarr, x, y, xa, ya, z) bind(c)
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: interp
       type(c_ptr), value :: xarr, yarr, zarr
       real(c_double), value :: x, y
       type(c_ptr), value :: xa, ya
       real(c_double) :: z
       integer(c_int) :: gsl_interp2d_eval_deriv_xy_e
     end function gsl_interp2d_eval_deriv_xy_e
     function gsl_spline2d_alloc(T, xsize, ysize) bind(c)
       import :: c_ptr, c_size_t
       type(c_ptr), value :: T
       integer(c_size_t), value :: xsize, ysize
       type(c_ptr) :: gsl_spline2d_alloc
     end function gsl_spline2d_alloc
     function gsl_spline2d_init(interp, xa, ya, za, xsize, ysize) bind(c)
       import :: c_ptr, c_double, c_size_t, c_int
       type(c_ptr), value :: interp
       type(c_ptr), value :: xa, ya, za
       integer(c_size_t), value :: xsize, ysize
       integer(c_int) :: gsl_spline2d_init
     end function gsl_spline2d_init
     subroutine gsl_spline2d_free(interp) bind(c)
       import :: c_ptr
       type(c_ptr), value :: interp
     end subroutine gsl_spline2d_free
     function gsl_spline2d_eval(interp, x, y, xa, ya) bind(c)
       import :: c_ptr, c_double
       type(c_ptr), value :: interp, xa, ya
       real(c_double), value :: x, y
       real(c_double) :: gsl_spline2d_eval
     end function gsl_spline2d_eval
     function gsl_spline2d_eval_e(interp, x, y, xa, ya, z) bind(c)
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: interp, xa, ya
       real(c_double), value :: x, y
       real(c_double) :: z
       integer(c_int) :: gsl_spline2d_eval_e
     end function gsl_spline2d_eval_e
     function gsl_spline2d_eval_extrap(interp, x, y, xa, ya) bind(c)
       import :: c_ptr, c_double
       type(c_ptr), value :: interp, xa, ya
       real(c_double), value :: x, y
       real(c_double) :: gsl_spline2d_eval_extrap
     end function gsl_spline2d_eval_extrap
     function gsl_spline2d_eval_extrap_e(interp, x, y, xa, ya, z) bind(c)
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: interp, xa, ya
       real(c_double), value :: x, y
       real(c_double) :: z
       integer(c_int) :: gsl_spline2d_eval_extrap_e
     end function gsl_spline2d_eval_extrap_e
     function gsl_spline2d_eval_deriv_x(interp, x, y, xa, ya) bind(c)
       import :: c_ptr, c_double
       type(c_ptr), value :: interp, xa, ya
       real(c_double), value :: x, y
       real(c_double) :: gsl_spline2d_eval_deriv_x
     end function gsl_spline2d_eval_deriv_x
     function gsl_spline2d_eval_deriv_x_e(interp, x, y, xa, ya, z) bind(c)
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: interp, xa, ya
       real(c_double), value :: x, y
       real(c_double) :: z
       integer(c_int) :: gsl_spline2d_eval_deriv_x_e
     end function gsl_spline2d_eval_deriv_x_e
     function gsl_spline2d_eval_deriv_y(interp, x, y, xa, ya) bind(c)
       import :: c_ptr, c_double
       type(c_ptr), value :: interp, xa, ya
       real(c_double), value :: x, y
       real(c_double) :: gsl_spline2d_eval_deriv_y
     end function gsl_spline2d_eval_deriv_y
     function gsl_spline2d_eval_deriv_y_e(interp, x, y, xa, ya, z) bind(c)
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: interp, xa, ya
       real(c_double), value :: x, y
       real(c_double) :: z
       integer(c_int) :: gsl_spline2d_eval_deriv_y_e
     end function gsl_spline2d_eval_deriv_y_e
     function gsl_spline2d_eval_deriv_xx(interp, x, y, xa, ya) bind(c)
       import :: c_ptr, c_double
       type(c_ptr), value :: interp, xa, ya
       real(c_double), value :: x, y
       real(c_double) :: gsl_spline2d_eval_deriv_xx
     end function gsl_spline2d_eval_deriv_xx
     function gsl_spline2d_eval_deriv_xx_e(interp, x, y, xa, ya, z) bind(c)
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: interp, xa, ya
       real(c_double), value :: x, y
       real(c_double) :: z
       integer(c_int) :: gsl_spline2d_eval_deriv_xx_e
     end function gsl_spline2d_eval_deriv_xx_e
     function gsl_spline2d_eval_deriv_yy(interp, x, y, xa, ya) bind(c)
       import :: c_ptr, c_double
       type(c_ptr), value :: interp, xa, ya
       real(c_double), value :: x, y
       real(c_double) :: gsl_spline2d_eval_deriv_yy
     end function gsl_spline2d_eval_deriv_yy
     function gsl_spline2d_eval_deriv_yy_e(interp, x, y, xa, ya, z) bind(c)
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: interp, xa, ya
       real(c_double), value :: x, y
       real(c_double) :: z
       integer(c_int) :: gsl_spline2d_eval_deriv_yy_e
     end function gsl_spline2d_eval_deriv_yy_e
     function gsl_spline2d_eval_deriv_xy(interp, x, y, xa, ya) bind(c)
       import :: c_ptr, c_double
       type(c_ptr), value :: interp, xa, ya
       real(c_double), value :: x, y
       real(c_double) :: gsl_spline2d_eval_deriv_xy
     end function gsl_spline2d_eval_deriv_xy
     function gsl_spline2d_eval_deriv_xy_e(interp, x, y, xa, ya, z) bind(c)
       import :: c_ptr, c_double, c_int
       type(c_ptr), value :: interp, xa, ya
       real(c_double), value :: x, y
       real(c_double) :: z
       integer(c_int) :: gsl_spline2d_eval_deriv_xy_e
     end function gsl_spline2d_eval_deriv_xy_e
     function gsl_spline2d_min_size(interp) bind(c)
       import :: c_ptr, c_size_t
       type(c_ptr), value :: interp
       integer(c_size_t) :: gsl_spline2d_min_size
     end function gsl_spline2d_min_size
     function gsl_spline2d_name(interp) bind(c)
       import :: c_ptr
       type(c_ptr), value :: interp
       type(c_ptr) :: gsl_spline2d_name
     end function gsl_spline2d_name
     function gsl_spline2d_set(interp, zarr, i, j, z) bind(c)
       import :: c_ptr, c_int, c_size_t, c_double
       type(c_ptr), value :: interp
       real(c_double) :: zarr(*)
       integer(c_size_t), value :: i, j
       real(c_double), value :: z
       integer(c_int) :: gsl_spline2d_set
     end function gsl_spline2d_set
     function gsl_spline2d_get(interp, zarr, i, j) bind(c)
       import :: c_ptr, c_double, c_size_t
       type(c_ptr), value :: interp
       real(c_double) :: zarr(*)
       integer(c_size_t), value :: i, j
       real(c_double) :: gsl_spline2d_get
     end function gsl_spline2d_get
  end interface  
  
contains
!> API
  function fgsl_interp_alloc(interp_type, size)
    type(fgsl_interp_type), intent(in) :: interp_type
    integer(fgsl_size_t), intent(in) :: size
    type(fgsl_interp) :: fgsl_interp_alloc
!
    type(c_ptr) :: it
    integer(c_int) :: i
    integer(c_size_t) :: sz
!
    i = interp_type%which
    sz = size
    it = fgsl_aux_interp_alloc(i)
!       write(*, *) 'DEBUG: alloc size is ',sz
       fgsl_interp_alloc%gsl_interp = gsl_interp_alloc(it, sz)
  end function fgsl_interp_alloc
  subroutine fgsl_interp_free(interp)
    type(fgsl_interp), intent(inout) :: interp
    call gsl_interp_free(interp%gsl_interp)
  end subroutine fgsl_interp_free
  function fgsl_interp_accel_alloc()
    type(fgsl_interp_accel) :: fgsl_interp_accel_alloc
    fgsl_interp_accel_alloc%gsl_interp_accel = gsl_interp_accel_alloc()
  end function fgsl_interp_accel_alloc
  subroutine fgsl_interp_accel_free(acc)
    type(fgsl_interp_accel), intent(inout) :: acc
    call gsl_interp_accel_free(acc%gsl_interp_accel)
  end subroutine fgsl_interp_accel_free
  function fgsl_interp_status(interp)
    type(fgsl_interp), intent(in) :: interp
    logical :: fgsl_interp_status
    fgsl_interp_status = .true.
    if (.not. c_associated(interp%gsl_interp)) fgsl_interp_status = .false.
  end function fgsl_interp_status
  function fgsl_interp2d_status(interp)
    type(fgsl_interp2d), intent(in) :: interp
    logical :: fgsl_interp2d_status
    fgsl_interp2d_status = .true.
    if (.not. c_associated(interp%gsl_interp2d)) fgsl_interp2d_status = .false.
  end function fgsl_interp2d_status
  function fgsl_interp_accel_status(acc)
    type(fgsl_interp_accel), intent(in) :: acc
    logical :: fgsl_interp_accel_status
    fgsl_interp_accel_status = .true.
    if (.not. c_associated(acc%gsl_interp_accel)) fgsl_interp_accel_status = .false.
  end function fgsl_interp_accel_status
  function fgsl_interp_init(interp, xa, ya)
    type(fgsl_interp), intent(inout) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xa, ya
    integer(fgsl_int) :: fgsl_interp_init
    !check that the array dimensions match
    if (size(xa) /= size(ya)) then
      call fgsl_error('xa and ya dimensions do not match', 'fgsl_interp', __LINE__, fgsl_ebadlen)
      fgsl_interp_init = fgsl_ebadlen
      return
    endif
    fgsl_interp_init = gsl_interp_init(interp%gsl_interp, c_loc(xa), c_loc(ya), size(xa, dim=1, kind=fgsl_size_t))
  end function fgsl_interp_init
  function fgsl_interp_eval(interp, xa, ya, x, acc)
    type(fgsl_interp), intent(in) :: interp
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xa, ya
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: fgsl_interp_eval
    fgsl_interp_eval = gsl_interp_eval(interp%gsl_interp, c_loc(xa), c_loc(ya), x, &
         acc%gsl_interp_accel)
  end function fgsl_interp_eval
  function fgsl_interp_eval_e(interp, xa, ya, x, acc, y)
    type(fgsl_interp), intent(in) :: interp
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xa, ya
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(out) :: y
    integer(fgsl_int) :: fgsl_interp_eval_e
    fgsl_interp_eval_e = gsl_interp_eval_e(interp%gsl_interp, c_loc(xa), c_loc(ya), x, &
         acc%gsl_interp_accel, y)
  end function fgsl_interp_eval_e
  function fgsl_interp_eval_integ(interp, xa, ya, a, b, acc)
    type(fgsl_interp), intent(in) :: interp
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xa, ya
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_interp_eval_integ
    fgsl_interp_eval_integ = gsl_interp_eval_integ(interp%gsl_interp, c_loc(xa), c_loc(ya), a, b, &
         acc%gsl_interp_accel)
  end function fgsl_interp_eval_integ
  function fgsl_interp_eval_integ_e(interp, xa, ya, a, b, acc, result)
    type(fgsl_interp), intent(in) :: interp
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xa, ya
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double), intent(out) :: result
    integer(fgsl_int) :: fgsl_interp_eval_integ_e
    fgsl_interp_eval_integ_e = gsl_interp_eval_integ_e(interp%gsl_interp, c_loc(xa), c_loc(ya), a, b, &
         acc%gsl_interp_accel, result)
  end function fgsl_interp_eval_integ_e
  function fgsl_interp_eval_deriv(interp, xa, ya, x, acc)
    type(fgsl_interp), intent(in) :: interp
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xa, ya
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: fgsl_interp_eval_deriv
    fgsl_interp_eval_deriv = gsl_interp_eval_deriv(interp%gsl_interp, c_loc(xa), c_loc(ya), x, &
         acc%gsl_interp_accel)
  end function fgsl_interp_eval_deriv
  function fgsl_interp_eval_deriv_e(interp, xa, ya, x, acc, d)
    type(fgsl_interp), intent(in) :: interp
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xa, ya
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(out) :: d
    integer(fgsl_int) :: fgsl_interp_eval_deriv_e
    fgsl_interp_eval_deriv_e = gsl_interp_eval_deriv_e(interp%gsl_interp, c_loc(xa), c_loc(ya), x, &
         acc%gsl_interp_accel, d)
  end function fgsl_interp_eval_deriv_e
  function fgsl_interp_eval_deriv2(interp, xa, ya, x, acc)
    type(fgsl_interp), intent(in) :: interp
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xa, ya
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: fgsl_interp_eval_deriv2
    fgsl_interp_eval_deriv2 = gsl_interp_eval_deriv2(interp%gsl_interp, c_loc(xa), c_loc(ya), x, &
         acc%gsl_interp_accel)
  end function fgsl_interp_eval_deriv2
  function fgsl_interp_eval_deriv2_e(interp, xa, ya, x, acc, d2)
    type(fgsl_interp), intent(in) :: interp
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xa, ya
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(out) :: d2
    integer(fgsl_int) :: fgsl_interp_eval_deriv2_e
    fgsl_interp_eval_deriv2_e = gsl_interp_eval_deriv2_e(interp%gsl_interp, c_loc(xa), c_loc(ya), x, &
         acc%gsl_interp_accel, d2)
  end function fgsl_interp_eval_deriv2_e
  function fgsl_interp_name(interp)
    type(fgsl_interp), intent(in) :: interp
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_interp_name
!
    type(c_ptr) :: name
!
    name = gsl_interp_name(interp%gsl_interp)
    fgsl_interp_name = fgsl_name(name)
  end function fgsl_interp_name
  function fgsl_interp_min_size(interp)
    type(fgsl_interp), intent(in) :: interp
    integer(fgsl_long) :: fgsl_interp_min_size
    fgsl_interp_min_size = gsl_interp_min_size(interp%gsl_interp)
  end function fgsl_interp_min_size
  function fgsl_interp_type_min_size(interp)
    type(fgsl_interp_type), intent(in) :: interp
    integer(fgsl_long) :: fgsl_interp_type_min_size
    type(c_ptr) :: it
    it = fgsl_aux_interp_alloc(interp%which)
    fgsl_interp_type_min_size = gsl_interp_type_min_size(it)
  end function fgsl_interp_type_min_size
  function fgsl_interp_bsearch(xa, x, index_lo, index_hi)
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xa
    real(fgsl_double), intent(in) :: x
    integer(fgsl_size_t), intent(in) :: index_lo, index_hi
    integer(fgsl_size_t) ::  fgsl_interp_bsearch
    fgsl_interp_bsearch = gsl_interp_bsearch(c_loc(xa), x, index_lo, index_hi) &
    + lbound(xa, dim=1, kind=fgsl_size_t)
  end function fgsl_interp_bsearch
  function fgsl_interp_accel_find(acc, xa, x)
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xa
    real(fgsl_double), intent(in) :: x
    integer(fgsl_size_t) ::  fgsl_interp_accel_find
    fgsl_interp_accel_find = gsl_interp_accel_find(acc%gsl_interp_accel, c_loc(xa), &
    size(xa, kind=fgsl_size_t), x) + lbound(xa, dim=1, kind=fgsl_size_t)
  end function fgsl_interp_accel_find
  function fgsl_spline_alloc(interp_type, size)
    type(fgsl_interp_type), intent(in) :: interp_type
    integer(fgsl_size_t), intent(in) :: size
    type(fgsl_spline) :: fgsl_spline_alloc
!
    type(c_ptr) :: it
    integer(c_int) :: i
    integer(fgsl_size_t) :: sz
!
    i = interp_type%which
    sz = size
    it = fgsl_aux_interp_alloc(i)
    fgsl_spline_alloc%gsl_spline = gsl_spline_alloc(it, sz)
  end function fgsl_spline_alloc
  subroutine fgsl_spline_free(spline)
    type(fgsl_spline), intent(inout) :: spline
!
    call gsl_spline_free(spline%gsl_spline)
  end subroutine fgsl_spline_free
  function fgsl_spline_init(spline, xa, ya)
    type(fgsl_spline), intent(inout) :: spline
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xa, ya
    integer(fgsl_int) :: fgsl_spline_init
    !check if dimenions match
    if (size(xa) /= size(ya)) then
      call fgsl_error('xa and ya dimensions do not match', 'fgsl_interp', __LINE__, fgsl_ebadlen)
      fgsl_spline_init = fgsl_ebadlen
      return
    endif
    fgsl_spline_init = gsl_spline_init(spline%gsl_spline, c_loc(xa), c_loc(ya), size(xa, kind=fgsl_size_t))
  end function fgsl_spline_init
  function fgsl_spline_name(spline)
    type(fgsl_spline), intent(in) :: spline
    character(len=fgsl_strmax) :: fgsl_spline_name
    type(c_ptr) :: name
    name = gsl_spline_name(spline%gsl_spline)
    fgsl_spline_name = fgsl_name(name)
  end function fgsl_spline_name
  function fgsl_spline_min_size(spline)
    type(fgsl_spline), intent(in) :: spline
    integer(fgsl_long) :: fgsl_spline_min_size
    fgsl_spline_min_size = gsl_spline_min_size(spline%gsl_spline)
  end function fgsl_spline_min_size
  function fgsl_spline_eval(spline, x, acc)
    type(fgsl_spline), intent(in) :: spline
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), intent(in) ::  x
    real(fgsl_double) :: fgsl_spline_eval
    fgsl_spline_eval = gsl_spline_eval(spline%gsl_spline, x, &
         acc%gsl_interp_accel)
  end function fgsl_spline_eval
  function fgsl_spline_eval_e(spline, x, acc, y)
    type(fgsl_spline), intent(in) :: spline
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), intent(in) ::  x
    real(fgsl_double), intent(out) :: y
    integer(fgsl_int) :: fgsl_spline_eval_e
    fgsl_spline_eval_e = gsl_spline_eval_e(spline%gsl_spline, x, &
         acc%gsl_interp_accel, y)
  end function fgsl_spline_eval_e
  function fgsl_spline_eval_deriv(spline, x, acc)
    type(fgsl_spline), intent(in) :: spline
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), intent(in) ::  x
    real(fgsl_double) :: fgsl_spline_eval_deriv
    fgsl_spline_eval_deriv = gsl_spline_eval_deriv(spline%gsl_spline, x, &
         acc%gsl_interp_accel)
  end function fgsl_spline_eval_deriv
  function fgsl_spline_eval_deriv_e(spline, x, acc, y)
    type(fgsl_spline), intent(in) :: spline
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), intent(in) ::  x
    real(fgsl_double), intent(out) :: y
    integer(fgsl_int) :: fgsl_spline_eval_deriv_e
    fgsl_spline_eval_deriv_e = gsl_spline_eval_deriv_e(spline%gsl_spline, x, &
         acc%gsl_interp_accel, y)
  end function fgsl_spline_eval_deriv_e
  function fgsl_spline_eval_deriv2(spline, x, acc)
    type(fgsl_spline), intent(in) :: spline
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), intent(in) ::  x
    real(fgsl_double) :: fgsl_spline_eval_deriv2
    fgsl_spline_eval_deriv2 = gsl_spline_eval_deriv2(spline%gsl_spline, x, &
         acc%gsl_interp_accel)
  end function fgsl_spline_eval_deriv2
  function fgsl_spline_eval_deriv2_e(spline, x, acc, y)
    type(fgsl_spline), intent(in) :: spline
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), intent(in) ::  x
    real(fgsl_double), intent(out) :: y
    integer(fgsl_int) :: fgsl_spline_eval_deriv2_e
    fgsl_spline_eval_deriv2_e = gsl_spline_eval_deriv2_e(spline%gsl_spline, x, &
         acc%gsl_interp_accel, y)
  end function fgsl_spline_eval_deriv2_e
  function fgsl_spline_eval_integ(spline, a, b, acc)
    type(fgsl_spline), intent(in) :: spline
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_spline_eval_integ
    fgsl_spline_eval_integ = gsl_spline_eval_integ(spline%gsl_spline, a, b, &
         acc%gsl_interp_accel)
  end function fgsl_spline_eval_integ
  function fgsl_spline_eval_integ_e(spline, a, b, acc, y)
    type(fgsl_spline), intent(in) :: spline
    type(fgsl_interp_accel), intent(inout) :: acc
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double), intent(out) :: y
    integer(fgsl_int) :: fgsl_spline_eval_integ_e
    fgsl_spline_eval_integ_e = gsl_spline_eval_integ_e(spline%gsl_spline, a, b, &
         acc%gsl_interp_accel, y)
  end function fgsl_spline_eval_integ_e
  function fgsl_spline_status(spline)
    type(fgsl_spline), intent(in) :: spline
    logical :: fgsl_spline_status
    fgsl_spline_status = .true.
    if (.not. c_associated(spline%gsl_spline)) fgsl_spline_status = .false.
  end function fgsl_spline_status
  function fgsl_spline2d_status(spline)
    type(fgsl_spline2d), intent(in) :: spline
    logical :: fgsl_spline2d_status
    fgsl_spline2d_status = .true.
    if (.not. c_associated(spline%gsl_spline2d)) fgsl_spline2d_status = .false.
  end function fgsl_spline2d_status
  function fgsl_sizeof_interp(w)
    type(fgsl_interp), intent(in) :: w
    integer(fgsl_size_t) :: fgsl_sizeof_interp
    fgsl_sizeof_interp = gsl_aux_sizeof_interp()
  end function fgsl_sizeof_interp
  function fgsl_interp2d_alloc(T, xsize, ysize)
    type(fgsl_interp2d_type), intent(in) :: T
    integer(fgsl_size_t), intent(in) :: xsize, ysize
    type(fgsl_interp2d) :: fgsl_interp2d_alloc
    fgsl_interp2d_alloc%gsl_interp2d = gsl_interp2d_alloc(&
    fgsl_aux_interp2d_alloc(T%which), xsize, ysize)
  end function fgsl_interp2d_alloc
  function fgsl_interp2d_name(interp)
    type(fgsl_interp2d), intent(in) :: interp
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_interp2d_name
    type(c_ptr) :: name
    name = gsl_interp2d_name(interp%gsl_interp2d)
    fgsl_interp2d_name = fgsl_name(name)
  end function fgsl_interp2d_name
  function fgsl_interp2d_min_size(interp)
    type(fgsl_interp2d), intent(in) :: interp
    integer(fgsl_size_t) :: fgsl_interp2d_min_size
    fgsl_interp2d_min_size = gsl_interp2d_min_size(interp%gsl_interp2d)
  end function fgsl_interp2d_min_size
  function fgsl_interp2d_type_min_size(T)
    type(fgsl_interp2d_type), intent(in) :: T
    integer(fgsl_size_t) :: fgsl_interp2d_type_min_size
    fgsl_interp2d_type_min_size = gsl_interp2d_type_min_size(&
    fgsl_aux_interp2d_alloc(T%which))
  end function fgsl_interp2d_type_min_size
  function fgsl_interp2d_init(interp, xa, ya, za)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xa, ya
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: za
    integer(fgsl_int) :: fgsl_interp2d_init
    ! this may be a good time to check the dimensions of za vs those of xa and ya
    if (size(xa) /= size(za, dim=1)) then
      call fgsl_error('xa and za dimensions do not match', 'fgsl_interp', __LINE__, fgsl_ebadlen)
      fgsl_interp2d_init = fgsl_ebadlen
      return
    endif
    if (size(ya) /= size(za, dim=2)) then
      call fgsl_error('ya and za dimensions do not match', 'fgsl_interp', __LINE__, fgsl_ebadlen)
      fgsl_interp2d_init = fgsl_ebadlen
      return
    endif
    fgsl_interp2d_init = gsl_interp2d_init(interp%gsl_interp2d, &
    c_loc(xa), c_loc(ya), c_loc(za), size(xa, kind=fgsl_size_t), &
    size(ya, kind=fgsl_size_t))
  end function fgsl_interp2d_init
  subroutine fgsl_interp2d_free(interp)
    type(fgsl_interp2d), intent(in) :: interp
    call gsl_interp2d_free(interp%gsl_interp2d)
  end subroutine fgsl_interp2d_free
  function fgsl_interp2d_eval(interp, xarr, yarr, zarr, x, y, xa, ya)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xarr, yarr
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: zarr
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double) :: fgsl_interp2d_eval
    ! I could do more bounds checking here, but seems like overkill
    fgsl_interp2d_eval = gsl_interp2d_eval(interp%gsl_interp2d, c_loc(xarr), &
    c_loc(yarr), c_loc(zarr), x, y, xa%gsl_interp_accel, ya%gsl_interp_accel)
  end function fgsl_interp2d_eval
  function fgsl_interp2d_eval_extrap(interp, xarr, yarr, zarr, x, y, xa, ya)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xarr, yarr
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: zarr
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double) :: fgsl_interp2d_eval_extrap
    ! I could do more bounds checking here, but seems like overkill
    fgsl_interp2d_eval_extrap = gsl_interp2d_eval_extrap(interp%gsl_interp2d, c_loc(xarr), &
    c_loc(yarr), c_loc(zarr), x, y, xa%gsl_interp_accel, ya%gsl_interp_accel)
  end function fgsl_interp2d_eval_extrap
  function fgsl_interp2d_eval_e(interp, xarr, yarr, zarr, x, y, xa, ya, z)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xarr, yarr
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: zarr
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double), intent(out) :: z
    integer(fgsl_int) :: fgsl_interp2d_eval_e
    ! I could do more bounds checking here, but seems like overkill
    fgsl_interp2d_eval_e = gsl_interp2d_eval_e(interp%gsl_interp2d, c_loc(xarr), &
    c_loc(yarr), c_loc(zarr), x, y, xa%gsl_interp_accel, ya%gsl_interp_accel, z)
  end function fgsl_interp2d_eval_e
  function fgsl_interp2d_eval_e_extrap(interp, xarr, yarr, zarr, x, y, xa, ya, z)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xarr, yarr
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: zarr
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double), intent(out) :: z
    integer(fgsl_int) :: fgsl_interp2d_eval_e_extrap
    ! I could do more bounds checking here, but seems like overkill
    fgsl_interp2d_eval_e_extrap = gsl_interp2d_eval_e_extrap(interp%gsl_interp2d, c_loc(xarr), &
    c_loc(yarr), c_loc(zarr), x, y, xa%gsl_interp_accel, ya%gsl_interp_accel, z)
  end function fgsl_interp2d_eval_e_extrap
  function fgsl_interp2d_eval_extrap_e(interp, xarr, yarr, zarr, x, y, xa, ya, z)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xarr, yarr
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: zarr
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double), intent(out) :: z
    integer(fgsl_int) :: fgsl_interp2d_eval_extrap_e
    ! I could do more bounds checking here, but seems like overkill
    fgsl_interp2d_eval_extrap_e = gsl_interp2d_eval_extrap_e(interp%gsl_interp2d, c_loc(xarr), &
    c_loc(yarr), c_loc(zarr), x, y, xa%gsl_interp_accel, ya%gsl_interp_accel, z)
  end function fgsl_interp2d_eval_extrap_e
  function fgsl_interp2d_eval_deriv_x(interp, xarr, yarr, zarr, x, y, xa, ya)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xarr, yarr
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: zarr
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double) :: fgsl_interp2d_eval_deriv_x
    ! I could do more bounds checking here, but seems like overkill
    fgsl_interp2d_eval_deriv_x = gsl_interp2d_eval_deriv_x(interp%gsl_interp2d, c_loc(xarr), &
    c_loc(yarr), c_loc(zarr), x, y, xa%gsl_interp_accel, ya%gsl_interp_accel)
  end function fgsl_interp2d_eval_deriv_x
  function fgsl_interp2d_eval_deriv_x_e(interp, xarr, yarr, zarr, x, y, xa, ya, z)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xarr, yarr
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: zarr
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double), intent(out) :: z
    integer(fgsl_int) :: fgsl_interp2d_eval_deriv_x_e
    ! I could do more bounds checking here, but seems like overkill
    fgsl_interp2d_eval_deriv_x_e = gsl_interp2d_eval_deriv_x_e(interp%gsl_interp2d, c_loc(xarr), &
    c_loc(yarr), c_loc(zarr), x, y, xa%gsl_interp_accel, ya%gsl_interp_accel, z)
  end function fgsl_interp2d_eval_deriv_x_e
  function fgsl_interp2d_eval_deriv_y(interp, xarr, yarr, zarr, x, y, xa, ya)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xarr, yarr
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: zarr
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double) :: fgsl_interp2d_eval_deriv_y
    ! I could do more bounds checking here, but seems like overkill
    fgsl_interp2d_eval_deriv_y = gsl_interp2d_eval_deriv_y(interp%gsl_interp2d, c_loc(xarr), &
    c_loc(yarr), c_loc(zarr), x, y, xa%gsl_interp_accel, ya%gsl_interp_accel)
  end function fgsl_interp2d_eval_deriv_y
  function fgsl_interp2d_eval_deriv_y_e(interp, xarr, yarr, zarr, x, y, xa, ya, z)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xarr, yarr
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: zarr
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double), intent(out) :: z
    integer(fgsl_int) :: fgsl_interp2d_eval_deriv_y_e
    ! I could do more bounds checking here, but seems like overkill
    fgsl_interp2d_eval_deriv_y_e = gsl_interp2d_eval_deriv_y_e(interp%gsl_interp2d, c_loc(xarr), &
    c_loc(yarr), c_loc(zarr), x, y, xa%gsl_interp_accel, ya%gsl_interp_accel, z)
  end function fgsl_interp2d_eval_deriv_y_e
  function fgsl_interp2d_eval_deriv_xx(interp, xarr, yarr, zarr, x, y, xa, ya)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xarr, yarr
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: zarr
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double) :: fgsl_interp2d_eval_deriv_xx
    ! I could do more bounds checking here, but seems like overkill
    fgsl_interp2d_eval_deriv_xx = gsl_interp2d_eval_deriv_xx(interp%gsl_interp2d, c_loc(xarr), &
    c_loc(yarr), c_loc(zarr), x, y, xa%gsl_interp_accel, ya%gsl_interp_accel)
  end function fgsl_interp2d_eval_deriv_xx
  function fgsl_interp2d_eval_deriv_xx_e(interp, xarr, yarr, zarr, x, y, xa, ya, z)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xarr, yarr
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: zarr
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double), intent(out) :: z
    integer(fgsl_int) :: fgsl_interp2d_eval_deriv_xx_e
    ! I could do more bounds checking here, but seems like overkill
    fgsl_interp2d_eval_deriv_xx_e = gsl_interp2d_eval_deriv_xx_e(interp%gsl_interp2d, c_loc(xarr), &
    c_loc(yarr), c_loc(zarr), x, y, xa%gsl_interp_accel, ya%gsl_interp_accel, z)
  end function fgsl_interp2d_eval_deriv_xx_e
  function fgsl_interp2d_eval_deriv_yy(interp, xarr, yarr, zarr, x, y, xa, ya)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xarr, yarr
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: zarr
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double) :: fgsl_interp2d_eval_deriv_yy
    ! I could do more bounds checking here, but seems like overkill
    fgsl_interp2d_eval_deriv_yy = gsl_interp2d_eval_deriv_yy(interp%gsl_interp2d, c_loc(xarr), &
    c_loc(yarr), c_loc(zarr), x, y, xa%gsl_interp_accel, ya%gsl_interp_accel)
  end function fgsl_interp2d_eval_deriv_yy
  function fgsl_interp2d_eval_deriv_yy_e(interp, xarr, yarr, zarr, x, y, xa, ya, z)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xarr, yarr
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: zarr
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double), intent(out) :: z
    integer(fgsl_int) :: fgsl_interp2d_eval_deriv_yy_e
    ! I could do more bounds checking here, but seems like overkill
    fgsl_interp2d_eval_deriv_yy_e = gsl_interp2d_eval_deriv_yy_e(interp%gsl_interp2d, c_loc(xarr), &
    c_loc(yarr), c_loc(zarr), x, y, xa%gsl_interp_accel, ya%gsl_interp_accel, z)
  end function fgsl_interp2d_eval_deriv_yy_e
  function fgsl_interp2d_eval_deriv_xy(interp, xarr, yarr, zarr, x, y, xa, ya)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xarr, yarr
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: zarr
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double) :: fgsl_interp2d_eval_deriv_xy
    ! I could do more bounds checking here, but seems like overkill
    fgsl_interp2d_eval_deriv_xy = gsl_interp2d_eval_deriv_xy(interp%gsl_interp2d, c_loc(xarr), &
    c_loc(yarr), c_loc(zarr), x, y, xa%gsl_interp_accel, ya%gsl_interp_accel)
  end function fgsl_interp2d_eval_deriv_xy
  function fgsl_interp2d_eval_deriv_xy_e(interp, xarr, yarr, zarr, x, y, xa, ya, z)
    type(fgsl_interp2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xarr, yarr
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: zarr
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double), intent(out) :: z
    integer(fgsl_int) :: fgsl_interp2d_eval_deriv_xy_e
    ! I could do more bounds checking here, but seems like overkill
    fgsl_interp2d_eval_deriv_xy_e = gsl_interp2d_eval_deriv_xy_e(interp%gsl_interp2d, c_loc(xarr), &
    c_loc(yarr), c_loc(zarr), x, y, xa%gsl_interp_accel, ya%gsl_interp_accel, z)
  end function fgsl_interp2d_eval_deriv_xy_e
  function fgsl_spline2d_alloc(T, xsize, ysize)
    type(fgsl_interp2d_type), intent(in) :: T
    integer(fgsl_size_t), intent(in) :: xsize, ysize
    type(fgsl_spline2d) :: fgsl_spline2d_alloc
    fgsl_spline2d_alloc%gsl_spline2d = gsl_spline2d_alloc(&
    fgsl_aux_interp2d_alloc(T%which), xsize, ysize)
  end function fgsl_spline2d_alloc
  function fgsl_spline2d_init(interp, xa, ya, za)
    type(fgsl_spline2d), intent(in) :: interp
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: xa, ya
    real(fgsl_double), dimension(:,:), intent(in), target, contiguous :: za
    integer(fgsl_int) :: fgsl_spline2d_init
    ! this may be a good time to check the dimensions of za vs those of xa and ya
    if (size(xa) /= size(za, dim=1)) then
      call fgsl_error('xa and za dimensions do not match', 'fgsl_interp', __LINE__, fgsl_ebadlen)
      fgsl_spline2d_init = fgsl_ebadlen
      return
    endif
    if (size(ya) /= size(za, dim=2)) then
      call fgsl_error('ya and za dimensions do not match', 'fgsl_interp', __LINE__, fgsl_ebadlen)
      fgsl_spline2d_init = fgsl_ebadlen
      return
    endif
    fgsl_spline2d_init = gsl_spline2d_init(interp%gsl_spline2d, &
    c_loc(xa), c_loc(ya), c_loc(za), size(xa, kind=fgsl_size_t), &
    size(ya, kind=fgsl_size_t))
  end function fgsl_spline2d_init
  subroutine fgsl_spline2d_free(interp)
    type(fgsl_spline2d), intent(in) :: interp
    call gsl_spline2d_free(interp%gsl_spline2d)
  end subroutine fgsl_spline2d_free
  function fgsl_spline2d_eval(interp, x, y, xa, ya)
    type(fgsl_spline2d), intent(in) :: interp
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double) :: fgsl_spline2d_eval
    fgsl_spline2d_eval = gsl_spline2d_eval(interp%gsl_spline2d, x, y, &
    xa%gsl_interp_accel, ya%gsl_interp_accel)
  end function fgsl_spline2d_eval
  function fgsl_spline2d_eval_e(interp, x, y, xa, ya, z)
    type(fgsl_spline2d), intent(in) :: interp
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double), intent(out) :: z
    integer(fgsl_int) :: fgsl_spline2d_eval_e
    fgsl_spline2d_eval_e = gsl_spline2d_eval_e(interp%gsl_spline2d, x, y, &
    xa%gsl_interp_accel, ya%gsl_interp_accel, z)
  end function fgsl_spline2d_eval_e
  function fgsl_spline2d_eval_extrap(interp, x, y, xa, ya)
    type(fgsl_spline2d), intent(in) :: interp
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double) :: fgsl_spline2d_eval_extrap
    fgsl_spline2d_eval_extrap = gsl_spline2d_eval_extrap(&
         interp%gsl_spline2d, x, y, xa%gsl_interp_accel, ya%gsl_interp_accel)
  end function fgsl_spline2d_eval_extrap
  function fgsl_spline2d_eval_extrap_e(interp, x, y, xa, ya, z)
    type(fgsl_spline2d), intent(in) :: interp
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double), intent(out) :: z
    integer(fgsl_int) :: fgsl_spline2d_eval_extrap_e
    fgsl_spline2d_eval_extrap_e = gsl_spline2d_eval_extrap_e(&
         interp%gsl_spline2d, x, y, xa%gsl_interp_accel, ya%gsl_interp_accel, z)
  end function fgsl_spline2d_eval_extrap_e
   function fgsl_spline2d_eval_deriv_x(interp, x, y, xa, ya)
    type(fgsl_spline2d), intent(in) :: interp
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double) :: fgsl_spline2d_eval_deriv_x
    fgsl_spline2d_eval_deriv_x = gsl_spline2d_eval_deriv_x(&
         interp%gsl_spline2d, x, y, xa%gsl_interp_accel, ya%gsl_interp_accel)
  end function fgsl_spline2d_eval_deriv_x
  function fgsl_spline2d_eval_deriv_x_e(interp, x, y, xa, ya, z)
    type(fgsl_spline2d), intent(in) :: interp
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double), intent(out) :: z
    integer(fgsl_int) :: fgsl_spline2d_eval_deriv_x_e
    fgsl_spline2d_eval_deriv_x_e = gsl_spline2d_eval_deriv_x_e(&
         interp%gsl_spline2d, x, y, xa%gsl_interp_accel, ya%gsl_interp_accel, z)
  end function fgsl_spline2d_eval_deriv_x_e
  function fgsl_spline2d_eval_deriv_y(interp, x, y, xa, ya)
    type(fgsl_spline2d), intent(in) :: interp
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double) :: fgsl_spline2d_eval_deriv_y
    fgsl_spline2d_eval_deriv_y = gsl_spline2d_eval_deriv_y(interp%gsl_spline2d, x, y, &
    xa%gsl_interp_accel, ya%gsl_interp_accel)
  end function fgsl_spline2d_eval_deriv_y
  function fgsl_spline2d_eval_deriv_y_e(interp, x, y, xa, ya, z)
    type(fgsl_spline2d), intent(in) :: interp
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double), intent(out) :: z
    integer(fgsl_int) :: fgsl_spline2d_eval_deriv_y_e
    fgsl_spline2d_eval_deriv_y_e = gsl_spline2d_eval_deriv_y_e(interp%gsl_spline2d, x, y, &
    xa%gsl_interp_accel, ya%gsl_interp_accel, z)
  end function fgsl_spline2d_eval_deriv_y_e
  function fgsl_spline2d_eval_deriv_xx(interp, x, y, xa, ya)
    type(fgsl_spline2d), intent(in) :: interp
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double) :: fgsl_spline2d_eval_deriv_xx
    fgsl_spline2d_eval_deriv_xx = gsl_spline2d_eval_deriv_xx(interp%gsl_spline2d, x, y, &
    xa%gsl_interp_accel, ya%gsl_interp_accel)
  end function fgsl_spline2d_eval_deriv_xx
  function fgsl_spline2d_eval_deriv_xx_e(interp, x, y, xa, ya, z)
    type(fgsl_spline2d), intent(in) :: interp
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double), intent(out) :: z
    integer(fgsl_int) :: fgsl_spline2d_eval_deriv_xx_e
    fgsl_spline2d_eval_deriv_xx_e = gsl_spline2d_eval_deriv_xx_e(interp%gsl_spline2d, x, y, &
    xa%gsl_interp_accel, ya%gsl_interp_accel, z)
  end function fgsl_spline2d_eval_deriv_xx_e
  function fgsl_spline2d_eval_deriv_yy(interp, x, y, xa, ya)
    type(fgsl_spline2d), intent(in) :: interp
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double) :: fgsl_spline2d_eval_deriv_yy
    fgsl_spline2d_eval_deriv_yy = gsl_spline2d_eval_deriv_yy(interp%gsl_spline2d, x, y, &
    xa%gsl_interp_accel, ya%gsl_interp_accel)
  end function fgsl_spline2d_eval_deriv_yy
  function fgsl_spline2d_eval_deriv_yy_e(interp, x, y, xa, ya, z)
    type(fgsl_spline2d), intent(in) :: interp
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double), intent(out) :: z
    integer(fgsl_int) :: fgsl_spline2d_eval_deriv_yy_e
    fgsl_spline2d_eval_deriv_yy_e = gsl_spline2d_eval_deriv_yy_e(interp%gsl_spline2d, x, y, &
    xa%gsl_interp_accel, ya%gsl_interp_accel, z)
  end function fgsl_spline2d_eval_deriv_yy_e
  function fgsl_spline2d_eval_deriv_xy(interp, x, y, xa, ya)
    type(fgsl_spline2d), intent(in) :: interp
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double) :: fgsl_spline2d_eval_deriv_xy
    fgsl_spline2d_eval_deriv_xy = gsl_spline2d_eval_deriv_xy(interp%gsl_spline2d, x, y, &
    xa%gsl_interp_accel, ya%gsl_interp_accel)
  end function fgsl_spline2d_eval_deriv_xy
  function fgsl_spline2d_eval_deriv_xy_e(interp, x, y, xa, ya, z)
    type(fgsl_spline2d), intent(in) :: interp
    real(fgsl_double), intent(in) :: x, y
    type(fgsl_interp_accel), intent(inout) :: xa, ya
    real(fgsl_double), intent(out) :: z
    integer(fgsl_int) :: fgsl_spline2d_eval_deriv_xy_e
    fgsl_spline2d_eval_deriv_xy_e = gsl_spline2d_eval_deriv_xy_e(interp%gsl_spline2d, x, y, &
    xa%gsl_interp_accel, ya%gsl_interp_accel, z)
  end function fgsl_spline2d_eval_deriv_xy_e
  function fgsl_spline2d_name(interp)
    type(fgsl_spline2d), intent(in) :: interp
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_spline2d_name
    type(c_ptr) :: name
    name = gsl_spline2d_name(interp%gsl_spline2d)
    fgsl_spline2d_name = fgsl_name(name)
  end function fgsl_spline2d_name
  function fgsl_spline2d_set(spline, za, i, j, z)
    type(fgsl_spline2d), intent(in) :: spline
    real(fgsl_double), intent(inout) :: za(*)
    integer(fgsl_size_t), intent(in) :: i, j
    real(fgsl_double), intent(in) :: z
    integer(fgsl_int) :: fgsl_spline2d_set
    fgsl_spline2d_set = gsl_spline2d_set(spline%gsl_spline2d, za, i, j, z)
  end function fgsl_spline2d_set
  function fgsl_spline2d_get(spline, za, i, j)
    type(fgsl_spline2d), intent(in) :: spline
    real(fgsl_double), intent(in) :: za(*)
    integer(fgsl_size_t), intent(in) :: i, j
    real(fgsl_double) :: fgsl_spline2d_get
    fgsl_spline2d_get = gsl_spline2d_get(spline%gsl_spline2d, za, i, j)
  end function fgsl_spline2d_get 
  function fgsl_spline2d_min_size(interp)
    type(fgsl_spline2d), intent(in) :: interp
    integer(fgsl_size_t) :: fgsl_spline2d_min_size
    fgsl_spline2d_min_size = gsl_spline2d_min_size(interp%gsl_spline2d)
  end function fgsl_spline2d_min_size
	 
end module fgsl_interpolation
