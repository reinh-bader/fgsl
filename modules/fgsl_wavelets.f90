module fgsl_wavelets
  !> Wavelet Transforms
  !> Note: This module maps the functions from gsl_wavelet.h and gsl_wavelet2d.h
  use fgsl_base
  use fgsl_array

  implicit none
  
  private :: gsl_wavelet_alloc, gsl_wavelet_name, gsl_wavelet_free, &
    gsl_wavelet_workspace_alloc, gsl_wavelet_workspace_free, &
    gsl_wavelet_transform, gsl_wavelet_transform_forward, &
    gsl_wavelet_transform_inverse
  private :: gsl_wavelet2d_transform, gsl_wavelet2d_transform_forward, &
    gsl_wavelet2d_transform_inverse, gsl_wavelet2d_transform_matrix, &
    gsl_wavelet2d_transform_matrix_forward, gsl_wavelet2d_transform_matrix_inverse, &
    gsl_wavelet2d_nstransform, gsl_wavelet2d_nstransform_forward, &
    gsl_wavelet2d_nstransform_inverse, gsl_wavelet2d_nstransform_matrix, &
    gsl_wavelet2d_nstransform_matrix_forward, &
    gsl_wavelet2d_nstransform_matrix_inverse
  private :: fgsl_aux_wavelet_alloc, gsl_aux_sizeof_wavelet_workspace, &
    gsl_aux_sizeof_wavelet
  !
  !> Generics
  interface fgsl_well_defined
     module procedure fgsl_wavelet_status
     module procedure fgsl_wavelet_workspace_status
  end interface fgsl_well_defined
  interface fgsl_sizeof
     module procedure fgsl_sizeof_wavelet
     module procedure fgsl_sizeof_wavelet_workspace
  end interface
  !
  !> Types
  type, public :: fgsl_wavelet
     private
     type(c_ptr) :: gsl_wavelet = c_null_ptr
  end type fgsl_wavelet
  type, public :: fgsl_wavelet_type
     private
     integer(c_int) :: which = 0
  end type fgsl_wavelet_type
  type(fgsl_wavelet_type), public, parameter :: &
       fgsl_wavelet_daubechies = fgsl_wavelet_type(1), &
       fgsl_wavelet_daubechies_centered = fgsl_wavelet_type(2), &
       fgsl_wavelet_haar = fgsl_wavelet_type(3), &
       fgsl_wavelet_haar_centered = fgsl_wavelet_type(4), &
       fgsl_wavelet_bspline = fgsl_wavelet_type(5), &
       fgsl_wavelet_bspline_centered = fgsl_wavelet_type(6)
  type, public :: fgsl_wavelet_workspace
     private
     type(c_ptr) :: gsl_wavelet_workspace
  end type fgsl_wavelet_workspace
  !
  !> C interfaces
  interface
	  function gsl_wavelet_alloc(t, k) bind(c)
	    import
	    type(c_ptr), value :: t
	    integer(c_size_t), value :: k
	    type(c_ptr) :: gsl_wavelet_alloc
	  end function gsl_wavelet_alloc
	  function gsl_wavelet_name(wavelet) bind(c)
	    import
	    type(c_ptr), value :: wavelet
	    type(c_ptr) :: gsl_wavelet_name
	  end function gsl_wavelet_name
	  subroutine gsl_wavelet_free(w) bind(c)
	    import
	    type(c_ptr), value :: w
	  end subroutine gsl_wavelet_free
	  function gsl_wavelet_workspace_alloc(n) bind(c)
	    import
	    integer(c_size_t), value :: n
	    type(c_ptr) :: gsl_wavelet_workspace_alloc
	  end function gsl_wavelet_workspace_alloc
	  subroutine gsl_wavelet_workspace_free(w) bind(c)
	    import
	    type(c_ptr), value :: w
	  end subroutine gsl_wavelet_workspace_free
	  function gsl_wavelet_transform(w, data, stride, n, dir, work) bind(c)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    integer(c_int), value :: dir
	    type(c_ptr), value :: work
	    integer(c_int) :: gsl_wavelet_transform
	  end function gsl_wavelet_transform
	  function gsl_wavelet_transform_forward(w, data, stride, n, work) bind(c)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    type(c_ptr), value :: work
	    integer(c_int) :: gsl_wavelet_transform_forward
	  end function gsl_wavelet_transform_forward
	  function gsl_wavelet_transform_inverse(w, data, stride, n, work) bind(c)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: stride, n
	    type(c_ptr), value :: work
	    integer(c_int) :: gsl_wavelet_transform_inverse
	  end function gsl_wavelet_transform_inverse
	  function gsl_wavelet2d_transform(w, data, tda, size1, size2, dir, work) bind(c)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: tda, size1, size2
	    integer(c_int), value :: dir
	    type(c_ptr), value :: work
	    integer(c_int) :: gsl_wavelet2d_transform
	  end function gsl_wavelet2d_transform
	  function gsl_wavelet2d_transform_forward(w, data, tda, size1, size2, work) bind(c)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: tda, size1, size2
	    type(c_ptr), value :: work
	    integer(c_int) :: gsl_wavelet2d_transform_forward
	  end function gsl_wavelet2d_transform_forward
	  function gsl_wavelet2d_transform_inverse(w, data, tda, size1, size2, work) bind(c)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: tda, size1, size2
	    type(c_ptr), value :: work
	    integer(c_int) :: gsl_wavelet2d_transform_inverse
	  end function gsl_wavelet2d_transform_inverse
	  function gsl_wavelet2d_transform_matrix(w, m, dir, work) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: w, m, work
	    integer(c_int), value :: dir
	    integer(c_int) :: gsl_wavelet2d_transform_matrix
	  end function gsl_wavelet2d_transform_matrix
	  function gsl_wavelet2d_transform_matrix_forward(w, m, work) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: w, m, work
	    integer(c_int) :: gsl_wavelet2d_transform_matrix_forward
	  end function gsl_wavelet2d_transform_matrix_forward
	  function gsl_wavelet2d_transform_matrix_inverse(w, m, work) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: w, m, work
	    integer(c_int) :: gsl_wavelet2d_transform_matrix_inverse
	  end function gsl_wavelet2d_transform_matrix_inverse
	  function gsl_wavelet2d_nstransform(w, data, tda, size1, size2, dir, work) bind(c)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: tda, size1, size2
	    integer(c_int), value :: dir
	    type(c_ptr), value :: work
	    integer(c_int) :: gsl_wavelet2d_nstransform
	  end function gsl_wavelet2d_nstransform
	  function gsl_wavelet2d_nstransform_forward(w, data, tda, size1, size2, work) bind(c)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: tda, size1, size2
	    type(c_ptr), value :: work
	    integer(c_int) :: gsl_wavelet2d_nstransform_forward
	  end function gsl_wavelet2d_nstransform_forward
	  function gsl_wavelet2d_nstransform_inverse(w, data, tda, size1, size2, work) bind(c)
	    import
	    type(c_ptr), value :: w
	    type(c_ptr), value :: data
	    integer(c_size_t), value :: tda, size1, size2
	    type(c_ptr), value :: work
	    integer(c_int) :: gsl_wavelet2d_nstransform_inverse
	  end function gsl_wavelet2d_nstransform_inverse
	  function gsl_wavelet2d_nstransform_matrix(w, m, dir, work) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: w, m, work
	    integer(c_int), value :: dir
	    integer(c_int) :: gsl_wavelet2d_nstransform_matrix
	  end function gsl_wavelet2d_nstransform_matrix
	  function gsl_wavelet2d_nstransform_matrix_forward(w, m, work) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: w, m, work
	    integer(c_int) :: gsl_wavelet2d_nstransform_matrix_forward
	  end function gsl_wavelet2d_nstransform_matrix_forward
	  function gsl_wavelet2d_nstransform_matrix_inverse(w, m, work) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: w, m, work
	    integer(c_int) :: gsl_wavelet2d_nstransform_matrix_inverse
	  end function gsl_wavelet2d_nstransform_matrix_inverse
	!
	  function fgsl_aux_wavelet_alloc(i) bind(c)
	    import
	    integer(c_int), value :: i
	    type(c_ptr) :: fgsl_aux_wavelet_alloc
	  end function fgsl_aux_wavelet_alloc
	  function gsl_aux_sizeof_wavelet_workspace() bind(c)
	    import :: c_size_t
	    integer(c_size_t) :: gsl_aux_sizeof_wavelet_workspace
	  end function gsl_aux_sizeof_wavelet_workspace
	  function gsl_aux_sizeof_wavelet() bind(c)
	    import :: c_size_t
	    integer(c_size_t) :: gsl_aux_sizeof_wavelet
	  end function gsl_aux_sizeof_wavelet
  end interface
contains
!> API
  function fgsl_wavelet_alloc(t, k)
    type(fgsl_wavelet_type), intent(in) :: t
    integer(fgsl_size_t), intent(in) :: k
    type(fgsl_wavelet) :: fgsl_wavelet_alloc
!
    type(c_ptr) :: cp
    integer(c_int) :: i
    i = t%which
    cp = fgsl_aux_wavelet_alloc(i)
    fgsl_wavelet_alloc%gsl_wavelet = gsl_wavelet_alloc(cp, k)
  end function fgsl_wavelet_alloc
  function fgsl_wavelet_name(wavelet)
    type(fgsl_wavelet), intent(in) :: wavelet
    character(kind=fgsl_char,len=fgsl_strmax) :: fgsl_wavelet_name
!
    type(c_ptr) :: name
!
    name = gsl_wavelet_name(wavelet%gsl_wavelet)
    fgsl_wavelet_name = fgsl_name(name)
  end function fgsl_wavelet_name
  subroutine fgsl_wavelet_free(w)
    type(fgsl_wavelet), intent(inout) :: w
    call gsl_wavelet_free(w%gsl_wavelet)
  end subroutine fgsl_wavelet_free
  function fgsl_wavelet_workspace_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_wavelet_workspace) :: fgsl_wavelet_workspace_alloc
    fgsl_wavelet_workspace_alloc%gsl_wavelet_workspace = &
         gsl_wavelet_workspace_alloc(n)
  end function fgsl_wavelet_workspace_alloc
  subroutine fgsl_wavelet_workspace_free(w)
    type(fgsl_wavelet_workspace), intent(inout) :: w
    call gsl_wavelet_workspace_free(w%gsl_wavelet_workspace)
  end subroutine fgsl_wavelet_workspace_free
  function fgsl_wavelet_transform(w, data, stride, n, dir, work)
    type(fgsl_wavelet), intent(in) :: w
    real(fgsl_double), intent(inout), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_int), intent(in) :: dir
    type(fgsl_wavelet_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_wavelet_transform
    fgsl_wavelet_transform = gsl_wavelet_transform(w%gsl_wavelet, c_loc(data), &
         stride, n, dir, work%gsl_wavelet_workspace)
  end function fgsl_wavelet_transform
  function fgsl_wavelet_transform_forward(w, data, stride, n, work)
    type(fgsl_wavelet), intent(in) :: w
    real(fgsl_double), intent(inout), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    type(fgsl_wavelet_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_wavelet_transform_forward
    fgsl_wavelet_transform_forward = gsl_wavelet_transform_forward(w%gsl_wavelet, c_loc(data), &
         stride, n, work%gsl_wavelet_workspace)
  end function fgsl_wavelet_transform_forward
  function fgsl_wavelet_transform_inverse(w, data, stride, n, work)
    type(fgsl_wavelet), intent(in) :: w
    real(fgsl_double), intent(inout), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: stride, n
    type(fgsl_wavelet_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_wavelet_transform_inverse
    fgsl_wavelet_transform_inverse = gsl_wavelet_transform_inverse(w%gsl_wavelet, c_loc(data), &
         stride, n, work%gsl_wavelet_workspace)
  end function fgsl_wavelet_transform_inverse
  function fgsl_wavelet2d_transform(w, data, tda, size1, size2, dir, work)
    type(fgsl_wavelet), intent(in) :: w
    real(fgsl_double), intent(inout), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: tda, size1, size2
    integer(fgsl_int), intent(in) :: dir
    type(fgsl_wavelet_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_wavelet2d_transform
    fgsl_wavelet2d_transform = gsl_wavelet2d_transform(w%gsl_wavelet, c_loc(data), &
         tda, size1, size2, dir, work%gsl_wavelet_workspace)
  end function fgsl_wavelet2d_transform
  function fgsl_wavelet2d_transform_forward(w, data, tda, size1, size2, work)
    type(fgsl_wavelet), intent(in) :: w
    real(fgsl_double), intent(inout), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: tda, size1, size2
    type(fgsl_wavelet_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_wavelet2d_transform_forward
    fgsl_wavelet2d_transform_forward = &
         gsl_wavelet2d_transform_forward(w%gsl_wavelet, c_loc(data), &
         tda, size1, size2, work%gsl_wavelet_workspace)
  end function fgsl_wavelet2d_transform_forward
  function fgsl_wavelet2d_transform_inverse(w, data, tda, size1, size2, work)
    type(fgsl_wavelet), intent(in) :: w
    real(fgsl_double), intent(inout), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: tda, size1, size2
    type(fgsl_wavelet_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_wavelet2d_transform_inverse
    fgsl_wavelet2d_transform_inverse = &
         gsl_wavelet2d_transform_inverse(w%gsl_wavelet, c_loc(data), &
         tda, size1, size2, work%gsl_wavelet_workspace)
  end function fgsl_wavelet2d_transform_inverse
  function fgsl_wavelet2d_transform_matrix(w, m, dir, work)
    type(fgsl_wavelet), intent(in) :: w
    type(fgsl_matrix), intent(inout) :: m
    type(fgsl_wavelet_workspace) :: work
    integer(fgsl_int), intent(in) :: dir
    integer(fgsl_int) :: fgsl_wavelet2d_transform_matrix
    fgsl_wavelet2d_transform_matrix = &
         gsl_wavelet2d_transform_matrix(w%gsl_wavelet, m%gsl_matrix, dir, &
         work%gsl_wavelet_workspace)
  end function fgsl_wavelet2d_transform_matrix
  function fgsl_wavelet2d_transform_matrix_forward(w, m, work)
    type(fgsl_wavelet), intent(in) :: w
    type(fgsl_matrix), intent(inout) :: m
    type(fgsl_wavelet_workspace) :: work
    integer(fgsl_int) :: fgsl_wavelet2d_transform_matrix_forward
    fgsl_wavelet2d_transform_matrix_forward = &
         gsl_wavelet2d_transform_matrix_forward(w%gsl_wavelet, m%gsl_matrix, &
         work%gsl_wavelet_workspace)
  end function fgsl_wavelet2d_transform_matrix_forward
  function fgsl_wavelet2d_transform_matrix_inverse(w, m, work)
    type(fgsl_wavelet), intent(in) :: w
    type(fgsl_matrix), intent(inout) :: m
    type(fgsl_wavelet_workspace) :: work
    integer(fgsl_int) :: fgsl_wavelet2d_transform_matrix_inverse
    fgsl_wavelet2d_transform_matrix_inverse = &
         gsl_wavelet2d_transform_matrix_inverse(w%gsl_wavelet, m%gsl_matrix, &
         work%gsl_wavelet_workspace)
  end function fgsl_wavelet2d_transform_matrix_inverse
  function fgsl_wavelet2d_nstransform(w, data, tda, size1, size2, dir, work)
    type(fgsl_wavelet), intent(in) :: w
    real(fgsl_double), intent(inout), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: tda, size1, size2
    integer(fgsl_int), intent(in) :: dir
    type(fgsl_wavelet_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_wavelet2d_nstransform
    fgsl_wavelet2d_nstransform = gsl_wavelet2d_nstransform(w%gsl_wavelet, c_loc(data), &
         tda, size1, size2, dir, work%gsl_wavelet_workspace)
  end function fgsl_wavelet2d_nstransform
  function fgsl_wavelet2d_nstransform_forward(w, data, tda, size1, size2, work)
    type(fgsl_wavelet), intent(in) :: w
    real(fgsl_double), intent(inout), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: tda, size1, size2
    type(fgsl_wavelet_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_wavelet2d_nstransform_forward
    fgsl_wavelet2d_nstransform_forward = &
         gsl_wavelet2d_nstransform_forward(w%gsl_wavelet, c_loc(data), &
         tda, size1, size2, work%gsl_wavelet_workspace)
  end function fgsl_wavelet2d_nstransform_forward
  function fgsl_wavelet2d_nstransform_inverse(w, data, tda, size1, size2, work)
    type(fgsl_wavelet), intent(in) :: w
    real(fgsl_double), intent(inout), target, contiguous :: data(:)
    integer(fgsl_size_t), intent(in) :: tda, size1, size2
    type(fgsl_wavelet_workspace), intent(inout) :: work
    integer(fgsl_int) :: fgsl_wavelet2d_nstransform_inverse
    fgsl_wavelet2d_nstransform_inverse = &
         gsl_wavelet2d_nstransform_inverse(w%gsl_wavelet, c_loc(data), &
         tda, size1, size2, work%gsl_wavelet_workspace)
  end function fgsl_wavelet2d_nstransform_inverse
  function fgsl_wavelet2d_nstransform_matrix(w, m, dir, work)
    type(fgsl_wavelet), intent(in) :: w
    type(fgsl_matrix), intent(inout) :: m
    type(fgsl_wavelet_workspace) :: work
    integer(fgsl_int), intent(in) :: dir
    integer(fgsl_int) :: fgsl_wavelet2d_nstransform_matrix
    fgsl_wavelet2d_nstransform_matrix = &
         gsl_wavelet2d_nstransform_matrix(w%gsl_wavelet, m%gsl_matrix, dir, &
         work%gsl_wavelet_workspace)
  end function fgsl_wavelet2d_nstransform_matrix
  function fgsl_wavelet2d_nstransform_matrix_forward(w, m, work)
    type(fgsl_wavelet), intent(in) :: w
    type(fgsl_matrix), intent(inout) :: m
    type(fgsl_wavelet_workspace) :: work
    integer(fgsl_int) :: fgsl_wavelet2d_nstransform_matrix_forward
    fgsl_wavelet2d_nstransform_matrix_forward = &
         gsl_wavelet2d_nstransform_matrix_forward(w%gsl_wavelet, m%gsl_matrix, &
         work%gsl_wavelet_workspace)
  end function fgsl_wavelet2d_nstransform_matrix_forward
  function fgsl_wavelet2d_nstransform_matrix_inverse(w, m, work)
    type(fgsl_wavelet), intent(in) :: w
    type(fgsl_matrix), intent(inout) :: m
    type(fgsl_wavelet_workspace) :: work
    integer(fgsl_int) :: fgsl_wavelet2d_nstransform_matrix_inverse
    fgsl_wavelet2d_nstransform_matrix_inverse = &
         gsl_wavelet2d_nstransform_matrix_inverse(w%gsl_wavelet, m%gsl_matrix, &
         work%gsl_wavelet_workspace)
  end function fgsl_wavelet2d_nstransform_matrix_inverse
  function fgsl_wavelet_status(wavelet)
    type(fgsl_wavelet), intent(in) :: wavelet
    logical :: fgsl_wavelet_status
    fgsl_wavelet_status = .true.
    if (.not. c_associated(wavelet%gsl_wavelet)) fgsl_wavelet_status = .false.
  end function fgsl_wavelet_status
  function fgsl_wavelet_workspace_status(wavelet_workspace)
    type(fgsl_wavelet_workspace), intent(in) :: wavelet_workspace
    logical :: fgsl_wavelet_workspace_status
    fgsl_wavelet_workspace_status = .true.
    if (.not. c_associated(wavelet_workspace%gsl_wavelet_workspace)) &
         fgsl_wavelet_workspace_status = .false.
  end function fgsl_wavelet_workspace_status
  function fgsl_sizeof_wavelet(w)
    type(fgsl_wavelet), intent(in) :: w
    integer(fgsl_size_t) :: fgsl_sizeof_wavelet
    fgsl_sizeof_wavelet = gsl_aux_sizeof_wavelet()
  end function fgsl_sizeof_wavelet
  function fgsl_sizeof_wavelet_workspace(w)
    type(fgsl_wavelet_workspace), intent(in) :: w
    integer(fgsl_size_t) :: fgsl_sizeof_wavelet_workspace
    fgsl_sizeof_wavelet_workspace = gsl_aux_sizeof_wavelet_workspace()
  end function fgsl_sizeof_wavelet_workspace
end module fgsl_wavelets
