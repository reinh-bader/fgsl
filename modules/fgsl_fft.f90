module fgsl_fft
  !> Fast Fourier Transforms
  use fgsl_base
  implicit none

  private :: gsl_fft_complex_radix2_forward, gsl_fft_complex_radix2_transform, &
	gsl_fft_complex_radix2_backward, gsl_fft_complex_radix2_inverse, &
	gsl_fft_complex_radix2_dif_forward, gsl_fft_complex_radix2_dif_transform, &
	gsl_fft_complex_radix2_dif_backward, gsl_fft_complex_radix2_dif_inverse
	
  private :: gsl_fft_complex_wavetable_alloc, gsl_fft_complex_wavetable_free, &
    gsl_fft_complex_workspace_alloc, gsl_fft_complex_workspace_free, &
    gsl_fft_complex_forward, gsl_fft_complex_backward, gsl_fft_complex_inverse

  private :: gsl_fft_real_radix2_transform, gsl_fft_halfcomplex_radix2_inverse, &
    gsl_fft_halfcomplex_radix2_backward, gsl_fft_real_wavetable_alloc, &
    gsl_fft_real_wavetable_free, gsl_fft_real_workspace_alloc, &
    gsl_fft_real_workspace_free, gsl_fft_real_transform, &
    gsl_fft_halfcomplex_transform, gsl_fft_real_unpack, gsl_fft_halfcomplex_unpack

!  
! Types
  type, public :: fgsl_fft_complex_wavetable
     private
     type(c_ptr) :: gsl_fft_complex_wavetable = c_null_ptr
  end type fgsl_fft_complex_wavetable
  type, public :: fgsl_fft_real_wavetable
     private
     type(c_ptr) :: gsl_fft_real_wavetable = c_null_ptr
  end type fgsl_fft_real_wavetable
  type, public :: fgsl_fft_halfcomplex_wavetable
     private
     type(c_ptr) :: gsl_fft_halfcomplex_wavetable = c_null_ptr
  end type fgsl_fft_halfcomplex_wavetable
  type, public :: fgsl_fft_complex_workspace
     private
     type(c_ptr) :: gsl_fft_complex_workspace = c_null_ptr
  end type fgsl_fft_complex_workspace
  type, public :: fgsl_fft_real_workspace
     private
     type(c_ptr) :: gsl_fft_real_workspace = c_null_ptr
  end type fgsl_fft_real_workspace

!
! C interfaces
  interface
     function gsl_fft_complex_radix2_forward(data, stride, n) bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data
       integer(c_int) :: gsl_fft_complex_radix2_forward
     end function gsl_fft_complex_radix2_forward
     function gsl_fft_complex_radix2_transform(data, stride, n, sign) bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data
       integer(c_int), value :: sign
       integer(c_int) :: gsl_fft_complex_radix2_transform
     end function gsl_fft_complex_radix2_transform
     function gsl_fft_complex_radix2_backward(data, stride, n) bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data
       integer(c_int) :: gsl_fft_complex_radix2_backward
     end function gsl_fft_complex_radix2_backward
     function gsl_fft_complex_radix2_inverse(data, stride, n) bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data
       integer(c_int) :: gsl_fft_complex_radix2_inverse
     end function gsl_fft_complex_radix2_inverse
     function gsl_fft_complex_radix2_dif_forward(data, stride, n) bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data
       integer(c_int) :: gsl_fft_complex_radix2_dif_forward
     end function gsl_fft_complex_radix2_dif_forward
     function gsl_fft_complex_radix2_dif_transform(data, stride, n, sign) bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data
       integer(c_int), value :: sign
       integer(c_int) :: gsl_fft_complex_radix2_dif_transform
     end function gsl_fft_complex_radix2_dif_transform
     function gsl_fft_complex_radix2_dif_backward(data, stride, n) bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data
       integer(c_int) :: gsl_fft_complex_radix2_dif_backward
     end function gsl_fft_complex_radix2_dif_backward
     function gsl_fft_complex_radix2_dif_inverse(data, stride, n) bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data
       integer(c_int) :: gsl_fft_complex_radix2_dif_inverse
     end function gsl_fft_complex_radix2_dif_inverse
     !
     function gsl_fft_complex_wavetable_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_fft_complex_wavetable_alloc
     end function gsl_fft_complex_wavetable_alloc
     subroutine gsl_fft_complex_wavetable_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_fft_complex_wavetable_free
     function gsl_fft_complex_workspace_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_fft_complex_workspace_alloc
     end function gsl_fft_complex_workspace_alloc
     subroutine gsl_fft_complex_workspace_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_fft_complex_workspace_free
     function gsl_fft_complex_forward(data, stride, n, wavetable, work) bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data, wavetable, work
       integer(c_int) :: gsl_fft_complex_forward
     end function gsl_fft_complex_forward
     function gsl_fft_complex_transform(data, stride, n, wavetable, work, sign) &
          bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data, wavetable, work
       integer(c_int), value :: sign
       integer(c_int) :: gsl_fft_complex_transform
     end function gsl_fft_complex_transform
     function gsl_fft_complex_backward(data, stride, n, wavetable, work) bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data, wavetable, work
       integer(c_int) :: gsl_fft_complex_backward
     end function gsl_fft_complex_backward
     function gsl_fft_complex_inverse(data, stride, n, wavetable, work) bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data, wavetable, work
       integer(c_int) :: gsl_fft_complex_inverse
     end function gsl_fft_complex_inverse
     !
     function gsl_fft_real_radix2_transform(data, stride, n) bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data
       integer(c_int) :: gsl_fft_real_radix2_transform
     end function gsl_fft_real_radix2_transform
     function gsl_fft_halfcomplex_radix2_inverse(data, stride, n) bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data
       integer(c_int) :: gsl_fft_halfcomplex_radix2_inverse
     end function gsl_fft_halfcomplex_radix2_inverse
     function gsl_fft_halfcomplex_radix2_backward(data, stride, n) bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data
       integer(c_int) :: gsl_fft_halfcomplex_radix2_backward
     end function gsl_fft_halfcomplex_radix2_backward
     function gsl_fft_real_wavetable_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_fft_real_wavetable_alloc
     end function gsl_fft_real_wavetable_alloc
     subroutine gsl_fft_real_wavetable_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_fft_real_wavetable_free
     function gsl_fft_halfcomplex_wavetable_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_fft_halfcomplex_wavetable_alloc
     end function gsl_fft_halfcomplex_wavetable_alloc
     subroutine gsl_fft_halfcomplex_wavetable_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_fft_halfcomplex_wavetable_free
     function gsl_fft_real_workspace_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_fft_real_workspace_alloc
     end function gsl_fft_real_workspace_alloc
     subroutine gsl_fft_real_workspace_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_fft_real_workspace_free
     function gsl_fft_real_transform(data, stride, n, wavetable, work) &
          bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data, wavetable, work
       integer(c_int) :: gsl_fft_real_transform
     end function gsl_fft_real_transform
     function gsl_fft_halfcomplex_transform(data, stride, n, wavetable, work) &
          bind(c)
       import :: c_size_t, c_ptr, c_int
       integer(c_size_t), value :: stride, n
       type(c_ptr), value :: data, wavetable, work
       integer(c_int) :: gsl_fft_halfcomplex_transform
     end function gsl_fft_halfcomplex_transform
     function gsl_fft_real_unpack (real_coefficient, complex_coefficient, &
          stride, n) bind(c)
       import :: c_size_t, c_ptr, c_int
       type(c_ptr), value :: real_coefficient, complex_coefficient
       integer(c_size_t), value :: stride, n
       integer(c_int) :: gsl_fft_real_unpack
     end function gsl_fft_real_unpack
     function gsl_fft_halfcomplex_unpack (halfcomplex_coefficient, complex_coefficient, &
          stride, n) bind(c)
       import :: c_size_t, c_ptr, c_int
       type(c_ptr), value :: halfcomplex_coefficient, complex_coefficient
       integer(c_size_t), value :: stride, n
       integer(c_int) :: gsl_fft_halfcomplex_unpack
     end function gsl_fft_halfcomplex_unpack
  end interface

contains
  function fgsl_fft_complex_radix2_forward(data, stride, n)
    complex(fgsl_double_complex), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_int) :: fgsl_fft_complex_radix2_forward
    fgsl_fft_complex_radix2_forward = gsl_fft_complex_radix2_forward( &
         c_loc(data), stride, n)
  end function fgsl_fft_complex_radix2_forward
  function fgsl_fft_complex_radix2_transform(data, stride, n, sign)
    complex(fgsl_double_complex), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_int), intent(in) :: sign
    integer(fgsl_int) :: fgsl_fft_complex_radix2_transform
    fgsl_fft_complex_radix2_transform = gsl_fft_complex_radix2_transform( &
         c_loc(data), stride, n,sign)
  end function fgsl_fft_complex_radix2_transform
  function fgsl_fft_complex_radix2_backward(data, stride, n)
    complex(fgsl_double_complex), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_int) :: fgsl_fft_complex_radix2_backward
    fgsl_fft_complex_radix2_backward = gsl_fft_complex_radix2_backward( &
         c_loc(data), stride, n)
  end function fgsl_fft_complex_radix2_backward
  function fgsl_fft_complex_radix2_inverse(data, stride, n)
    complex(fgsl_double_complex), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_int) :: fgsl_fft_complex_radix2_inverse
    fgsl_fft_complex_radix2_inverse = gsl_fft_complex_radix2_inverse( &
         c_loc(data), stride, n)
  end function fgsl_fft_complex_radix2_inverse
  function fgsl_fft_complex_radix2_dif_forward(data, stride, n)
    complex(fgsl_double_complex), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_int) :: fgsl_fft_complex_radix2_dif_forward
    fgsl_fft_complex_radix2_dif_forward = gsl_fft_complex_radix2_dif_forward( &
         c_loc(data), stride, n)
  end function fgsl_fft_complex_radix2_dif_forward
  function fgsl_fft_complex_radix2_dif_transform(data, stride, n, sign)
    complex(fgsl_double_complex), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_int), intent(in) :: sign
    integer(fgsl_int) :: fgsl_fft_complex_radix2_dif_transform
    fgsl_fft_complex_radix2_dif_transform = gsl_fft_complex_radix2_dif_transform( &
         c_loc(data), stride, n,sign)
  end function fgsl_fft_complex_radix2_dif_transform
  function fgsl_fft_complex_radix2_dif_backward(data, stride, n)
    complex(fgsl_double_complex), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_int) :: fgsl_fft_complex_radix2_dif_backward
    fgsl_fft_complex_radix2_dif_backward = gsl_fft_complex_radix2_dif_backward( &
         c_loc(data), stride, n)
  end function fgsl_fft_complex_radix2_dif_backward
  function fgsl_fft_complex_radix2_dif_inverse(data, stride, n)
    complex(fgsl_double_complex), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_int) :: fgsl_fft_complex_radix2_dif_inverse
    fgsl_fft_complex_radix2_dif_inverse = gsl_fft_complex_radix2_dif_inverse( &
         c_loc(data), stride, n)
  end function fgsl_fft_complex_radix2_dif_inverse
  function fgsl_fft_complex_wavetable_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_fft_complex_wavetable) :: fgsl_fft_complex_wavetable_alloc
    fgsl_fft_complex_wavetable_alloc%gsl_fft_complex_wavetable = &
         gsl_fft_complex_wavetable_alloc(n)
  end function fgsl_fft_complex_wavetable_alloc
  subroutine fgsl_fft_complex_wavetable_free(w) 
    type(fgsl_fft_complex_wavetable) :: w
    call gsl_fft_complex_wavetable_free(w%gsl_fft_complex_wavetable)
  end subroutine fgsl_fft_complex_wavetable_free
  function fgsl_fft_complex_workspace_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_fft_complex_workspace) :: fgsl_fft_complex_workspace_alloc
    fgsl_fft_complex_workspace_alloc%gsl_fft_complex_workspace = &
         gsl_fft_complex_workspace_alloc(n)
  end function fgsl_fft_complex_workspace_alloc
  subroutine fgsl_fft_complex_workspace_free(w) 
    type(fgsl_fft_complex_workspace) :: w
    call gsl_fft_complex_workspace_free(w%gsl_fft_complex_workspace)
  end subroutine fgsl_fft_complex_workspace_free
  function fgsl_fft_complex_forward(data, stride, n, wavetable, work)
    complex(fgsl_double_complex), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    type(fgsl_fft_complex_wavetable), intent(in) :: wavetable
    type(fgsl_fft_complex_workspace) :: work
    integer(fgsl_int) :: fgsl_fft_complex_forward
    fgsl_fft_complex_forward = gsl_fft_complex_forward( &
         c_loc(data), stride, n, wavetable%gsl_fft_complex_wavetable, &
         work%gsl_fft_complex_workspace)
  end function fgsl_fft_complex_forward
  function fgsl_fft_complex_transform(data, stride, n, wavetable, work, sign)
    complex(fgsl_double_complex), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    type(fgsl_fft_complex_wavetable), intent(in) :: wavetable
    type(fgsl_fft_complex_workspace) :: work
    integer(fgsl_int), intent(in) :: sign
    integer(fgsl_int) :: fgsl_fft_complex_transform
    fgsl_fft_complex_transform = gsl_fft_complex_transform( &
         c_loc(data), stride, n, wavetable%gsl_fft_complex_wavetable, &
         work%gsl_fft_complex_workspace, sign)
  end function fgsl_fft_complex_transform
  function fgsl_fft_complex_backward(data, stride, n, wavetable, work)
    complex(fgsl_double_complex), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    type(fgsl_fft_complex_wavetable), intent(in) :: wavetable
    type(fgsl_fft_complex_workspace) :: work
    integer(fgsl_int) :: fgsl_fft_complex_backward
    fgsl_fft_complex_backward = gsl_fft_complex_backward( &
         c_loc(data), stride, n, wavetable%gsl_fft_complex_wavetable, &
         work%gsl_fft_complex_workspace)
  end function fgsl_fft_complex_backward
  function fgsl_fft_complex_inverse(data, stride, n, wavetable, work)
    complex(fgsl_double_complex), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    type(fgsl_fft_complex_wavetable), intent(in) :: wavetable
    type(fgsl_fft_complex_workspace) :: work
    integer(fgsl_int) :: fgsl_fft_complex_inverse
    fgsl_fft_complex_inverse = gsl_fft_complex_inverse( &
         c_loc(data), stride, n, wavetable%gsl_fft_complex_wavetable, &
         work%gsl_fft_complex_workspace)
  end function fgsl_fft_complex_inverse
  function fgsl_fft_real_radix2_transform(data, stride, n)
    real(fgsl_double), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_int) :: fgsl_fft_real_radix2_transform
    fgsl_fft_real_radix2_transform = gsl_fft_real_radix2_transform( &
         c_loc(data), stride, n)
  end function fgsl_fft_real_radix2_transform
  function fgsl_fft_halfcomplex_radix2_inverse(data, stride, n)
    real(fgsl_double), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_int) :: fgsl_fft_halfcomplex_radix2_inverse
    fgsl_fft_halfcomplex_radix2_inverse = gsl_fft_halfcomplex_radix2_inverse( &
         c_loc(data), stride, n)
  end function fgsl_fft_halfcomplex_radix2_inverse
  function fgsl_fft_halfcomplex_radix2_backward(data, stride, n)
    real(fgsl_double), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_int) :: fgsl_fft_halfcomplex_radix2_backward
    fgsl_fft_halfcomplex_radix2_backward = gsl_fft_halfcomplex_radix2_backward( &
         c_loc(data), stride, n)
  end function fgsl_fft_halfcomplex_radix2_backward
  function fgsl_fft_real_wavetable_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_fft_real_wavetable) :: fgsl_fft_real_wavetable_alloc
    fgsl_fft_real_wavetable_alloc%gsl_fft_real_wavetable = &
         gsl_fft_real_wavetable_alloc(n)
  end function fgsl_fft_real_wavetable_alloc
  subroutine fgsl_fft_real_wavetable_free(w) 
    type(fgsl_fft_real_wavetable) :: w
    call gsl_fft_real_wavetable_free(w%gsl_fft_real_wavetable)
  end subroutine fgsl_fft_real_wavetable_free
  function fgsl_fft_halfcomplex_wavetable_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_fft_halfcomplex_wavetable) :: fgsl_fft_halfcomplex_wavetable_alloc
    fgsl_fft_halfcomplex_wavetable_alloc%gsl_fft_halfcomplex_wavetable = &
         gsl_fft_halfcomplex_wavetable_alloc(n)
  end function fgsl_fft_halfcomplex_wavetable_alloc
  subroutine fgsl_fft_halfcomplex_wavetable_free(w) 
    type(fgsl_fft_halfcomplex_wavetable) :: w
    call gsl_fft_halfcomplex_wavetable_free(w%gsl_fft_halfcomplex_wavetable)
  end subroutine fgsl_fft_halfcomplex_wavetable_free
  function fgsl_fft_real_workspace_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_fft_real_workspace) :: fgsl_fft_real_workspace_alloc
    fgsl_fft_real_workspace_alloc%gsl_fft_real_workspace = &
         gsl_fft_real_workspace_alloc(n)
  end function fgsl_fft_real_workspace_alloc
  subroutine fgsl_fft_real_workspace_free(w) 
    type(fgsl_fft_real_workspace) :: w
    call gsl_fft_real_workspace_free(w%gsl_fft_real_workspace)
  end subroutine fgsl_fft_real_workspace_free
  function fgsl_fft_real_transform(data, stride, n, wavetable, work)
    real(fgsl_double), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    type(fgsl_fft_real_wavetable), intent(in) :: wavetable
    type(fgsl_fft_real_workspace) :: work
    integer(fgsl_int) :: fgsl_fft_real_transform
    fgsl_fft_real_transform = gsl_fft_real_transform( &
         c_loc(data), stride, n, wavetable%gsl_fft_real_wavetable, &
         work%gsl_fft_real_workspace)
  end function fgsl_fft_real_transform
  function fgsl_fft_halfcomplex_transform(data, stride, n, wavetable, work)
    real(fgsl_double), intent(inout), target :: data(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    type(fgsl_fft_halfcomplex_wavetable), intent(in) :: wavetable
    type(fgsl_fft_real_workspace) :: work
    integer(fgsl_int) :: fgsl_fft_halfcomplex_transform
    fgsl_fft_halfcomplex_transform = gsl_fft_halfcomplex_transform( &
         c_loc(data), stride, n, wavetable%gsl_fft_halfcomplex_wavetable, &
         work%gsl_fft_real_workspace)
  end function fgsl_fft_halfcomplex_transform
  function fgsl_fft_real_unpack (real_coefficient, complex_coefficient, &
       stride, n)
    real(fgsl_double), intent(in), target :: real_coefficient(*)
    complex(fgsl_double_complex), intent(inout), target :: complex_coefficient(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_int) :: fgsl_fft_real_unpack
    fgsl_fft_real_unpack = gsl_fft_real_unpack(c_loc(real_coefficient), &
         c_loc(complex_coefficient), stride, n)
  end function fgsl_fft_real_unpack
  function fgsl_fft_halfcomplex_unpack (halfcomplex_coefficient, &
       complex_coefficient, stride, n)
    real(fgsl_double), intent(in), target :: halfcomplex_coefficient(*)
    complex(fgsl_double_complex), intent(inout), target :: complex_coefficient(*)
    integer(fgsl_size_t), intent(in) :: stride, n
    integer(fgsl_int) :: fgsl_fft_halfcomplex_unpack
    fgsl_fft_halfcomplex_unpack = gsl_fft_halfcomplex_unpack( &
         c_loc(halfcomplex_coefficient), &
         c_loc(complex_coefficient), stride, n)
  end function fgsl_fft_halfcomplex_unpack

end module fgsl_fft
