module fgsl_qrngen
  !> Quasi-random sequences
  use fgsl_base
  implicit none

  private :: gsl_qrng_alloc, gsl_qrng_free, gsl_qrng_init, gsl_qrng_get, &
    gsl_qrng_name, gsl_qrng_memcpy, gsl_qrng_clone, fgsl_aux_qrng_assign

  !
  ! Types
  type, public :: fgsl_qrng
     private
     type(c_ptr) :: gsl_qrng
  end type fgsl_qrng
  type, public :: fgsl_qrng_type
     private
     integer(fgsl_int) :: type = 0
  end type fgsl_qrng_type
  type(fgsl_qrng_type), parameter, public :: &
       fgsl_qrng_niederreiter_2 = fgsl_qrng_type(1), &
       fgsl_qrng_sobol = fgsl_qrng_type(2), &
       fgsl_qrng_halton =  fgsl_qrng_type(3), &
       fgsl_qrng_reversehalton =  fgsl_qrng_type(4)
  !
  ! Generic interfaces

  interface fgsl_well_defined
     module procedure fgsl_qrng_status
  end interface
  !
  ! C interfaces
  interface
	  function gsl_qrng_alloc(t, d) bind(c)
	    import
	    type(c_ptr), value :: t
	    integer(c_int), value :: d
	    type(c_ptr) :: gsl_qrng_alloc
	  end function gsl_qrng_alloc
	!
	  subroutine gsl_qrng_free(q) bind(c)
	    import
	    type(c_ptr), value :: q
	  end subroutine gsl_qrng_free
	  subroutine gsl_qrng_init(q) bind(c)
	    import
	    type(c_ptr), value :: q
	  end subroutine gsl_qrng_init
	  function gsl_qrng_get(q, x) bind(c)
	    import
	    type(c_ptr), value :: q, x
	    integer(c_int) :: gsl_qrng_get
	  end function gsl_qrng_get
	  function gsl_qrng_name(q) bind(c)
	    import
	    type(c_ptr), value :: q
	    type(c_ptr) :: gsl_qrng_name
	  end function gsl_qrng_name
	  function gsl_qrng_memcpy(cpy, src) bind(c)
	    import
	    type(c_ptr), value :: cpy
	    type(c_ptr), value :: src
	    integer(c_int) :: gsl_qrng_memcpy
	  end function gsl_qrng_memcpy
	  function gsl_qrng_clone(q) bind(c)
	    import
	    type(c_ptr), value :: q
	    type(c_ptr) :: gsl_qrng_clone
	  end function gsl_qrng_clone
	!
	  function fgsl_aux_qrng_assign(i) bind(c)
	    import
	    integer(c_int), value :: i
	    type(c_ptr) :: fgsl_aux_qrng_assign
	  end function fgsl_aux_qrng_assign  
  end interface
contains
  function fgsl_qrng_alloc(t, d)
    type(fgsl_qrng_type), intent(in) :: t
    integer(fgsl_int), intent(in) :: d
    type(fgsl_qrng) :: fgsl_qrng_alloc
!
    type(c_ptr) :: type
    type = fgsl_aux_qrng_assign(t%type)
    fgsl_qrng_alloc%gsl_qrng = gsl_qrng_alloc(type, d)
  end function fgsl_qrng_alloc
  subroutine fgsl_qrng_free(r)
    type(fgsl_qrng), intent(inout) :: r
    call gsl_qrng_free(r%gsl_qrng)
  end subroutine fgsl_qrng_free
  subroutine fgsl_qrng_init(r)
    type(fgsl_qrng), intent(inout) :: r
    call gsl_qrng_init(r%gsl_qrng)
  end subroutine fgsl_qrng_init
  function fgsl_qrng_get(q, x)
    type(fgsl_qrng), intent(in) :: q
    real(fgsl_double), intent(out), target, contiguous :: x(:)
    integer(fgsl_int) :: fgsl_qrng_get
    fgsl_qrng_get = gsl_qrng_get(q%gsl_qrng, c_loc(x))
  end function fgsl_qrng_get
  function fgsl_qrng_name(q)
    type(fgsl_qrng), intent(in) :: q
    character(kind=fgsl_char, len=fgsl_strmax) :: fgsl_qrng_name
!
    type(c_ptr) :: name
    name = gsl_qrng_name(q%gsl_qrng)
    fgsl_qrng_name = fgsl_name(name)
  end function fgsl_qrng_name
! FIXME: state and size routines not (yet?) implemented
  function fgsl_qrng_memcpy(cpy, src)
    type(fgsl_qrng), intent(inout) :: cpy
    type(fgsl_qrng), intent(in) :: src
    integer(fgsl_int) :: fgsl_qrng_memcpy
    fgsl_qrng_memcpy = gsl_qrng_memcpy(cpy%gsl_qrng, src%gsl_qrng)
  end function fgsl_qrng_memcpy
  function fgsl_qrng_clone(q)
    type(fgsl_qrng), intent(in) :: q
    type(fgsl_qrng) :: fgsl_qrng_clone
    fgsl_qrng_clone%gsl_qrng = gsl_qrng_clone(q%gsl_qrng)
  end function fgsl_qrng_clone
  function fgsl_qrng_status(qrng)
    type(fgsl_qrng), intent(in) :: qrng
    logical :: fgsl_qrng_status
    fgsl_qrng_status = .true.
    if (.not. c_associated(qrng%gsl_qrng)) fgsl_qrng_status = .false.
  end function fgsl_qrng_status  
end module fgsl_qrngen
