module fgsl_rngen
  !> Random number generation
  use fgsl_base
  use fgsl_io
  implicit none

  private :: gsl_rng_alloc, gsl_rng_set, gsl_rng_free, gsl_rng_get, gsl_rng_uniform, &
     gsl_rng_uniform_pos, gsl_rng_uniform_int, gsl_rng_name, gsl_rng_max, gsl_rng_min, &
     gsl_rng_env_setup, gsl_rng_memcpy, gsl_rng_clone, gsl_rng_fwrite, gsl_rng_fread
     
  private :: fgsl_aux_rng_assign
    
  ! Types: Random numbers
  !
  type, public :: fgsl_rng
     type(c_ptr) :: gsl_rng = c_null_ptr
  end type fgsl_rng
  type, public :: fgsl_rng_type
     private
     type(c_ptr) :: gsl_rng_type = c_null_ptr
     integer(fgsl_int) :: type = 0
  end type fgsl_rng_type
! Note: we need a dynamic component here, since
! fgsl_rng_default needs to change at run time.
! fgsl_rng_default will be set by fgsl_rng_env_setup,
! and the static objects will be all set at the
! first call of fgsl_rng_alloc
!  integer, parameter :: rngmax = 61
! check new g95  type(fgsl_rng_type) :: fgsl_rng_allgen(rngmax) = (/(fgsl_rng_type(c_null_ptr, i)), i=1, rngmax/)
! cannot have protected attribute since modified by fgsl_rng_alloc
  type(fgsl_rng_type), public :: &
       fgsl_rng_default = fgsl_rng_type(c_null_ptr, -1), &
       fgsl_rng_borosh13 = fgsl_rng_type(c_null_ptr, 1), &
       fgsl_rng_coveyou = fgsl_rng_type(c_null_ptr, 2), &
       fgsl_rng_cmrg = fgsl_rng_type(c_null_ptr, 3), &
       fgsl_rng_fishman18 = fgsl_rng_type(c_null_ptr, 4), &
       fgsl_rng_fishman20 = fgsl_rng_type(c_null_ptr, 5), &
       fgsl_rng_fishman2x = fgsl_rng_type(c_null_ptr, 6), &
       fgsl_rng_gfsr4 = fgsl_rng_type(c_null_ptr, 7), &
       fgsl_rng_knuthran = fgsl_rng_type(c_null_ptr, 8), &
       fgsl_rng_knuthran2 = fgsl_rng_type(c_null_ptr, 9), &
       fgsl_rng_lecuyer21 = fgsl_rng_type(c_null_ptr, 10), &
       fgsl_rng_minstd = fgsl_rng_type(c_null_ptr, 11), &
       fgsl_rng_mrg = fgsl_rng_type(c_null_ptr, 12), &
       fgsl_rng_mt19937 = fgsl_rng_type(c_null_ptr, 13), &
       fgsl_rng_mt19937_1999 = fgsl_rng_type(c_null_ptr, 14), &
       fgsl_rng_mt19937_1998 = fgsl_rng_type(c_null_ptr, 15), &
       fgsl_rng_r250 = fgsl_rng_type(c_null_ptr, 16), &
       fgsl_rng_ran0 = fgsl_rng_type(c_null_ptr, 17), &
       fgsl_rng_ran1 = fgsl_rng_type(c_null_ptr, 18), &
       fgsl_rng_ran2 = fgsl_rng_type(c_null_ptr, 19), &
       fgsl_rng_ran3 = fgsl_rng_type(c_null_ptr, 20), &
       fgsl_rng_rand = fgsl_rng_type(c_null_ptr, 21), &
       fgsl_rng_rand48 = fgsl_rng_type(c_null_ptr, 22), &
       fgsl_rng_random128_bsd = fgsl_rng_type(c_null_ptr, 23), &
       fgsl_rng_random128_glibc2 = fgsl_rng_type(c_null_ptr, 24), &
       fgsl_rng_random128_libc5 = fgsl_rng_type(c_null_ptr, 25), &
       fgsl_rng_random256_bsd = fgsl_rng_type(c_null_ptr, 26), &
       fgsl_rng_random256_glibc2 = fgsl_rng_type(c_null_ptr, 27), &
       fgsl_rng_random256_libc5 = fgsl_rng_type(c_null_ptr, 28), &
       fgsl_rng_random32_bsd = fgsl_rng_type(c_null_ptr, 29), &
       fgsl_rng_random32_glibc2 = fgsl_rng_type(c_null_ptr, 30), &
       fgsl_rng_random32_libc5 = fgsl_rng_type(c_null_ptr, 31), &
       fgsl_rng_random64_bsd = fgsl_rng_type(c_null_ptr, 32), &
       fgsl_rng_random64_glibc2 = fgsl_rng_type(c_null_ptr, 33), &
       fgsl_rng_random64_libc5 = fgsl_rng_type(c_null_ptr, 34), &
       fgsl_rng_random8_bsd = fgsl_rng_type(c_null_ptr, 35)
  type(fgsl_rng_type), public ::  &
       fgsl_rng_random8_glibc2 = fgsl_rng_type(c_null_ptr, 36), &
       fgsl_rng_random8_libc5 = fgsl_rng_type(c_null_ptr, 37), &
       fgsl_rng_random_bsd = fgsl_rng_type(c_null_ptr, 38), &
       fgsl_rng_random_glibc2 = fgsl_rng_type(c_null_ptr, 39), &
       fgsl_rng_random_libc5 = fgsl_rng_type(c_null_ptr, 40), &
       fgsl_rng_randu = fgsl_rng_type(c_null_ptr, 41), &
       fgsl_rng_ranf = fgsl_rng_type(c_null_ptr, 42), &
       fgsl_rng_ranlux = fgsl_rng_type(c_null_ptr, 43), &
       fgsl_rng_ranlux389 = fgsl_rng_type(c_null_ptr, 44), &
       fgsl_rng_ranlxd1 = fgsl_rng_type(c_null_ptr, 45), &
       fgsl_rng_ranlxd2 = fgsl_rng_type(c_null_ptr, 46), &
       fgsl_rng_ranlxs0 = fgsl_rng_type(c_null_ptr, 47), &
       fgsl_rng_ranlxs1 = fgsl_rng_type(c_null_ptr, 48), &
       fgsl_rng_ranlxs2 = fgsl_rng_type(c_null_ptr, 49), &
       fgsl_rng_ranmar = fgsl_rng_type(c_null_ptr, 50), &
       fgsl_rng_slatec = fgsl_rng_type(c_null_ptr, 51), &
       fgsl_rng_taus = fgsl_rng_type(c_null_ptr, 52), &
       fgsl_rng_taus2 = fgsl_rng_type(c_null_ptr, 53), &
       fgsl_rng_taus113 = fgsl_rng_type(c_null_ptr, 54), &
       fgsl_rng_transputer = fgsl_rng_type(c_null_ptr, 55), &
       fgsl_rng_tt800 = fgsl_rng_type(c_null_ptr, 56), &
       fgsl_rng_uni = fgsl_rng_type(c_null_ptr, 57), &
       fgsl_rng_uni32 = fgsl_rng_type(c_null_ptr, 58), &
       fgsl_rng_vax = fgsl_rng_type(c_null_ptr, 59), &
       fgsl_rng_waterman14 = fgsl_rng_type(c_null_ptr, 60), &
       fgsl_rng_zuf = fgsl_rng_type(c_null_ptr, 61), &
       fgsl_rng_knuthran2002 = fgsl_rng_type(c_null_ptr, 62)
  integer(fgsl_long), public, bind(c, name='gsl_rng_default_seed') :: fgsl_rng_default_seed
  !
  !> Generic interfaces
  interface fgsl_well_defined
     module procedure fgsl_rng_status
  end interface
  interface fgsl_obj_c_ptr
     module procedure fgsl_rng_c_ptr
  end interface 
  !
  !> C interfaces
  interface
	  function gsl_rng_alloc(t) bind(c)
	    import
	    type(c_ptr), value :: t
	    type(c_ptr) :: gsl_rng_alloc
	  end function gsl_rng_alloc
	  subroutine gsl_rng_set(r, s) bind(c)
	    import
	    type(c_ptr), value :: r
	    integer(c_long), value :: s
	  end subroutine gsl_rng_set
	  subroutine gsl_rng_free(r) bind(c)
	    import
	    type(c_ptr), value :: r
	  end subroutine gsl_rng_free
	  function gsl_rng_get(r) bind(c)
	    import
	    type(c_ptr), value :: r
	    integer(c_long) :: gsl_rng_get
	  end function gsl_rng_get
	  function gsl_rng_uniform(r) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double) :: gsl_rng_uniform
	  end function gsl_rng_uniform
	  function gsl_rng_uniform_pos(r) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double) :: gsl_rng_uniform_pos
	  end function gsl_rng_uniform_pos
	  function gsl_rng_uniform_int(r, n) bind(c)
	    import
	    type(c_ptr), value :: r
	    integer(c_long), value :: n
	    integer(c_long) :: gsl_rng_uniform_int
	  end function gsl_rng_uniform_int
	  function gsl_rng_name(r) bind(c)
	    import
	    type(c_ptr), value :: r
	    type(c_ptr) :: gsl_rng_name
	  end function gsl_rng_name
	  function gsl_rng_max(r) bind(c)
	    import
	    type(c_ptr), value :: r
	    integer(c_long) :: gsl_rng_max
	  end function gsl_rng_max
	  function gsl_rng_min(r) bind(c)
	    import
	    type(c_ptr), value :: r
	    integer(c_long) :: gsl_rng_min
	  end function gsl_rng_min
	  function gsl_rng_env_setup() bind(c)
	    import
	    type(c_ptr) :: gsl_rng_env_setup
	  end function gsl_rng_env_setup
	  function gsl_rng_memcpy(cpy, src) bind(c)
	    import
	    type(c_ptr), value :: cpy
	    type(c_ptr), value :: src
	    integer(c_int) :: gsl_rng_memcpy
	  end function gsl_rng_memcpy
	  function gsl_rng_clone(r) bind(c)
	    import
	    type(c_ptr), value :: r
	    type(c_ptr) :: gsl_rng_clone
	  end function gsl_rng_clone
	  function gsl_rng_fwrite(stream, r) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: stream, r
	    integer(c_int) :: gsl_rng_fwrite
	  end function gsl_rng_fwrite
	  function gsl_rng_fread(stream, r) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: stream, r
	    integer(c_int) :: gsl_rng_fread
	  end function gsl_rng_fread
	  function fgsl_aux_rng_assign(i) bind(c)
	    import
	    integer(c_int), value :: i
	    type(c_ptr) :: fgsl_aux_rng_assign
	  end function fgsl_aux_rng_assign
  end interface
contains
! API
!
  function fgsl_rng_alloc(t)
    type(fgsl_rng_type), intent(inout) :: t
    type(fgsl_rng) :: fgsl_rng_alloc
    if (.not. c_associated(t%gsl_rng_type)) then
       t%gsl_rng_type = fgsl_aux_rng_assign(t%type)
    end if
    if (c_associated(t%gsl_rng_type)) &
         fgsl_rng_alloc%gsl_rng = gsl_rng_alloc(t%gsl_rng_type)
  end function fgsl_rng_alloc
  subroutine fgsl_rng_set(r, s)
    type(fgsl_rng), intent(inout) :: r
    integer(fgsl_long), intent(in) :: s
    call gsl_rng_set(r%gsl_rng, s)
  end subroutine fgsl_rng_set
  subroutine fgsl_rng_free(r)
    type(fgsl_rng), intent(inout) :: r
    call gsl_rng_free(r%gsl_rng)
  end subroutine fgsl_rng_free
  function fgsl_rng_get(r)
    type(fgsl_rng), intent(in) :: r
    integer(fgsl_long) :: fgsl_rng_get
    fgsl_rng_get = gsl_rng_get(r%gsl_rng)
  end function fgsl_rng_get
  function fgsl_rng_uniform(r)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double) :: fgsl_rng_uniform
    fgsl_rng_uniform = gsl_rng_uniform(r%gsl_rng)
  end function fgsl_rng_uniform
  function fgsl_rng_uniform_pos(r)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double) :: fgsl_rng_uniform_pos
    fgsl_rng_uniform_pos = gsl_rng_uniform_pos(r%gsl_rng)
  end function fgsl_rng_uniform_pos
  function fgsl_rng_uniform_int(r, n)
    type(fgsl_rng), intent(in) :: r
    integer(fgsl_long), intent(in) :: n
    integer(fgsl_long) :: fgsl_rng_uniform_int
    fgsl_rng_uniform_int = gsl_rng_uniform_int(r%gsl_rng, n)
  end function fgsl_rng_uniform_int
  function fgsl_rng_name(r)
    type(fgsl_rng), intent(in) :: r
    character(kind=fgsl_char, len=fgsl_strmax) :: fgsl_rng_name
!
    type(c_ptr) :: name
    name = gsl_rng_name(r%gsl_rng)
    fgsl_rng_name = fgsl_name(name)
  end function fgsl_rng_name
  function fgsl_rng_max(r)
    type(fgsl_rng), intent(in) :: r
    integer(fgsl_long) :: fgsl_rng_max
    fgsl_rng_max = gsl_rng_max(r%gsl_rng)
  end function fgsl_rng_max
  function fgsl_rng_min(r)
    type(fgsl_rng), intent(in) :: r
    integer(fgsl_long) :: fgsl_rng_min
    fgsl_rng_min = gsl_rng_min(r%gsl_rng)
  end function fgsl_rng_min
! FIXME: state, size and types_setup routines not (yet?) implemented
  function fgsl_rng_env_setup()
    type(fgsl_rng_type) :: fgsl_rng_env_setup
    fgsl_rng_env_setup%gsl_rng_type = gsl_rng_env_setup()
  end function fgsl_rng_env_setup
  function fgsl_rng_memcpy(cpy, src)
    type(fgsl_rng), intent(inout) :: cpy
    type(fgsl_rng), intent(in) :: src
    integer(fgsl_int) :: fgsl_rng_memcpy
    fgsl_rng_memcpy = gsl_rng_memcpy(cpy%gsl_rng, src%gsl_rng)
  end function fgsl_rng_memcpy
  function fgsl_rng_clone(r)
    type(fgsl_rng), intent(in) :: r
    type(fgsl_rng) :: fgsl_rng_clone
    fgsl_rng_clone%gsl_rng = gsl_rng_clone(r%gsl_rng)
  end function fgsl_rng_clone
  function fgsl_rng_fwrite(stream, r)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_rng), intent(in) :: r
    integer(fgsl_int) :: fgsl_rng_fwrite
    fgsl_rng_fwrite = gsl_rng_fwrite(stream%gsl_file, r%gsl_rng)
  end function fgsl_rng_fwrite
  function fgsl_rng_fread(stream, r)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_rng), intent(inout) :: r
    integer(fgsl_int) :: fgsl_rng_fread
    fgsl_rng_fread = gsl_rng_fread(stream%gsl_file, r%gsl_rng)
  end function fgsl_rng_fread
  function fgsl_rng_status(rng)
    type(fgsl_rng), intent(in) :: rng
    logical :: fgsl_rng_status
    fgsl_rng_status = .true.
    if (.not. c_associated(rng%gsl_rng)) fgsl_rng_status = .false.
  end function fgsl_rng_status
  subroutine fgsl_rng_c_ptr(res, src)
    type(c_ptr), intent(in) :: src
    type(fgsl_rng), intent(out) :: res
    res%gsl_rng = src
  end subroutine fgsl_rng_c_ptr
end module fgsl_rngen
