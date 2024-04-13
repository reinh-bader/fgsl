!-*-f90-*-
module fgsl_poly
  !> Polynomials
  use fgsl_base
  use fgsl_errno
  implicit none

  private :: gsl_poly_eval, gsl_poly_complex_eval, gsl_complex_poly_complex_eval, &
       gsl_poly_eval_derivs, gsl_poly_dd_init, gsl_poly_dd_eval, gsl_poly_dd_taylor, &
       gsl_poly_dd_hermite_init, gsl_poly_solve_quadratic, gsl_poly_solve_cubic, &
       gsl_poly_complex_solve_cubic, gsl_poly_complex_workspace_alloc, &
       gsl_poly_complex_workspace_free, gsl_poly_complex_solve

  !
  ! Types
  type, public :: fgsl_poly_complex_workspace
     private
     type(c_ptr) :: gsl_poly_complex_workspace
  end type fgsl_poly_complex_workspace

  !
  ! generic interfaces
  interface fgsl_well_defined
     module procedure fgsl_poly_complex_workspace_stat
  end interface fgsl_well_defined

  !
  ! C interfaces
  interface
     function gsl_poly_eval(c, len, x) bind(c)
       import
       type(c_ptr), value :: c
       integer(c_int), value :: len
       real(c_double), value :: x
       real(c_double) :: gsl_poly_eval
     end function gsl_poly_eval
     function gsl_poly_complex_eval(c, len, z) bind(c)
       import
       type(c_ptr), value :: c
       integer(c_int), value :: len
       complex(c_double_complex), value :: z
       complex(c_double_complex) :: gsl_poly_complex_eval
     end function gsl_poly_complex_eval
     function gsl_complex_poly_complex_eval(c, len, z) bind(c)
       import
       type(c_ptr), value :: c
       integer(c_int), value :: len
       complex(c_double_complex), value :: z
       complex(c_double_complex) :: gsl_complex_poly_complex_eval
     end function gsl_complex_poly_complex_eval
     function gsl_poly_eval_derivs(c, lenc, x, res, lenres) bind(c)
       import :: c_double, c_size_t, c_int, c_ptr
       integer(c_int) :: gsl_poly_eval_derivs
       integer(c_size_t), value :: lenc, lenres
       type(c_ptr), value :: c
       type(c_ptr), value :: res
       real(c_double), value :: x
     end function gsl_poly_eval_derivs
     function gsl_poly_dd_init(dd, x, y, size) bind(c)
       import
       type(c_ptr), value :: dd
       type(c_ptr), value :: x, y
       integer(c_size_t), value :: size
       integer(c_int) :: gsl_poly_dd_init
     end function gsl_poly_dd_init
     function gsl_poly_dd_eval(dd, xa, size, x) bind(c)
       import
       type(c_ptr), value :: dd, xa
       real(c_double), value :: x
       integer(c_size_t), value :: size
       real(c_double) :: gsl_poly_dd_eval
     end function gsl_poly_dd_eval
     function gsl_poly_dd_taylor(c, xp, dd, x, size, w) bind(c)
       import
       type(c_ptr), value :: c
       real(c_double), value :: xp
       type(c_ptr), value :: dd, x
       type(c_ptr), value :: w
       integer(c_size_t), value :: size
       integer(c_int) :: gsl_poly_dd_taylor
     end function gsl_poly_dd_taylor
     function gsl_poly_dd_hermite_init(dd, z, xa, ya, dya, size) bind(c)
       import
       type(c_ptr), value :: dd, z
       type(c_ptr), value :: xa, ya, dya
       integer(c_size_t), value :: size
       integer(c_int) :: gsl_poly_dd_hermite_init
     end function gsl_poly_dd_hermite_init
     function gsl_poly_solve_quadratic(a, b, c, x0, x1) bind(c)
       import
       real(c_double), value :: a, b, c
       real(c_double) :: x0, x1
       integer(c_int) :: gsl_poly_solve_quadratic
     end function gsl_poly_solve_quadratic
     function gsl_poly_complex_solve_quadratic(a, b, c, x0, x1) bind(c, name='gsl_poly_complex_solve_quadratic')
       import
       real(c_double), value :: a, b, c
       complex(c_double_complex) :: x0, x1
       integer(c_int) :: gsl_poly_complex_solve_quadratic
     end function gsl_poly_complex_solve_quadratic
     function gsl_poly_solve_cubic(a, b, c, x0, x1, x2) bind(c)
       import
       real(c_double), value :: a, b, c
       real(c_double) :: x0, x1, x2
       integer(c_int) :: gsl_poly_solve_cubic
     end function gsl_poly_solve_cubic
     function gsl_poly_complex_solve_cubic(a, b, c, x0, x1, x2) bind(c, name='gsl_poly_complex_solve_cubic')
       import
       real(c_double), value :: a, b, c
       complex(c_double_complex) :: x0, x1, x2
       integer(c_int) :: gsl_poly_complex_solve_cubic
     end function gsl_poly_complex_solve_cubic
     function gsl_poly_complex_workspace_alloc(n) bind(c, name='gsl_poly_complex_workspace_alloc')
       import
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_poly_complex_workspace_alloc
     end function gsl_poly_complex_workspace_alloc
     subroutine gsl_poly_complex_workspace_free(w) bind(c, name='gsl_poly_complex_workspace_free')
       import
       type(c_ptr), value :: w
     end subroutine gsl_poly_complex_workspace_free
     function gsl_poly_complex_solve(a, n, w, z) bind(c, name='gsl_poly_complex_solve')
       import
       type(c_ptr), value :: a
       integer(c_size_t), value :: n
       type(c_ptr), value :: w
       type(c_ptr), value :: z
       integer(c_int) :: gsl_poly_complex_solve
     end function gsl_poly_complex_solve
  end interface
contains
  !
  ! API
  function fgsl_poly_eval(c, x)
    real(fgsl_double), intent(in), target, contiguous :: c(:)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: fgsl_poly_eval
    fgsl_poly_eval = gsl_poly_eval(c_loc(c), size(c, kind=fgsl_int), x)
  end function fgsl_poly_eval
  function fgsl_poly_complex_eval(c, z)
    real(fgsl_double), intent(in), target, contiguous :: c(:)
    complex(fgsl_double_complex), intent(in) :: z
    complex(fgsl_double_complex) :: fgsl_poly_complex_eval
    complex(c_double_complex) :: zz
    zz = z
    fgsl_poly_complex_eval = gsl_poly_complex_eval(c_loc(c), size(c, kind=fgsl_int), zz)
  end function fgsl_poly_complex_eval
  function fgsl_complex_poly_complex_eval(c, z)
    complex(fgsl_double_complex), intent(in) :: c(:)
    complex(fgsl_double_complex), intent(in) :: z
    complex(fgsl_double_complex) :: fgsl_complex_poly_complex_eval
    complex(c_double_complex) :: zz, cz(size(c))
    target :: cz
    zz = z
    cz = c
    fgsl_complex_poly_complex_eval = gsl_complex_poly_complex_eval(c_loc(cz), &
         size(c, kind=fgsl_int), zz)
  end function fgsl_complex_poly_complex_eval
  function fgsl_poly_eval_derivs(c, x, res)
    integer(fgsl_int) :: fgsl_poly_eval_derivs
    real(fgsl_double), dimension(:), intent(in), target, contiguous :: c
    real(fgsl_double), dimension(:), target, contiguous :: res
    real(fgsl_double), intent(in) :: x
    fgsl_poly_eval_derivs = gsl_poly_eval_derivs(c_loc(c), size(c, kind=fgsl_size_t), &
         x, c_loc(res), size(res, kind=fgsl_size_t))
  end function fgsl_poly_eval_derivs
  function fgsl_poly_dd_init(dd, x, y)
    real(fgsl_double), intent(inout), target, contiguous :: dd(:)
    real(fgsl_double), intent(in), target, contiguous :: x(:), y(:)
    integer(fgsl_int) :: fgsl_poly_dd_init
    !check dims
    if (size(x) /= size(y) .or.&
         size(x) /= size(dd)) then
       call fgsl_error('x, y and dd dimensions do not match', 'fgsl_poly', __LINE__, fgsl_ebadlen)
       fgsl_poly_dd_init = fgsl_ebadlen
       return
    endif
    fgsl_poly_dd_init = gsl_poly_dd_init(c_loc(dd), c_loc(x), c_loc(y), &
         size(dd, kind=fgsl_size_t))
  end function fgsl_poly_dd_init
  function fgsl_poly_dd_eval(dd, xa, x)
    real(fgsl_double), intent(in), target, contiguous :: dd(:), xa(:)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: fgsl_poly_dd_eval
    if (size(xa) /= size(dd)) then
       call fgsl_error('xa and dd dimensions do not match', 'fgsl_poly', __LINE__, fgsl_ebadlen)
       fgsl_poly_dd_eval = fgsl_ebadlen
       return
    endif
    fgsl_poly_dd_eval = gsl_poly_dd_eval(c_loc(dd), c_loc(xa), size(dd, kind=fgsl_size_t), x)
  end function fgsl_poly_dd_eval
  function fgsl_poly_dd_taylor(c, xp, dd, x, w)
    real(fgsl_double), intent(inout), target, contiguous :: c(:)
    real(fgsl_double), intent(in) :: xp
    real(fgsl_double), intent(in), target, contiguous :: dd(:), x(:)
    real(fgsl_double), intent(out), target, contiguous :: w(:)
    integer(fgsl_int) :: fgsl_poly_dd_taylor
    if (size(x) /= size(c) .or.&
         size(x) /= size(dd) .or.&
         size(x) /= size(w)) then
       call fgsl_error('x, c, dd and w dimensions do not match', 'fgsl_poly', __LINE__, fgsl_ebadlen)
       fgsl_poly_dd_taylor = fgsl_ebadlen
       return
    endif
    fgsl_poly_dd_taylor = gsl_poly_dd_taylor(c_loc(c), xp, c_loc(dd), c_loc(x),&
         size(dd, kind=fgsl_size_t), c_loc(w))
  end function fgsl_poly_dd_taylor
  function fgsl_poly_dd_hermite_init(dd, z, xa, ya, dya)
    real(fgsl_double), intent(inout), target, contiguous :: dd(:), z(:)
    real(fgsl_double), intent(in), target, contiguous :: xa(:), ya(:), dya(:)
    integer(fgsl_int) :: fgsl_poly_dd_hermite_init
    if (2*size(xa) /= size(dd) .or.&
         2*size(xa) /= size(z) .or.&
         size(xa) /= size(ya) .or.&
         size(xa) /= size(dya)) then
       call fgsl_error('dimensions do not match', 'fgsl_poly', __LINE__, fgsl_ebadlen)
       fgsl_poly_dd_hermite_init = fgsl_ebadlen
       return
    endif
    fgsl_poly_dd_hermite_init = gsl_poly_dd_hermite_init(c_loc(dd), c_loc(z), &
         c_loc(xa), c_loc(ya), c_loc(dya), size(xa, kind=fgsl_size_t))
  end function fgsl_poly_dd_hermite_init
  function fgsl_poly_solve_quadratic(a, b, c, x0, x1)
    real(fgsl_double), intent(in) :: a, b, c
    real(fgsl_double), intent(out) :: x0, x1
    integer(fgsl_int) :: fgsl_poly_solve_quadratic
    fgsl_poly_solve_quadratic = gsl_poly_solve_quadratic(a, b, c, x0, x1)
  end function fgsl_poly_solve_quadratic
  function fgsl_poly_complex_solve_quadratic(a, b, c, x0, x1)
    real(fgsl_double), intent(in) :: a, b, c
    complex(fgsl_double_complex), intent(out) :: x0, x1
    integer(fgsl_int) :: fgsl_poly_complex_solve_quadratic
    !
    complex(c_double_complex) :: z0, z1
    fgsl_poly_complex_solve_quadratic = gsl_poly_complex_solve_quadratic(a, b, c, z0, z1)
    x0 = z0
    x1 = z1
  end function fgsl_poly_complex_solve_quadratic
  function fgsl_poly_solve_cubic(a, b, c, x0, x1, x2)
    real(fgsl_double), intent(in) :: a, b, c
    real(fgsl_double), intent(out) :: x0, x1, x2
    integer(fgsl_int) :: fgsl_poly_solve_cubic
    fgsl_poly_solve_cubic = gsl_poly_solve_cubic(a, b, c, x0, x1, x2)
  end function fgsl_poly_solve_cubic
  function fgsl_poly_complex_solve_cubic(a, b, c, x0, x1, x2)
    real(fgsl_double), intent(in) :: a, b, c
    complex(fgsl_double_complex), intent(out) :: x0, x1, x2
    integer(fgsl_int) :: fgsl_poly_complex_solve_cubic
    !
    complex(c_double_complex) :: z0, z1, z2
    fgsl_poly_complex_solve_cubic = gsl_poly_complex_solve_cubic(a, b, c, z0, z1, z2)
    x0 = z0
    x1 = z1
    x2 = z2
  end function fgsl_poly_complex_solve_cubic
  function fgsl_poly_complex_workspace_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_poly_complex_workspace) :: fgsl_poly_complex_workspace_alloc
    fgsl_poly_complex_workspace_alloc%gsl_poly_complex_workspace = gsl_poly_complex_workspace_alloc(n)
  end function fgsl_poly_complex_workspace_alloc
  subroutine fgsl_poly_complex_workspace_free(w)
    type(fgsl_poly_complex_workspace), intent(inout) :: w
    call gsl_poly_complex_workspace_free(w%gsl_poly_complex_workspace)
  end subroutine fgsl_poly_complex_workspace_free
  function fgsl_poly_complex_workspace_stat(w)
    type(fgsl_poly_complex_workspace), intent(in) :: w
    logical :: fgsl_poly_complex_workspace_stat
    fgsl_poly_complex_workspace_stat = .true.
    if (.not. c_associated(w%gsl_poly_complex_workspace)) then
       fgsl_poly_complex_workspace_stat = .false.
    end if
  end function fgsl_poly_complex_workspace_stat
  function fgsl_poly_complex_solve(a, n, w, z)
    real(fgsl_double), intent(in), target, contiguous :: a(:)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_poly_complex_workspace), intent(inout) :: w
    complex(fgsl_double_complex), intent(out) :: z(:)
    integer(fgsl_int) :: fgsl_poly_complex_solve
    !
    real(fgsl_double), allocatable, target :: zz(:)
    integer :: istat
    integer(fgsl_size_t) :: i
    allocate(zz(2*n-2), stat=istat)
    if (istat /= 0) then
       fgsl_poly_complex_solve = fgsl_enomem
    else
       fgsl_poly_complex_solve = gsl_poly_complex_solve(c_loc(a), n, w%gsl_poly_complex_workspace, c_loc(zz))
       do i=1,n-1
          z(i) = zz(2*i-1) + (0.0_fgsl_double, 1.0_fgsl_double) * zz(2*i)
       end do
       deallocate(zz)
    end if
  end function fgsl_poly_complex_solve

end module fgsl_poly
