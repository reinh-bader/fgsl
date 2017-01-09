module mod_nlfit2
  use fgsl
  use, intrinsic :: iso_c_binding
  implicit none
  integer(fgsl_size_t), parameter :: max_iter = 200
  real(fgsl_double), parameter :: xtol = 1.0e-8_fgsl_double, &
       gtol = 1.0e-8_fgsl_double, ftol = 1.0e-8_fgsl_double
contains
  integer(c_int) function func_f(x, params, f) BIND(C)
    type(c_ptr), value :: x, params, f
    type(fgsl_vector) :: f_x, f_f
    real(fgsl_double), pointer :: p_x(:), p_f(:)
    integer(c_int) :: status
!
!   cast *void to an fgsl_vector
    call fgsl_obj_c_ptr(f_x, x)
    call fgsl_obj_c_ptr(f_f, f)
!
!   align fgsl_vector with Fortran POINTER
    status = fgsl_vector_align(p_x, f_x)
    status = fgsl_vector_align(p_f, f_f)
!
    p_f(1) = 100._fgsl_double * (p_x(2) - p_x(1)**2) 
    p_f(2) = 1._fgsl_double - p_x(1)
    func_f = fgsl_success
  end function func_f
  integer(c_int) function func_df(x, params, j) BIND(C)
    type(c_ptr), value :: x, params, j
    type(fgsl_vector) :: f_x
    type(fgsl_matrix) :: f_j
    real(fgsl_double), pointer :: p_x(:), p_j(:,:)
    integer(c_int) :: status

    call fgsl_obj_c_ptr(f_x, x)
    call fgsl_obj_c_ptr(f_j, j)
    status = fgsl_vector_align(p_x, f_x)
    status = fgsl_matrix_align(p_j, f_j)
    p_j(1, 1) = -200._fgsl_double * p_x(1)
    p_j(2, 1) = 100._fgsl_double
    p_j(1, 2) = -1._fgsl_double
    p_j(2, 2) = 0._fgsl_double
    func_df = fgsl_success
  end function func_df
  integer(c_int) function func_fvv(x, v, params, fvv) BIND(C)
    type(c_ptr), value :: x, v, params, fvv
    type(fgsl_vector) :: f_v, f_fvv
    real(fgsl_double), pointer :: p_v(:), p_fvv(:)
    integer(c_int) :: status
!
!   cast *void to an fgsl_vector
    call fgsl_obj_c_ptr(f_v, v)
    call fgsl_obj_c_ptr(f_fvv, fvv)
!
!   align fgsl_vector with Fortran POINTER
    status = fgsl_vector_align(p_v, f_v)
    status = fgsl_vector_align(p_fvv, f_fvv)

    p_fvv(1) = -200._fgsl_double * p_v(1)**2
    p_fvv(2) = 0._fgsl_double
    func_fvv = fgsl_success
  end function func_fvv
  subroutine callback(iter, params, wp) BIND(C)
    integer(c_size_t), value :: iter
    type(c_ptr), value :: params, wp
    type(fgsl_vector) :: x
    real(fgsl_double), pointer :: p_x(:)
    integer(c_int) :: status

    x = fgsl_multifit_nlinear_position(fgsl_multifit_nlinear_workspace(wp))
    status = fgsl_vector_align(p_x, x)
    write(*, '(2(1PE12.5,2X))') p_x(1:2)
  end subroutine callback
  subroutine solve_system(x0, fdf, params)
    type(fgsl_vector) :: x0
    type(fgsl_multifit_nlinear_fdf) :: fdf
    type(fgsl_multifit_nlinear_parameters) :: params

    type(fgsl_multifit_nlinear_type) :: t
    type(fgsl_multifit_nlinear_workspace) :: work
    integer(fgsl_size_t) :: n, p
    type(fgsl_vector) :: f, x
    real(fgsl_double), pointer :: p_x(:), p_f(:)
    integer(fgsl_int) :: info, status
    integer(fgsl_size_t) :: nevalf, nevaldf, nevalfvv
    real(fgsl_double) :: chisq0, chisq, rcond

    t = fgsl_multifit_nlinear_type('trust')
    call fgsl_multifit_nlinear_fdf_get(fdf, N=n, P=p)
    work = fgsl_multifit_nlinear_alloc(t, params, n, p)
    f = fgsl_multifit_nlinear_residual(work)
    x = fgsl_multifit_nlinear_position(work)
    status = fgsl_vector_align(p_x, x)
    status = fgsl_vector_align(p_f, f)
!
! initialize solver
    status = fgsl_multifit_nlinear_init(x0, fdf, work)
!
! store initial cost
    chisq0 = dot_product(p_f,p_f)
!
! iterate to convergence
    status = fgsl_multifit_nlinear_driver(max_iter, xtol, gtol, ftol, &
         callback, c_null_ptr, info, work)
!
! store final cost
    chisq = dot_product(p_f,p_f)
!
! store cond(J(x))
    status = fgsl_multifit_nlinear_rcond(rcond, work)
!
! print summary
    call fgsl_multifit_nlinear_fdf_get(fdf, NEVALF=nevalf, &
         NEVALDF=nevaldf, NEVALFVV=nevalfvv)

    write(*, '(''NITER         = '',i0)') fgsl_multifit_nlinear_niter(work)
    write(*, '(''NFEV          = '',i0)') nevalf
    write(*, '(''NJEV          = '',i0)') nevaldf
    write(*, '(''NAEV          = '',i0)') nevalfvv
    write(*, '(''initial cost  = '',1pe12.3)') chisq0
    write(*, '(''final cost    = '',1pe12.3)') chisq
    write(*, '(''final x       = '',1p,2(e12.3,2X))') p_x(1:2)
    write(*, '(''final cond(J) = '',f12.4)') 1.0_fgsl_double / rcond
    write(*,*)

    call fgsl_multifit_nlinear_free(work)
    
  end subroutine solve_system
end module mod_nlfit2
program nlfit2
  use mod_nlfit2
  implicit none
  integer(fgsl_size_t), parameter :: n = 2, p = 2, nmax = 25
  integer(fgsl_int) :: status, i, j
  type(fgsl_vector) :: f, x
  real(fgsl_double), target :: tf(n), tx(p), xmin(p), dx, chisq 
  type(fgsl_multifit_nlinear_fdf) :: fdf
  type(fgsl_multifit_nlinear_parameters) :: fdf_params

  fdf_params = fgsl_multifit_nlinear_default_parameters()

  f = fgsl_vector_init(type = 1.0_fgsl_double)
  x = fgsl_vector_init(type = 1.0_fgsl_double)
  status = fgsl_vector_align(tf, n, f, n, 0_fgsl_size_t, 1_fgsl_size_t)
  status = fgsl_vector_align(tx, p, x, p, 0_fgsl_size_t, 1_fgsl_size_t)

  xmin(1) = -1.2_fgsl_double
  xmin(2) = -0.5_fgsl_double
  dx = 0.1_fgsl_double
  tx(1) = xmin(1)
  do j=1, nmax
     tx(2) = xmin(2)
     do i = 1, nmax
        status = func_f(x%gsl_vector, c_null_ptr, f%gsl_vector)
        chisq = tf(1)**2 + tf(2)**2
        write(*, '(1P,3(E12.4,2X))') tx, chisq
        tx(2) = tx(2) + dx
     end do
     write(*,*)
     tx(1) = tx(1) + dx
  end do
  write(*,*)
!
! define function to be minimized
  fdf = fgsl_multifit_nlinear_fdf_init(n, p, c_null_ptr, &
       func_f, func_df, func_fvv)
!
! starting point
  tx = [ -0.5_fgsl_double, 1.75_fgsl_double ]

!
!
  write(*, '(''=== Solving system without acceleration ==='')')
  call fgsl_multifit_nlinear_parameters_set(fdf_params, &
       trs=fgsl_multifit_nlinear_trs_lm)
  call solve_system(x, fdf, fdf_params)

  write(*, '(''=== Solving system with acceleration ==='')')
  call fgsl_multifit_nlinear_parameters_set(fdf_params, &
       trs=fgsl_multifit_nlinear_trs_lmaccel)
  call solve_system(x, fdf, fdf_params)

  call fgsl_vector_free(x)
  call fgsl_vector_free(f)
  call fgsl_multifit_nlinear_fdf_free(fdf)
end program nlfit2
