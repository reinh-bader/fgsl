module timer
! Timer implementation based on system_clock intrinsic
! Note that this facility is not thread-safe
  implicit none
  private
  public :: dwalltime
  integer, parameter :: ik = selected_int_kind(12)
  logical, save :: first = .true.
  integer(ik), save ::  count_rate, count_max
  double precision, save :: conversion = 0.0d0
contains
  double precision function dwalltime()
    integer(ik) :: count
    if (first) then
       first = .false.
       call system_clock(count, count_rate, count_max)
       conversion = 1.0d0 / dble(count_rate)
    else
       call system_clock(count)
    end if
    dwalltime = count * conversion
  end function dwalltime
end module timer
module mod_nlfit4 
  use fgsl
  use, intrinsic :: iso_c_binding
  implicit none
  integer(fgsl_size_t), parameter :: max_iter = 200
  real(fgsl_double), parameter :: xtol = 1.0e-8_fgsl_double, &
       gtol = 1.0e-8_fgsl_double, ftol = 1.0e-8_fgsl_double
! 
! parameters for functions
  type :: model_params
     real(fgsl_double) :: alpha
     type(fgsl_spmatrix) :: J
  end type model_params
contains
!
! penalty function
  integer(c_int) function penalty_f(x, params, f) bind(c)
    type(c_ptr), value :: x, params, f

    type(model_params), pointer :: par
    real(fgsl_double) :: sqrt_alpha, sx
    type(fgsl_vector) :: xx, ff
    real(fgsl_double), pointer :: xxp(:), ffp(:)
    integer(fgsl_size_t) :: i

    call c_f_pointer(params, par)
    sqrt_alpha = sqrt(par%alpha)
    call fgsl_obj_c_ptr(xx, x)
    xxp => fgsl_vector_to_fptr(xx)
    call fgsl_obj_c_ptr(ff, f)
    ffp => fgsl_vector_to_fptr(ff)
    sx = 0.0_fgsl_double
    do i = 1, size(xxp)
       ffp(i) = sqrt_alpha * (xxp(i) - 1.0_fgsl_double)
       sx = sx + xxp(i)**2
    end do
    ffp(size(xxp)+1) = sx - 0.25_fgsl_double
    penalty_f = fgsl_success
  end function penalty_f
  integer(c_int) function penalty_df(transj, x, u, params, &
       v, jtj) bind(c)
    integer(c_int), value :: transj
    type(c_ptr), value :: x, u, params, v, jtj
    
    type(model_params), pointer :: par
    type(fgsl_vector) :: xx, uu, vv
    real(fgsl_double), pointer :: xxp(:)
    type(fgsl_matrix) :: jj
    real(fgsl_double), pointer :: jtjp(:,:)
    integer(fgsl_size_t) :: i, j
    integer(fgsl_int) :: status

    call c_f_pointer(params, par)
    call fgsl_obj_c_ptr(xx, x)
    xxp => fgsl_vector_to_fptr(xx)
    call fgsl_obj_c_ptr(uu, u)
!
!   store 2*x in last row of J
    do i = 1, size(xxp)
       status = fgsl_spmatrix_set(par%J, size(xxp, KIND=fgsl_size_t), i-1, &
            2.0_fgsl_double * xxp(i))
    end do
!
!   compute v = op(J) * u
    if (c_associated(v)) then
       call fgsl_obj_c_ptr(vv, v)
       call fgsl_spmatrix_size(par%J, i, j)
       
       status = fgsl_spblas_dgemv(transj, 1.0_fgsl_double, &
            par%J, uu, 0.0_fgsl_double, vv)
    end if

!
!   compute JTJ = alpha*I_p + 4 * x * x^T
!   note that "Lower" requires i <= j for GSL
    if (c_associated(jtj)) then
       call fgsl_obj_c_ptr(jj, jtj)
       status = fgsl_matrix_align(jtjp, jj)
       do j = 1, size(jtjp, 2)
          jtjp(j, j) = 4.0_fgsl_double * xxp(j)**2 + par%alpha
          do i = 1, j-1
             jtjp(i, j) = 4.0_fgsl_double * xxp(i) * xxp(j)
          end do
       end do
    end if
    penalty_df = fgsl_success
  end function penalty_df
  integer(fgsl_int) function penalty_fvv(x, v, params, fvv) BIND(C)
    type(c_ptr), value :: x, v, params, fvv
    type(fgsl_vector) :: xx, f_v, f_fvv
    real(fgsl_double), pointer :: p_xx(:), p_v(:), p_fvv(:)

    call fgsl_obj_c_ptr(f_v, v)
    p_v => fgsl_vector_to_fptr(f_v)
    call fgsl_obj_c_ptr(f_fvv, fvv)
    p_fvv => fgsl_vector_to_fptr(f_fvv)
    call fgsl_obj_c_ptr(xx, x)
    p_xx => fgsl_vector_to_fptr(xx)
   
    p_fvv = 0.0_fgsl_double
    p_fvv( size(p_xx) + 1 ) = 2.0_fgsl_double * sum(p_v * p_v)
    
    penalty_fvv = fgsl_success
  end function penalty_fvv

  subroutine solve_system(x0, fdf, params)
    use timer
    type(fgsl_vector) :: x0
    type(fgsl_multilarge_nlinear_fdf) :: fdf
    type(fgsl_multilarge_nlinear_parameters) :: params

    type(fgsl_multilarge_nlinear_type) :: t
    type(fgsl_multilarge_nlinear_workspace) :: work
    integer(fgsl_size_t) :: n, p
    type(fgsl_vector) :: f, x
    real(fgsl_double), pointer :: p_x(:), p_f(:)
    integer(fgsl_int) :: info, status
    integer(fgsl_size_t) :: nevalf, nevaldfu, nevaldf2, nevalfvv
    real(fgsl_double) :: chisq0, chisq, rcond, xsq, ti

    t = fgsl_multilarge_nlinear_type('trust')
    call fgsl_multilarge_nlinear_fdf_get(fdf, N=n, P=p)
    work = fgsl_multilarge_nlinear_alloc(t, params, n, p)
    f = fgsl_multilarge_nlinear_residual(work)
    x = fgsl_multilarge_nlinear_position(work)
    p_x => fgsl_vector_to_fptr(x)
    p_f => fgsl_vector_to_fptr(f)

   ! write(*, *) 'SIzes n, p: ', n, p
   !write(*, *) 'SIzes f, x: ', size(p_f), size(p_x)

    ti = dwalltime()
!
! initialize solver
    status = fgsl_multilarge_nlinear_init(x0, fdf, work)
!
! store initial cost
    chisq0 = dot_product(p_f,p_f)
!
! iterate to convergence
    status = fgsl_multilarge_nlinear_driver(max_iter, xtol, gtol, ftol, &
         CALLBACK_PARAMS=c_null_ptr, INFO=info, W=work)

    ti = dwalltime() - ti
!
! store final cost
    chisq = dot_product(p_f,p_f)
!
! store final x^2
    xsq = dot_product(p_x,p_x)
!
! store cond(J(x))
    status = fgsl_multilarge_nlinear_rcond(rcond, work)

!
! print summary
    call fgsl_multilarge_nlinear_fdf_get(fdf, NEVALF=nevalf, &
         NEVALDFU=nevaldfu, NEVALDF2=nevaldf2, NEVALFVV=nevalfvv)

    write(*, fmt='(A25,I4,I4,I4,I4,I4,1P,4(E11.4),1X,0P,F6.2)') &
         fgsl_multilarge_nlinear_trs_name(work), &
         fgsl_multilarge_nlinear_niter(work), &
         nevalf, nevaldfu, nevaldf2, nevalfvv, &
         chisq0, chisq, 1.0_fgsl_double/rcond, xsq, ti

    call fgsl_multilarge_nlinear_free(work)
  end subroutine solve_system
end module mod_nlfit4
program nlfit4
  use mod_nlfit4
  implicit none
! Problem size reduced to decrease run time
  integer(fgsl_size_t), parameter :: p = 500, n = p+1
  integer(fgsl_int) :: status
  integer(fgsl_size_t) :: i

  type(fgsl_vector) :: f, x
  real(fgsl_double), target :: tf(n), tx(p)
  type(fgsl_multilarge_nlinear_fdf) :: fdf
  type(fgsl_multilarge_nlinear_parameters) :: fdf_params
  type(model_params), target :: params

  fdf_params = fgsl_multilarge_nlinear_default_parameters()

  f = fgsl_vector_init(tf)
  x = fgsl_vector_init(tx)
!
! model params: sparse Jacobian matrix with 2*p non-zero elements
! in triplet format
  params%alpha = 1.0e-5_fgsl_double
  params%J = fgsl_spmatrix_alloc_nzmax(n, p, 2*p, FGSL_SPMATRIX_TRIPLET)
!
! define function to be minimized
  fdf = fgsl_multilarge_nlinear_fdf_init(n, p, c_loc(params), &
       penalty_f, penalty_df, penalty_fvv)

  do i=1, p
!    starting point
     tx(i) = real(i, fgsl_double)
!    store sqrt(alpha)*I_p in upper p-by-p block of J
     status = fgsl_spmatrix_set( params%J, i-1, i-1, sqrt(params%alpha) )
  end do

  write(*, '(''Method                NITER   NFEV  NJUEV NJTJEV NAEV&
       &Init Cost Final Cost cond(J) Final x^2 Time (s)'')')

  call fgsl_multilarge_nlinear_parameters_set(fdf_params, &
       SCALE=fgsl_multilarge_nlinear_scale_levenberg, &
       TRS=fgsl_multilarge_nlinear_trs_lm)
  call solve_system(x, fdf, fdf_params)

  call fgsl_multilarge_nlinear_parameters_set(fdf_params, &
       TRS=fgsl_multilarge_nlinear_trs_lmaccel)
  call solve_system(x, fdf, fdf_params)

  call fgsl_multilarge_nlinear_parameters_set(fdf_params, &
       TRS=fgsl_multilarge_nlinear_trs_dogleg)
  call solve_system(x, fdf, fdf_params)

  call fgsl_multilarge_nlinear_parameters_set(fdf_params, &
       TRS=fgsl_multilarge_nlinear_trs_ddogleg)
  call solve_system(x, fdf, fdf_params)

  call fgsl_multilarge_nlinear_parameters_set(fdf_params, &
       TRS=fgsl_multilarge_nlinear_trs_subspace2d)
  call solve_system(x, fdf, fdf_params)

  call fgsl_multilarge_nlinear_parameters_set(fdf_params, &
       TRS=fgsl_multilarge_nlinear_trs_cgst)
  call solve_system(x, fdf, fdf_params)

  call fgsl_vector_free(x)
  call fgsl_vector_free(f)
  call fgsl_multilarge_nlinear_fdf_free(fdf)
  call fgsl_spmatrix_free(params%J)
end program nlfit4
