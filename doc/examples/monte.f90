module mod_monte
  use fgsl
  use, intrinsic :: iso_c_binding
  implicit none
  real(fgsl_double), parameter :: exact = 1.3932039296856768_fgsl_double
  integer(fgsl_size_t), parameter :: calls = 500000
contains
  function g(v, n, params) bind(c)
    integer(c_size_t), value :: n
    real(c_double), dimension(*) :: v
    type(c_ptr), value :: params
    real(c_double) :: g
    g = 1.0_c_double/(m_pi**3)/ &
         (1.0_fgsl_double - cos(v(1))*cos(v(2))*cos(v(3)))
  end function g
  subroutine display_results(title,result,error)
    character(kind=fgsl_char,len=*), intent(in) :: title
    real(fgsl_double), intent(in) :: result, error
    write(6, '(A,'' ================='')') trim(title)
    write(6, '(''result = '',F10.6)') result
    write(6, '(''sigma  = '',F10.6)') error
    write(6, '(''exact  = '',F10.6)') exact
    write(6, '(''error  = '',F10.6,'' = '',F5.1,'' sigma'')') &
         result - exact, abs(result - exact) / error
  end subroutine display_results
end module mod_monte
program monte
! Computation of the integral,
!
!      I = int (dx dy dz)/(2pi)^3  1/(1-cos(x)cos(y)cos(z))
!
!   over (-pi,-pi,-pi) to (+pi, +pi, +pi).  The exact answer
!   is Gamma(1/4)^4/(4 pi^3).  This example is taken from
!   C.Itzykson, J.M.Drouffe, "Statistical Field Theory -
!   Volume 1", Section 1.1, p21, which cites the original
!   paper M.L.Glasser, I.J.Zucker, Proc.Natl.Acad.Sci.USA 74
!   1800 (1977) 

! For simplicity we compute the integral over the region 
!   (0,0,0) -> (pi,pi,pi) and multiply by 8
  use mod_monte
  implicit none
  real(fgsl_double) :: chisq, xl(3), xu(3), res, err, y, yy, yyy
  integer(fgsl_int) :: status, stage, mode, verbose
  integer(fgsl_size_t) :: its
  type(fgsl_file) :: file
  type(fgsl_rng) :: r
  type(fgsl_rng_type) :: t
  type(fgsl_monte_plain_state) :: s
  type(fgsl_monte_miser_state) :: m
  type(fgsl_monte_vegas_state) :: v
  type(fgsl_monte_function) :: gfun
  type(c_ptr) :: ptr
!
  xl = 0.0_fgsl_double ; xu = m_pi
  t = fgsl_rng_env_setup()
  r = fgsl_rng_alloc(t)
  gfun = fgsl_monte_function_init(g, 3_fgsl_size_t, ptr)
  s = fgsl_monte_plain_alloc(3_fgsl_size_t)
  status = fgsl_monte_plain_integrate(gfun, xl, xu, 3_fgsl_size_t, &
       calls, r, s, res, err)
  call fgsl_monte_plain_free(s)
  call display_results('plain',res,err)
!
  m = fgsl_monte_miser_alloc(3_fgsl_size_t)
  status = fgsl_monte_miser_integrate(gfun, xl, xu, 3_fgsl_size_t, &
       calls, r, m, res, err)
  call fgsl_monte_miser_free(m)
  call display_results('miser',res,err)
!
  v = fgsl_monte_vegas_alloc(3_fgsl_size_t)
  status = fgsl_monte_vegas_integrate(gfun, xl, xu, 3_fgsl_size_t, &
       10000_fgsl_size_t, r, v, res, err)
  call display_results('vegas warm-up',res,err)
  write(6, *) 'converging ...'
  do 
     status = fgsl_monte_vegas_integrate(gfun, xl, xu, 3_fgsl_size_t, &
          calls/5_fgsl_size_t, r, v, res, err)
     call fgsl_monte_vegas_getparams(v, y, yy, chisq, yyy, &
       its, stage, mode, verbose, file)
     write(6, '(''result = '',F10.6,'' sigma = '',F10.6, &
          & '' chisq/dof = '',F6.1)') res,err,chisq
     if (abs(chisq - 1.0_fgsl_double) <= 0.5_fgsl_double) exit 
  end do
  call display_results('vegas converged',res,err)
  call fgsl_monte_vegas_free(v)
  call fgsl_monte_function_free(gfun)
  call fgsl_rng_free(r)
end program monte
