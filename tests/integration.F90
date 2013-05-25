#include "config.h"
module mod_integration
  use fgsl
  use mod_unit
  use, intrinsic :: iso_c_binding
  implicit none  
  real(fgsl_double), parameter :: eps7 = 1.0d-7
  real(fgsl_double), parameter :: eps10 = 1.0d-10
!  real(fgsl_double), parameter :: eps12 = 1.0d-12
  real(fgsl_double) :: pts(4)
  integer(fgsl_size_t), parameter :: limit = 1000_fgsl_size_t, &
       isub  = 8_fgsl_size_t, ifixed = 2_fgsl_size_t
contains
  function integrate_fun1(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: integrate_fun1
!
    real(c_double), pointer :: alpha
    call c_f_pointer(params, alpha)
!    write(6, *) 'Alpha = ',alpha
    integrate_fun1 = log(alpha * x) / sqrt(x)
  end function integrate_fun1
  function integrate_fun2(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: integrate_fun2
!
    real(c_double), pointer :: alpha
    call c_f_pointer(params, alpha)
    integrate_fun2 = 1.0_c_double + alpha * x * x
!    write(6, *) 'Alpha = ',alpha, ' x = ',x, ' Func: ', integrate_fun2
  end function integrate_fun2
  function integrate_fun3(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: integrate_fun3
!
    real(c_double), pointer :: alpha
    call c_f_pointer(params, alpha)
!    write(6, *) 'Alpha = ',alpha, ' x = ',x
    integrate_fun3 = 1.0_c_double / (1.0_c_double + alpha * x * x)
  end function integrate_fun3
  function integrate_fun4(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: integrate_fun4
!
    integrate_fun4 = 1.0_c_double / sqrt(abs(x))
  end function integrate_fun4
  function integrate_fun5(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: integrate_fun5
!
    integrate_fun5 = exp(-x*x)
  end function integrate_fun5
  function integrate_fun6(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: integrate_fun6
!
    integrate_fun6 = 2*x
  end function integrate_fun6
  function integrate_fun7(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: integrate_fun7
!
    real(c_double), pointer :: alpha
    call c_f_pointer(params, alpha)
    integrate_fun7 = exp(-alpha*x)
  end function integrate_fun7
 end module mod_integration
program integration
  use mod_integration
  implicit none
  real(fgsl_double), target :: xx
  real(fgsl_double) :: ra, rda
  integer(fgsl_int) :: status
  integer(fgsl_size_t) :: neval
  type(c_ptr) :: ptr
  type(fgsl_error_handler_t) :: std
  type(fgsl_function) :: stdfunc
  type(fgsl_integration_workspace) :: integ_wk, integ_wc
  type(fgsl_integration_qaws_table) :: qaws_wk
  type(fgsl_integration_qawo_table) :: qawo_wk
#if GSL_VERSION_MAJOR_FORTRAN >= 1 && GSL_VERSION_MINOR_FORTRAN >= 14
  type(fgsl_integration_glfixed_table) :: glfixed_wk
#endif
  
!
! Test quadrature routines
!
  call unit_init(200)
  std = fgsl_set_error_handler_off()
  ptr = c_loc(xx)
  integ_wk = fgsl_integration_workspace_alloc(limit)
  call unit_assert_true('fgsl_integration_workspace_alloc',&
       fgsl_well_defined(integ_wk),.true.)
  qaws_wk = fgsl_integration_qaws_table_alloc(1.0d0,2.0d0,0,0)
  call unit_assert_true('fgsl_integration_qaws_table_alloc',&
       fgsl_well_defined(qaws_wk),.true.)
  status = fgsl_integration_qaws_table_set(qaws_wk,0.0d0,1.0d0,0,0)
  call unit_assert_equal('fgsl_integration_qaws_table_set:status',&
       fgsl_success,status)
  qawo_wk = fgsl_integration_qawo_table_alloc(2.0d0,4.0d0,&
       fgsl_integ_cosine,isub)
  call unit_assert_true('fgsl_integration_qawo_table_alloc',&
       fgsl_well_defined(qawo_wk),.true.)
  status = fgsl_integration_qawo_table_set(qawo_wk,2.0d0,4.0d0,&
       fgsl_integ_sine)
  call unit_assert_equal('fgsl_integration_qawo_table_set:status',&
       fgsl_success,status)
  status = fgsl_integration_qawo_table_set_length(qawo_wk,1.0d0)
  call unit_assert_equal('fgsl_integration_qawo_table_set_length:status',&
       fgsl_success,status)
#if GSL_VERSION_MAJOR_FORTRAN >= 1 && GSL_VERSION_MINOR_FORTRAN >= 14
  glfixed_wk = fgsl_integration_glfixed_table_alloc(ifixed)
  call unit_assert_true('fgsl_integration_glfixed_table_alloc',&
       fgsl_well_defined(glfixed_wk),.true.)
#endif
!
  xx = sqrt(2.0D0)
  stdfunc = fgsl_function_init(integrate_fun2, ptr)
  status = fgsl_integration_qng(stdfunc, -1.0_fgsl_double, 1.0_fgsl_double, &
       eps10, eps10, ra, rda, neval)
  call unit_assert_equal('fgsl_integration_qng:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_integration_qng',&
       2.0d0*(3+sqrt(2.0d0))/3.0d0,ra,rda)
  call fgsl_function_free(stdfunc)
!
  stdfunc = fgsl_function_init(integrate_fun2, ptr)
  status = fgsl_integration_qag(stdfunc, -1.0_fgsl_double, 1.0_fgsl_double, &
       eps10, eps10, limit, 2, integ_wk, ra, rda)
  call unit_assert_equal('fgsl_integration_qags:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_integration_qags',&
       2.0d0*(3+sqrt(2.0d0))/3.0d0,ra,rda)
  call fgsl_function_free(stdfunc)
!
  xx = 1.0D0
  stdfunc = fgsl_function_init(integrate_fun1, ptr)
  status = fgsl_integration_qags(stdfunc, 0.0_fgsl_double, 1.0_fgsl_double, &
       eps10, eps10, limit, integ_wk, ra, rda)
  call unit_assert_equal('fgsl_integration_qags:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_integration_qags',-4.0d0,ra,rda)
  call fgsl_function_free(stdfunc)
!
  xx = 1.0D0
  pts(1:3) = (/-1.0d0, 0.0d0, 1.0d0/)
  stdfunc = fgsl_function_init(integrate_fun4, ptr)
  status = fgsl_integration_qagp(stdfunc, pts, 3_fgsl_size_t, &
       eps10, eps10, limit, integ_wk, ra, rda);
  call unit_assert_equal('fgsl_integration_qagp:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_integration_qagp',4.0d0,ra,rda)
  call fgsl_function_free(stdfunc)
!
  stdfunc = fgsl_function_init(integrate_fun5, ptr)
  status = fgsl_integration_qagi(stdfunc, eps10, eps10, limit, integ_wk, ra, rda)
  call unit_assert_equal('fgsl_integration_qagi:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_integration_qagi',sqrt(m_pi),ra,rda)
  call fgsl_function_free(stdfunc)
!
  stdfunc = fgsl_function_init(integrate_fun5, ptr)
  status = fgsl_integration_qagiu(stdfunc, 0.0d0, eps10, eps10, limit, integ_wk, ra, rda)
  call unit_assert_equal('fgsl_integration_qagiu:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_integration_qagiu',sqrt(m_pi)/2.0d0,ra,rda)
  call fgsl_function_free(stdfunc)
!
  stdfunc = fgsl_function_init(integrate_fun5, ptr)
  status = fgsl_integration_qagil(stdfunc, 0.0d0, eps10, eps10, limit, integ_wk, ra, rda)
  call unit_assert_equal('fgsl_integration_qagil:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_integration_qagil',sqrt(m_pi)/2.0d0,ra,rda)
  call fgsl_function_free(stdfunc)
!
  stdfunc = fgsl_function_init(integrate_fun6, ptr)
  status = fgsl_integration_qawc(stdfunc, -1.0_fgsl_double, 1.0_fgsl_double, &
       0.0_fgsl_double, eps10, eps10, limit, integ_wk, ra, rda)
  call unit_assert_equal('fgsl_integration_qawc:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_integration_qawc',&
       4.0d0,ra,eps10)
! NOTE replaced rda with eps10 (fails on 32 bit with NAG)
  call fgsl_function_free(stdfunc)
!
  stdfunc = fgsl_function_init(integrate_fun6, ptr)
  status = fgsl_integration_qaws(stdfunc, 0.0_fgsl_double, 1.0_fgsl_double, &
       qaws_wk, eps10, eps10, limit, integ_wk, ra, rda)
  call unit_assert_equal('fgsl_integration_qaws:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_integration_qaws',&
       1.0d0/3.0d0,ra,rda)
  call fgsl_function_free(stdfunc)
!
  stdfunc = fgsl_function_init(integrate_fun6, ptr)
  status = fgsl_integration_qawo(stdfunc, 0.0_fgsl_double, &
       eps10, eps10, limit, integ_wk, qawo_wk, ra, rda)
  call unit_assert_equal('fgsl_integration_qawo:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_integration_qawo',&
       0.5d0*sin(2.0d0)-cos(2.0d0),ra,rda)
  status = fgsl_integration_qawo_table_set(qawo_wk,2.0d0,1.0d0,&
       fgsl_integ_cosine)
  status = fgsl_integration_qawo(stdfunc, 0.0_fgsl_double, &
       eps10, eps10, limit, integ_wk, qawo_wk, ra, rda)
  call unit_assert_equal('fgsl_integration_qawo:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_integration_qawo',&
       sin(2.0d0)+0.5d0*cos(2.0d0)-0.5d0,ra,rda)
  call fgsl_function_free(stdfunc)
!
  integ_wc = fgsl_integration_workspace_alloc(limit)
  call unit_assert_true('fgsl_integration_workspace_alloc',&
       fgsl_well_defined(integ_wc),.true.)
  xx = 1.0D0
  stdfunc = fgsl_function_init(integrate_fun7, ptr)
  status = fgsl_integration_qawo_table_set(qawo_wk,2.0d0,1.0d0,&
       fgsl_integ_sine)
  status = fgsl_integration_qawf(stdfunc, 0.0_fgsl_double, &
       eps10, limit, integ_wk, integ_wc, qawo_wk, ra, rda)
  call unit_assert_equal('fgsl_integration_qawf:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_integration_qawf',&
       0.4d0,ra,rda)
  call fgsl_integration_workspace_free(integ_wc)
  call fgsl_function_free(stdfunc)
!
#if GSL_VERSION_MAJOR_FORTRAN >= 1 && GSL_VERSION_MINOR_FORTRAN >= 14
  xx = sqrt(2.0D0)
  stdfunc = fgsl_function_init(integrate_fun2, ptr)
  ra = fgsl_integration_glfixed(stdfunc, -1.0_fgsl_double, 1.0_fgsl_double, &
       glfixed_wk)
  call unit_assert_equal_within('fgsl_integration_glfixed',&
       2.0d0*(3+sqrt(2.0d0))/3.0d0,ra,eps7)
  call fgsl_function_free(stdfunc)
#endif
!
  call fgsl_integration_qaws_table_free(qaws_wk)
  call fgsl_integration_qawo_table_free(qawo_wk)
#if GSL_VERSION_MAJOR_FORTRAN >= 1 && GSL_VERSION_MINOR_FORTRAN >= 14
  call fgsl_integration_glfixed_table_free(glfixed_wk)
#endif
  call fgsl_integration_workspace_free(integ_wk)
!
! Done
!
  call unit_finalize()
end program integration
