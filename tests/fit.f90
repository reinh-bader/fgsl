program fit
  use fgsl
  use mod_unit
  implicit none
  integer(fgsl_size_t), parameter :: nfmax = 19
  real(fgsl_double), parameter :: eps10 = 1.0d-10
  real(fgsl_double), parameter :: eps5 = 1.0d-5
  real(fgsl_double) :: d(4), ya(4), xa(4), &
       chisq, c0, c1, cov00, cov01, cov11, &
       t, ri, ra
  real(fgsl_double), dimension(3, nfmax), target :: xmf
  real(fgsl_double), dimension(nfmax), target :: ymf, wmf, rmf, rmf_c
  real(fgsl_double), dimension(3,3), target :: covmf
  real(fgsl_double), dimension(3), target :: cmf
  integer(fgsl_int) :: status, i
  integer(fgsl_size_t) :: istr, nrt, irk
  type(fgsl_rng_type) :: rng_type
  type(fgsl_rng) :: rng
  type(fgsl_vector) :: cvec, yvec, wvec, rvec
  type(fgsl_matrix) :: fmat, cov
  type(fgsl_multifit_linear_workspace) :: lfit_ws
!
! Test linear multifit routines
!
  call unit_init(100)
!
  d(1:4) = (/ 1970_fgsl_double, 1980_fgsl_double, 1990_fgsl_double, &
       2000_fgsl_double /)
  ya(1:4) = (/12_fgsl_double, 11_fgsl_double, 14_fgsl_double, &
       13_fgsl_double /)
  xa(1:4) = (/ 0.1_fgsl_double,  0.2_fgsl_double,  0.3_fgsl_double, &
       0.4_fgsl_double /)
  nrt = 4
  istr = 1
  status = fgsl_fit_wlinear (d, istr, xa, istr, ya, istr, nrt, &
       c0, c1, cov00, cov01, cov11, chisq)
  call unit_assert_equal('fgsl_fit_wlinear:status',fgsl_success,status)
!  write(6, *) 'Fit function: ',c0,' + ',c1,' * X'
!  write(6, *) 'Covariance Matrix (lower triangle): ',cov00,cov01,cov11
!  write(6, *) 'ChiSq: ',chisq
!  write(6, *) 'eps2: ',eps2
  call unit_assert_equal_within('fgsl_fit_wlinear:c0',-106.6d0, c0, eps10)
  call unit_assert_equal_within('fgsl_fit_wlinear:c1',0.06d0, c1, eps10)
  call unit_assert_equal_within('fgsl_fit_wlinear:chisq',0.8d0, chisq, eps10)
  call unit_assert_equal_within('fgsl_fit_wlinear:cov00',39602d0, cov00, eps10)
  call unit_assert_equal_within('fgsl_fit_wlinear:cov01',-19.9d0, cov01, eps10)
  call unit_assert_equal_within('fgsl_fit_wlinear:cov11',0.01d0, cov11, eps10)
!
  rng_type = fgsl_rng_env_setup()
  rng = fgsl_rng_alloc(fgsl_rng_default)
  t = 0.1_fgsl_double
  do i=1,nfmax
     xmf(1, i) = 1.0_fgsl_double
     xmf(2, i) = t
     xmf(3, i) = t*t
     ra = exp(t)
     ri = 0.1 * ra
     ymf(i) = ra + fgsl_ran_gaussian(rng, ri)
     wmf(i) = 1.0_fgsl_double/(ri*ri)
!     write(6, fmt='(3(1PD17.8,1X))') t, ymf(i), ri
     t = t + 0.1_fgsl_double
  end do
  lfit_ws = fgsl_multifit_linear_alloc(nfmax, 3_fgsl_size_t)
  call unit_assert_true('fgsl_multifit_linear_alloc(', &
       fgsl_well_defined(lfit_ws), .true.)
  yvec = fgsl_vector_init(1.0_fgsl_double)
  wvec = fgsl_vector_init(1.0_fgsl_double)
  rvec = fgsl_vector_init(1.0_fgsl_double)
  fmat = fgsl_matrix_init(1.0_fgsl_double)
  cvec = fgsl_vector_init(1.0_fgsl_double)
  cov = fgsl_matrix_init(1.0_fgsl_double)
  status = fgsl_vector_align(ymf,nfmax,yvec,nfmax,0_fgsl_size_t,1_fgsl_size_t)
  status = fgsl_vector_align(rmf,nfmax,rvec,nfmax,0_fgsl_size_t,1_fgsl_size_t)
  status = fgsl_vector_align(wmf,nfmax,wvec,nfmax,0_fgsl_size_t,1_fgsl_size_t)
  status = fgsl_matrix_align(xmf, 3_fgsl_size_t, 3_fgsl_size_t,nfmax,fmat)
  status = fgsl_vector_align(cmf,3_fgsl_size_t,cvec,3_fgsl_size_t,0_fgsl_size_t, &
       1_fgsl_size_t)
  status = fgsl_matrix_align(covmf, 3_fgsl_size_t, 3_fgsl_size_t,3_fgsl_size_t,cov)
!
  status = fgsl_multifit_wlinear (fmat, wvec, yvec, cvec, cov, chisq, lfit_ws)
  call unit_assert_equal('fgsl_multifit_wlinear:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_multifit_wlinear:cmf', &
       (/1.1824632501803487d0, 0.1845715862795292d0, &
       1.3031038162033384d0 /),cmf,eps10)
  covmf(1, 2) = 0.0d0
  covmf(1, 3) = 0.0d0
  covmf(2, 3) = 0.0d0
  call unit_assert_equal_within('fgsl_multifit_wlinear:covmf', &
       (/1.25611919d-02,-3.64387341d-02, 1.94389116d-02, &
       0.0d0, 1.42339296d-01, -8.48762160d-02, &
       0.0d0, 0.0d0, 5.60243141d-02 /),reshape(covmf, (/ 9 /)),eps5)

  status = fgsl_multifit_linear_residuals(fmat, yvec, cvec, rvec)
  call unit_assert_equal('fgsl_multifit_linear_residuals:status', &
       fgsl_success,status)
  rmf_c = ymf - matmul(transpose(xmf),cmf)
  call unit_assert_equal_within('fgsl_multifit_linear_residuals', &
       rmf_c,rmf,eps10)
!
  status = fgsl_multifit_linear (fmat, yvec, cvec, cov, chisq, lfit_ws)
  call unit_assert_equal('fgsl_multifit_linear:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_multifit_linear:cmf', &
       (/1.239227245954253487d0, -2.773475128673630330d-2, &
       1.429249233259944463d0 /),cmf,eps10)

  status = fgsl_multifit_linear_svd (fmat, yvec, 1.0E-7_fgsl_double, irk, cvec, cov, chisq, lfit_ws)
  call unit_assert_equal('fgsl_multifit_linear_svd:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_multifit_linear_svd:cmf', &
       (/1.239227245954253487d0, -2.773475128673630330d-2, &
       1.429249233259944463d0 /),cmf,eps10)
  call unit_assert_equal('fgsl_multifit_linear_svd:rank',3,int(irk,fgsl_int))

  status = fgsl_multifit_linear_usvd (fmat, yvec, 1.0E-7_fgsl_double, irk, cvec, cov, chisq, lfit_ws)
  call unit_assert_equal('fgsl_multifit_linear_usvd:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_multifit_linear_usvd:cmf', &
       (/1.239227245954253487d0, -2.773475128673630330d-2, &
       1.429249233259944463d0 /),cmf,eps10)
  call unit_assert_equal('fgsl_multifit_linear_usvd:rank',3,int(irk,fgsl_int))

  status = fgsl_multifit_wlinear_svd (fmat, wvec, yvec, 1.0E-7_fgsl_double, irk, &
       cvec, cov, chisq, lfit_ws)
  call unit_assert_equal('fgsl_multifit_wlinear_svd:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_multifit_wlinear_svd:cmf', &
       (/1.1824632501803487d0, 0.1845715862795292d0, &
       1.3031038162033384d0 /),cmf,eps10)
  call unit_assert_equal('fgsl_multifit_wlinear_svd:rank',3,int(irk,fgsl_int))

  status = fgsl_multifit_wlinear_usvd (fmat, wvec, yvec, 1.0E-7_fgsl_double, irk, &
       cvec, cov, chisq, lfit_ws)
  call unit_assert_equal('fgsl_multifit_wlinear_usvd:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_multifit_wlinear_usvd:cmf', &
       (/1.1824632501803487d0, 0.1845715862795292d0, &
       1.3031038162033384d0 /),cmf,eps10)
  call unit_assert_equal('fgsl_multifit_wlinear_usvd:rank',3,int(irk,fgsl_int))

  call fgsl_rng_free(rng)
  call fgsl_vector_free(cvec)
  call fgsl_vector_free(yvec)
  call fgsl_vector_free(rvec)
  call fgsl_vector_free(wvec)
  call fgsl_matrix_free(fmat)
  call fgsl_matrix_free(cov)
  call fgsl_multifit_linear_free(lfit_ws)
!
! Done
!
  call unit_finalize() 
end program fit
