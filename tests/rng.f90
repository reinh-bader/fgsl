program rng
  use fgsl
  use mod_unit
  implicit none
  real(fgsl_double), parameter :: eps10 = 1.0d-10
  character(kind=fgsl_char, len=fgsl_strmax) :: name
  integer(fgsl_long) :: rn
  real(fgsl_double) :: rd, rd_copy, re, rid, rie, qd(1), qd_copy(1)
  type(fgsl_rng) :: r, copy, clone
  type(fgsl_qrng) :: q, qcopy, qclone
  type(fgsl_rng_type) :: t
  type(fgsl_file) :: rfile
  integer :: status
!
! Test random generators
!
  call unit_init(500)
! rng
  t = fgsl_rng_default
  r = fgsl_rng_alloc(t)
  call unit_assert_true('fgsl_rng_alloc',fgsl_well_defined(r),.true.)
!
  name = fgsl_rng_name(r)
  call fgsl_rng_set(r, 219_fgsl_long)
  call unit_assert_equal('fgsl_rng_name:mt19937','mt19937',trim(name))
  rn = fgsl_rng_get(r)
  call unit_assert_equal('fgsl_rng_get',57953729,int(rn))
  rd = fgsl_rng_uniform(r)
  call unit_assert_equal_within('fgsl_rng_uniform', &
       1.232697358354926109D-01,rd,eps10)
  rd = fgsl_rng_uniform_pos(r)
  call unit_assert_equal_within('fgsl_rng_uniform', &
       2.761673277709633112D-01,rd,eps10)
  rn = fgsl_rng_uniform_int(r,1000_fgsl_long)
  call unit_assert_equal('fgsl_rng_max',425,int(rn))
  rn = fgsl_rng_max(r)
! FIXME for 32 bit platforms
!  rn = rn - 4294967295_fgsl_long
!  write(6, *) rn
  rn = fgsl_rng_min(r)
  call unit_assert_equal('fgsl_rng_min',0,int(rn))
!
  copy = fgsl_rng_alloc(t)
  status = fgsl_rng_memcpy(copy,r)
  call unit_assert_true('fgsl_rng_memcpy:defined',fgsl_well_defined(copy),.true.)
  rfile = fgsl_open('rng.dat','w')
  status = fgsl_rng_fwrite(rfile, copy)
  call unit_assert_equal('fgsl_rng_fwrite:status',fgsl_success,status)
  status = fgsl_close(rfile)
  clone = fgsl_rng_clone(r)
  call unit_assert_true('fgsl_rng_clone:defined',fgsl_well_defined(clone),.true.)
  rd = fgsl_rng_uniform(r)
  rd_copy = fgsl_rng_uniform(copy)
  call unit_assert_equal_within('fgsl_rng_memcpy',rd,rd_copy,eps10)
  rd_copy = fgsl_rng_uniform(clone)
  call unit_assert_equal_within('fgsl_rng_clone',rd,rd_copy,eps10)
  rfile = fgsl_open('rng.dat','r')
  status = fgsl_rng_fread(rfile, copy)
  call unit_assert_equal('fgsl_rng_fread:status',fgsl_success,status)
  status = fgsl_close(rfile)
  rd_copy = fgsl_rng_uniform(copy)
  call unit_assert_equal_within('fgsl_rng_fwrite/read',rd,rd_copy,eps10)
! qrng
  q = fgsl_qrng_alloc(fgsl_qrng_sobol,1)
  name = fgsl_qrng_name(q)
  call unit_assert_equal('fgsl_qrng_name','sobol',trim(name))
  call unit_assert_true('fgsl_qrng_alloc',fgsl_well_defined(q),.true.)
  status = fgsl_qrng_get(q, qd)
  call unit_assert_equal_within('fgsl_qrng_get',(/5.d-1/),qd,eps10)
  call fgsl_qrng_init(q)
  status = fgsl_qrng_get(q, qd)
  call unit_assert_equal_within('fgsl_qrng_get',(/5.d-1/),qd,eps10)
  status = fgsl_qrng_get(q, qd)
  qcopy = fgsl_qrng_alloc(fgsl_qrng_sobol,1)
  status = fgsl_qrng_memcpy(qcopy,q)
  call unit_assert_true('fgsl_qrng_memcpy:defined',fgsl_well_defined(qcopy),.true.)
  qclone = fgsl_qrng_clone(q)
  call unit_assert_true('fgsl_qrng_clone:defined',fgsl_well_defined(qclone),.true.)
  status = fgsl_qrng_get(q, qd)
  status = fgsl_qrng_get(qcopy, qd_copy)
  call unit_assert_equal_within('fgsl_qrng_memcpy',qd,qd_copy,eps10)
  status = fgsl_qrng_get(qclone, qd_copy)
  call unit_assert_equal_within('fgsl_qrng_clone',qd,qd_copy,eps10)
! ran
  call fgsl_rng_free(r)
  r = fgsl_rng_alloc(t)
  rd = fgsl_ran_gaussian(r,1.5d0)
  call unit_assert_equal_within('fgsl_ran_gaussian',&
       0.2008779121780138d0,rd,eps10)
  rd = fgsl_ran_gaussian_pdf(2.0d0,1.5d0)
  call unit_assert_equal_within('fgsl_ran_gaussian_pdf',&
       0.1093400497839958d0,rd,eps10)
! updated for 1.12 (old values see commented out lines below)
  rd = fgsl_ran_gaussian_ziggurat(r,1.5d0)
  call unit_assert_equal_within('fgsl_ran_gaussian_ziggurat',&
       6.785042481322760555d-01,rd,eps10)
  rd = fgsl_ran_gaussian_ratio_method(r,1.5d0)
  call unit_assert_equal_within('fgsl_ran_gaussian_ratio_method',&
       2.285846381320067788d+00,rd,eps10)
  rd = fgsl_ran_ugaussian(r)
  call unit_assert_equal_within('fgsl_ran_ugaussian',&
       2.712258328755056125d-01,rd,eps10)
!  before 1.12
!  call unit_assert_equal_within('fgsl_ran_gaussian_ziggurat',&
!       -1.0801457088799626d0,rd,eps10)
!  rd = fgsl_ran_gaussian_ratio_method(r,1.5d0)
!  call unit_assert_equal_within('fgsl_ran_gaussian_ratio_method',&
!       1.3425078569348390d0,rd,eps10)
!  rd = fgsl_ran_ugaussian(r)
!  call unit_assert_equal_within('fgsl_ran_ugaussian',&
!       0.7336411072925795d0,rd,eps10)
  rd = fgsl_ran_ugaussian_pdf(2.0d0)
  call unit_assert_equal_within('fgsl_ran_ugaussian_pdf',&
       0.0539909665131881d0,rd,eps10)
  rd = fgsl_ran_ugaussian_ratio_method(r)
  call unit_assert_equal_within('fgsl_ran_ugaussian_ratio_method',&
       1.714919027680205543d+0,rd,eps10)
!  rd = fgsl_ran_ugaussian_ratio_method(r)
!  call unit_assert_equal_within('fgsl_ran_ugaussian_ratio_method',&
!       0.7630954425575337d0,rd,eps10)
  rd = fgsl_cdf_gaussian_p(2.0d0,1.0d0)
  call unit_assert_equal_within('fgsl_cdf_gaussian_p',&
       0.9772498680518208d0,rd,eps10)
  re = fgsl_cdf_gaussian_q(2.0d0,1.0d0)
  call unit_assert_equal_within('fgsl_cdf_gaussian_q',&
       0.0227501319481792d0,re,eps10)
  rid = fgsl_cdf_gaussian_pinv(rd,1.0d0)
  call unit_assert_equal_within('fgsl_cdf_gaussian_pinv',&
       2.0d0,rid,eps10)
  rie = fgsl_cdf_gaussian_qinv(re,1.0d0)
  call unit_assert_equal_within('fgsl_cdf_gaussian_qinv',&
       2.0d0,rie,eps10)
  rd = fgsl_cdf_ugaussian_p(2.0d0)
  call unit_assert_equal_within('fgsl_cdf_ugaussian_p',&
       0.9772498680518208d0,rd,eps10)
  re = fgsl_cdf_ugaussian_q(2.0d0)
  call unit_assert_equal_within('fgsl_cdf_ugaussian_q',&
       0.0227501319481792d0,re,eps10)
  rid = fgsl_cdf_ugaussian_pinv(rd)
  call unit_assert_equal_within('fgsl_cdf_ugaussian_pinv',&
       2.0d0,rid,eps10)
  rie = fgsl_cdf_ugaussian_qinv(re)
  call unit_assert_equal_within('fgsl_cdf_ugaussian_qinv',&
       2.0d0,rie,eps10)

! 
  call fgsl_rng_free(r)
  call fgsl_rng_free(copy)
  call fgsl_rng_free(clone)
  call fgsl_qrng_free(q)
  call fgsl_qrng_free(qcopy)
  call fgsl_qrng_free(qclone)
!
! Done
!
  call unit_finalize()
end program rng
