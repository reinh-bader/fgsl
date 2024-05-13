program linalg
  use fgsl
  use mod_unit
  implicit none
  real(fgsl_double), parameter :: eps10 = 1.0d-10
  complex(fgsl_double), parameter :: ai = (0.0d0, 1.0d0), ui=(1.0d0, 0.0d0)
  type(fgsl_matrix) :: a, a_orig, inv, q, r
  type(fgsl_matrix_complex) :: ac, ac_orig, invc
  type(fgsl_vector) :: b, x, res, tau, sd, work, wk2
  type(fgsl_vector_complex) :: bc, xc, resc
  real(fgsl_double), target :: af(3, 3), af_orig(3, 3), bf(3), xf(3), resf(3), &
       invf(3, 3), tauf(3), qf(3, 3), rf(3, 3), mf(3, 3), sdf(3), workf(9), wk2f(3)
  complex(fgsl_double_complex) :: acf(3, 3), xcf(3), bcf(3), rescf(3), acf_orig(3, 3), &
       invcf(3,3)
  real(fgsl_double) :: det, lndet, rcond
  complex(fgsl_double_complex) :: detc, sgnc
  type(fgsl_permutation) :: p
  integer(fgsl_int) :: status, signum, ip(3), sgn
  integer(fgsl_size_t) :: i, rank
  !
  ! Test linear algebra
  ! remember that matrices are transposed vs. usual Fortran convention
  !
  call unit_init(200)
  sd = fgsl_vector_init(sdf)
  work = fgsl_vector_init(workf)
  wk2 = fgsl_vector_init(wk2f)

  lu : block
    !
    ! LU - real
    !
    af = reshape( [1.0d0, 1.0d0, 0.0d0, 1.0d0, 0.0d0, 1.0d0, &
         0.0d0, 2.0d0, 1.0d0 ], shape(af))
    af_orig = af
    a = fgsl_matrix_init(af)
    a_orig = fgsl_matrix_init(af_orig)
    inv = fgsl_matrix_init(invf)

    p = fgsl_permutation_alloc(3_fgsl_size_t)
    status = fgsl_linalg_lu_decomp(a, p, signum)
    call unit_assert_equal('fgsl_linalg_lu_decomp:status',fgsl_success,status)
    !  write(*, *) af
    do i=1,3
       ip(i) = fgsl_permutation_get(p, i-1)
    end do
    !  write(*, *) ip, signum
    call unit_assert_equal_within('fgsl_linalg_lu_decomp:a',&
         reshape( [1.d0, 1.d0, 0.d0, 0.d0, 2.d0, 1.d0, 1.d0, -0.5d0, 1.5d0 ], &
         shape(af)),af,eps10)
    call unit_assert_equal('fgsl_linalg_lu_decomp:p',[ 0, 2, 1],ip)
    call unit_assert_equal('fgsl_linalg_lu_decomp:signum',-1,signum)
    bf = [ 1.0d0, 2.0d0, 3.0d0 ]
    b = fgsl_vector_init(bf)
    x = fgsl_vector_init(xf)
    res = fgsl_vector_init(resf)
    status = fgsl_linalg_lu_solve(a, p, b, x)
    call unit_assert_equal('fgsl_linalg_lu_solve:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_lu_solve:x',&
         [1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0],xf,eps10)
    !  write(*, *) xf
    xf = bf
    status = fgsl_linalg_lu_svx(a, p, x)
    call unit_assert_equal('fgsl_linalg_lu_svx:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_lu_svx:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ],xf,eps10)
    ! small deformation of solution for refinement
    xf(1) = xf(1) + 1.0d-5
    xf(3) = xf(3) - 2.0d-5
    status = fgsl_linalg_lu_refine (a_orig, a, p, b, x, res)
    call unit_assert_equal('fgsl_linalg_lu_refine:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_lu_refine:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ],xf,eps10)
    call unit_assert_equal_within('fgsl_linalg_lu_refine:res',&
         [ 1.0d-5, 0.0d0, -2.0d-5 ],resf,eps10)
    status = fgsl_linalg_lu_invert(a, p, inv)
    call unit_assert_equal_within('fgsl_linalg_lu_decomp:a',&
         reshape([ 2.d0/3.0d0, 1.d0/3.0d0, -1.d0/3.0d0, &
         1.d0/3.0d0, -1.d0/3.0d0, 1.d0/3.0d0, &
         -2.d0/3.0d0, 2.d0/3.0d0, 1.d0/3.0d0 ], &
         shape(invf)),invf,eps10)
    det = fgsl_linalg_lu_det(a, signum)
    call unit_assert_equal_within('fgsl_linalg_lu_det',&
         -3.0d0,det,eps10)
    lndet = fgsl_linalg_lu_lndet(a)
    call unit_assert_equal_within('fgsl_linalg_lu_lndet',&
         log(3.0d0),lndet,eps10)
    sgn = fgsl_linalg_lu_sgndet(a, signum)
    call unit_assert_equal('fgsl_linalg_lu_sgndet',-1,sgn)
    !
    ! LU complex
    !

    acf = reshape( [ ui, ai, 0.0d0*ui, -ai, 0.0d0*ui, ui, &
         0.0d0*ui, 2.0d0*ui, ai ], shape(acf))
    ac = fgsl_matrix_init(acf)
    acf_orig = acf
    ac_orig = fgsl_matrix_init(acf_orig)
    bcf = [ ai, 2.0d0*ui, ui+ai ]
    bc = fgsl_vector_init(bcf)
    xc = fgsl_vector_init(xcf)
    resc = fgsl_vector_init(rescf)

    status = fgsl_linalg_complex_lu_decomp(ac, p, signum)
    call unit_assert_equal('fgsl_linalg_complex_lu_decomp:status',fgsl_success,status)
    !  write(*, *) acf
    do i=1,3
       ip(i) = fgsl_permutation_get(p, i-1)
    end do
    !  write(*, *) ip, signum
    call unit_assert_equal_within('fgsl_linalg_complex_lu_decomp:a',&
         reshape( [ ui, ai, 0*ui, 0*ui, 2*ui, ai, -ai, -0.5d0*ui, ui+0.5d0*ai ], &
         shape(acf)),acf,eps10)
    call unit_assert_equal('fgsl_linalg_lu_decomp:p',[ 0, 2, 1 ],ip)
    call unit_assert_equal('fgsl_linalg_lu_decomp:signum',-1,signum)
    status = fgsl_linalg_complex_lu_solve(ac, p, bc, xc)
    call unit_assert_equal('fgsl_linalg_complex_lu_solve:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_complex_lu_solve:x',&
         [ (-ui+3*ai)/5.0d0, (2*ui-ai)/5.0d0, (7*ui-ai)/5.0d0 ],xcf,eps10)
    xcf = bcf
    status = fgsl_linalg_complex_lu_svx(ac, p, xc)
    call unit_assert_equal('fgsl_linalg_complex_lu_svx:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_complex_lu_svx:x',&
         [ (-ui+3*ai)/5.0d0, (2*ui-ai)/5.0d0, (7*ui-ai)/5.0d0 ],xcf,eps10)
    xcf(1) = xcf(1) + 1.0d-5 * ui
    xcf(3) = xcf(3) - 2.0d-5 * ui
    status = fgsl_linalg_complex_lu_refine (ac_orig, ac, p, bc, xc, resc)
    call unit_assert_equal('fgsl_linalg_lu_refine:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_lu_refine:x',&
         [ (-ui+3*ai)/5.0d0, (2*ui-ai)/5.0d0, (7*ui-ai)/5.0d0 ],xcf,eps10)
    call unit_assert_equal_within('fgsl_linalg_lu_refine:res',&
         [ 1.0d-5*ui, 0.0d0*ui, -2.0d-5*ui ],rescf,eps10)

    invc = fgsl_matrix_init(invcf)
    status = fgsl_linalg_complex_lu_invert(ac, p, invc)
    call unit_assert_equal_within('fgsl_linalg_complex_lu_decomp:a',&
         reshape([ .8d0*ui-.4d0*ai, -.4d0*ui+.2d0*ai,-.2d0*ui-.4d0*ai, &
         .4d0*ui-.2d0*ai, -.2d0*ui-.4d0*ai, .4d0*ui-.2d0*ai, &
         .4d0*ui+.8d0*ai, .8d0*ui-.4d0*ai, .4d0*ui-.2d0*ai ], &
         shape(invcf)),invcf,eps10)

    detc = fgsl_linalg_complex_lu_det(ac, signum)
    call unit_assert_equal_within('fgsl_linalg_complex_lu_det',&
         -2.d0*ui-1.0d0*ai,detc,eps10)

    lndet = fgsl_linalg_complex_lu_lndet(ac)
    call unit_assert_equal_within('fgsl_linalg_complex_lu_lndet',&
         8.047189562170502519D-01,lndet,eps10)
    sgnc = fgsl_linalg_complex_lu_sgndet(ac, signum)
    call unit_assert_equal_within('fgsl_linalg_complex_lu_sgndet',&
         -8.944271909999158554d-01*(ui+.5d0*ai),sgnc,eps10)
  end block lu
  qr : block
    !
    ! QR
    !
    q = fgsl_matrix_init(qf)
    r = fgsl_matrix_init(rf)
    tau = fgsl_vector_init(tauf)
    af = af_orig
    status = fgsl_linalg_qr_decomp(a, tau)
    call unit_assert_equal('fgsl_linalg_qr_decomp:status',fgsl_success,status)
    status = fgsl_linalg_qr_solve(a, tau, b, x)
    call unit_assert_equal('fgsl_linalg_qr_solve:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_qr_solve:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ],xf,eps10)
    xf = bf
    status = fgsl_linalg_qr_svx(a, tau, x)
    call unit_assert_equal('fgsl_linalg_qr_svx:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_qr_svx:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ],xf,eps10)
    status = fgsl_linalg_qr_lssolve(a, tau, b, x, res)
    call unit_assert_equal('fgsl_linalg_qr_lssolve:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_qr_lssolve:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ],xf,eps10)
    call unit_assert_equal_within('fgsl_linalg_qr_lssolve:res',&
         [ 0.0d0, 0.0d0, 0.0d0 ],resf,eps10)
    status = fgsl_linalg_qr_unpack (a, tau, q, r)
    call unit_assert_equal('fgsl_linalg_qr_unpack:status',fgsl_success,status)
    resf = [ 1.0d0, 2.0d0, 3.0d0 ]
    status = fgsl_linalg_qr_qtvec (a, tau, res)
    call unit_assert_equal('fgsl_linalg_qr_qtvec:status',fgsl_success,status)
    xf = matmul(qf,  [ 1.0d0, 2.0d0, 3.0d0 ])
    call unit_assert_equal_within('fgsl_linalg_qr_qtvec:res',&
         xf,resf,eps10)
    resf = [ 1.0d0, 2.0d0, 3.0d0 ]
    status = fgsl_linalg_qr_qvec (a, tau, res)
    call unit_assert_equal('fgsl_linalg_qr_qvec:status',fgsl_success,status)
    xf = matmul(transpose(qf),  [ 1.0d0, 2.0d0, 3.0d0 ])
    call unit_assert_equal_within('fgsl_linalg_qr_qvec:res',&
         xf,resf,eps10)
    invf = reshape([ 1.0d0, 0.0d0, 0.0d0, 0.0d0, 1.0d0, 0.0d0, &
         0.0d0, 0.0d0, 1.0d0 ], shape(invf))
    status = fgsl_linalg_qr_qtmat (a, tau, inv)
    call unit_assert_equal('fgsl_linalg_qr_qtvec:status',fgsl_success,status)
    mf = matmul(transpose(qf), &
         reshape([ 1.0d0, 0.0d0, 0.0d0, 0.0d0, 1.0d0, 0.0d0, &
         0.0d0, 0.0d0, 1.0d0 ], shape(mf)) )
    call unit_assert_equal_within('fgsl_linalg_qr_qtvec:res',&
         mf,invf,eps10)
    status = fgsl_linalg_qr_rsolve(a, b, x)
    call unit_assert_equal('fgsl_linalg_qr_rsolve:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_qr_rsolve:x',&
         [ -(1.0d0+5.0d0/(3.0d0*sqrt(2.0d0))), &
         (2.0d0*sqrt(2.0d0)-3.0d0)/3.0d0, 3.0d0 ],xf,eps10)
    xf = bf
    status = fgsl_linalg_qr_rsvx(a, x)
    call unit_assert_equal('fgsl_linalg_qr_rsvx:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_qr_rsvx:x',&
         [ -(1.0d0+5.0d0/(3.0d0*sqrt(2.0d0))), &
         (2.0d0*sqrt(2.0d0)-3.0d0)/3.0d0, 3.0d0 ],xf,eps10)
    status = fgsl_linalg_qr_qrsolve(q, r, b, x)
    call unit_assert_equal('fgsl_linalg_qr_qrsolve:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_qr_qrsolve:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ],xf,eps10)
    status = fgsl_linalg_r_solve(r, b, x)
    call unit_assert_equal('fgsl_linalg_r_solve:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_r_solve:x',&
         [ -(1.0d0+5.0d0/(3.0d0*sqrt(2.0d0))), &
         (2.0d0*sqrt(2.0d0)-3.0d0)/3.0d0, 3.0d0 ],xf,eps10)
    xf = bf
    status = fgsl_linalg_r_svx(r, x)
    call unit_assert_equal('fgsl_linalg_r_svx:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_r_svx:x',&
         [ -(1.0d0+5.0d0/(3.0d0*sqrt(2.0d0))), &
         (2.0d0*sqrt(2.0d0)-3.0d0)/3.0d0, 3.0d0 ],xf,eps10)
    xf = [ -1.0d0, 0.0d0, 0.0d0 ]
    status = fgsl_linalg_qr_update (q, r, x, b)
    call unit_assert_equal('fgsl_linalg_qr_update:status',fgsl_success,status)
    mf = matmul(rf, qf)
    af = af_orig - matmul(reshape([ 1.0d0, 2.0d0, 3.0d0, &
         0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0 ], shape(af)),qf)
    call unit_assert_equal_within('fgsl_linalg_qr_update:mf',&
         af,mf,eps10)
    af = af_orig
    status = fgsl_linalg_qrpt_decomp(a, tau, p, signum, res)
    call unit_assert_equal('fgsl_linalg_qrpt_decomp:status',fgsl_success,status)
    status = fgsl_linalg_qrpt_solve(a, tau, p, b, x)
    call unit_assert_equal('fgsl_linalg_qrpt_solve:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_qrpt_solve:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ],xf,eps10)
    xf = bf
    status = fgsl_linalg_qrpt_svx(a, tau, p, x)
    call unit_assert_equal('fgsl_linalg_qrpt_svx:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_qrpt_svx:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ],xf,eps10)
    status = fgsl_linalg_qrpt_lssolve(a, tau, p, b, x, sd)
    call unit_assert_equal('fgsl_linalg_qrpt_lssolve:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_qrpt_lssolve:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ],xf,eps10)
    call unit_assert_equal_within('fgsl_linalg_qrpt_lssolve:residual',&
         [ 0.0d0, 0.0d0, 0.0d0 ],sdf,eps10)
    status = fgsl_linalg_qrpt_rsolve(a, p, b, x)
    call unit_assert_equal('fgsl_linalg_qrpt_rsolve:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_qrpt_rsolve:x',&
         [ -1.0d0-2*sqrt(5.0d0)/3.0d0, &
         -1.0d0-1.0d0/(3*sqrt(5.0d0)), 3.0d0 ],xf,eps10)
    xf = bf
    status = fgsl_linalg_qrpt_rsvx(a, p, x)
    call unit_assert_equal('fgsl_linalg_qrpt_rsvx:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_qrpt_rsvx:x',&
         [ -1.0d0-2*sqrt(5.0d0)/3.0d0, &
         -1.0d0-1.0d0/(3*sqrt(5.0d0)), 3.0d0 ],xf,eps10)
    af = af_orig
    status = fgsl_linalg_qrpt_decomp2(a, q, r, tau, p, signum, res)
    call unit_assert_equal('fgsl_linalg_qrpt_decomp2:status',fgsl_success,status)
    status = fgsl_linalg_qrpt_qrsolve(q, r, p, b, x)
    call unit_assert_equal('fgsl_linalg_qrpt_qrsolve:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_qrpt_qrsolve:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ],xf,eps10)
    xf = [ -1.0d0, 0.0d0, 0.0d0 ]
    status = fgsl_linalg_qrpt_update (q, r, p, x, b)
    call unit_assert_equal('fgsl_linalg_qrpt_update:status',fgsl_success,status)
    mf = matmul(rf, qf)
    invf =  - matmul(reshape([ 1.0d0, 2.0d0, 3.0d0, &
         0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0 ], shape(invf)),qf)
    ! p is (1 0 2)
    af(1, 1:3) = af_orig(2, 1:3) + invf(2, 1:3)
    af(2, 1:3) = af_orig(1, 1:3) + invf(1, 1:3)
    af(3, 1:3) = af_orig(3, 1:3) + invf(3, 1:3)
    call unit_assert_equal_within('fgsl_linalg_qrpt_update:mf',&
         af,mf,eps10)

    af = reshape( [ 1.0d0, 1.0d0, 0.0d0, 1.0d0, 0.0d0, 1.0d0, &
         2.0d0, 1.0d0, 1.0d0 ], shape(af) )
    status = fgsl_linalg_qrpt_decomp(a, tau, p, signum, res)
    call unit_assert_equal('fgsl_linalg_qrpt_decomp(singular):status',fgsl_success,status)
    rank = fgsl_linalg_qrpt_rank(a, tol=-1.0d0)
    call unit_assert_equal('fgsl_linalg_qrpt_rank',2_fgsl_size_t,rank)
    status = fgsl_linalg_qrpt_lssolve2(a, tau, p, b, rank, x, sd)
    call unit_assert_equal('fgsl_linalg_qrpt_lssolve2:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_qrpt_lssolve2:x',&
         [ 2.0d0, -1.0d0, 0.0d0 ],xf,eps10)
    call unit_assert_equal_within('fgsl_linalg_qrpt_lssolve2:residual',&
         [ 0.0d0, 0.0d0, 0.0d0 ],sdf,eps10)
  end block qr
  lq : block
    !
    ! LQ
    !
    af = af_orig
    status = fgsl_linalg_lq_decomp(a, tau)
    call unit_assert_equal('fgsl_linalg_lq_decomp:status',fgsl_success,status)
    status = fgsl_linalg_lq_lssolve(a, tau, b, x, res)
    call unit_assert_equal_within('fgsl_linalg_lq_lssolve:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ],xf,eps10)
    !
    ! QL
    !
    af = af_orig
    status = fgsl_linalg_ql_decomp(a, tau)
    call unit_assert_equal('fgsl_linalg_ql_decomp:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_ql_decomp:af',&
         reshape( [  1.22474487139D+00, -3.17837245195D-01, 0.D0, &
         1.11022302463D-16,  1.73205080757D+00, 4.14213562373D-01, &
         -7.07106781187D-01, -1.41421356237D+00,-1.41421356237D+00 &
         ], shape(af)),af,eps10)
    call unit_assert_equal_within('fgsl_linalg_ql_decomp:tau',&
         [ 0.0D0,  1.8164965809D+00, 1.7071067812D+00  ],tauf,eps10)
  end block lq
  cod : block
    !
    ! COD
    !
    af = af_orig
    status = fgsl_linalg_cod_decomp(a, tau, sd, p, rank, x)
    call unit_assert_equal('fgsl_linalg_cod_decomp:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_cod_decomp:tau_q',&
         [ 1.44721359549996d0, 1.74535599249993d0, 0.00000000000000d0 ],tauf,eps10)
    call unit_assert_equal_within('fgsl_linalg_cod_decomp:tau_z',&
         [ -3.525431591703201d-16, -3.525431591703199d-16, 3.525431591703201d-16 ],sdf,eps10)
    status = fgsl_linalg_cod_lssolve(a, tau, sd, p, rank, b, x, res)
    call unit_assert_equal_within('fgsl_linalg_cod_lssolve:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ],xf,eps10)
    call unit_assert_equal_within('fgsl_linalg_cod_lssolve:res',&
         [ 0.0d0, 0.0d0, 0.0d0 ],resf,eps10)
    status = fgsl_linalg_cod_lssolve2(1.e-12_fgsl_double, a, tau, sd, p, rank, b, x, res, q, wk2)
    call unit_assert_equal_within('fgsl_linalg_cod_lssolve2:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ] ,xf,eps10)
    call unit_assert_equal_within('fgsl_linalg_cod_lssolve2:res',&
         [ 0.0d0, 0.0d0, 0.0d0 ],resf,eps10)
  end block cod
  svd : block
    !
    ! SVD
    !
    af = af_orig
    status = fgsl_linalg_sv_decomp(a, q, tau, res)
    call unit_assert_equal('fgsl_linalg_sv_decomp:status',fgsl_success,status)
    status = fgsl_linalg_sv_solve (a, q, tau, b, x)
    call unit_assert_equal('fgsl_linalg_sv_solve:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_sv_solve:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ],xf,eps10)

    af = af_orig
    status = fgsl_linalg_sv_leverage(a, x)
    call unit_assert_equal_within('fgsl_linalg_sv_leverage:x',&
         [ 2.0d0, 2.0d0, 5.0d0 ],xf,eps10)
    af = af_orig
    status = fgsl_linalg_sv_decomp_mod(a, r, q, tau, res)
    call unit_assert_equal('fgsl_linalg_sv_decomp_mod:status',fgsl_success,status)
    status = fgsl_linalg_sv_solve (a, q, tau, b, x)
    call unit_assert_equal('fgsl_linalg_sv_solve_mod:status',fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_sv_solve_mod:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ],xf,eps10)
    af = af_orig
    status = fgsl_linalg_sv_decomp_jacobi(a, q, tau)
    call unit_assert_equal('fgsl_linalg_sv_decomp_jacobi:status',&
         fgsl_success,status)
    status = fgsl_linalg_sv_solve (a, q, tau, b, x)
    call unit_assert_equal('fgsl_linalg_sv_solve_jacobi:status',&
         fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_sv_solve:x',&
         [ 1.0d0/3.0d0, 2.0d0/3.0d0, 5.0d0/3.0d0 ],xf,eps10)
  end block svd
  chol : block
    !
    ! Cholesky
    !
    af_orig = reshape([ 2.0d0, 0.0d0, 0.0d0, 0.0d0, 2.0d0, 0.0d0, &
         1.0d0, 1.0d0, 2.0d0 ], shape(af_orig))
    af = af_orig
    status = fgsl_linalg_cholesky_decomp1(a)
    call unit_assert_equal('fgsl_linalg_cholesky_decomp1:status', &
         fgsl_success,status)
    status = fgsl_linalg_cholesky_solve (a, b, x)
    call unit_assert_equal('fgsl_linalg_cholesky_solve:status',&
         fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_cholesky_solve:x',&
         [ -.25d0, .25d0, 1.5d0 ],xf,eps10)
    xf = bf
    status = fgsl_linalg_cholesky_svx (a, x)
    call unit_assert_equal('fgsl_linalg_cholesky_svx:status',&
         fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_cholesky_svx:x',&
         [ -.25d0, .25d0, 1.5d0 ],xf,eps10)
    status = fgsl_linalg_cholesky_rcond (a, rcond, work)
    call unit_assert_equal('fgsl_linalg_cholesky_rcond:status',&
         fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_cholesky_rcond:rcond',&
         .125d0,rcond,eps10)

    af = af_orig
    status = fgsl_linalg_cholesky_decomp2(a, sd)
    call unit_assert_equal('fgsl_linalg_cholesky_decomp2:status', &
         fgsl_success,status)
    status = fgsl_linalg_cholesky_solve2 (a, sd, b, x)
    call unit_assert_equal('fgsl_linalg_cholesky_solve2:status',&
         fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_cholesky_solve2:x',&
         [ -.25d0, .25d0, 1.5d0 ],xf,eps10)
    xf = bf
    status = fgsl_linalg_cholesky_svx2 (a, sd, x)
    call unit_assert_equal('fgsl_linalg_cholesky_svx2:status',&
         fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_cholesky_svx2:x',&
         [ -.25d0, .25d0, 1.5d0 ],xf,eps10)
    !
    !  complex Cholesky
    !
    acf = af_orig * ui
    bcf = bf * ui
    status = fgsl_linalg_complex_cholesky_decomp(ac)
    call unit_assert_equal('fgsl_linalg_complex_cholesky_decomp:status', &
         fgsl_success,status)
    status = fgsl_linalg_complex_cholesky_solve (ac, bc, xc)
    call unit_assert_equal('fgsl_linalg_complex_cholesky_solve:status',&
         fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_complex_cholesky_solve:xc',&
         [ -.25d0, .25d0, 1.5d0 ]*ui,xcf,eps10)
    xcf = bf * ui
    status = fgsl_linalg_complex_cholesky_svx (ac, xc)
    call unit_assert_equal('fgsl_linalg_complex_cholesky_svx:status',&
         fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_complex_cholesky_svx:x',&
         [ -.25d0, .25d0, 1.5d0 ]*ui,xcf,eps10)
    !
    !  pivoted Cholesky
    !
    af = af_orig
    status = fgsl_linalg_pcholesky_decomp(a, p)
    call unit_assert_equal('fgsl_linalg_pcholesky_decomp:status', &
         fgsl_success,status)
    status = fgsl_linalg_pcholesky_solve (a, p, b, x)
    call unit_assert_equal('fgsl_linalg_pcholesky_solve:status',&
         fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_pcholesky_solve:x',&
         [ -.25d0, .25d0, 1.5d0 ],xf,eps10)
    xf = bf
    status = fgsl_linalg_pcholesky_svx (a, p, x)
    call unit_assert_equal('fgsl_linalg_pcholesky_svx:status',&
         fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_pcholesky_svx:x',&
         [ -.25d0, .25d0, 1.5d0 ],xf,eps10)
    af = af_orig
    status = fgsl_linalg_pcholesky_decomp2(a, p, sd)
    call unit_assert_equal('fgsl_linalg_pcholesky_decomp2:status', &
         fgsl_success,status)
    status = fgsl_linalg_pcholesky_solve2 (a, p, sd, b, x)
    call unit_assert_equal('fgsl_linalg_pcholesky_solve2:status',&
         fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_pcholesky_solve2:x',&
         [ -.25d0, .25d0, 1.5d0 ],xf,eps10)
    xf = bf
    status = fgsl_linalg_pcholesky_svx2 (a, p, sd, x)
    call unit_assert_equal('fgsl_linalg_pcholesky_svx2:status',&
         fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_pcholesky_svx2:x',&
         [ -.25d0, .25d0, 1.5d0 ],xf,eps10)
    !
    ! modified Cholesky
    !
    af_orig = reshape( [ 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
         1.0d0, 1.0d0, 2.0d0 ], shape(af_orig))
    af = af_orig
    status = fgsl_linalg_mcholesky_decomp(a, p, sd)
    call unit_assert_equal('fgsl_linalg_mcholesky_decomp:status', &
         fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_mcholesky_decomp:work',&
         [ 1.d0, 2.d0, 0.d0 ],sdf,eps10)
    status = fgsl_linalg_mcholesky_solve (a, p, b, x)
    call unit_assert_equal('fgsl_linalg_mcholesky_solve:status',&
         fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_mcholesky_solve:x',&
         [ -1.d0, 0.d0, 2.d0 ],xf,eps10)
    xf = bf
    status = fgsl_linalg_mcholesky_svx (a, p, x)
    call unit_assert_equal('fgsl_linalg_mcholesky_svx:status',&
         fgsl_success,status)
    call unit_assert_equal_within('fgsl_linalg_mcholesky_svx:x',&
         [ -1.d0, 0.d0, 2.d0 ],xf,eps10)
  end block chol
  !
  ! LDLT decomposition
  !
  ldlt : block
    af_orig = reshape( [ 2.0d0, 0.0d0, 0.0d0, 0.0d0, 2.0d0, 0.0d0, &
         1.0d0, 1.0d0, 2.0d0 ], shape(af_orig))
    af = af_orig
    status = fgsl_linalg_ldlt_decomp(a)
    call unit_assert_equal('fgsl_linalg_ldlt_decomp:status', &
         fgsl_success,status)
    status = fgsl_linalg_ldlt_solve (a, b, x)
    call unit_assert_equal('fgsl_linalg_ldlt_solve:status',&
         fgsl_success,status)   
    call unit_assert_equal_within('fgsl_linalg_ldlt_solve:x',&
         [ -.25d0, .25d0, 1.5d0 ],xf,eps10)  
    xf = bf
    status = fgsl_linalg_ldlt_svx (a, x)
    call unit_assert_equal('fgsl_linalg_mcholesky_svx:status',&
         fgsl_success,status)  
    call unit_assert_equal_within('fgsl_linalg_ldlt_solve:x',&
         [ -.25d0, .25d0, 1.5d0 ],xf,eps10)
  end block ldlt
  !
  ! cleanup
  !
  call fgsl_permutation_free(p)
  call fgsl_matrix_free(a)
  call fgsl_matrix_free(ac)
  call fgsl_matrix_free(inv)
  call fgsl_matrix_free(invc)
  call fgsl_matrix_free(q)
  call fgsl_matrix_free(r)
  call fgsl_matrix_free(a_orig)
  call fgsl_matrix_free(ac_orig)
  call fgsl_vector_free(b)
  call fgsl_vector_free(bc)
  call fgsl_vector_free(sd)
  call fgsl_vector_free(work)
  call fgsl_vector_free(x)
  call fgsl_vector_free(xc)
  call fgsl_vector_free(res)
  call fgsl_vector_free(resc)
  call fgsl_vector_free(tau)
  !
  ! Done
  !
  call unit_finalize()
end program linalg
