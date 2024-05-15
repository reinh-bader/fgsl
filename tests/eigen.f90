program eigen
  use fgsl
  use mod_unit
  implicit none
  real(fgsl_double), parameter :: eps10 = 1.0d-10
  type(fgsl_matrix) :: a, evec
  type(fgsl_matrix_complex) :: ac, evecc
  type(fgsl_vector) :: eval
  real(fgsl_double) :: af(3, 3), evalf(3), evecf(3, 3)
  complex(fgsl_double_complex) :: acf(3, 3), eveccf(3,3)
  complex(fgsl_double_complex), parameter :: ai = (0.0d0, 1.0d0), ui=(1.0d0, 0.0d0)
  type(fgsl_eigen_symm_workspace) :: wks
  type(fgsl_eigen_symmv_workspace) :: wksv
  type(fgsl_eigen_herm_workspace) :: wkh
  type(fgsl_eigen_hermv_workspace) :: wkhv
  type(fgsl_eigen_nonsymm_workspace) :: wkns
  type(fgsl_eigen_nonsymmv_workspace) :: wknsv
  integer(fgsl_int) :: status
  !
  ! Test eigenvectors / eigenvalues
  ! remember that matrices are transposed vs. usual Fortran convention
  !
  call unit_init(200)

  a = fgsl_matrix_init(af)
  evec = fgsl_matrix_init(evecf)
  eval = fgsl_vector_init(evalf)
  ac = fgsl_matrix_init(acf)
  evecc = fgsl_matrix_init(eveccf)
  !
  symm : block
    wks = fgsl_eigen_symm_alloc(3_fgsl_size_t)
    wksv = fgsl_eigen_symmv_alloc(3_fgsl_size_t)
    af = reshape( [ 1.0d0, -1.0d0, 0.0d0, -1.0d0, 2.0d0, 3.0d0, &
         0.0d0, 3.0d0, 1.0d0 ], shape(af) )
    status = fgsl_eigen_symm(a, eval, wks)
    call unit_assert_equal_within('fgsl_eigen_symm:eval',&
         [ (3.0d0+sqrt(41.0d0))/2.0d0, 1.0d0, (3.0d0-sqrt(41.0d0))/2.0d0 ], &
         evalf,eps10)
    af = reshape( [  1.0d0, -1.0d0, 0.0d0, -1.0d0, 2.0d0, 3.0d0, &
         0.0d0, 3.0d0, 1.0d0 ], shape(af))
    status = fgsl_eigen_symmv(a, eval, evec, wksv)
    call unit_assert_equal_within('fgsl_eigen_symmv:eval',&
         [ (3.0d0+sqrt(41.0d0))/2.0d0, 1.0d0, (3.0d0-sqrt(41.0d0))/2.0d0 ], &
         evalf,eps10)
    call unit_assert_equal_within('fgsl_eigen_symmv:evec',&
         reshape( [ 2.054052382622448270D-01, &
         -9.486832980505137680D-01, &
         2.404343737788554036D-01, & 
         -7.603202489374478734D-01, &  
         1.942890293094023946D-16, &  
         6.495483962382611054D-01, & 
         -6.162157147867355356D-01, &
         -3.162277660168379967D-01, &
         -7.213031213365660443D-01 ], shape(evecf)), evecf,eps10)
    call fgsl_eigen_symm_free(wks)
    call fgsl_eigen_symmv_free(wksv)
  end block symm
  !
  herm : block
    wkh = fgsl_eigen_herm_alloc(3_fgsl_size_t)
    wkhv = fgsl_eigen_hermv_alloc(3_fgsl_size_t)
    acf = reshape( ui*[ 1.0d0, -1.0d0, 0.0d0, -1.0d0, 2.0d0, 3.0d0, &
         0.0d0, 3.0d0, 1.0d0 ], shape(acf) )
    status = fgsl_eigen_herm(ac, eval, wkh)
    call unit_assert_equal_within('fgsl_eigen_herm:eval',&
         [ (3.0d0+sqrt(41.0d0))/2.0d0, 1.0d0, (3.0d0-sqrt(41.0d0))/2.0d0 ], &
         evalf,eps10)
    acf = reshape( ui*[  1.0d0, -1.0d0, 0.0d0, -1.0d0, 2.0d0, 3.0d0, &
         0.0d0, 3.0d0, 1.0d0 ], shape(acf))
    status = fgsl_eigen_hermv(ac, eval, evecc, wkhv)
    call unit_assert_equal_within('fgsl_eigen_hermv:eval',&
         [ (3.0d0+sqrt(41.0d0))/2.0d0, 1.0d0, (3.0d0-sqrt(41.0d0))/2.0d0 ], &
         evalf,eps10)
    call unit_assert_equal_within('fgsl_eigen_hermv:evec',&
         reshape( ui*[ 2.054052382622448270D-01, &
         9.486832980505137680D-01, &
         2.404343737788554036D-01, & 
         -7.603202489374478734D-01, &  
         -1.942890293094023946D-16, &  
         6.495483962382611054D-01, & 
         -6.162157147867355356D-01, &
         3.162277660168379967D-01, &
         -7.213031213365660443D-01 ], shape(eveccf)), eveccf,eps10)
    call fgsl_eigen_herm_free(wkh)
    call fgsl_eigen_hermv_free(wkhv)
  end block herm

  !
  nonsymm : block
    wkns = fgsl_eigen_nonsymm_alloc(3_fgsl_size_t)
    wknsv = fgsl_eigen_nonsymmv_alloc(3_fgsl_size_t)
    af = reshape( [ 1.0d0, -1.0d0, 0.0d0, -1.0d0, 2.0d0, 3.0d0, &
         0.0d0, -3.0d0, 1.0d0 ], shape(af) )
    call fgsl_eigen_nonsymm_params(1, 0, wkns)

    
    
    call fgsl_eigen_nonsymm_free(wkns)
    call fgsl_eigen_nonsymmv_free(wknsv)    
  end block nonsymm
  
  ! FIXME: remaining routines

  ! Free matrix and vector resources
  call fgsl_matrix_free(a)
  call fgsl_vector_free(eval)
  call fgsl_matrix_free(evec)
  call fgsl_matrix_free(ac)
  call fgsl_matrix_free(evecc)

  ! 
  ! Done
  !
  call unit_finalize()
end program eigen
