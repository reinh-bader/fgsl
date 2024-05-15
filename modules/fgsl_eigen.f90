module fgsl_eigen
  !> Eigensystems
  use fgsl_array
  implicit none

  private :: gsl_eigen_symm_alloc, gsl_eigen_symm_free, gsl_eigen_symm, &
       gsl_eigen_symmv_alloc, gsl_eigen_symmv_free,  gsl_eigen_symmv, &
       gsl_eigen_herm_alloc, gsl_eigen_herm_free, gsl_eigen_herm, &
       gsl_eigen_hermv_alloc, gsl_eigen_hermv_free, gsl_eigen_hermv, &
       gsl_eigen_nonsymm_alloc, gsl_eigen_nonsymm_free, gsl_eigen_nonsymm_params, &
       gsl_eigen_nonsymm, gsl_eigen_nonsymm_z, &
       gsl_eigen_nonsymmv_alloc, gsl_eigen_nonsymmv_free, gsl_eigen_nonsymmv_params, &
       gsl_eigen_nonsymmv, gsl_eigen_nonsymmv_z, &
       gsl_eigen_gensymm_alloc, gsl_eigen_gensymm_free, gsl_eigen_gensymm, &
       gsl_eigen_gensymmv_alloc, gsl_eigen_gensymmv_free, gsl_eigen_gensymmv, &
       gsl_eigen_genherm_alloc, gsl_eigen_genherm_free, gsl_eigen_genherm, &
       gsl_eigen_genhermv_alloc, gsl_eigen_genhermv_free, gsl_eigen_genhermv, &
       gsl_eigen_gen_alloc, gsl_eigen_gen_free, gsl_eigen_gen_params, gsl_eigen_gen, &
       gsl_eigen_gen_qz, &
       gsl_eigen_genv_alloc,  gsl_eigen_genv_free, gsl_eigen_genv,  gsl_eigen_genv_qz, &
       gsl_eigen_gensymmv_sort, gsl_eigen_genhermv_sort, gsl_eigen_genv_sort   
       
  !
  ! Types: Eigensystems
  type, public :: fgsl_eigen_symm_workspace
     private
     type(c_ptr) :: gsl_eigen_symm_workspace = c_null_ptr
  end type fgsl_eigen_symm_workspace
  type, public :: fgsl_eigen_symmv_workspace
     private
     type(c_ptr) :: gsl_eigen_symmv_workspace = c_null_ptr
  end type fgsl_eigen_symmv_workspace
  type, public :: fgsl_eigen_herm_workspace
     private
     type(c_ptr) :: gsl_eigen_herm_workspace = c_null_ptr
  end type fgsl_eigen_herm_workspace
  type, public :: fgsl_eigen_hermv_workspace
     private
     type(c_ptr) :: gsl_eigen_hermv_workspace = c_null_ptr
  end type fgsl_eigen_hermv_workspace
  type, public :: fgsl_eigen_nonsymm_workspace
     private
     type(c_ptr) :: gsl_eigen_nonsymm_workspace = c_null_ptr
  end type fgsl_eigen_nonsymm_workspace
  type, public :: fgsl_eigen_nonsymmv_workspace
     private
     type(c_ptr) :: gsl_eigen_nonsymmv_workspace = c_null_ptr
  end type fgsl_eigen_nonsymmv_workspace
  type, public :: fgsl_eigen_gensymm_workspace
     private
     type(c_ptr) :: gsl_eigen_gensymm_workspace = c_null_ptr
  end type fgsl_eigen_gensymm_workspace
  type, public :: fgsl_eigen_gensymmv_workspace
     private
     type(c_ptr) :: gsl_eigen_gensymmv_workspace = c_null_ptr
  end type fgsl_eigen_gensymmv_workspace
  type, public :: fgsl_eigen_genherm_workspace
     private
     type(c_ptr) :: gsl_eigen_genherm_workspace = c_null_ptr
  end type fgsl_eigen_genherm_workspace
  type, public :: fgsl_eigen_genhermv_workspace
     private
     type(c_ptr) :: gsl_eigen_genhermv_workspace = c_null_ptr
  end type fgsl_eigen_genhermv_workspace
  type, public :: fgsl_eigen_gen_workspace
     private
     type(c_ptr) :: gsl_eigen_gen_workspace = c_null_ptr
  end type fgsl_eigen_gen_workspace
  type, public :: fgsl_eigen_genv_workspace
     private
     type(c_ptr) :: gsl_eigen_genv_workspace = c_null_ptr
  end type fgsl_eigen_genv_workspace
  integer(c_int), parameter, public :: fgsl_eigen_sort_val_asc = 0
  integer(c_int), parameter, public :: fgsl_eigen_sort_val_desc = 1
  integer(c_int), parameter, public :: fgsl_eigen_sort_abs_asc = 2
  integer(c_int), parameter, public :: fgsl_eigen_sort_abs_desc = 3
  !
  ! C interfaces
  interface
     function gsl_eigen_symm_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_eigen_symm_alloc
     end function gsl_eigen_symm_alloc
     subroutine gsl_eigen_symm_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_eigen_symm_free
     function gsl_eigen_symm(a, eval, w) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a, eval, w
       integer(c_int) :: gsl_eigen_symm
     end function gsl_eigen_symm
     function gsl_eigen_symmv_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_eigen_symmv_alloc
     end function gsl_eigen_symmv_alloc
     subroutine gsl_eigen_symmv_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_eigen_symmv_free
     function gsl_eigen_symmv(a, eval, evec, w) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a, eval, evec, w
       integer(c_int) :: gsl_eigen_symmv
     end function gsl_eigen_symmv
     function gsl_eigen_herm_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_eigen_herm_alloc
     end function gsl_eigen_herm_alloc
     subroutine gsl_eigen_herm_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_eigen_herm_free
     function gsl_eigen_herm(a, eval, w) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a, eval, w
       integer(c_int) :: gsl_eigen_herm
     end function gsl_eigen_herm
     function gsl_eigen_hermv_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_eigen_hermv_alloc
     end function gsl_eigen_hermv_alloc
     subroutine gsl_eigen_hermv_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_eigen_hermv_free
     function gsl_eigen_hermv(a, eval, evec, w) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a, eval, evec, w
       integer(c_int) :: gsl_eigen_hermv
     end function gsl_eigen_hermv
     function gsl_eigen_nonsymm_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_eigen_nonsymm_alloc
     end function gsl_eigen_nonsymm_alloc
     subroutine gsl_eigen_nonsymm_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_eigen_nonsymm_free
     subroutine  gsl_eigen_nonsymm_params (compute_t, balance, w) bind(c)
       import :: c_int, c_ptr
       integer(c_int), value :: compute_t, balance
       type(c_ptr), value :: w
     end subroutine gsl_eigen_nonsymm_params
     function gsl_eigen_nonsymm(a, eval, w) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a, eval, w
       integer(c_int) :: gsl_eigen_nonsymm
     end function gsl_eigen_nonsymm
     function gsl_eigen_nonsymm_z(a, eval, z, w) bind(c, name='gsl_eigen_nonsymm_Z')
       import :: c_int, c_ptr
       type(c_ptr), value :: a, eval, z, w
       integer(c_int) :: gsl_eigen_nonsymm_z
     end function gsl_eigen_nonsymm_z
     function gsl_eigen_nonsymmv_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_eigen_nonsymmv_alloc
     end function gsl_eigen_nonsymmv_alloc
     subroutine gsl_eigen_nonsymmv_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_eigen_nonsymmv_free
     subroutine  gsl_eigen_nonsymmv_params (balance, w) bind(c)
       import :: c_int, c_ptr
       integer(c_int), value :: balance
       type(c_ptr), value :: w
     end subroutine gsl_eigen_nonsymmv_params
     function gsl_eigen_nonsymmv(a, eval, evec, w) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a, eval, evec, w
       integer(c_int) :: gsl_eigen_nonsymmv
     end function gsl_eigen_nonsymmv
     function gsl_eigen_nonsymmv_z(a, eval, evec, z, w) &
          bind(c, name='gsl_eigen_nonsymmv_Z')
       import :: c_int, c_ptr
       type(c_ptr), value :: a, eval, evec, z, w
       integer(c_int) :: gsl_eigen_nonsymmv_z
     end function gsl_eigen_nonsymmv_z
     function gsl_eigen_gensymm_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_eigen_gensymm_alloc
     end function gsl_eigen_gensymm_alloc
     subroutine gsl_eigen_gensymm_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_eigen_gensymm_free
     function gsl_eigen_gensymm(a, b, eval, w) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a, b, eval, w
       integer(c_int) :: gsl_eigen_gensymm
     end function gsl_eigen_gensymm
     function gsl_eigen_gensymmv_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_eigen_gensymmv_alloc
     end function gsl_eigen_gensymmv_alloc
     subroutine gsl_eigen_gensymmv_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_eigen_gensymmv_free
     function gsl_eigen_gensymmv(a, b, eval, evec, w) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a, b, eval, evec, w
       integer(c_int) :: gsl_eigen_gensymmv
     end function gsl_eigen_gensymmv
     function gsl_eigen_genherm_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_eigen_genherm_alloc
     end function gsl_eigen_genherm_alloc
     subroutine gsl_eigen_genherm_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_eigen_genherm_free
     function gsl_eigen_genherm(a, b, eval, w) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a, b, eval, w
       integer(c_int) :: gsl_eigen_genherm
     end function gsl_eigen_genherm
     function gsl_eigen_genhermv_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_eigen_genhermv_alloc
     end function gsl_eigen_genhermv_alloc
     subroutine gsl_eigen_genhermv_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_eigen_genhermv_free
     function gsl_eigen_genhermv(a, b, eval, evec, w) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a, b, eval, evec, w
       integer(c_int) :: gsl_eigen_genhermv
     end function gsl_eigen_genhermv
     function gsl_eigen_gen_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_eigen_gen_alloc
     end function gsl_eigen_gen_alloc
     subroutine gsl_eigen_gen_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_eigen_gen_free
     subroutine  gsl_eigen_gen_params (compute_s, compute_t, balance, w) bind(c)
       import :: c_int, c_ptr
       integer(c_int), value :: compute_s, compute_t, balance
       type(c_ptr), value :: w
     end subroutine gsl_eigen_gen_params
     function gsl_eigen_gen(a, b, alpha, beta, w) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a, b, alpha, beta, w
       integer(c_int) :: gsl_eigen_gen
     end function gsl_eigen_gen
     function gsl_eigen_gen_qz(a, b, alpha, beta, q, z, w) &
          bind(c, name='gsl_eigen_gen_QZ')
       import :: c_int, c_ptr
       type(c_ptr), value :: a, b, alpha, beta, q, z, w
       integer(c_int) :: gsl_eigen_gen_qz
     end function gsl_eigen_gen_qz
     function gsl_eigen_genv_alloc(n) bind(c)
       import :: c_size_t, c_ptr
       integer(c_size_t), value :: n
       type(c_ptr) :: gsl_eigen_genv_alloc
     end function gsl_eigen_genv_alloc
     subroutine gsl_eigen_genv_free(w) bind(c)
       import :: c_ptr
       type(c_ptr), value :: w
     end subroutine gsl_eigen_genv_free
     function gsl_eigen_genv(a, b, alpha, beta, evec, w) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: a, b, alpha, beta, evec, w
       integer(c_int) :: gsl_eigen_genv
     end function gsl_eigen_genv
     function gsl_eigen_genv_qz(a, b, alpha, beta, evec, q, z, w) &
          bind(c, name='gsl_eigen_genv_QZ')
       import :: c_int, c_ptr
       type(c_ptr), value :: a, b, alpha, beta, evec, q, z, w
       integer(c_int) :: gsl_eigen_genv_qz
     end function gsl_eigen_genv_qz
     function gsl_eigen_symmv_sort (eval, evec, sort_type) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: eval, evec
       integer(c_int), value :: sort_type
       integer(c_int) :: gsl_eigen_symmv_sort
     end function gsl_eigen_symmv_sort
     function gsl_eigen_hermv_sort (eval, evec, sort_type) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: eval, evec
       integer(c_int), value :: sort_type
       integer(c_int) :: gsl_eigen_hermv_sort
     end function gsl_eigen_hermv_sort
     function gsl_eigen_nonsymmv_sort (eval, evec, sort_type) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: eval, evec
       integer(c_int), value :: sort_type
       integer(c_int) :: gsl_eigen_nonsymmv_sort
     end function gsl_eigen_nonsymmv_sort
     function gsl_eigen_gensymmv_sort (eval, evec, sort_type) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: eval, evec
       integer(c_int), value :: sort_type
       integer(c_int) :: gsl_eigen_gensymmv_sort
     end function gsl_eigen_gensymmv_sort
     function gsl_eigen_genhermv_sort (eval, evec, sort_type) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: eval, evec
       integer(c_int), value :: sort_type
       integer(c_int) :: gsl_eigen_genhermv_sort
     end function gsl_eigen_genhermv_sort
     function gsl_eigen_genv_sort (alpha, beta, evec, sort_type) bind(c)
       import :: c_int, c_ptr
       type(c_ptr), value :: alpha, beta, evec
       integer(c_int), value :: sort_type
       integer(c_int) :: gsl_eigen_genv_sort
     end function gsl_eigen_genv_sort

  end interface
contains
  function fgsl_eigen_symm_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_eigen_symm_workspace) :: fgsl_eigen_symm_alloc
    fgsl_eigen_symm_alloc%gsl_eigen_symm_workspace = gsl_eigen_symm_alloc(n)
  end function fgsl_eigen_symm_alloc
  subroutine fgsl_eigen_symm_free(w)
    type(fgsl_eigen_symm_workspace) :: w
    call gsl_eigen_symm_free(w%gsl_eigen_symm_workspace)
  end subroutine fgsl_eigen_symm_free
  function fgsl_eigen_symm(a, eval, w)
    type(fgsl_matrix), intent(inout) :: a
    type(fgsl_vector), intent(inout) :: eval
    type(fgsl_eigen_symm_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_symm
    fgsl_eigen_symm = gsl_eigen_symm(a%gsl_matrix, eval%gsl_vector, &
         w%gsl_eigen_symm_workspace)
  end function fgsl_eigen_symm
  function fgsl_eigen_symmv_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_eigen_symmv_workspace) :: fgsl_eigen_symmv_alloc
    fgsl_eigen_symmv_alloc%gsl_eigen_symmv_workspace = gsl_eigen_symmv_alloc(n)
  end function fgsl_eigen_symmv_alloc
  subroutine fgsl_eigen_symmv_free(w)
    type(fgsl_eigen_symmv_workspace) :: w
    call gsl_eigen_symmv_free(w%gsl_eigen_symmv_workspace)
  end subroutine fgsl_eigen_symmv_free
  function fgsl_eigen_symmv(a, eval, evec, w)
    type(fgsl_matrix), intent(inout) :: a, evec
    type(fgsl_vector), intent(inout) :: eval
    type(fgsl_eigen_symmv_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_symmv
    fgsl_eigen_symmv = gsl_eigen_symmv(a%gsl_matrix, eval%gsl_vector, &
         evec%gsl_matrix, w%gsl_eigen_symmv_workspace)
  end function fgsl_eigen_symmv
  function fgsl_eigen_herm_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_eigen_herm_workspace) :: fgsl_eigen_herm_alloc
    fgsl_eigen_herm_alloc%gsl_eigen_herm_workspace = gsl_eigen_herm_alloc(n)
  end function fgsl_eigen_herm_alloc
  subroutine fgsl_eigen_herm_free(w)
    type(fgsl_eigen_herm_workspace) :: w
    call gsl_eigen_herm_free(w%gsl_eigen_herm_workspace)
  end subroutine fgsl_eigen_herm_free
  function fgsl_eigen_herm(a, eval, w)
    type(fgsl_matrix_complex), intent(inout) :: a
    type(fgsl_vector), intent(inout) :: eval
    type(fgsl_eigen_herm_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_herm
    fgsl_eigen_herm = gsl_eigen_herm(a%gsl_matrix_complex, eval%gsl_vector, &
         w%gsl_eigen_herm_workspace)
  end function fgsl_eigen_herm
  function fgsl_eigen_hermv_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_eigen_hermv_workspace) :: fgsl_eigen_hermv_alloc
    fgsl_eigen_hermv_alloc%gsl_eigen_hermv_workspace = gsl_eigen_hermv_alloc(n)
  end function fgsl_eigen_hermv_alloc
  subroutine fgsl_eigen_hermv_free(w)
    type(fgsl_eigen_hermv_workspace) :: w
    call gsl_eigen_hermv_free(w%gsl_eigen_hermv_workspace)
  end subroutine fgsl_eigen_hermv_free
  function fgsl_eigen_hermv(a, eval, evec, w)
    type(fgsl_matrix_complex), intent(inout) :: a, evec
    type(fgsl_vector), intent(inout) :: eval
    type(fgsl_eigen_hermv_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_hermv
    fgsl_eigen_hermv = gsl_eigen_hermv(a%gsl_matrix_complex, eval%gsl_vector, &
         evec%gsl_matrix_complex, w%gsl_eigen_hermv_workspace)
  end function fgsl_eigen_hermv
  function fgsl_eigen_nonsymm_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_eigen_nonsymm_workspace) :: fgsl_eigen_nonsymm_alloc
    fgsl_eigen_nonsymm_alloc%gsl_eigen_nonsymm_workspace = &
         gsl_eigen_nonsymm_alloc(n)
  end function fgsl_eigen_nonsymm_alloc
  subroutine fgsl_eigen_nonsymm_free(w)
    type(fgsl_eigen_nonsymm_workspace) :: w
    call gsl_eigen_nonsymm_free(w%gsl_eigen_nonsymm_workspace)
  end subroutine fgsl_eigen_nonsymm_free
  subroutine fgsl_eigen_nonsymm_params (compute_t, balance, w)
    integer(fgsl_int), intent(in) :: compute_t, balance
    type(fgsl_eigen_nonsymm_workspace), intent(inout) :: w
    call gsl_eigen_nonsymm_params (compute_t, balance, &
         w%gsl_eigen_nonsymm_workspace)
  end subroutine fgsl_eigen_nonsymm_params
  function fgsl_eigen_nonsymm(a, eval, w)
    type(fgsl_matrix), intent(inout) :: a
    type(fgsl_vector_complex), intent(inout) :: eval
    type(fgsl_eigen_nonsymm_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_nonsymm
    fgsl_eigen_nonsymm = gsl_eigen_nonsymm(a%gsl_matrix, &
         eval%gsl_vector_complex, w%gsl_eigen_nonsymm_workspace)
  end function fgsl_eigen_nonsymm
  function fgsl_eigen_nonsymm_z(a, eval, z, w)
    type(fgsl_matrix), intent(inout) :: a, z
    type(fgsl_vector_complex), intent(inout) :: eval
    type(fgsl_eigen_nonsymm_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_nonsymm_z
    fgsl_eigen_nonsymm_z = gsl_eigen_nonsymm_z(a%gsl_matrix, &
         eval%gsl_vector_complex, z%gsl_matrix, w%gsl_eigen_nonsymm_workspace)
  end function fgsl_eigen_nonsymm_z
  function fgsl_eigen_nonsymmv_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_eigen_nonsymmv_workspace) :: fgsl_eigen_nonsymmv_alloc
    fgsl_eigen_nonsymmv_alloc%gsl_eigen_nonsymmv_workspace = &
         gsl_eigen_nonsymmv_alloc(n)
  end function fgsl_eigen_nonsymmv_alloc
  subroutine fgsl_eigen_nonsymmv_free(w)
    type(fgsl_eigen_nonsymmv_workspace) :: w
    call gsl_eigen_nonsymmv_free(w%gsl_eigen_nonsymmv_workspace)
  end subroutine fgsl_eigen_nonsymmv_free
  subroutine fgsl_eigen_nonsymmv_params (balance, w)
    integer(fgsl_int), intent(in) :: balance
    type(fgsl_eigen_nonsymm_workspace), intent(inout) :: w
    call gsl_eigen_nonsymmv_params (balance, &
         w%gsl_eigen_nonsymm_workspace)
  end subroutine fgsl_eigen_nonsymmv_params
  function fgsl_eigen_nonsymmv(a, eval, evec, w)
    type(fgsl_matrix), intent(inout) :: a
    type(fgsl_vector_complex), intent(inout) :: eval
    type(fgsl_matrix_complex), intent(inout) :: evec
    type(fgsl_eigen_nonsymmv_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_nonsymmv
    fgsl_eigen_nonsymmv = gsl_eigen_nonsymmv(a%gsl_matrix, &
         eval%gsl_vector_complex, evec%gsl_matrix_complex, &
         w%gsl_eigen_nonsymmv_workspace)
  end function fgsl_eigen_nonsymmv
  function fgsl_eigen_nonsymmv_z(a, eval, evec, z, w)
    type(fgsl_matrix), intent(inout) :: a, z
    type(fgsl_vector_complex), intent(inout) :: eval
    type(fgsl_matrix_complex), intent(inout) :: evec
    type(fgsl_eigen_nonsymmv_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_nonsymmv_z
    fgsl_eigen_nonsymmv_z = gsl_eigen_nonsymmv_z(a%gsl_matrix, &
         eval%gsl_vector_complex, evec%gsl_matrix_complex, &
         z%gsl_matrix, w%gsl_eigen_nonsymmv_workspace)
  end function fgsl_eigen_nonsymmv_z
  function fgsl_eigen_gensymm_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_eigen_gensymm_workspace) :: fgsl_eigen_gensymm_alloc
    fgsl_eigen_gensymm_alloc%gsl_eigen_gensymm_workspace = &
         gsl_eigen_gensymm_alloc(n)
  end function fgsl_eigen_gensymm_alloc
  subroutine fgsl_eigen_gensymm_free(w)
    type(fgsl_eigen_gensymm_workspace) :: w
    call gsl_eigen_gensymm_free(w%gsl_eigen_gensymm_workspace)
  end subroutine fgsl_eigen_gensymm_free
  function fgsl_eigen_gensymm(a, b, eval, w)
    type(fgsl_matrix), intent(inout) :: a, b
    type(fgsl_vector), intent(inout) :: eval
    type(fgsl_eigen_gensymm_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_gensymm
    fgsl_eigen_gensymm = gsl_eigen_gensymm(a%gsl_matrix, b%gsl_matrix, &
         eval%gsl_vector, w%gsl_eigen_gensymm_workspace)
  end function fgsl_eigen_gensymm
  function fgsl_eigen_gensymmv_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_eigen_gensymmv_workspace) :: fgsl_eigen_gensymmv_alloc
    fgsl_eigen_gensymmv_alloc%gsl_eigen_gensymmv_workspace = &
         gsl_eigen_gensymmv_alloc(n)
  end function fgsl_eigen_gensymmv_alloc
  subroutine fgsl_eigen_gensymmv_free(w)
    type(fgsl_eigen_gensymmv_workspace) :: w
    call gsl_eigen_gensymmv_free(w%gsl_eigen_gensymmv_workspace)
  end subroutine fgsl_eigen_gensymmv_free
  function fgsl_eigen_gensymmv(a, b, eval, evec, w)
    type(fgsl_matrix), intent(inout) :: a, b, evec
    type(fgsl_vector), intent(inout) :: eval
    type(fgsl_eigen_gensymmv_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_gensymmv
    fgsl_eigen_gensymmv = gsl_eigen_gensymmv(a%gsl_matrix, b%gsl_matrix, &
         eval%gsl_vector, evec%gsl_matrix, w%gsl_eigen_gensymmv_workspace)
  end function fgsl_eigen_gensymmv
  function fgsl_eigen_genherm_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_eigen_genherm_workspace) :: fgsl_eigen_genherm_alloc
    fgsl_eigen_genherm_alloc%gsl_eigen_genherm_workspace = &
         gsl_eigen_genherm_alloc(n)
  end function fgsl_eigen_genherm_alloc
  subroutine fgsl_eigen_genherm_free(w)
    type(fgsl_eigen_genherm_workspace) :: w
    call gsl_eigen_genherm_free(w%gsl_eigen_genherm_workspace)
  end subroutine fgsl_eigen_genherm_free
  function fgsl_eigen_genherm(a, b, eval, w)
    type(fgsl_matrix_complex), intent(inout) :: a, b
    type(fgsl_vector), intent(inout) :: eval
    type(fgsl_eigen_genherm_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_genherm
    fgsl_eigen_genherm = gsl_eigen_genherm(a%gsl_matrix_complex, b%gsl_matrix_complex, &
         eval%gsl_vector, w%gsl_eigen_genherm_workspace)
  end function fgsl_eigen_genherm
  function fgsl_eigen_genhermv_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_eigen_genhermv_workspace) :: fgsl_eigen_genhermv_alloc
    fgsl_eigen_genhermv_alloc%gsl_eigen_genhermv_workspace = &
         gsl_eigen_genhermv_alloc(n)
  end function fgsl_eigen_genhermv_alloc
  subroutine fgsl_eigen_genhermv_free(w)
    type(fgsl_eigen_genhermv_workspace) :: w
    call gsl_eigen_genhermv_free(w%gsl_eigen_genhermv_workspace)
  end subroutine fgsl_eigen_genhermv_free
  function fgsl_eigen_genhermv(a, b, eval, evec, w)
    type(fgsl_matrix_complex), intent(inout) :: a, b, evec
    type(fgsl_vector), intent(inout) :: eval
    type(fgsl_eigen_genhermv_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_genhermv
    fgsl_eigen_genhermv = gsl_eigen_genhermv(a%gsl_matrix_complex, &
         b%gsl_matrix_complex, eval%gsl_vector, evec%gsl_matrix_complex, &
         w%gsl_eigen_genhermv_workspace)
  end function fgsl_eigen_genhermv
  function fgsl_eigen_gen_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_eigen_gen_workspace) :: fgsl_eigen_gen_alloc
    fgsl_eigen_gen_alloc%gsl_eigen_gen_workspace = &
         gsl_eigen_gen_alloc(n)
  end function fgsl_eigen_gen_alloc
  subroutine fgsl_eigen_gen_free(w)
    type(fgsl_eigen_gen_workspace) :: w
    call gsl_eigen_gen_free(w%gsl_eigen_gen_workspace)
  end subroutine fgsl_eigen_gen_free
  subroutine fgsl_eigen_gen_params (compute_s, compute_t, balance, w)
    integer(fgsl_int), intent(in) :: compute_s, compute_t, balance
    type(fgsl_eigen_gen_workspace), intent(inout) :: w
    call gsl_eigen_gen_params (compute_s, compute_t, balance, &
         w%gsl_eigen_gen_workspace)
  end subroutine fgsl_eigen_gen_params
  function fgsl_eigen_gen(a, b, alpha, beta, w)
    type(fgsl_matrix), intent(inout) :: a, b
    type(fgsl_vector_complex), intent(inout) :: alpha
    type(fgsl_vector), intent(inout) :: beta
    type(fgsl_eigen_gen_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_gen
    fgsl_eigen_gen = gsl_eigen_gen(a%gsl_matrix, &
         b%gsl_matrix, alpha%gsl_vector_complex, &
         beta%gsl_vector, w%gsl_eigen_gen_workspace)
  end function fgsl_eigen_gen
  function fgsl_eigen_gen_qz(a, b, alpha, beta, q, z, w)
    type(fgsl_matrix), intent(inout) :: a, b, q, z
    type(fgsl_vector_complex), intent(inout) :: alpha
    type(fgsl_vector), intent(inout) :: beta
    type(fgsl_eigen_gen_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_gen_qz
    fgsl_eigen_gen_qz = gsl_eigen_gen_qz(a%gsl_matrix, &
         b%gsl_matrix, alpha%gsl_vector_complex, &
         beta%gsl_vector, q%gsl_matrix, z%gsl_matrix, &
         w%gsl_eigen_gen_workspace)
  end function fgsl_eigen_gen_qz
  function fgsl_eigen_genv_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_eigen_genv_workspace) :: fgsl_eigen_genv_alloc
    fgsl_eigen_genv_alloc%gsl_eigen_genv_workspace = &
         gsl_eigen_genv_alloc(n)
  end function fgsl_eigen_genv_alloc
  subroutine fgsl_eigen_genv_free(w)
    type(fgsl_eigen_genv_workspace) :: w
    call gsl_eigen_genv_free(w%gsl_eigen_genv_workspace)
  end subroutine fgsl_eigen_genv_free
  function fgsl_eigen_genv(a, b, alpha, beta, evec, w)
    type(fgsl_matrix), intent(inout) :: a, b
    type(fgsl_vector_complex), intent(inout) :: alpha
    type(fgsl_vector), intent(inout) :: beta
    type(fgsl_matrix_complex), intent(inout) :: evec
    type(fgsl_eigen_genv_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_genv
    fgsl_eigen_genv = gsl_eigen_genv(a%gsl_matrix, &
         b%gsl_matrix, alpha%gsl_vector_complex, &
         beta%gsl_vector, evec%gsl_matrix_complex, &
         w%gsl_eigen_genv_workspace)
  end function fgsl_eigen_genv
  function fgsl_eigen_genv_qz(a, b, alpha, beta, evec, q, z, w)
    type(fgsl_matrix), intent(inout) :: a, b, q, z
    type(fgsl_vector_complex), intent(inout) :: alpha
    type(fgsl_vector), intent(inout) :: beta
    type(fgsl_matrix_complex), intent(inout) :: evec
    type(fgsl_eigen_genv_workspace) :: w
    integer(fgsl_int) :: fgsl_eigen_genv_qz
    fgsl_eigen_genv_qz = gsl_eigen_genv_qz(a%gsl_matrix, &
         b%gsl_matrix, alpha%gsl_vector_complex, &
         beta%gsl_vector,  evec%gsl_matrix_complex, &
         q%gsl_matrix, z%gsl_matrix, &
         w%gsl_eigen_genv_workspace)
  end function fgsl_eigen_genv_qz
  function fgsl_eigen_symmv_sort (eval, evec, sort_type)
    type(fgsl_vector), intent(inout) :: eval
    type(fgsl_matrix), intent(inout) :: evec
    integer(fgsl_int), intent(in) :: sort_type
    integer(fgsl_int) :: fgsl_eigen_symmv_sort
    fgsl_eigen_symmv_sort = gsl_eigen_symmv_sort(eval%gsl_vector, &
         evec%gsl_matrix, sort_type)
  end function fgsl_eigen_symmv_sort
  function fgsl_eigen_hermv_sort (eval, evec, sort_type)
    type(fgsl_vector), intent(inout) :: eval
    type(fgsl_matrix_complex), intent(inout) :: evec
    integer(fgsl_int), intent(in) :: sort_type
    integer(fgsl_int) :: fgsl_eigen_hermv_sort
    fgsl_eigen_hermv_sort = gsl_eigen_hermv_sort(eval%gsl_vector, &
         evec%gsl_matrix_complex, sort_type)
  end function fgsl_eigen_hermv_sort
  function fgsl_eigen_nonsymmv_sort (eval, evec, sort_type)
    type(fgsl_vector_complex), intent(inout) :: eval
    type(fgsl_matrix_complex), intent(inout) :: evec
    integer(fgsl_int), intent(in) :: sort_type
    integer(fgsl_int) :: fgsl_eigen_nonsymmv_sort
    fgsl_eigen_nonsymmv_sort = gsl_eigen_nonsymmv_sort( &
         eval%gsl_vector_complex, evec%gsl_matrix_complex, sort_type)
  end function fgsl_eigen_nonsymmv_sort
  function fgsl_eigen_gensymmv_sort (eval, evec, sort_type)
    type(fgsl_vector), intent(inout) :: eval
    type(fgsl_matrix), intent(inout) :: evec
    integer(fgsl_int), intent(in) :: sort_type
    integer(fgsl_int) :: fgsl_eigen_gensymmv_sort
    fgsl_eigen_gensymmv_sort = gsl_eigen_gensymmv_sort(eval%gsl_vector, &
         evec%gsl_matrix, sort_type)
  end function fgsl_eigen_gensymmv_sort
  function fgsl_eigen_genhermv_sort (eval, evec, sort_type)
    type(fgsl_vector), intent(inout) :: eval
    type(fgsl_matrix_complex), intent(inout) :: evec
    integer(fgsl_int), intent(in) :: sort_type
    integer(fgsl_int) :: fgsl_eigen_genhermv_sort
    fgsl_eigen_genhermv_sort = gsl_eigen_genhermv_sort(eval%gsl_vector, &
         evec%gsl_matrix_complex, sort_type)
  end function fgsl_eigen_genhermv_sort
  function fgsl_eigen_genv_sort (alpha, beta, evec, sort_type)
    type(fgsl_vector_complex), intent(inout) :: alpha
    type(fgsl_vector), intent(inout) :: beta
    type(fgsl_matrix_complex), intent(inout) :: evec
    integer(fgsl_int), intent(in) :: sort_type
    integer(fgsl_int) :: fgsl_eigen_genv_sort
    fgsl_eigen_genv_sort = gsl_eigen_genv_sort(alpha%gsl_vector_complex, &
         beta%gsl_vector, evec%gsl_matrix_complex, sort_type)
  end function fgsl_eigen_genv_sort

end module fgsl_eigen
