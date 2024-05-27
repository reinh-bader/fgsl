module fgsl_histograms
  !> Histograms
  !> Note: this module contains the interfaces for both one and two
  !> dimensional histograms
  use fgsl_base
  use fgsl_array
  use fgsl_io

  implicit none
  
  private :: gsl_histogram_alloc, gsl_histogram_free, gsl_histogram_set_ranges, &
    gsl_histogram_set_ranges_uniform, gsl_histogram_memcpy, gsl_histogram_clone, &
    gsl_histogram_increment, gsl_histogram_accumulate, gsl_histogram_get, &
    gsl_histogram_get_range, gsl_histogram_max, gsl_histogram_min, &
    gsl_histogram_bins, gsl_histogram_reset, gsl_histogram_find, &
    gsl_histogram_max_val, gsl_histogram_max_bin, gsl_histogram_min_val, &
    gsl_histogram_min_bin, gsl_histogram_mean, gsl_histogram_sigma, &
    gsl_histogram_sum, gsl_histogram_equal_bins_p, gsl_histogram_add, &
    gsl_histogram_sub, gsl_histogram_mul, gsl_histogram_div, gsl_histogram_scale, &
    gsl_histogram_shift, gsl_histogram_fwrite, gsl_histogram_fread, &
    gsl_histogram_fprintf, gsl_histogram_fscanf, gsl_histogram_pdf_alloc, &
    gsl_histogram_pdf_init, gsl_histogram_pdf_free, gsl_histogram_pdf_sample
    
  private :: gsl_histogram2d_alloc, gsl_histogram2d_set_ranges,  &
    gsl_histogram2d_set_ranges_uniform, gsl_histogram2d_free, gsl_histogram2d_memcpy, &
    gsl_histogram2d_clone, gsl_histogram2d_increment, gsl_histogram2d_accumulate, &
    gsl_histogram2d_get, gsl_histogram2d_get_xrange, gsl_histogram2d_get_yrange, &
    gsl_histogram2d_xmax, gsl_histogram2d_xmin, gsl_histogram2d_nx, &
    gsl_histogram2d_ymax, gsl_histogram2d_ymin, gsl_histogram2d_ny, &
    gsl_histogram2d_reset, gsl_histogram2d_find, gsl_histogram2d_max_val, &
    gsl_histogram2d_max_bin, gsl_histogram2d_min_val, gsl_histogram2d_min_bin, &
    gsl_histogram2d_xmean, gsl_histogram2d_ymean, gsl_histogram2d_xsigma, &
    gsl_histogram2d_ysigma, gsl_histogram2d_cov, gsl_histogram2d_sum, &
    gsl_histogram2d_equal_bins_p, gsl_histogram2d_add, gsl_histogram2d_sub, &
    gsl_histogram2d_mul, gsl_histogram2d_div, gsl_histogram2d_scale, &
    gsl_histogram2d_shift, gsl_histogram2d_fwrite, gsl_histogram2d_fread, &
    gsl_histogram2d_fprintf, gsl_histogram2d_fscanf, gsl_histogram2d_pdf_alloc, &
    gsl_histogram2d_pdf_init, gsl_histogram2d_pdf_free, gsl_histogram2d_pdf_sample
    
  !
  !> Types
  type, public :: fgsl_histogram
     type(c_ptr) :: gsl_histogram = c_null_ptr
  end type fgsl_histogram
  type, public :: fgsl_histogram_pdf
     private
     type(c_ptr) :: gsl_histogram_pdf = c_null_ptr
  end type fgsl_histogram_pdf
  type, public :: fgsl_histogram2d
     private
     type(c_ptr) :: gsl_histogram2d = c_null_ptr
  end type fgsl_histogram2d
  type, public :: fgsl_histogram2d_pdf
     private
     type(c_ptr) :: gsl_histogram2d_pdf = c_null_ptr
  end type fgsl_histogram2d_pdf  
  
  !
  !> Generics
  interface fgsl_well_defined
     module procedure fgsl_histogram_status
  end interface fgsl_well_defined
  
  !
  !> C interfaces
  interface
	  function gsl_histogram_alloc(n) bind(c)
	    import
	    integer(c_size_t), value :: n
	    type(c_ptr) :: gsl_histogram_alloc
	  end function gsl_histogram_alloc
	  function gsl_histogram_set_ranges(h, range, size) bind(c)
	    import
	    type(c_ptr), value :: h
	    integer(c_size_t), value :: size
	    type(c_ptr), value :: range
	    integer(c_int) :: gsl_histogram_set_ranges
	  end function gsl_histogram_set_ranges
	  function gsl_histogram_set_ranges_uniform(h, xmin, xmax) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double), value :: xmin, xmax
	    integer(c_int) :: gsl_histogram_set_ranges_uniform
	  end function gsl_histogram_set_ranges_uniform
	  subroutine gsl_histogram_free(h) bind(c)
	    import
	    type(c_ptr), value :: h
	  end subroutine gsl_histogram_free
	  function gsl_histogram_memcpy(dest, src) bind(c)
	    import
	    type(c_ptr), value :: dest, src
	    integer(c_int) :: gsl_histogram_memcpy
	  end function gsl_histogram_memcpy
	  function gsl_histogram_clone(src) bind(c)
	    import
	    type(c_ptr), value :: src
	    type(c_ptr) :: gsl_histogram_clone
	  end function gsl_histogram_clone
	  function gsl_histogram_increment(h, x) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double), value :: x
	    integer(c_int) :: gsl_histogram_increment
	  end function gsl_histogram_increment
	  function gsl_histogram_accumulate(h, x, weight) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double), value :: x, weight
	    integer(c_int) :: gsl_histogram_accumulate
	  end function gsl_histogram_accumulate
	  function gsl_histogram_get(h, i) bind(c)
	    import
	    type(c_ptr), value :: h
	    integer(c_size_t), value :: i
	    real(c_double) :: gsl_histogram_get
	  end function gsl_histogram_get
	  function gsl_histogram_get_range(h, i, lower, upper) bind(c)
	    import
	    type(c_ptr), value :: h
	    integer(c_size_t), value :: i
	    real(c_double), intent(out) :: lower, upper
	    integer(c_int) :: gsl_histogram_get_range
	  end function gsl_histogram_get_range
	  function gsl_histogram_max(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram_max
	  end function gsl_histogram_max
	  function gsl_histogram_min(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram_min
	  end function gsl_histogram_min
	  function gsl_histogram_bins(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    integer(c_size_t) :: gsl_histogram_bins
	  end function gsl_histogram_bins
	  subroutine gsl_histogram_reset(h) bind(c)
	    import
	    type(c_ptr), value :: h
	  end subroutine gsl_histogram_reset
	  function gsl_histogram_find(h, x, i) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double), value :: x
	    integer(c_size_t), intent(out) :: i
	    integer(c_int) :: gsl_histogram_find
	  end function gsl_histogram_find
	  function gsl_histogram_max_val(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram_max_val
	  end function gsl_histogram_max_val
	  function gsl_histogram_max_bin(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    integer(c_size_t) :: gsl_histogram_max_bin
	  end function gsl_histogram_max_bin
	  function gsl_histogram_min_val(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram_min_val
	  end function gsl_histogram_min_val
	  function gsl_histogram_min_bin(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    integer(c_size_t) :: gsl_histogram_min_bin
	  end function gsl_histogram_min_bin
	  function gsl_histogram_mean(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram_mean
	  end function gsl_histogram_mean
	  function gsl_histogram_sigma(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram_sigma
	  end function gsl_histogram_sigma
	  function gsl_histogram_sum(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram_sum
	  end function gsl_histogram_sum
	  function gsl_histogram_equal_bins_p(h1, h2) bind(c)
	    import
	    type(c_ptr), value :: h1, h2
	    integer(c_int) :: gsl_histogram_equal_bins_p
	  end function gsl_histogram_equal_bins_p
	  function gsl_histogram_add(h1, h2) bind(c)
	    import
	    type(c_ptr), value :: h1, h2
	    integer(c_int) :: gsl_histogram_add
	  end function gsl_histogram_add
	  function gsl_histogram_sub(h1, h2) bind(c)
	    import
	    type(c_ptr), value :: h1, h2
	    integer(c_int) :: gsl_histogram_sub
	  end function gsl_histogram_sub
	  function gsl_histogram_mul(h1, h2) bind(c)
	    import
	    type(c_ptr), value :: h1, h2
	    integer(c_int) :: gsl_histogram_mul
	  end function gsl_histogram_mul
	  function gsl_histogram_div(h1, h2) bind(c)
	    import
	    type(c_ptr), value :: h1, h2
	    integer(c_int) :: gsl_histogram_div
	  end function gsl_histogram_div
	  function gsl_histogram_scale(h, scale) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double), value :: scale
	    integer(c_int) :: gsl_histogram_scale
	  end function gsl_histogram_scale
	  function gsl_histogram_shift(h, offset) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double), value :: offset
	    integer(c_int) :: gsl_histogram_shift
	  end function gsl_histogram_shift
	  function gsl_histogram_fwrite(stream, h) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: stream, h
	    integer(c_int) :: gsl_histogram_fwrite
	  end function gsl_histogram_fwrite
	  function gsl_histogram_fread(stream, h) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: stream, h
	    integer(c_int) :: gsl_histogram_fread
	  end function gsl_histogram_fread
	  function gsl_histogram_fprintf(stream, h, range_format, bin_format) bind(c)
	    import :: c_ptr, c_int, c_char
	    type(c_ptr), value :: stream, h
	    character(kind=c_char) :: range_format(*), bin_format(*)
	    integer(c_int) :: gsl_histogram_fprintf
	  end function gsl_histogram_fprintf
	  function gsl_histogram_fscanf(stream, h) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: stream, h
	    integer(c_int) :: gsl_histogram_fscanf
	  end function gsl_histogram_fscanf
	  function gsl_histogram_pdf_alloc(n) bind(c)
	    import
	    integer(c_size_t), value :: n
	    type(c_ptr) :: gsl_histogram_pdf_alloc
	  end function gsl_histogram_pdf_alloc
	  function gsl_histogram_pdf_init(p, h) bind(c)
	    import
	    type(c_ptr), value :: p, h
	    integer(c_int) :: gsl_histogram_pdf_init
	  end function gsl_histogram_pdf_init
	  subroutine gsl_histogram_pdf_free(p) bind(c)
	    import
	    type(c_ptr), value :: p
	  end subroutine gsl_histogram_pdf_free
	  function gsl_histogram_pdf_sample(p, r) bind(c)
	    import
	    type(c_ptr), value :: p
	    real(c_double), value :: r
	    real(c_double) :: gsl_histogram_pdf_sample
	  end function gsl_histogram_pdf_sample
	  function gsl_histogram2d_alloc(nx, ny) bind(c)
	    import
	    integer(c_size_t), value :: nx, ny
	    type(c_ptr) :: gsl_histogram2d_alloc
	  end function gsl_histogram2d_alloc
	  function gsl_histogram2d_set_ranges(h, xrange, xsize, yrange, ysize) bind(c)
	    import
	    type(c_ptr), value :: h
	    integer(c_size_t), value :: xsize, ysize
	    type(c_ptr), value :: xrange, yrange
	    integer(c_int) :: gsl_histogram2d_set_ranges
	  end function gsl_histogram2d_set_ranges
	  function gsl_histogram2d_set_ranges_uniform(h, xmin, xmax, ymin, ymax) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double), value :: xmin, xmax, ymin, ymax
	    integer(c_int) :: gsl_histogram2d_set_ranges_uniform
	  end function gsl_histogram2d_set_ranges_uniform
	  subroutine gsl_histogram2d_free(h) bind(c)
	    import
	    type(c_ptr), value :: h
	  end subroutine gsl_histogram2d_free
	  function gsl_histogram2d_memcpy(dest, src) bind(c)
	    import
	    type(c_ptr), value :: dest, src
	    integer(c_int) :: gsl_histogram2d_memcpy
	  end function gsl_histogram2d_memcpy
	  function gsl_histogram2d_clone(src) bind(c)
	    import
	    type(c_ptr), value :: src
	    type(c_ptr) :: gsl_histogram2d_clone
	  end function gsl_histogram2d_clone
	  function gsl_histogram2d_increment(h, x, y) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double), value :: x, y
	    integer(c_int) :: gsl_histogram2d_increment
	  end function gsl_histogram2d_increment
	  function gsl_histogram2d_accumulate(h, x, y, weight) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double), value :: x, y, weight
	    integer(c_int) :: gsl_histogram2d_accumulate
	  end function gsl_histogram2d_accumulate
	  function gsl_histogram2d_get(h, i, j) bind(c)
	    import
	    type(c_ptr), value :: h
	    integer(c_size_t), value :: i, j
	    real(c_double) :: gsl_histogram2d_get
	  end function gsl_histogram2d_get
	  function gsl_histogram2d_get_xrange(h, i, xlower, xupper) bind(c)
	    import
	    type(c_ptr), value :: h
	    integer(c_size_t), value :: i
	    real(c_double), intent(out) :: xlower, xupper
	    integer(c_int) :: gsl_histogram2d_get_xrange
	  end function gsl_histogram2d_get_xrange
	  function gsl_histogram2d_get_yrange(h, i, ylower, yupper) bind(c)
	    import
	    type(c_ptr), value :: h
	    integer(c_size_t), value :: i
	    real(c_double), intent(out) :: ylower, yupper
	    integer(c_int) :: gsl_histogram2d_get_yrange
	  end function gsl_histogram2d_get_yrange
	  function gsl_histogram2d_xmax(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram2d_xmax
	  end function gsl_histogram2d_xmax
	  function gsl_histogram2d_xmin(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram2d_xmin
	  end function gsl_histogram2d_xmin
	  function gsl_histogram2d_nx(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    integer(c_size_t) :: gsl_histogram2d_nx
	  end function gsl_histogram2d_nx
	  function gsl_histogram2d_ymax(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram2d_ymax
	  end function gsl_histogram2d_ymax
	  function gsl_histogram2d_ymin(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram2d_ymin
	  end function gsl_histogram2d_ymin
	  function gsl_histogram2d_ny(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    integer(c_size_t) :: gsl_histogram2d_ny
	  end function gsl_histogram2d_ny
	  subroutine gsl_histogram2d_reset(h) bind(c)
	    import
	    type(c_ptr), value :: h
	  end subroutine gsl_histogram2d_reset
	  function gsl_histogram2d_find(h, x, y, i, j) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double), value :: x, y
	    integer(c_size_t), intent(out) :: i, j
	    integer(c_int) :: gsl_histogram2d_find
	  end function gsl_histogram2d_find
	  function gsl_histogram2d_max_val(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram2d_max_val
	  end function gsl_histogram2d_max_val
	  subroutine gsl_histogram2d_max_bin(h, i, j) bind(c)
	    import
	    type(c_ptr), value :: h
	    integer(c_size_t), intent(out) :: i, j
	  end subroutine gsl_histogram2d_max_bin
	  function gsl_histogram2d_min_val(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram2d_min_val
	  end function gsl_histogram2d_min_val
	  subroutine gsl_histogram2d_min_bin(h, i, j) bind(c)
	    import
	    type(c_ptr), value :: h
	    integer(c_size_t), intent(out) :: i, j
	  end subroutine gsl_histogram2d_min_bin
	  function gsl_histogram2d_xmean(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram2d_xmean
	  end function gsl_histogram2d_xmean
	  function gsl_histogram2d_ymean(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram2d_ymean
	  end function gsl_histogram2d_ymean
	  function gsl_histogram2d_xsigma(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram2d_xsigma
	  end function gsl_histogram2d_xsigma
	  function gsl_histogram2d_ysigma(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram2d_ysigma
	  end function gsl_histogram2d_ysigma
	  function gsl_histogram2d_cov(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram2d_cov
	  end function gsl_histogram2d_cov
	  function gsl_histogram2d_sum(h) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double) :: gsl_histogram2d_sum
	  end function gsl_histogram2d_sum
	  function gsl_histogram2d_equal_bins_p(h1, h2) bind(c)
	    import
	    type(c_ptr), value :: h1, h2
	    integer(c_int) :: gsl_histogram2d_equal_bins_p
	  end function gsl_histogram2d_equal_bins_p
	  function gsl_histogram2d_add(h1, h2) bind(c)
	    import
	    type(c_ptr), value :: h1, h2
	    integer(c_int) :: gsl_histogram2d_add
	  end function gsl_histogram2d_add
	  function gsl_histogram2d_sub(h1, h2) bind(c)
	    import
	    type(c_ptr), value :: h1, h2
	    integer(c_int) :: gsl_histogram2d_sub
	  end function gsl_histogram2d_sub
	  function gsl_histogram2d_mul(h1, h2) bind(c)
	    import
	    type(c_ptr), value :: h1, h2
	    integer(c_int) :: gsl_histogram2d_mul
	  end function gsl_histogram2d_mul
	  function gsl_histogram2d_div(h1, h2) bind(c)
	    import
	    type(c_ptr), value :: h1, h2
	    integer(c_int) :: gsl_histogram2d_div
	  end function gsl_histogram2d_div
	  function gsl_histogram2d_scale(h, scale) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double), value :: scale
	    integer(c_int) :: gsl_histogram2d_scale
	  end function gsl_histogram2d_scale
	  function gsl_histogram2d_shift(h, offset) bind(c)
	    import
	    type(c_ptr), value :: h
	    real(c_double), value :: offset
	    integer(c_int) :: gsl_histogram2d_shift
	  end function gsl_histogram2d_shift
	  function gsl_histogram2d_fwrite(stream, h) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: stream, h
	    integer(c_int) :: gsl_histogram2d_fwrite
	  end function gsl_histogram2d_fwrite
	  function gsl_histogram2d_fread(stream, h) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: stream, h
	    integer(c_int) :: gsl_histogram2d_fread
	  end function gsl_histogram2d_fread
	  function gsl_histogram2d_fprintf(stream, h, range_format, bin_format) bind(c)
	    import :: c_ptr, c_int, c_char
	    type(c_ptr), value :: stream, h
	    character(kind=c_char) :: range_format(*), bin_format(*)
	    integer(c_int) :: gsl_histogram2d_fprintf
	  end function gsl_histogram2d_fprintf
	  function gsl_histogram2d_fscanf(stream, h) bind(c)
	    import :: c_ptr, c_int
	    type(c_ptr), value :: stream, h
	    integer(c_int) :: gsl_histogram2d_fscanf
	  end function gsl_histogram2d_fscanf
	  function gsl_histogram2d_pdf_alloc(nx, ny) bind(c)
	    import
	    integer(c_size_t), value :: nx, ny
	    type(c_ptr) :: gsl_histogram2d_pdf_alloc
	  end function gsl_histogram2d_pdf_alloc
	  function gsl_histogram2d_pdf_init(p, h) bind(c)
	    import
	    type(c_ptr), value :: p, h
	    integer(c_int) :: gsl_histogram2d_pdf_init
	  end function gsl_histogram2d_pdf_init
	  subroutine gsl_histogram2d_pdf_free(p) bind(c)
	    import
	    type(c_ptr), value :: p
	  end subroutine gsl_histogram2d_pdf_free
	  function gsl_histogram2d_pdf_sample(p, r1, r2, x, y) bind(c)
	    import
	    type(c_ptr), value :: p
	    real(c_double), value :: r1, r2
	    real(c_double), intent(out) :: x, y
	    integer(c_int) :: gsl_histogram2d_pdf_sample
	  end function gsl_histogram2d_pdf_sample
  end interface
contains
  function fgsl_histogram_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_histogram) :: fgsl_histogram_alloc
    fgsl_histogram_alloc%gsl_histogram = gsl_histogram_alloc(n)
  end function fgsl_histogram_alloc
  function fgsl_histogram_set_ranges(h, range)
    type(fgsl_histogram), intent(inout) :: h
    real(fgsl_double), intent(in), target, contiguous :: range(:)
    integer(fgsl_int) :: fgsl_histogram_set_ranges
    fgsl_histogram_set_ranges = gsl_histogram_set_ranges(h%gsl_histogram, c_loc(range), &
    size(range, dim=1, kind=fgsl_size_t))
  end function fgsl_histogram_set_ranges
  function fgsl_histogram_set_ranges_uniform(h, xmin, xmax)
    type(fgsl_histogram), intent(inout) :: h
    real(fgsl_double), intent(in) :: xmin, xmax
    integer(fgsl_int) :: fgsl_histogram_set_ranges_uniform
    fgsl_histogram_set_ranges_uniform = &
         gsl_histogram_set_ranges_uniform(h%gsl_histogram, xmin, xmax)
  end function fgsl_histogram_set_ranges_uniform
  subroutine fgsl_histogram_free(h)
    type(fgsl_histogram), intent(inout) :: h
    call gsl_histogram_free(h%gsl_histogram)
  end subroutine fgsl_histogram_free
  function fgsl_histogram_memcpy(dest, src)
    type(fgsl_histogram), intent(inout) :: dest
    type(fgsl_histogram), intent(in) :: src
    integer(fgsl_int) :: fgsl_histogram_memcpy
    fgsl_histogram_memcpy = gsl_histogram_memcpy(dest%gsl_histogram, src%gsl_histogram)
  end function fgsl_histogram_memcpy
  function fgsl_histogram_clone(src)
    type(fgsl_histogram), intent(in) :: src
    type(fgsl_histogram) :: fgsl_histogram_clone
    fgsl_histogram_clone%gsl_histogram = gsl_histogram_clone(src%gsl_histogram)
  end function fgsl_histogram_clone
  function fgsl_histogram_increment(h, x)
    type(fgsl_histogram), intent(inout) :: h
    real(fgsl_double), intent(in) :: x
    integer(fgsl_int) :: fgsl_histogram_increment
    fgsl_histogram_increment = gsl_histogram_increment(h%gsl_histogram, x)
  end function fgsl_histogram_increment
  function fgsl_histogram_accumulate(h, x, weight)
    type(fgsl_histogram), intent(inout) :: h
    real(fgsl_double), intent(in) :: x, weight
    integer(fgsl_int) :: fgsl_histogram_accumulate
    fgsl_histogram_accumulate = gsl_histogram_accumulate(h%gsl_histogram, x, weight)
  end function fgsl_histogram_accumulate
  function fgsl_histogram_get(h, i)
    type(fgsl_histogram), intent(in) :: h
    integer(fgsl_size_t), intent(in) :: i
    real(fgsl_double) :: fgsl_histogram_get
    fgsl_histogram_get = gsl_histogram_get(h%gsl_histogram, i)
  end function fgsl_histogram_get
  function fgsl_histogram_get_range(h, i, lower, upper)
    type(fgsl_histogram), intent(in) :: h
    integer(fgsl_size_t), intent(in) :: i
    real(fgsl_double), intent(out) :: lower, upper
    integer(fgsl_int) :: fgsl_histogram_get_range
    fgsl_histogram_get_range = &
         gsl_histogram_get_range(h%gsl_histogram, i, lower, upper)
  end function fgsl_histogram_get_range
  function fgsl_histogram_max(h)
    type(fgsl_histogram), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram_max
    fgsl_histogram_max = gsl_histogram_max(h%gsl_histogram)
  end function fgsl_histogram_max
  function fgsl_histogram_min(h)
    type(fgsl_histogram), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram_min
    fgsl_histogram_min = gsl_histogram_min(h%gsl_histogram)
  end function fgsl_histogram_min
  function fgsl_histogram_bins(h)
    type(fgsl_histogram), intent(in) :: h
    integer(fgsl_size_t) :: fgsl_histogram_bins
    fgsl_histogram_bins = gsl_histogram_bins(h%gsl_histogram)
  end function fgsl_histogram_bins
  subroutine fgsl_histogram_reset(h)
    type(fgsl_histogram), intent(inout) :: h
    call gsl_histogram_reset(h%gsl_histogram)
  end subroutine fgsl_histogram_reset
  function fgsl_histogram_find(h, x, i)
    type(fgsl_histogram), intent(in) :: h
    real(fgsl_double), intent(in) :: x
    integer(fgsl_size_t), intent(out) :: i
    integer(fgsl_int) :: fgsl_histogram_find
    fgsl_histogram_find = gsl_histogram_find(h%gsl_histogram, x, i)
  end function fgsl_histogram_find
  function fgsl_histogram_max_val(h)
    type(fgsl_histogram), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram_max_val
    fgsl_histogram_max_val = gsl_histogram_max_val(h%gsl_histogram)
  end function fgsl_histogram_max_val
  function fgsl_histogram_max_bin(h)
    type(fgsl_histogram), intent(in) :: h
    integer(fgsl_size_t) :: fgsl_histogram_max_bin
    fgsl_histogram_max_bin = gsl_histogram_max_bin(h%gsl_histogram)
  end function fgsl_histogram_max_bin
  function fgsl_histogram_min_val(h)
    type(fgsl_histogram), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram_min_val
    fgsl_histogram_min_val = gsl_histogram_min_val(h%gsl_histogram)
  end function fgsl_histogram_min_val
  function fgsl_histogram_min_bin(h)
    type(fgsl_histogram), intent(in) :: h
    integer(fgsl_size_t) :: fgsl_histogram_min_bin
    fgsl_histogram_min_bin = gsl_histogram_min_bin(h%gsl_histogram)
  end function fgsl_histogram_min_bin
  function fgsl_histogram_mean(h)
    type(fgsl_histogram), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram_mean
    fgsl_histogram_mean = gsl_histogram_mean(h%gsl_histogram)
  end function fgsl_histogram_mean
  function fgsl_histogram_sigma(h)
    type(fgsl_histogram), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram_sigma
    fgsl_histogram_sigma = gsl_histogram_sigma(h%gsl_histogram)
  end function fgsl_histogram_sigma
  function fgsl_histogram_sum(h)
    type(fgsl_histogram), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram_sum
    fgsl_histogram_sum = gsl_histogram_sum(h%gsl_histogram)
  end function fgsl_histogram_sum
  function fgsl_histogram_equal_bins_p(h1, h2)
    type(fgsl_histogram), intent(in) :: h1, h2
    real(fgsl_double) :: fgsl_histogram_equal_bins_p
    fgsl_histogram_equal_bins_p = &
         gsl_histogram_equal_bins_p(h1%gsl_histogram, h2%gsl_histogram)
  end function fgsl_histogram_equal_bins_p
  function fgsl_histogram_add(h1, h2)
    type(fgsl_histogram), intent(inout) :: h1
    type(fgsl_histogram), intent(in) :: h2
    real(fgsl_double) :: fgsl_histogram_add
    fgsl_histogram_add = &
         gsl_histogram_add(h1%gsl_histogram, h2%gsl_histogram)
  end function fgsl_histogram_add
  function fgsl_histogram_sub(h1, h2)
    type(fgsl_histogram), intent(inout) :: h1
    type(fgsl_histogram), intent(in) :: h2
    real(fgsl_double) :: fgsl_histogram_sub
    fgsl_histogram_sub = &
         gsl_histogram_sub(h1%gsl_histogram, h2%gsl_histogram)
  end function fgsl_histogram_sub
  function fgsl_histogram_mul(h1, h2)
    type(fgsl_histogram), intent(inout) :: h1
    type(fgsl_histogram), intent(in) :: h2
    real(fgsl_double) :: fgsl_histogram_mul
    fgsl_histogram_mul = &
         gsl_histogram_mul(h1%gsl_histogram, h2%gsl_histogram)
  end function fgsl_histogram_mul
  function fgsl_histogram_div(h1, h2)
    type(fgsl_histogram), intent(inout) :: h1
    type(fgsl_histogram), intent(in) :: h2
    real(fgsl_double) :: fgsl_histogram_div
    fgsl_histogram_div = &
         gsl_histogram_div(h1%gsl_histogram, h2%gsl_histogram)
  end function fgsl_histogram_div
  function fgsl_histogram_scale(h, scale)
    type(fgsl_histogram), intent(inout) :: h
    real(fgsl_double), intent(in) :: scale
    integer(fgsl_int) :: fgsl_histogram_scale
    fgsl_histogram_scale = gsl_histogram_scale(h%gsl_histogram, scale)
  end function fgsl_histogram_scale
  function fgsl_histogram_shift(h, offset)
    type(fgsl_histogram), intent(inout) :: h
    real(fgsl_double), intent(in) :: offset
    integer(fgsl_int) :: fgsl_histogram_shift
    fgsl_histogram_shift = gsl_histogram_shift(h%gsl_histogram, offset)
  end function fgsl_histogram_shift
  function fgsl_histogram_fwrite(stream, h)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_histogram), intent(in) :: h
    integer(fgsl_int) :: fgsl_histogram_fwrite
    fgsl_histogram_fwrite = gsl_histogram_fwrite(stream%gsl_file, h%gsl_histogram)
  end function fgsl_histogram_fwrite
  function fgsl_histogram_fread(stream, h)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_histogram), intent(inout) :: h
    integer(fgsl_int) :: fgsl_histogram_fread
    fgsl_histogram_fread = gsl_histogram_fread(stream%gsl_file, h%gsl_histogram)
  end function fgsl_histogram_fread
  function fgsl_histogram_fprintf(stream, h, range_format, bin_format)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_histogram), intent(in) :: h
    character(kind=fgsl_char, len=*), intent(in) :: range_format, bin_format
    integer(fgsl_int) :: fgsl_histogram_fprintf
!
    fgsl_histogram_fprintf = &
         gsl_histogram_fprintf(stream%gsl_file, h%gsl_histogram, &
         range_format // c_null_char, bin_format // c_null_char)
  end function fgsl_histogram_fprintf
  function fgsl_histogram_fscanf(stream, h)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_histogram), intent(inout) :: h
    integer(fgsl_int) :: fgsl_histogram_fscanf
    fgsl_histogram_fscanf = gsl_histogram_fscanf(stream%gsl_file, h%gsl_histogram)
  end function fgsl_histogram_fscanf
  function fgsl_histogram_pdf_alloc(n)
    integer(fgsl_size_t), intent(in) :: n
    type(fgsl_histogram_pdf) :: fgsl_histogram_pdf_alloc
    fgsl_histogram_pdf_alloc%gsl_histogram_pdf = gsl_histogram_pdf_alloc(n)
  end function fgsl_histogram_pdf_alloc
  function fgsl_histogram_pdf_init(p, h)
    type(fgsl_histogram_pdf), intent(inout) :: p
    type(fgsl_histogram), intent(in) :: h
    integer(fgsl_int) :: fgsl_histogram_pdf_init
    fgsl_histogram_pdf_init = gsl_histogram_pdf_init(p%gsl_histogram_pdf, h%gsl_histogram)
  end function fgsl_histogram_pdf_init
  subroutine fgsl_histogram_pdf_free(p)
    type(fgsl_histogram_pdf), intent(inout) :: p
    call gsl_histogram_pdf_free(p%gsl_histogram_pdf)
  end subroutine fgsl_histogram_pdf_free
  function fgsl_histogram_pdf_sample(p, r)
    type(fgsl_histogram_pdf), intent(in) :: p
    real(fgsl_double), intent(in) :: r
    real(fgsl_double) :: fgsl_histogram_pdf_sample
    fgsl_histogram_pdf_sample = gsl_histogram_pdf_sample(p%gsl_histogram_pdf, r)
  end function fgsl_histogram_pdf_sample
  function fgsl_histogram2d_alloc(nx, ny)
    integer(fgsl_size_t), intent(in) :: nx, ny
    type(fgsl_histogram2d) :: fgsl_histogram2d_alloc
    fgsl_histogram2d_alloc%gsl_histogram2d = gsl_histogram2d_alloc(nx, ny)
  end function fgsl_histogram2d_alloc
  function fgsl_histogram2d_set_ranges(h, xrange, yrange)
    type(fgsl_histogram2d), intent(inout) :: h
    real(fgsl_double), intent(in), target, contiguous :: xrange(:), yrange(:)
    integer(fgsl_int) :: fgsl_histogram2d_set_ranges
    fgsl_histogram2d_set_ranges = &
         gsl_histogram2d_set_ranges(h%gsl_histogram2d, c_loc(xrange), &
         size(xrange, dim=1, kind=fgsl_size_t), c_loc(yrange), &
         size(yrange, dim=1, kind=fgsl_size_t))
  end function fgsl_histogram2d_set_ranges
  function fgsl_histogram2d_set_ranges_uniform(h, xmin, xmax, ymin, ymax)
    type(fgsl_histogram2d), intent(inout) :: h
    real(fgsl_double), intent(in) :: xmin, xmax, ymin, ymax
    integer(fgsl_int) :: fgsl_histogram2d_set_ranges_uniform
    fgsl_histogram2d_set_ranges_uniform = &
         gsl_histogram2d_set_ranges_uniform(h%gsl_histogram2d, xmin, xmax, ymin, ymax)
  end function fgsl_histogram2d_set_ranges_uniform
  subroutine fgsl_histogram2d_free(h)
    type(fgsl_histogram2d), intent(inout) :: h
    call gsl_histogram2d_free(h%gsl_histogram2d)
  end subroutine fgsl_histogram2d_free
  function fgsl_histogram2d_memcpy(dest, src)
    type(fgsl_histogram2d), intent(inout) :: dest
    type(fgsl_histogram2d), intent(in) :: src
    integer(fgsl_int) :: fgsl_histogram2d_memcpy
    fgsl_histogram2d_memcpy = &
         gsl_histogram2d_memcpy(dest%gsl_histogram2d, src%gsl_histogram2d)
  end function fgsl_histogram2d_memcpy
  function fgsl_histogram2d_clone(src)
    type(fgsl_histogram2d), intent(in) :: src
    type(fgsl_histogram2d) :: fgsl_histogram2d_clone
    fgsl_histogram2d_clone%gsl_histogram2d = gsl_histogram2d_clone(src%gsl_histogram2d)
  end function fgsl_histogram2d_clone
  function fgsl_histogram2d_increment(h, x, y)
    type(fgsl_histogram2d), intent(inout) :: h
    real(fgsl_double), intent(in) :: x, y
    integer(fgsl_int) :: fgsl_histogram2d_increment
    fgsl_histogram2d_increment = gsl_histogram2d_increment(h%gsl_histogram2d, x, y)
  end function fgsl_histogram2d_increment
  function fgsl_histogram2d_accumulate(h, x, y, weight)
    type(fgsl_histogram2d), intent(inout) :: h
    real(fgsl_double), intent(in) :: x, y, weight
    integer(fgsl_int) :: fgsl_histogram2d_accumulate
    fgsl_histogram2d_accumulate = &
         gsl_histogram2d_accumulate(h%gsl_histogram2d, x, y, weight)
  end function fgsl_histogram2d_accumulate
  function fgsl_histogram2d_get(h, i, j)
    type(fgsl_histogram2d), intent(in) :: h
    integer(fgsl_size_t), intent(in) :: i, j
    real(fgsl_double) :: fgsl_histogram2d_get
    fgsl_histogram2d_get = gsl_histogram2d_get(h%gsl_histogram2d, i, j)
  end function fgsl_histogram2d_get
  function fgsl_histogram2d_get_xrange(h, i, xlower, xupper)
    type(fgsl_histogram2d), intent(in) :: h
    integer(fgsl_size_t), intent(in) :: i
    real(fgsl_double), intent(out) :: xlower, xupper
    integer(fgsl_int) :: fgsl_histogram2d_get_xrange
    fgsl_histogram2d_get_xrange = &
         gsl_histogram2d_get_xrange(h%gsl_histogram2d, i, xlower, xupper)
  end function fgsl_histogram2d_get_xrange
  function fgsl_histogram2d_get_yrange(h, i, ylower, yupper)
    type(fgsl_histogram2d), intent(in) :: h
    integer(fgsl_size_t), intent(in) :: i
    real(fgsl_double), intent(out) :: ylower, yupper
    integer(fgsl_int) :: fgsl_histogram2d_get_yrange
    fgsl_histogram2d_get_yrange = &
         gsl_histogram2d_get_yrange(h%gsl_histogram2d, i, ylower, yupper)
  end function fgsl_histogram2d_get_yrange
  function fgsl_histogram2d_xmax(h)
    type(fgsl_histogram2d), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram2d_xmax
    fgsl_histogram2d_xmax = gsl_histogram2d_xmax(h%gsl_histogram2d)
  end function fgsl_histogram2d_xmax
  function fgsl_histogram2d_xmin(h)
    type(fgsl_histogram2d), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram2d_xmin
    fgsl_histogram2d_xmin = gsl_histogram2d_xmin(h%gsl_histogram2d)
  end function fgsl_histogram2d_xmin
  function fgsl_histogram2d_nx(h)
    type(fgsl_histogram2d), intent(in) :: h
    integer(fgsl_size_t) :: fgsl_histogram2d_nx
    fgsl_histogram2d_nx = gsl_histogram2d_nx(h%gsl_histogram2d)
  end function fgsl_histogram2d_nx
  function fgsl_histogram2d_ymax(h)
    type(fgsl_histogram2d), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram2d_ymax
    fgsl_histogram2d_ymax = gsl_histogram2d_ymax(h%gsl_histogram2d)
  end function fgsl_histogram2d_ymax
  function fgsl_histogram2d_ymin(h)
    type(fgsl_histogram2d), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram2d_ymin
    fgsl_histogram2d_ymin = gsl_histogram2d_ymin(h%gsl_histogram2d)
  end function fgsl_histogram2d_ymin
  function fgsl_histogram2d_ny(h)
    type(fgsl_histogram2d), intent(in) :: h
    integer(fgsl_size_t) :: fgsl_histogram2d_ny
    fgsl_histogram2d_ny = gsl_histogram2d_ny(h%gsl_histogram2d)
  end function fgsl_histogram2d_ny
  subroutine fgsl_histogram2d_reset(h)
    type(fgsl_histogram2d), intent(inout) :: h
    call gsl_histogram2d_reset(h%gsl_histogram2d)
  end subroutine fgsl_histogram2d_reset
  function fgsl_histogram2d_find(h, x, y, i, j)
    type(fgsl_histogram2d), intent(in) :: h
    real(fgsl_double), intent(in) :: x, y
    integer(fgsl_size_t), intent(out) :: i, j
    integer(fgsl_int) :: fgsl_histogram2d_find
    fgsl_histogram2d_find = gsl_histogram2d_find(h%gsl_histogram2d, x, y, i, j)
  end function fgsl_histogram2d_find
  function fgsl_histogram2d_max_val(h)
    type(fgsl_histogram2d), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram2d_max_val
    fgsl_histogram2d_max_val = gsl_histogram2d_max_val(h%gsl_histogram2d)
  end function fgsl_histogram2d_max_val
  subroutine fgsl_histogram2d_max_bin(h, i, j)
    type(fgsl_histogram2d), intent(in) :: h
    integer(fgsl_size_t), intent(out) :: i, j
    call gsl_histogram2d_max_bin(h%gsl_histogram2d, i, j)
  end subroutine fgsl_histogram2d_max_bin
  function fgsl_histogram2d_min_val(h)
    type(fgsl_histogram2d), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram2d_min_val
    fgsl_histogram2d_min_val = gsl_histogram2d_min_val(h%gsl_histogram2d)
  end function fgsl_histogram2d_min_val
  subroutine fgsl_histogram2d_min_bin(h, i, j)
    type(fgsl_histogram2d), intent(in) :: h
    integer(fgsl_size_t), intent(out) :: i, j
    call gsl_histogram2d_min_bin(h%gsl_histogram2d, i, j)
  end subroutine fgsl_histogram2d_min_bin
  function fgsl_histogram2d_xmean(h)
    type(fgsl_histogram2d), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram2d_xmean
    fgsl_histogram2d_xmean = gsl_histogram2d_xmean(h%gsl_histogram2d)
  end function fgsl_histogram2d_xmean
  function fgsl_histogram2d_ymean(h)
    type(fgsl_histogram2d), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram2d_ymean
    fgsl_histogram2d_ymean = gsl_histogram2d_ymean(h%gsl_histogram2d)
  end function fgsl_histogram2d_ymean
  function fgsl_histogram2d_xsigma(h)
    type(fgsl_histogram2d), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram2d_xsigma
    fgsl_histogram2d_xsigma = gsl_histogram2d_xsigma(h%gsl_histogram2d)
  end function fgsl_histogram2d_xsigma
  function fgsl_histogram2d_ysigma(h)
    type(fgsl_histogram2d), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram2d_ysigma
    fgsl_histogram2d_ysigma = gsl_histogram2d_ysigma(h%gsl_histogram2d)
  end function fgsl_histogram2d_ysigma
  function fgsl_histogram2d_cov(h)
    type(fgsl_histogram2d), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram2d_cov
    fgsl_histogram2d_cov = gsl_histogram2d_cov(h%gsl_histogram2d)
  end function fgsl_histogram2d_cov
  function fgsl_histogram2d_sum(h)
    type(fgsl_histogram2d), intent(in) :: h
    real(fgsl_double) :: fgsl_histogram2d_sum
    fgsl_histogram2d_sum = gsl_histogram2d_sum(h%gsl_histogram2d)
  end function fgsl_histogram2d_sum
  function fgsl_histogram2d_equal_bins_p(h1, h2)
    type(fgsl_histogram2d), intent(in) :: h1, h2
    real(fgsl_double) :: fgsl_histogram2d_equal_bins_p
    fgsl_histogram2d_equal_bins_p = &
         gsl_histogram2d_equal_bins_p(h1%gsl_histogram2d, h2%gsl_histogram2d)
  end function fgsl_histogram2d_equal_bins_p
  function fgsl_histogram2d_add(h1, h2)
    type(fgsl_histogram2d), intent(inout) :: h1
    type(fgsl_histogram2d), intent(in) :: h2
    real(fgsl_double) :: fgsl_histogram2d_add
    fgsl_histogram2d_add = &
         gsl_histogram2d_add(h1%gsl_histogram2d, h2%gsl_histogram2d)
  end function fgsl_histogram2d_add
  function fgsl_histogram2d_sub(h1, h2)
    type(fgsl_histogram2d), intent(inout) :: h1
    type(fgsl_histogram2d), intent(in) :: h2
    real(fgsl_double) :: fgsl_histogram2d_sub
    fgsl_histogram2d_sub = &
         gsl_histogram2d_sub(h1%gsl_histogram2d, h2%gsl_histogram2d)
  end function fgsl_histogram2d_sub
  function fgsl_histogram2d_mul(h1, h2)
    type(fgsl_histogram2d), intent(inout) :: h1
    type(fgsl_histogram2d), intent(in) :: h2
    real(fgsl_double) :: fgsl_histogram2d_mul
    fgsl_histogram2d_mul = &
         gsl_histogram2d_mul(h1%gsl_histogram2d, h2%gsl_histogram2d)
  end function fgsl_histogram2d_mul
  function fgsl_histogram2d_div(h1, h2)
    type(fgsl_histogram2d), intent(inout) :: h1
    type(fgsl_histogram2d), intent(in) :: h2
    real(fgsl_double) :: fgsl_histogram2d_div
    fgsl_histogram2d_div = &
         gsl_histogram2d_div(h1%gsl_histogram2d, h2%gsl_histogram2d)
  end function fgsl_histogram2d_div
  function fgsl_histogram2d_scale(h, scale)
    type(fgsl_histogram2d), intent(inout) :: h
    real(fgsl_double), intent(in) :: scale
    integer(fgsl_int) :: fgsl_histogram2d_scale
    fgsl_histogram2d_scale = gsl_histogram2d_scale(h%gsl_histogram2d, scale)
  end function fgsl_histogram2d_scale
  function fgsl_histogram2d_shift(h, offset)
    type(fgsl_histogram2d), intent(inout) :: h
    real(fgsl_double), intent(in) :: offset
    integer(fgsl_int) :: fgsl_histogram2d_shift
    fgsl_histogram2d_shift = gsl_histogram2d_shift(h%gsl_histogram2d, offset)
  end function fgsl_histogram2d_shift
  function fgsl_histogram2d_fwrite(stream, h)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_histogram2d), intent(in) :: h
    integer(fgsl_int) :: fgsl_histogram2d_fwrite
    fgsl_histogram2d_fwrite = gsl_histogram2d_fwrite(stream%gsl_file, h%gsl_histogram2d)
  end function fgsl_histogram2d_fwrite
  function fgsl_histogram2d_fread(stream, h)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_histogram2d), intent(inout) :: h
    integer(fgsl_int) :: fgsl_histogram2d_fread
    fgsl_histogram2d_fread = gsl_histogram2d_fread(stream%gsl_file, h%gsl_histogram2d)
  end function fgsl_histogram2d_fread
  function fgsl_histogram2d_fprintf(stream, h, range_format, bin_format)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_histogram2d), intent(in) :: h
    character(kind=fgsl_char, len=*), intent(in) :: range_format, bin_format
    integer(fgsl_int) :: fgsl_histogram2d_fprintf
!
    fgsl_histogram2d_fprintf = &
         gsl_histogram2d_fprintf(stream%gsl_file, h%gsl_histogram2d, &
         range_format // c_null_char, bin_format // c_null_char)
  end function fgsl_histogram2d_fprintf
  function fgsl_histogram2d_fscanf(stream, h)
    type(fgsl_file), intent(in) :: stream
    type(fgsl_histogram2d), intent(inout) :: h
    integer(fgsl_int) :: fgsl_histogram2d_fscanf
    fgsl_histogram2d_fscanf = gsl_histogram2d_fscanf(stream%gsl_file, h%gsl_histogram2d)
  end function fgsl_histogram2d_fscanf
  function fgsl_histogram2d_pdf_alloc(nx, ny)
    integer(fgsl_size_t), intent(in) :: nx, ny
    type(fgsl_histogram2d_pdf) :: fgsl_histogram2d_pdf_alloc
    fgsl_histogram2d_pdf_alloc%gsl_histogram2d_pdf = gsl_histogram2d_pdf_alloc(nx, ny)
  end function fgsl_histogram2d_pdf_alloc
  function fgsl_histogram2d_pdf_init(p, h)
    type(fgsl_histogram2d_pdf), intent(inout) :: p
    type(fgsl_histogram2d), intent(in) :: h
    integer(fgsl_int) :: fgsl_histogram2d_pdf_init
    fgsl_histogram2d_pdf_init = gsl_histogram2d_pdf_init(p%gsl_histogram2d_pdf, h%gsl_histogram2d)
  end function fgsl_histogram2d_pdf_init
  subroutine fgsl_histogram2d_pdf_free(p)
    type(fgsl_histogram2d_pdf), intent(inout) :: p
    call gsl_histogram2d_pdf_free(p%gsl_histogram2d_pdf)
  end subroutine fgsl_histogram2d_pdf_free
  function fgsl_histogram2d_pdf_sample(p, r1, r2, x, y)
    type(fgsl_histogram2d_pdf), intent(in) :: p
    real(fgsl_double), intent(in) :: r1, r2
    real(fgsl_double), intent(out) :: x, y
    integer(fgsl_int) :: fgsl_histogram2d_pdf_sample
    fgsl_histogram2d_pdf_sample = gsl_histogram2d_pdf_sample(p%gsl_histogram2d_pdf, r1, r2, x, y)
  end function fgsl_histogram2d_pdf_sample
  function fgsl_histogram_status(histogram)
    type(fgsl_histogram), intent(in) :: histogram
    logical :: fgsl_histogram_status
    fgsl_histogram_status = .true.
    if (.not. c_associated(histogram%gsl_histogram)) fgsl_histogram_status = .false.
  end function fgsl_histogram_status

end module fgsl_histograms
