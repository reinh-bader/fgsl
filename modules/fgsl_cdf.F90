module fgsl_cdf
  !> \page Randomdist Random number distributions
  !> See \ref fgsl_cdf for details.
  use fgsl_base
  use fgsl_rngen
  use fgsl_array
  implicit none
  
  private :: gsl_ran_gaussian, gsl_ran_gaussian_pdf, gsl_ran_gaussian_ziggurat, &
    gsl_ran_gaussian_ratio_method, gsl_ran_ugaussian, gsl_ran_ugaussian_pdf, &
    gsl_ran_ugaussian_ratio_method, gsl_cdf_gaussian_p, gsl_cdf_gaussian_q, &
    gsl_cdf_gaussian_pinv, gsl_cdf_gaussian_qinv, gsl_cdf_ugaussian_p, &
    gsl_cdf_ugaussian_q, gsl_cdf_ugaussian_pinv, gsl_cdf_ugaussian_qinv, &
    gsl_ran_gaussian_tail, gsl_ran_gaussian_tail_pdf, gsl_ran_ugaussian_tail, &
    gsl_ran_ugaussian_tail_pdf, gsl_ran_bivariate_gaussian, &
    gsl_ran_bivariate_gaussian_pdf, gsl_ran_multivariate_gaussian, &
    gsl_ran_multivariate_gaussian_pdf, gsl_ran_multivariate_gaussian_log_pdf, &
    gsl_ran_multivariate_gaussian_mean, gsl_ran_multivariate_gaussian_vcov, &
    gsl_ran_exponential, gsl_ran_exponential_pdf, gsl_cdf_exponential_p, &
    gsl_cdf_exponential_q, gsl_cdf_exponential_pinv, gsl_cdf_exponential_qinv, &
    gsl_ran_laplace, gsl_ran_laplace_pdf, gsl_cdf_laplace_p, gsl_cdf_laplace_q, &
    gsl_cdf_laplace_pinv, gsl_cdf_laplace_qinv, gsl_ran_exppow, gsl_ran_exppow_pdf, &
    gsl_cdf_exppow_p, gsl_cdf_exppow_q, gsl_ran_cauchy, gsl_ran_cauchy_pdf, &
    gsl_cdf_cauchy_p, gsl_cdf_cauchy_q, gsl_cdf_cauchy_pinv, gsl_cdf_cauchy_qinv, &
    gsl_ran_rayleigh, gsl_ran_rayleigh_pdf, gsl_cdf_rayleigh_p, gsl_cdf_rayleigh_q, &
    gsl_cdf_rayleigh_pinv, gsl_cdf_rayleigh_qinv, gsl_ran_rayleigh_tail, &
    gsl_ran_rayleigh_tail_pdf, gsl_ran_landau, gsl_ran_landau_pdf, gsl_ran_levy, &
    gsl_ran_levy_skew, gsl_ran_gamma, gsl_ran_gamma_mt, gsl_ran_gamma_pdf, &
    gsl_cdf_gamma_p, gsl_cdf_gamma_q, gsl_cdf_gamma_pinv, gsl_cdf_gamma_qinv, &
    gsl_ran_flat, gsl_ran_flat_pdf, gsl_cdf_flat_p, gsl_cdf_flat_q, &
    gsl_cdf_flat_pinv, gsl_cdf_flat_qinv, gsl_ran_lognormal, gsl_ran_lognormal_pdf, &
    gsl_cdf_lognormal_p, gsl_cdf_lognormal_q, gsl_cdf_lognormal_pinv, &
    gsl_cdf_lognormal_qinv, gsl_ran_chisq, gsl_ran_chisq_pdf, gsl_cdf_chisq_p, &
    gsl_cdf_chisq_q, gsl_cdf_chisq_pinv, gsl_cdf_chisq_qinv, gsl_ran_fdist, &
    gsl_ran_fdist_pdf, gsl_cdf_fdist_p, gsl_cdf_fdist_q, gsl_cdf_fdist_pinv, &
    gsl_cdf_fdist_qinv, gsl_ran_tdist, gsl_ran_tdist_pdf, gsl_cdf_tdist_p, &
    gsl_cdf_tdist_q, gsl_cdf_tdist_pinv, gsl_cdf_tdist_qinv, gsl_ran_beta, &
    gsl_ran_beta_pdf, gsl_cdf_beta_p, gsl_cdf_beta_q, gsl_cdf_beta_pinv, &
    gsl_cdf_beta_qinv, gsl_ran_logistic, gsl_ran_logistic_pdf, gsl_cdf_logistic_p, &
    gsl_cdf_logistic_q, gsl_cdf_logistic_pinv, gsl_cdf_logistic_qinv, &
    gsl_ran_pareto, gsl_ran_pareto_pdf, gsl_cdf_pareto_p, gsl_cdf_pareto_q, &
    gsl_cdf_pareto_pinv, gsl_cdf_pareto_qinv, gsl_ran_dir_2d, &
    gsl_ran_dir_2d_trig_method, gsl_ran_dir_3d, gsl_ran_dir_nd, &
    gsl_ran_weibull, gsl_ran_weibull_pdf, gsl_cdf_weibull_p, gsl_cdf_weibull_q, &
    gsl_cdf_weibull_pinv, gsl_cdf_weibull_qinv, gsl_ran_gumbel1, &
    gsl_ran_gumbel1_pdf, gsl_cdf_gumbel1_p, gsl_cdf_gumbel1_q, &
    gsl_cdf_gumbel1_pinv, gsl_cdf_gumbel1_qinv, gsl_ran_gumbel2, &
    gsl_ran_gumbel2_pdf, gsl_cdf_gumbel2_p, gsl_cdf_gumbel2_q, &
    gsl_cdf_gumbel2_pinv, gsl_cdf_gumbel2_qinv, gsl_ran_dirichlet, &
    gsl_ran_dirichlet_pdf, gsl_ran_dirichlet_lnpdf, gsl_ran_discrete_preproc, &
    gsl_ran_discrete, gsl_ran_discrete_pdf, gsl_ran_discrete_free, &
    gsl_ran_poisson, gsl_ran_poisson_pdf, gsl_cdf_poisson_p, gsl_cdf_poisson_q, &
    gsl_ran_bernoulli, gsl_ran_bernoulli_pdf, gsl_ran_binomial, gsl_ran_binomial_pdf, &
    gsl_cdf_binomial_p, gsl_cdf_binomial_q, gsl_ran_multinomial, &
    gsl_ran_multinomial_pdf, gsl_ran_multinomial_lnpdf, gsl_ran_negative_binomial, &
    gsl_ran_negative_binomial_pdf, gsl_cdf_negative_binomial_p, &
    gsl_cdf_negative_binomial_q, gsl_ran_pascal, gsl_ran_pascal_pdf, &
    gsl_cdf_pascal_p, gsl_cdf_pascal_q, gsl_ran_geometric, gsl_ran_geometric_pdf, &
    gsl_cdf_geometric_p, gsl_cdf_geometric_q, gsl_ran_hypergeometric, &
    gsl_ran_hypergeometric_pdf, gsl_cdf_hypergeometric_p, gsl_cdf_hypergeometric_q, &
    gsl_ran_logarithmic, gsl_ran_logarithmic_pdf, gsl_ran_wishart, gsl_ran_wishart_pdf, &
    gsl_ran_wishart_log_pdf, gsl_ran_shuffle, gsl_ran_choose, gsl_ran_sample
  !
  ! Types
  type, public :: fgsl_ran_discrete_t
     private
     type(c_ptr) :: gsl_ran_discrete_t
  end type fgsl_ran_discrete_t
  !
  ! Generics
  interface fgsl_ran_shuffle
     module procedure fgsl_ran_shuffle
     module procedure fgsl_ran_shuffle_double
     module procedure fgsl_ran_shuffle_size_t
  end interface 
  interface fgsl_well_defined
     module procedure fgsl_ran_discrete_t_status
  end interface
  !
  ! C interfaces
  interface
	  function gsl_ran_gaussian(r, sigma) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_ran_gaussian
	  end function gsl_ran_gaussian
	  function gsl_ran_gaussian_pdf(x, sigma) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_ran_gaussian_pdf
	  end function gsl_ran_gaussian_pdf
	  function gsl_ran_gaussian_ziggurat(r, sigma) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_ran_gaussian_ziggurat
	  end function gsl_ran_gaussian_ziggurat
	  function gsl_ran_gaussian_ratio_method(r, sigma) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_ran_gaussian_ratio_method
	  end function gsl_ran_gaussian_ratio_method
	  function gsl_ran_ugaussian(r) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double) :: gsl_ran_ugaussian
	  end function gsl_ran_ugaussian
	  function gsl_ran_ugaussian_pdf(x) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double) :: gsl_ran_ugaussian_pdf
	  end function gsl_ran_ugaussian_pdf
	  function gsl_ran_ugaussian_ratio_method(r) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double) :: gsl_ran_ugaussian_ratio_method
	  end function gsl_ran_ugaussian_ratio_method
	  function gsl_cdf_gaussian_p(x, sigma) bind(c, name='gsl_cdf_gaussian_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_cdf_gaussian_p
	  end function gsl_cdf_gaussian_p
	  function gsl_cdf_gaussian_q(x, sigma) bind(c, name='gsl_cdf_gaussian_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_cdf_gaussian_q
	  end function gsl_cdf_gaussian_q
	  function gsl_cdf_gaussian_pinv(p, sigma) bind(c, name='gsl_cdf_gaussian_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_cdf_gaussian_pinv
	  end function gsl_cdf_gaussian_pinv
	  function gsl_cdf_gaussian_qinv(q, sigma) bind(c, name='gsl_cdf_gaussian_Qinv')
	    import
	    real(c_double), value :: q
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_cdf_gaussian_qinv
	  end function gsl_cdf_gaussian_qinv
	  function gsl_cdf_ugaussian_p(x) bind(c, name='gsl_cdf_ugaussian_P')
	    import
	    real(c_double), value :: x
	    real(c_double) :: gsl_cdf_ugaussian_p
	  end function gsl_cdf_ugaussian_p
	  function gsl_cdf_ugaussian_q(x) bind(c, name='gsl_cdf_ugaussian_Q')
	    import
	    real(c_double), value :: x
	    real(c_double) :: gsl_cdf_ugaussian_q
	  end function gsl_cdf_ugaussian_q
	  function gsl_cdf_ugaussian_pinv(p) bind(c, name='gsl_cdf_ugaussian_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double) :: gsl_cdf_ugaussian_pinv
	  end function gsl_cdf_ugaussian_pinv
	  function gsl_cdf_ugaussian_qinv(q) bind(c, name='gsl_cdf_ugaussian_Qinv')
	    import
	    real(c_double), value :: q
	    real(c_double) :: gsl_cdf_ugaussian_qinv
	  end function gsl_cdf_ugaussian_qinv
	  function gsl_ran_gaussian_tail(r, a, sigma) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: a, sigma
	    real(c_double) :: gsl_ran_gaussian_tail
	  end function gsl_ran_gaussian_tail
	  function gsl_ran_gaussian_tail_pdf(x, a, sigma) bind(c)
	    import
	    real(c_double), value :: x, a
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_ran_gaussian_tail_pdf
	  end function gsl_ran_gaussian_tail_pdf
	  function gsl_ran_ugaussian_tail(r, a) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: a
	    real(c_double) :: gsl_ran_ugaussian_tail
	  end function gsl_ran_ugaussian_tail
	  function gsl_ran_ugaussian_tail_pdf(x, a) bind(c)
	    import
	    real(c_double), value :: x, a
	    real(c_double) :: gsl_ran_ugaussian_tail_pdf
	  end function gsl_ran_ugaussian_tail_pdf
	  subroutine gsl_ran_bivariate_gaussian(r, sigma_x, sigma_y, rho, x, y) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: sigma_x, sigma_y, rho
	    real(c_double), intent(out) :: x, y
	  end subroutine gsl_ran_bivariate_gaussian
	  function gsl_ran_bivariate_gaussian_pdf(x, y, sigma_x, sigma_y, rho) bind(c)
	    import
	    real(c_double), value :: x, y, sigma_x, sigma_y, rho
	    real(c_double) :: gsl_ran_bivariate_gaussian_pdf
	  end function gsl_ran_bivariate_gaussian_pdf
	  integer(c_int) function gsl_ran_multivariate_gaussian(r, mu, l, result) bind(c)
	    import
	    type(c_ptr), value :: r, mu, l, result
	  end function gsl_ran_multivariate_gaussian
	  integer(c_int) function gsl_ran_multivariate_gaussian_pdf( &
	       x, mu, l, result, work) bind(c)
	    import
	    type(c_ptr), value :: x, mu, l, work
	    real(fgsl_double) ::  result
	  end function gsl_ran_multivariate_gaussian_pdf
	  integer(c_int) function gsl_ran_multivariate_gaussian_log_pdf( &
	       x, mu, l, result, work) bind(c)
	    import
	    type(c_ptr), value :: x, mu, l, work
	    real(fgsl_double) ::  result
	  end function gsl_ran_multivariate_gaussian_log_pdf
	  integer(c_int) function gsl_ran_multivariate_gaussian_mean( &
	       x, mu_hat) bind(c)
	    import
	    type(c_ptr), value :: x, mu_hat
	  end function gsl_ran_multivariate_gaussian_mean
	  integer(c_int) function gsl_ran_multivariate_gaussian_vcov( &
	       x, sigma_hat) bind(c)
	    import
	    type(c_ptr), value :: x, sigma_hat
	  end function gsl_ran_multivariate_gaussian_vcov
	  function gsl_ran_exponential(r, mu) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: mu
	    real(c_double) :: gsl_ran_exponential
	  end function gsl_ran_exponential
	  function gsl_ran_exponential_pdf(x, mu) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: mu
	    real(c_double) :: gsl_ran_exponential_pdf
	  end function gsl_ran_exponential_pdf
	  function gsl_cdf_exponential_p(x, mu) bind(c, name='gsl_cdf_exponential_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: mu
	    real(c_double) :: gsl_cdf_exponential_p
	  end function gsl_cdf_exponential_p
	  function gsl_cdf_exponential_q(x, mu) bind(c, name='gsl_cdf_exponential_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: mu
	    real(c_double) :: gsl_cdf_exponential_q
	  end function gsl_cdf_exponential_q
	  function gsl_cdf_exponential_pinv(p, mu) bind(c, name='gsl_cdf_exponential_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: mu
	    real(c_double) :: gsl_cdf_exponential_pinv
	  end function gsl_cdf_exponential_pinv
	  function gsl_cdf_exponential_qinv(q, mu) bind(c, name='gsl_cdf_exponential_Qinv')
	    import
	    real(c_double), value :: q
	    real(c_double), value :: mu
	    real(c_double) :: gsl_cdf_exponential_qinv
	  end function gsl_cdf_exponential_qinv
	  function gsl_ran_laplace(r, a) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: a
	    real(c_double) :: gsl_ran_laplace
	  end function gsl_ran_laplace
	  function gsl_ran_laplace_pdf(x, a) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a
	    real(c_double) :: gsl_ran_laplace_pdf
	  end function gsl_ran_laplace_pdf
	  function gsl_cdf_laplace_p(x, a) bind(c, name='gsl_cdf_laplace_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a
	    real(c_double) :: gsl_cdf_laplace_p
	  end function gsl_cdf_laplace_p
	  function gsl_cdf_laplace_q(x, a) bind(c, name='gsl_cdf_laplace_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a
	    real(c_double) :: gsl_cdf_laplace_q
	  end function gsl_cdf_laplace_q
	  function gsl_cdf_laplace_pinv(p, a) bind(c, name='gsl_cdf_laplace_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: a
	    real(c_double) :: gsl_cdf_laplace_pinv
	  end function gsl_cdf_laplace_pinv
	  function gsl_cdf_laplace_qinv(q, a) bind(c, name='gsl_cdf_laplace_Qinv')
	    import
	    real(c_double), value :: q
	    real(c_double), value :: a
	    real(c_double) :: gsl_cdf_laplace_qinv
	  end function gsl_cdf_laplace_qinv
	  function gsl_ran_exppow(r, a, b) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_exppow
	  end function gsl_ran_exppow
	  function gsl_ran_exppow_pdf(x, a, b) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_exppow_pdf
	  end function gsl_ran_exppow_pdf
	  function gsl_cdf_exppow_p(x, a, b) bind(c, name='gsl_cdf_exppow_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_exppow_p
	  end function gsl_cdf_exppow_p
	  function gsl_cdf_exppow_q(x, a, b) bind(c, name='gsl_cdf_exppow_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_exppow_q
	  end function gsl_cdf_exppow_q
	  function gsl_ran_cauchy(r, a) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: a
	    real(c_double) :: gsl_ran_cauchy
	  end function gsl_ran_cauchy
	  function gsl_ran_cauchy_pdf(x, a) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a
	    real(c_double) :: gsl_ran_cauchy_pdf
	  end function gsl_ran_cauchy_pdf
	  function gsl_cdf_cauchy_p(x, a) bind(c, name='gsl_cdf_cauchy_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a
	    real(c_double) :: gsl_cdf_cauchy_p
	  end function gsl_cdf_cauchy_p
	  function gsl_cdf_cauchy_q(x, a) bind(c, name='gsl_cdf_cauchy_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a
	    real(c_double) :: gsl_cdf_cauchy_q
	  end function gsl_cdf_cauchy_q
	  function gsl_cdf_cauchy_pinv(p, a) bind(c, name='gsl_cdf_cauchy_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: a
	    real(c_double) :: gsl_cdf_cauchy_pinv
	  end function gsl_cdf_cauchy_pinv
	  function gsl_cdf_cauchy_qinv(q, a) bind(c, name='gsl_cdf_cauchy_Qinv')
	    import
	    real(c_double), value :: q
	    real(c_double), value :: a
	    real(c_double) :: gsl_cdf_cauchy_qinv
	  end function gsl_cdf_cauchy_qinv
	  function gsl_ran_rayleigh(r, sigma) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_ran_rayleigh
	  end function gsl_ran_rayleigh
	  function gsl_ran_rayleigh_pdf(x, sigma) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_ran_rayleigh_pdf
	  end function gsl_ran_rayleigh_pdf
	  function gsl_cdf_rayleigh_p(x, sigma) bind(c, name='gsl_cdf_rayleigh_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_cdf_rayleigh_p
	  end function gsl_cdf_rayleigh_p
	  function gsl_cdf_rayleigh_q(x, sigma) bind(c, name='gsl_cdf_rayleigh_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_cdf_rayleigh_q
	  end function gsl_cdf_rayleigh_q
	  function gsl_cdf_rayleigh_pinv(p, sigma) bind(c, name='gsl_cdf_rayleigh_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_cdf_rayleigh_pinv
	  end function gsl_cdf_rayleigh_pinv
	  function gsl_cdf_rayleigh_qinv(q, sigma) bind(c, name='gsl_cdf_rayleigh_Qinv')
	    import
	    real(c_double), value :: q
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_cdf_rayleigh_qinv
	  end function gsl_cdf_rayleigh_qinv
	  function gsl_ran_rayleigh_tail(r, a, sigma) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: a, sigma
	    real(c_double) :: gsl_ran_rayleigh_tail
	  end function gsl_ran_rayleigh_tail
	  function gsl_ran_rayleigh_tail_pdf(x, a, sigma) bind(c)
	    import
	    real(c_double), value :: x, a
	    real(c_double), value :: sigma
	    real(c_double) :: gsl_ran_rayleigh_tail_pdf
	  end function gsl_ran_rayleigh_tail_pdf
	  function gsl_ran_landau(r) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double) :: gsl_ran_landau
	  end function gsl_ran_landau
	  function gsl_ran_landau_pdf(x) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double) :: gsl_ran_landau_pdf
	  end function gsl_ran_landau_pdf
	  function gsl_ran_levy(r, c, alpha) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: c, alpha
	    real(c_double) :: gsl_ran_levy
	  end function gsl_ran_levy
	  function gsl_ran_levy_skew(r, c, alpha, beta) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: c, alpha, beta
	    real(c_double) :: gsl_ran_levy_skew
	  end function gsl_ran_levy_skew
	  function gsl_ran_gamma(r, a, b) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_gamma
	  end function gsl_ran_gamma
	  function gsl_ran_gamma_mt(r, a, b) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_gamma_mt
	  end function gsl_ran_gamma_mt
	  function gsl_ran_gamma_pdf(x, a, b) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_gamma_pdf
	  end function gsl_ran_gamma_pdf
	  function gsl_cdf_gamma_p(x, a, b) bind(c, name='gsl_cdf_gamma_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_gamma_p
	  end function gsl_cdf_gamma_p
	  function gsl_cdf_gamma_q(x, a, b) bind(c, name='gsl_cdf_gamma_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_gamma_q
	  end function gsl_cdf_gamma_q
	  function gsl_cdf_gamma_pinv(p, a, b) bind(c, name='gsl_cdf_gamma_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_gamma_pinv
	  end function gsl_cdf_gamma_pinv
	  function gsl_cdf_gamma_qinv(q, a, b) bind(c, name='gsl_cdf_gamma_Qinv')
	    import
	    real(c_double), value :: q
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_gamma_qinv
	  end function gsl_cdf_gamma_qinv
	  function gsl_ran_flat(r, a, b) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_flat
	  end function gsl_ran_flat
	  function gsl_ran_flat_pdf(x, a, b) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_flat_pdf
	  end function gsl_ran_flat_pdf
	  function gsl_cdf_flat_p(x, a, b) bind(c, name='gsl_cdf_flat_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_flat_p
	  end function gsl_cdf_flat_p
	  function gsl_cdf_flat_q(x, a, b) bind(c, name='gsl_cdf_flat_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_flat_q
	  end function gsl_cdf_flat_q
	  function gsl_cdf_flat_pinv(p, a, b) bind(c, name='gsl_cdf_flat_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_flat_pinv
	  end function gsl_cdf_flat_pinv
	  function gsl_cdf_flat_qinv(q, a, b) bind(c, name='gsl_cdf_flat_Qinv')
	    import
	    real(c_double), value :: q
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_flat_qinv
	  end function gsl_cdf_flat_qinv
	  function gsl_ran_lognormal(r, zeta, sigma) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: zeta, sigma
	    real(c_double) :: gsl_ran_lognormal
	  end function gsl_ran_lognormal
	  function gsl_ran_lognormal_pdf(x, zeta, sigma) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: zeta, sigma
	    real(c_double) :: gsl_ran_lognormal_pdf
	  end function gsl_ran_lognormal_pdf
	  function gsl_cdf_lognormal_p(x, zeta, sigma) bind(c, name='gsl_cdf_lognormal_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: zeta, sigma
	    real(c_double) :: gsl_cdf_lognormal_p
	  end function gsl_cdf_lognormal_p
	  function gsl_cdf_lognormal_q(x, zeta, sigma) bind(c, name='gsl_cdf_lognormal_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: zeta, sigma
	    real(c_double) :: gsl_cdf_lognormal_q
	  end function gsl_cdf_lognormal_q
	  function gsl_cdf_lognormal_pinv(p, zeta, sigma) bind(c, name='gsl_cdf_lognormal_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: zeta, sigma
	    real(c_double) :: gsl_cdf_lognormal_pinv
	  end function gsl_cdf_lognormal_pinv
	  function gsl_cdf_lognormal_qinv(q, zeta, sigma) bind(c, name='gsl_cdf_lognormal_Qinv')
	    import
	    real(c_double), value :: q
	    real(c_double), value :: zeta, sigma
	    real(c_double) :: gsl_cdf_lognormal_qinv
	  end function gsl_cdf_lognormal_qinv
	  function gsl_ran_chisq(r, nu) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: nu
	    real(c_double) :: gsl_ran_chisq
	  end function gsl_ran_chisq
	  function gsl_ran_chisq_pdf(x, nu) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: nu
	    real(c_double) :: gsl_ran_chisq_pdf
	  end function gsl_ran_chisq_pdf
	  function gsl_cdf_chisq_p(x, nu) bind(c, name='gsl_cdf_chisq_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: nu
	    real(c_double) :: gsl_cdf_chisq_p
	  end function gsl_cdf_chisq_p
	  function gsl_cdf_chisq_q(x, nu) bind(c, name='gsl_cdf_chisq_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: nu
	    real(c_double) :: gsl_cdf_chisq_q
	  end function gsl_cdf_chisq_q
	  function gsl_cdf_chisq_pinv(p, nu) bind(c, name='gsl_cdf_chisq_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: nu
	    real(c_double) :: gsl_cdf_chisq_pinv
	  end function gsl_cdf_chisq_pinv
	  function gsl_cdf_chisq_qinv(q, nu) bind(c, name='gsl_cdf_chisq_Qinv')
	    import
	    real(c_double), value :: q
	    real(c_double), value :: nu
	    real(c_double) :: gsl_cdf_chisq_qinv
	  end function gsl_cdf_chisq_qinv
	  function gsl_ran_fdist(r, nu1, nu2) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: nu1, nu2
	    real(c_double) :: gsl_ran_fdist
	  end function gsl_ran_fdist
	  function gsl_ran_fdist_pdf(x, nu1, nu2) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: nu1, nu2
	    real(c_double) :: gsl_ran_fdist_pdf
	  end function gsl_ran_fdist_pdf
	  function gsl_cdf_fdist_p(x, nu1, nu2) bind(c, name='gsl_cdf_fdist_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: nu1, nu2
	    real(c_double) :: gsl_cdf_fdist_p
	  end function gsl_cdf_fdist_p
	  function gsl_cdf_fdist_q(x, nu1, nu2) bind(c, name='gsl_cdf_fdist_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: nu1, nu2
	    real(c_double) :: gsl_cdf_fdist_q
	  end function gsl_cdf_fdist_q
	  function gsl_cdf_fdist_pinv(p, nu1, nu2) bind(c, name='gsl_cdf_fdist_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: nu1, nu2
	    real(c_double) :: gsl_cdf_fdist_pinv
	  end function gsl_cdf_fdist_pinv
	  function gsl_cdf_fdist_qinv(q, nu1, nu2) bind(c, name='gsl_cdf_fdist_Qinv')
	    import
	    real(c_double), value :: q
	    real(c_double), value :: nu1, nu2
	    real(c_double) :: gsl_cdf_fdist_qinv
	  end function gsl_cdf_fdist_qinv
	  function gsl_ran_tdist(r, nu) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: nu
	    real(c_double) :: gsl_ran_tdist
	  end function gsl_ran_tdist
	  function gsl_ran_tdist_pdf(x, nu) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: nu
	    real(c_double) :: gsl_ran_tdist_pdf
	  end function gsl_ran_tdist_pdf
	  function gsl_cdf_tdist_p(x, nu) bind(c, name='gsl_cdf_tdist_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: nu
	    real(c_double) :: gsl_cdf_tdist_p
	  end function gsl_cdf_tdist_p
	  function gsl_cdf_tdist_q(x, nu) bind(c, name='gsl_cdf_tdist_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: nu
	    real(c_double) :: gsl_cdf_tdist_q
	  end function gsl_cdf_tdist_q
	  function gsl_cdf_tdist_pinv(p, nu) bind(c, name='gsl_cdf_tdist_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: nu
	    real(c_double) :: gsl_cdf_tdist_pinv
	  end function gsl_cdf_tdist_pinv
	  function gsl_cdf_tdist_qinv(q, nu) bind(c, name='gsl_cdf_tdist_Qinv')
	    import
	    real(c_double), value :: q
	    real(c_double), value :: nu
	    real(c_double) :: gsl_cdf_tdist_qinv
	  end function gsl_cdf_tdist_qinv
	  function gsl_ran_beta(r, a, b) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_beta
	  end function gsl_ran_beta
	  function gsl_ran_beta_pdf(x, a, b) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_beta_pdf
	  end function gsl_ran_beta_pdf
	  function gsl_cdf_beta_p(x, a, b) bind(c, name='gsl_cdf_beta_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_beta_p
	  end function gsl_cdf_beta_p
	  function gsl_cdf_beta_q(x, a, b) bind(c, name='gsl_cdf_beta_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_beta_q
	  end function gsl_cdf_beta_q
	  function gsl_cdf_beta_pinv(p, a, b) bind(c, name='gsl_cdf_beta_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_beta_pinv
	  end function gsl_cdf_beta_pinv
	  function gsl_cdf_beta_qinv(q, a, b) bind(c, name='gsl_cdf_beta_Qinv')
	    import
	    real(c_double), value :: q
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_beta_qinv
	  end function gsl_cdf_beta_qinv
	  function gsl_ran_logistic(r, a) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: a
	    real(c_double) :: gsl_ran_logistic
	  end function gsl_ran_logistic
	  function gsl_ran_logistic_pdf(x, a) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a
	    real(c_double) :: gsl_ran_logistic_pdf
	  end function gsl_ran_logistic_pdf
	  function gsl_cdf_logistic_p(x, a) bind(c, name='gsl_cdf_logistic_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a
	    real(c_double) :: gsl_cdf_logistic_p
	  end function gsl_cdf_logistic_p
	  function gsl_cdf_logistic_q(x, a) bind(c, name='gsl_cdf_logistic_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a
	    real(c_double) :: gsl_cdf_logistic_q
	  end function gsl_cdf_logistic_q
	  function gsl_cdf_logistic_pinv(p, a) bind(c, name='gsl_cdf_logistic_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: a
	    real(c_double) :: gsl_cdf_logistic_pinv
	  end function gsl_cdf_logistic_pinv
	  function gsl_cdf_logistic_qinv(q, a) bind(c, name='gsl_cdf_logistic_Qinv')
	    import
	    real(c_double), value :: q
	    real(c_double), value :: a
	    real(c_double) :: gsl_cdf_logistic_qinv
	  end function gsl_cdf_logistic_qinv
	  function gsl_ran_pareto(r, a, b) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_pareto
	  end function gsl_ran_pareto
	  function gsl_ran_pareto_pdf(x, a, b) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_pareto_pdf
	  end function gsl_ran_pareto_pdf
	  function gsl_cdf_pareto_p(x, a, b) bind(c, name='gsl_cdf_pareto_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_pareto_p
	  end function gsl_cdf_pareto_p
	  function gsl_cdf_pareto_q(x, a, b) bind(c, name='gsl_cdf_pareto_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_pareto_q
	  end function gsl_cdf_pareto_q
	  function gsl_cdf_pareto_pinv(p, a, b) bind(c, name='gsl_cdf_pareto_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_pareto_pinv
	  end function gsl_cdf_pareto_pinv
	  function gsl_cdf_pareto_qinv(q, a, b) bind(c, name='gsl_cdf_pareto_Qinv')
	    import
	    real(c_double), value :: q
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_pareto_qinv
	  end function gsl_cdf_pareto_qinv
	  subroutine gsl_ran_dir_2d(r, x, y) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), intent(out) :: x, y
	  end subroutine gsl_ran_dir_2d
	  subroutine gsl_ran_dir_2d_trig_method(r, x, y) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), intent(out) :: x, y
	  end subroutine gsl_ran_dir_2d_trig_method
	  subroutine gsl_ran_dir_3d(r, x, y, z) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), intent(out) :: x, y, z
	  end subroutine gsl_ran_dir_3d
	  subroutine gsl_ran_dir_nd(r, n, x) bind(c)
	    import
	    type(c_ptr), value :: r
	    integer(c_size_t), value :: n
	    real(c_double), intent(out) :: x
	  end subroutine gsl_ran_dir_nd
	  function gsl_ran_weibull(r, a, b) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_weibull
	  end function gsl_ran_weibull
	  function gsl_ran_weibull_pdf(x, a, b) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_weibull_pdf
	  end function gsl_ran_weibull_pdf
	  function gsl_cdf_weibull_p(x, a, b) bind(c, name='gsl_cdf_weibull_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_weibull_p
	  end function gsl_cdf_weibull_p
	  function gsl_cdf_weibull_q(x, a, b) bind(c, name='gsl_cdf_weibull_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_weibull_q
	  end function gsl_cdf_weibull_q
	  function gsl_cdf_weibull_pinv(p, a, b) bind(c, name='gsl_cdf_weibull_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_weibull_pinv
	  end function gsl_cdf_weibull_pinv
	  function gsl_cdf_weibull_qinv(p, a, b) bind(c, name='gsl_cdf_weibull_Qinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_weibull_qinv
	  end function gsl_cdf_weibull_qinv
	  function gsl_ran_gumbel1(r, a, b) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_gumbel1
	  end function gsl_ran_gumbel1
	  function gsl_ran_gumbel1_pdf(x, a, b) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_gumbel1_pdf
	  end function gsl_ran_gumbel1_pdf
	  function gsl_cdf_gumbel1_p(x, a, b) bind(c, name='gsl_cdf_gumbel1_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_gumbel1_p
	  end function gsl_cdf_gumbel1_p
	  function gsl_cdf_gumbel1_q(x, a, b) bind(c, name='gsl_cdf_gumbel1_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_gumbel1_q
	  end function gsl_cdf_gumbel1_q
	  function gsl_cdf_gumbel1_pinv(p, a, b) bind(c, name='gsl_cdf_gumbel1_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_gumbel1_pinv
	  end function gsl_cdf_gumbel1_pinv
	  function gsl_cdf_gumbel1_qinv(p, a, b) bind(c, name='gsl_cdf_gumbel1_Qinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_gumbel1_qinv
	  end function gsl_cdf_gumbel1_qinv
	  function gsl_ran_gumbel2(r, a, b) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_gumbel2
	  end function gsl_ran_gumbel2
	  function gsl_ran_gumbel2_pdf(x, a, b) bind(c)
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_ran_gumbel2_pdf
	  end function gsl_ran_gumbel2_pdf
	  function gsl_cdf_gumbel2_p(x, a, b) bind(c, name='gsl_cdf_gumbel2_P')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_gumbel2_p
	  end function gsl_cdf_gumbel2_p
	  function gsl_cdf_gumbel2_q(x, a, b) bind(c, name='gsl_cdf_gumbel2_Q')
	    import
	    real(c_double), value :: x
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_gumbel2_q
	  end function gsl_cdf_gumbel2_q
	  function gsl_cdf_gumbel2_pinv(p, a, b) bind(c, name='gsl_cdf_gumbel2_Pinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_gumbel2_pinv
	  end function gsl_cdf_gumbel2_pinv
	  function gsl_cdf_gumbel2_qinv(p, a, b) bind(c, name='gsl_cdf_gumbel2_Qinv')
	    import
	    real(c_double), value :: p
	    real(c_double), value :: a, b
	    real(c_double) :: gsl_cdf_gumbel2_qinv
	  end function gsl_cdf_gumbel2_qinv
	  subroutine gsl_ran_dirichlet(r, k, alpha, theta) bind(c)
	    import
	    type(c_ptr), value :: r, alpha, theta
	    integer(c_size_t), value :: k
	  end subroutine gsl_ran_dirichlet
	  function gsl_ran_dirichlet_pdf(k, alpha, theta) bind(c)
	    import
	    integer(c_size_t), value :: k
	    type(c_ptr), value :: alpha, theta
	    real(c_double) :: gsl_ran_dirichlet_pdf
	  end function gsl_ran_dirichlet_pdf
	  function gsl_ran_dirichlet_lnpdf(k, alpha, theta) bind(c)
	    import
	    integer(c_size_t), value :: k
	    type(c_ptr), value :: alpha, theta
	    real(c_double) :: gsl_ran_dirichlet_lnpdf
	  end function gsl_ran_dirichlet_lnpdf
	  function gsl_ran_discrete_preproc(k, p) bind(c)
	    import
	    integer(c_size_t), value :: k
	    type(c_ptr), value :: p
	    type(c_ptr) :: gsl_ran_discrete_preproc
	  end function gsl_ran_discrete_preproc
	  function gsl_ran_discrete(r, g) bind(c)
	    import
	    type(c_ptr), value :: r, g
	    integer(c_size_t) :: gsl_ran_discrete
	  end function gsl_ran_discrete
	  function gsl_ran_discrete_pdf(k, g) bind(c)
	    import
	    integer(c_size_t), value :: k
	    type(c_ptr), value :: g
	    real(c_double) :: gsl_ran_discrete_pdf
	  end function gsl_ran_discrete_pdf
	  subroutine gsl_ran_discrete_free(g) bind(c)
	    import
	    type(c_ptr), value :: g
	  end subroutine gsl_ran_discrete_free
	  function gsl_ran_poisson(r, mu) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: mu
	    integer(c_int) :: gsl_ran_poisson
	  end function gsl_ran_poisson
	  function gsl_ran_poisson_pdf(k, mu) bind(c)
	    import
	    integer(c_int), value :: k
	    real(c_double), value :: mu
	    real(c_double) :: gsl_ran_poisson_pdf
	  end function gsl_ran_poisson_pdf
	  function gsl_cdf_poisson_p(k, mu) bind(c, name='gsl_cdf_poisson_P')
	    import
	    integer(c_int), value :: k
	    real(c_double), value :: mu
	    real(c_double) :: gsl_cdf_poisson_p
	  end function gsl_cdf_poisson_p
	  function gsl_cdf_poisson_q(k, mu) bind(c, name='gsl_cdf_poisson_Q')
	    import
	    integer(c_int), value :: k
	    real(c_double), value :: mu
	    real(c_double) :: gsl_cdf_poisson_q
	  end function gsl_cdf_poisson_q
	  function gsl_ran_bernoulli(r, p) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: p
	    integer(c_int) :: gsl_ran_bernoulli
	  end function gsl_ran_bernoulli
	  function gsl_ran_bernoulli_pdf(k, p) bind(c)
	    import
	    integer(c_int), value :: k
	    real(c_double), value :: p
	    real(c_double) :: gsl_ran_bernoulli_pdf
	  end function gsl_ran_bernoulli_pdf
	  function gsl_ran_binomial(r, p, n) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: p
	    integer(c_int), value :: n
	    real(c_double) :: gsl_ran_binomial
	  end function gsl_ran_binomial
	  function gsl_ran_binomial_pdf(k, p, n) bind(c)
	    import
	    integer(c_int), value :: k, n
	    real(c_double), value :: p
	    real(c_double) :: gsl_ran_binomial_pdf
	  end function gsl_ran_binomial_pdf
	  function gsl_cdf_binomial_p(k, p, n) bind(c, name='gsl_cdf_binomial_P')
	    import
	    integer(c_int), value :: k, n
	    real(c_double), value :: p
	    real(c_double) :: gsl_cdf_binomial_p
	  end function gsl_cdf_binomial_p
	  function gsl_cdf_binomial_q(k, p, n) bind(c, name='gsl_cdf_binomial_Q')
	    import
	    integer(c_int), value :: k, n
	    real(c_double), value :: p
	    real(c_double) :: gsl_cdf_binomial_q
	  end function gsl_cdf_binomial_q
	  subroutine gsl_ran_multinomial(r, k, nn, p, n) bind(c)
	    import
	    type(c_ptr), value :: r, p, n
	    integer(c_size_t), value :: k
	    integer(c_int), value :: nn
	  end subroutine gsl_ran_multinomial
	  function gsl_ran_multinomial_pdf(k, p, n) bind(c)
	    import
	    integer(c_size_t), value :: k
	    type(c_ptr), value :: p, n
	    real(c_double) :: gsl_ran_multinomial_pdf
	  end function gsl_ran_multinomial_pdf
	  function gsl_ran_multinomial_lnpdf(k, p, n) bind(c)
	    import
	    integer(c_size_t), value :: k
	    type(c_ptr), value :: p, n
	    real(c_double) :: gsl_ran_multinomial_lnpdf
	  end function gsl_ran_multinomial_lnpdf
	  function gsl_ran_negative_binomial(r, p, n) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: p, n
	    integer(c_int) :: gsl_ran_negative_binomial
	  end function gsl_ran_negative_binomial
	  function gsl_ran_negative_binomial_pdf(k, p, n) bind(c)
	    import
	    integer(c_int), value :: k
	    real(c_double), value :: p, n
	    real(c_double) :: gsl_ran_negative_binomial_pdf
	  end function gsl_ran_negative_binomial_pdf
	  function gsl_cdf_negative_binomial_p(k, p, n) bind(c, name='gsl_cdf_negative_binomial_P')
	    import
	    integer(c_int), value :: k
	    real(c_double), value :: p, n
	    real(c_double) :: gsl_cdf_negative_binomial_p
	  end function gsl_cdf_negative_binomial_p
	  function gsl_cdf_negative_binomial_q(k, p, n) bind(c, name='gsl_cdf_negative_binomial_Q')
	    import
	    integer(c_int), value :: k
	    real(c_double), value :: p, n
	    real(c_double) :: gsl_cdf_negative_binomial_q
	  end function gsl_cdf_negative_binomial_q
	  function gsl_ran_pascal(r, p, n) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: p, n
	    integer(c_int) :: gsl_ran_pascal
	  end function gsl_ran_pascal
	  function gsl_ran_pascal_pdf(k, p, n) bind(c)
	    import
	    integer(c_int), value :: k
	    real(c_double), value :: p, n
	    real(c_double) :: gsl_ran_pascal_pdf
	  end function gsl_ran_pascal_pdf
	  function gsl_cdf_pascal_p(k, p, n) bind(c, name='gsl_cdf_pascal_P')
	    import
	    integer(c_int), value :: k
	    real(c_double), value :: p, n
	    real(c_double) :: gsl_cdf_pascal_p
	  end function gsl_cdf_pascal_p
	  function gsl_cdf_pascal_q(k, p, n) bind(c, name='gsl_cdf_pascal_Q')
	    import
	    integer(c_int), value :: k
	    real(c_double), value :: p, n
	    real(c_double) :: gsl_cdf_pascal_q
	  end function gsl_cdf_pascal_q
	  function gsl_ran_geometric(r, p) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: p
	    integer(c_int) :: gsl_ran_geometric
	  end function gsl_ran_geometric
	  function gsl_ran_geometric_pdf(k, p) bind(c)
	    import
	    integer(c_int), value :: k
	    real(c_double), value :: p
	    real(c_double) :: gsl_ran_geometric_pdf
	  end function gsl_ran_geometric_pdf
	  function gsl_cdf_geometric_p(k, p) bind(c, name='gsl_cdf_geometric_P')
	    import
	    integer(c_int), value :: k
	    real(c_double), value :: p
	    real(c_double) :: gsl_cdf_geometric_p
	  end function gsl_cdf_geometric_p
	  function gsl_cdf_geometric_q(k, p) bind(c, name='gsl_cdf_geometric_Q')
	    import
	    integer(c_int), value :: k
	    real(c_double), value :: p
	    real(c_double) :: gsl_cdf_geometric_q
	  end function gsl_cdf_geometric_q
	  function gsl_ran_hypergeometric(r, n1, n2, t) bind(c)
	    import
	    type(c_ptr), value :: r
	    integer(c_int), value :: n1, n2, t
	    integer(c_int) :: gsl_ran_hypergeometric
	  end function gsl_ran_hypergeometric
	  function gsl_ran_hypergeometric_pdf(k, n1, n2, t) bind(c)
	    import
	    integer(c_int), value :: k
	    integer(c_int), value :: n1, n2, t
	    real(c_double) :: gsl_ran_hypergeometric_pdf
	  end function gsl_ran_hypergeometric_pdf
	  function gsl_cdf_hypergeometric_p(k, n1, n2, t) bind(c, name='gsl_cdf_hypergeometric_P')
	    import
	    integer(c_int), value :: k
	    integer(c_int), value :: n1, n2, t
	    real(c_double) :: gsl_cdf_hypergeometric_p
	  end function gsl_cdf_hypergeometric_p
	  function gsl_cdf_hypergeometric_q(k, n1, n2, t) bind(c, name='gsl_cdf_hypergeometric_Q')
	    import
	    integer(c_int), value :: k
	    integer(c_int), value :: n1, n2, t
	    real(c_double) :: gsl_cdf_hypergeometric_q
	  end function gsl_cdf_hypergeometric_q
	  function gsl_ran_logarithmic(r, p) bind(c)
	    import
	    type(c_ptr), value :: r
	    real(c_double), value :: p
	    integer(c_int) :: gsl_ran_logarithmic
	  end function gsl_ran_logarithmic
	  function gsl_ran_logarithmic_pdf(k, p) bind(c)
	    import
	    integer(c_int), value :: k
	    real(c_double), value :: p
	    real(c_double) :: gsl_ran_logarithmic_pdf
	  end function gsl_ran_logarithmic_pdf
	  function gsl_ran_wishart(r, df, l, result, work) bind(c)
	    import :: c_int, c_ptr, c_double
	    type(c_ptr), value :: r, l, result, work
	    real(c_double), value :: df
	    integer(c_int) :: gsl_ran_wishart
	  end function gsl_ran_wishart
	  function gsl_ran_wishart_pdf(x, l_x, df, l, result, work) bind(c)
	    import :: c_int, c_ptr, c_double
	    type(c_ptr), value :: x, l_x, l, work
	    real(c_double), value :: df
	    real(c_double), intent(inout) :: result
	    integer(c_int) :: gsl_ran_wishart_pdf
	  end function gsl_ran_wishart_pdf
	  function gsl_ran_wishart_log_pdf(x, l_x, df, l, result, work) bind(c)
	    import :: c_int, c_ptr, c_double
	    type(c_ptr), value :: x, l_x, l, work
	    real(c_double), value :: df
	    real(c_double), intent(inout) :: result
	    integer(c_int) :: gsl_ran_wishart_log_pdf
	  end function gsl_ran_wishart_log_pdf
	  subroutine gsl_ran_shuffle(r, base, n, size) bind(c)
	    import :: c_ptr, c_size_t
	    type(c_ptr), value :: r, base
	    integer(c_size_t), value :: n, size
	  end subroutine gsl_ran_shuffle
	  function gsl_ran_choose(r, dest, k, src, n, size) bind(c)
	    import :: c_ptr, c_size_t, c_int
	    type(c_ptr), value :: r, dest, src
	    integer(c_size_t), value :: k, n, size
	    integer(c_int) :: gsl_ran_choose
	  end function gsl_ran_choose
	  subroutine gsl_ran_sample(r, dest, k, src, n, size) bind(c)  
	    import :: c_ptr, c_size_t
	    type(c_ptr), value :: r, dest, src
	    integer(c_size_t), value :: k, n, size
	  end subroutine gsl_ran_sample
  end interface
contains
!
! API
  function fgsl_ran_gaussian(r, sigma)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_ran_gaussian
    fgsl_ran_gaussian = gsl_ran_gaussian(r%gsl_rng, sigma)
  end function fgsl_ran_gaussian
  function fgsl_ran_gaussian_pdf(x, sigma)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_ran_gaussian_pdf
    fgsl_ran_gaussian_pdf = gsl_ran_gaussian_pdf(x, sigma)
  end function fgsl_ran_gaussian_pdf
  function fgsl_ran_gaussian_ziggurat(r, sigma)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_ran_gaussian_ziggurat
    fgsl_ran_gaussian_ziggurat = gsl_ran_gaussian_ziggurat(r%gsl_rng, sigma)
  end function fgsl_ran_gaussian_ziggurat
  function fgsl_ran_gaussian_ratio_method(r, sigma)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_ran_gaussian_ratio_method
    fgsl_ran_gaussian_ratio_method = gsl_ran_gaussian_ratio_method(r%gsl_rng, sigma)
  end function fgsl_ran_gaussian_ratio_method
  function fgsl_ran_ugaussian(r)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double) :: fgsl_ran_ugaussian
    fgsl_ran_ugaussian = gsl_ran_ugaussian(r%gsl_rng)
  end function fgsl_ran_ugaussian
  function fgsl_ran_ugaussian_pdf(x)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: fgsl_ran_ugaussian_pdf
    fgsl_ran_ugaussian_pdf = gsl_ran_ugaussian_pdf(x)
  end function fgsl_ran_ugaussian_pdf
  function fgsl_ran_ugaussian_ratio_method(r)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double) :: fgsl_ran_ugaussian_ratio_method
    fgsl_ran_ugaussian_ratio_method = gsl_ran_ugaussian_ratio_method(r%gsl_rng)
  end function fgsl_ran_ugaussian_ratio_method
  function fgsl_cdf_gaussian_p(x, sigma)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_cdf_gaussian_p
    fgsl_cdf_gaussian_p = gsl_cdf_gaussian_p(x, sigma)
  end function fgsl_cdf_gaussian_p
  function fgsl_cdf_gaussian_q(x, sigma)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_cdf_gaussian_q
    fgsl_cdf_gaussian_q = gsl_cdf_gaussian_q(x, sigma)
  end function fgsl_cdf_gaussian_q
  function fgsl_cdf_gaussian_pinv(p, sigma)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_cdf_gaussian_pinv
    fgsl_cdf_gaussian_pinv = gsl_cdf_gaussian_pinv(p, sigma)
  end function fgsl_cdf_gaussian_pinv
  function fgsl_cdf_gaussian_qinv(q, sigma)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_cdf_gaussian_qinv
    fgsl_cdf_gaussian_qinv = gsl_cdf_gaussian_qinv(q, sigma)
  end function fgsl_cdf_gaussian_qinv
  function fgsl_cdf_ugaussian_p(x)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: fgsl_cdf_ugaussian_p
    fgsl_cdf_ugaussian_p = gsl_cdf_ugaussian_p(x)
  end function fgsl_cdf_ugaussian_p
  function fgsl_cdf_ugaussian_q(x)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: fgsl_cdf_ugaussian_q
    fgsl_cdf_ugaussian_q = gsl_cdf_ugaussian_q(x)
  end function fgsl_cdf_ugaussian_q
  function fgsl_cdf_ugaussian_pinv(p)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double) :: fgsl_cdf_ugaussian_pinv
    fgsl_cdf_ugaussian_pinv = gsl_cdf_ugaussian_pinv(p)
  end function fgsl_cdf_ugaussian_pinv
  function fgsl_cdf_ugaussian_qinv(q)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double) :: fgsl_cdf_ugaussian_qinv
    fgsl_cdf_ugaussian_qinv = gsl_cdf_ugaussian_qinv(q)
  end function fgsl_cdf_ugaussian_qinv
  function fgsl_ran_gaussian_tail(r, a, sigma)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: a, sigma
    real(fgsl_double) :: fgsl_ran_gaussian_tail
    fgsl_ran_gaussian_tail = gsl_ran_gaussian_tail(r%gsl_rng, a, sigma)
  end function fgsl_ran_gaussian_tail
  function fgsl_ran_gaussian_tail_pdf(x, a, sigma)
    real(fgsl_double), intent(in) :: x, a
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_ran_gaussian_tail_pdf
    fgsl_ran_gaussian_tail_pdf = gsl_ran_gaussian_tail_pdf(x, a, sigma)
  end function fgsl_ran_gaussian_tail_pdf
  function fgsl_ran_ugaussian_tail(r, a)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_ran_ugaussian_tail
    fgsl_ran_ugaussian_tail = gsl_ran_ugaussian_tail(r%gsl_rng, a)
  end function fgsl_ran_ugaussian_tail
  function fgsl_ran_ugaussian_tail_pdf(x, a)
    real(fgsl_double), intent(in) :: x, a
    real(fgsl_double) :: fgsl_ran_ugaussian_tail_pdf
    fgsl_ran_ugaussian_tail_pdf = gsl_ran_ugaussian_tail_pdf(x, a)
  end function fgsl_ran_ugaussian_tail_pdf
  subroutine fgsl_ran_bivariate_gaussian(r, sigma_x, sigma_y, rho, x, y)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: sigma_x, sigma_y, rho
    real(fgsl_double), intent(out) :: x, y
    call gsl_ran_bivariate_gaussian(r%gsl_rng, sigma_x, sigma_y, rho, x, y)
  end subroutine fgsl_ran_bivariate_gaussian
  function fgsl_ran_bivariate_gaussian_pdf(x, y, sigma_x, sigma_y, rho)
    real(fgsl_double), intent(in) :: x, y, sigma_x, sigma_y, rho
    real(fgsl_double) :: fgsl_ran_bivariate_gaussian_pdf
    fgsl_ran_bivariate_gaussian_pdf = &
         gsl_ran_bivariate_gaussian_pdf(x, y, sigma_x, sigma_y, rho)
  end function fgsl_ran_bivariate_gaussian_pdf
  function fgsl_ran_multivariate_gaussian(r, mu, l, result)
    type(fgsl_rng), intent(in) :: r
    type(fgsl_vector), intent(in) :: mu
    type(fgsl_vector), intent(inout) :: result
    type(fgsl_matrix), intent(in) :: l
    integer(fgsl_int) :: fgsl_ran_multivariate_gaussian
    fgsl_ran_multivariate_gaussian = gsl_ran_multivariate_gaussian(r%gsl_rng, &
         mu%gsl_vector, l%gsl_matrix, result%gsl_vector)
  end function fgsl_ran_multivariate_gaussian
  function fgsl_ran_multivariate_gaussian_pdf(x, mu, l, result, work)
    type(fgsl_vector), intent(in) :: x, mu
    real(fgsl_double), intent(inout) :: result
    type(fgsl_vector), intent(inout) :: work
    type(fgsl_matrix), intent(in) :: l
    integer(fgsl_int) :: fgsl_ran_multivariate_gaussian_pdf
    fgsl_ran_multivariate_gaussian_pdf = gsl_ran_multivariate_gaussian_pdf(x%gsl_vector, &
         mu%gsl_vector, l%gsl_matrix, result, work%gsl_vector)
  end function fgsl_ran_multivariate_gaussian_pdf
  function fgsl_ran_multivariate_gaussian_log_pdf(x, mu, l, result, work)
    type(fgsl_vector), intent(in) :: x, mu
    real(fgsl_double), intent(inout) :: result
    type(fgsl_vector), intent(inout) :: work
    type(fgsl_matrix), intent(in) :: l
    integer(fgsl_int) :: fgsl_ran_multivariate_gaussian_log_pdf
    fgsl_ran_multivariate_gaussian_log_pdf = gsl_ran_multivariate_gaussian_log_pdf( &
         x%gsl_vector, mu%gsl_vector, l%gsl_matrix, result, work%gsl_vector)
  end function fgsl_ran_multivariate_gaussian_log_pdf
  function fgsl_ran_multivariate_gaussian_mean(x, mu_hat)
    type(fgsl_matrix), intent(in) :: x
    type(fgsl_vector), intent(inout) :: mu_hat
    integer(fgsl_int) :: fgsl_ran_multivariate_gaussian_mean
    fgsl_ran_multivariate_gaussian_mean = gsl_ran_multivariate_gaussian_mean( &
         x%gsl_matrix, mu_hat%gsl_vector )
  end function fgsl_ran_multivariate_gaussian_mean
  function fgsl_ran_multivariate_gaussian_vcov(x, sigma_hat)
    type(fgsl_matrix), intent(in) :: x
    type(fgsl_matrix), intent(inout) :: sigma_hat
    integer(fgsl_int) :: fgsl_ran_multivariate_gaussian_vcov
    fgsl_ran_multivariate_gaussian_vcov = gsl_ran_multivariate_gaussian_vcov( &
         x%gsl_matrix, sigma_hat%gsl_matrix )
  end function fgsl_ran_multivariate_gaussian_vcov
  function fgsl_ran_exponential(r, mu)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: mu
    real(fgsl_double) :: fgsl_ran_exponential
    fgsl_ran_exponential = gsl_ran_exponential(r%gsl_rng, mu)
  end function fgsl_ran_exponential
  function fgsl_ran_exponential_pdf(x, mu)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: mu
    real(fgsl_double) :: fgsl_ran_exponential_pdf
    fgsl_ran_exponential_pdf = gsl_ran_exponential_pdf(x, mu)
  end function fgsl_ran_exponential_pdf
  function fgsl_cdf_exponential_p(x, mu)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: mu
    real(fgsl_double) :: fgsl_cdf_exponential_p
    fgsl_cdf_exponential_p = gsl_cdf_exponential_p(x, mu)
  end function fgsl_cdf_exponential_p
  function fgsl_cdf_exponential_q(x, mu)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: mu
    real(fgsl_double) :: fgsl_cdf_exponential_q
    fgsl_cdf_exponential_q = gsl_cdf_exponential_q(x, mu)
  end function fgsl_cdf_exponential_q
  function fgsl_cdf_exponential_pinv(p, mu)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: mu
    real(fgsl_double) :: fgsl_cdf_exponential_pinv
    fgsl_cdf_exponential_pinv = gsl_cdf_exponential_pinv(p, mu)
  end function fgsl_cdf_exponential_pinv
  function fgsl_cdf_exponential_qinv(q, mu)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: mu
    real(fgsl_double) :: fgsl_cdf_exponential_qinv
    fgsl_cdf_exponential_qinv = gsl_cdf_exponential_qinv(q, mu)
  end function fgsl_cdf_exponential_qinv
  function fgsl_ran_laplace(r, a)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_ran_laplace
    fgsl_ran_laplace = gsl_ran_laplace(r%gsl_rng, a)
  end function fgsl_ran_laplace
  function fgsl_ran_laplace_pdf(x, a)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_ran_laplace_pdf
    fgsl_ran_laplace_pdf = gsl_ran_laplace_pdf(x, a)
  end function fgsl_ran_laplace_pdf
  function fgsl_cdf_laplace_p(x, a)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_cdf_laplace_p
    fgsl_cdf_laplace_p = gsl_cdf_laplace_p(x, a)
  end function fgsl_cdf_laplace_p
  function fgsl_cdf_laplace_q(x, a)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_cdf_laplace_q
    fgsl_cdf_laplace_q = gsl_cdf_laplace_q(x, a)
  end function fgsl_cdf_laplace_q
  function fgsl_cdf_laplace_pinv(p, a)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_cdf_laplace_pinv
    fgsl_cdf_laplace_pinv = gsl_cdf_laplace_pinv(p, a)
  end function fgsl_cdf_laplace_pinv
  function fgsl_cdf_laplace_qinv(q, a)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_cdf_laplace_qinv
    fgsl_cdf_laplace_qinv = gsl_cdf_laplace_qinv(q, a)
  end function fgsl_cdf_laplace_qinv
  function fgsl_ran_exppow(r, a, b)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_exppow
    fgsl_ran_exppow = gsl_ran_exppow(r%gsl_rng, a, b)
  end function fgsl_ran_exppow
  function fgsl_ran_exppow_pdf(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_exppow_pdf
    fgsl_ran_exppow_pdf = gsl_ran_exppow_pdf(x, a, b)
  end function fgsl_ran_exppow_pdf
  function fgsl_cdf_exppow_p(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_exppow_p
    fgsl_cdf_exppow_p = gsl_cdf_exppow_p(x, a, b)
  end function fgsl_cdf_exppow_p
  function fgsl_cdf_exppow_q(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_exppow_q
    fgsl_cdf_exppow_q = gsl_cdf_exppow_q(x, a, b)
  end function fgsl_cdf_exppow_q
  function fgsl_ran_cauchy(r, a)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_ran_cauchy
    fgsl_ran_cauchy = gsl_ran_cauchy(r%gsl_rng, a)
  end function fgsl_ran_cauchy
  function fgsl_ran_cauchy_pdf(x, a)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_ran_cauchy_pdf
    fgsl_ran_cauchy_pdf = gsl_ran_cauchy_pdf(x, a)
  end function fgsl_ran_cauchy_pdf
  function fgsl_cdf_cauchy_p(x, a)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_cdf_cauchy_p
    fgsl_cdf_cauchy_p = gsl_cdf_cauchy_p(x, a)
  end function fgsl_cdf_cauchy_p
  function fgsl_cdf_cauchy_q(x, a)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_cdf_cauchy_q
    fgsl_cdf_cauchy_q = gsl_cdf_cauchy_q(x, a)
  end function fgsl_cdf_cauchy_q
  function fgsl_cdf_cauchy_pinv(p, a)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_cdf_cauchy_pinv
    fgsl_cdf_cauchy_pinv = gsl_cdf_cauchy_pinv(p, a)
  end function fgsl_cdf_cauchy_pinv
  function fgsl_cdf_cauchy_qinv(q, a)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_cdf_cauchy_qinv
    fgsl_cdf_cauchy_qinv = gsl_cdf_cauchy_qinv(q, a)
  end function fgsl_cdf_cauchy_qinv
  function fgsl_ran_rayleigh(r, sigma)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_ran_rayleigh
    fgsl_ran_rayleigh = gsl_ran_rayleigh(r%gsl_rng, sigma)
  end function fgsl_ran_rayleigh
  function fgsl_ran_rayleigh_pdf(x, sigma)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_ran_rayleigh_pdf
    fgsl_ran_rayleigh_pdf = gsl_ran_rayleigh_pdf(x, sigma)
  end function fgsl_ran_rayleigh_pdf
  function fgsl_cdf_rayleigh_p(x, sigma)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_cdf_rayleigh_p
    fgsl_cdf_rayleigh_p = gsl_cdf_rayleigh_p(x, sigma)
  end function fgsl_cdf_rayleigh_p
  function fgsl_cdf_rayleigh_q(x, sigma)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_cdf_rayleigh_q
    fgsl_cdf_rayleigh_q = gsl_cdf_rayleigh_q(x, sigma)
  end function fgsl_cdf_rayleigh_q
  function fgsl_cdf_rayleigh_pinv(p, sigma)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_cdf_rayleigh_pinv
    fgsl_cdf_rayleigh_pinv = gsl_cdf_rayleigh_pinv(p, sigma)
  end function fgsl_cdf_rayleigh_pinv
  function fgsl_cdf_rayleigh_qinv(q, sigma)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_cdf_rayleigh_qinv
    fgsl_cdf_rayleigh_qinv = gsl_cdf_rayleigh_qinv(q, sigma)
  end function fgsl_cdf_rayleigh_qinv
  function fgsl_ran_rayleigh_tail(r, a, sigma)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: a, sigma
    real(fgsl_double) :: fgsl_ran_rayleigh_tail
    fgsl_ran_rayleigh_tail = gsl_ran_rayleigh_tail(r%gsl_rng, a, sigma)
  end function fgsl_ran_rayleigh_tail
  function fgsl_ran_rayleigh_tail_pdf(x, a, sigma)
    real(fgsl_double), intent(in) :: x, a
    real(fgsl_double), intent(in) :: sigma
    real(fgsl_double) :: fgsl_ran_rayleigh_tail_pdf
    fgsl_ran_rayleigh_tail_pdf = gsl_ran_rayleigh_tail_pdf(x, a, sigma)
  end function fgsl_ran_rayleigh_tail_pdf
  function fgsl_ran_landau(r)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double) :: fgsl_ran_landau
    fgsl_ran_landau = gsl_ran_landau(r%gsl_rng)
  end function fgsl_ran_landau
  function fgsl_ran_landau_pdf(x)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double) :: fgsl_ran_landau_pdf
    fgsl_ran_landau_pdf = gsl_ran_landau_pdf(x)
  end function fgsl_ran_landau_pdf
  function fgsl_ran_levy(r, c, alpha)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: c, alpha
    real(fgsl_double) :: fgsl_ran_levy
    fgsl_ran_levy = gsl_ran_levy(r%gsl_rng, c, alpha)
  end function fgsl_ran_levy
  function fgsl_ran_levy_skew(r, c, alpha, beta)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: c, alpha, beta
    real(fgsl_double) :: fgsl_ran_levy_skew
    fgsl_ran_levy_skew = gsl_ran_levy_skew(r%gsl_rng, c, alpha, beta)
  end function fgsl_ran_levy_skew
  function fgsl_ran_gamma(r, a, b)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_gamma
    fgsl_ran_gamma = gsl_ran_gamma(r%gsl_rng, a, b)
  end function fgsl_ran_gamma
  function fgsl_ran_gamma_mt(r, a, b)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_gamma_mt
    fgsl_ran_gamma_mt = gsl_ran_gamma_mt(r%gsl_rng, a, b)
  end function fgsl_ran_gamma_mt
  function fgsl_ran_gamma_pdf(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_gamma_pdf
    fgsl_ran_gamma_pdf = gsl_ran_gamma_pdf(x, a, b)
  end function fgsl_ran_gamma_pdf
  function fgsl_cdf_gamma_p(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_gamma_p
    fgsl_cdf_gamma_p = gsl_cdf_gamma_p(x, a, b)
  end function fgsl_cdf_gamma_p
  function fgsl_cdf_gamma_q(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_gamma_q
    fgsl_cdf_gamma_q = gsl_cdf_gamma_q(x, a, b)
  end function fgsl_cdf_gamma_q
  function fgsl_cdf_gamma_pinv(p, a, b)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_gamma_pinv
    fgsl_cdf_gamma_pinv = gsl_cdf_gamma_pinv(p, a, b)
  end function fgsl_cdf_gamma_pinv
  function fgsl_cdf_gamma_qinv(q, a, b)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_gamma_qinv
    fgsl_cdf_gamma_qinv = gsl_cdf_gamma_qinv(q, a, b)
  end function fgsl_cdf_gamma_qinv
  function fgsl_ran_flat(r, a, b)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_flat
    fgsl_ran_flat = gsl_ran_flat(r%gsl_rng, a, b)
  end function fgsl_ran_flat
  function fgsl_ran_flat_pdf(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_flat_pdf
    fgsl_ran_flat_pdf = gsl_ran_flat_pdf(x, a, b)
  end function fgsl_ran_flat_pdf
  function fgsl_cdf_flat_p(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_flat_p
    fgsl_cdf_flat_p = gsl_cdf_flat_p(x, a, b)
  end function fgsl_cdf_flat_p
  function fgsl_cdf_flat_q(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_flat_q
    fgsl_cdf_flat_q = gsl_cdf_flat_q(x, a, b)
  end function fgsl_cdf_flat_q
  function fgsl_cdf_flat_pinv(p, a, b)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_flat_pinv
    fgsl_cdf_flat_pinv = gsl_cdf_flat_pinv(p, a, b)
  end function fgsl_cdf_flat_pinv
  function fgsl_cdf_flat_qinv(q, a, b)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_flat_qinv
    fgsl_cdf_flat_qinv = gsl_cdf_flat_qinv(q, a, b)
  end function fgsl_cdf_flat_qinv
  function fgsl_ran_lognormal(r, zeta, sigma)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: zeta, sigma
    real(fgsl_double) :: fgsl_ran_lognormal
    fgsl_ran_lognormal = gsl_ran_lognormal(r%gsl_rng, zeta, sigma)
  end function fgsl_ran_lognormal
  function fgsl_ran_lognormal_pdf(x, zeta, sigma)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: zeta, sigma
    real(fgsl_double) :: fgsl_ran_lognormal_pdf
    fgsl_ran_lognormal_pdf = gsl_ran_lognormal_pdf(x, zeta, sigma)
  end function fgsl_ran_lognormal_pdf
  function fgsl_cdf_lognormal_p(x, zeta, sigma)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: zeta, sigma
    real(fgsl_double) :: fgsl_cdf_lognormal_p
    fgsl_cdf_lognormal_p = gsl_cdf_lognormal_p(x, zeta, sigma)
  end function fgsl_cdf_lognormal_p
  function fgsl_cdf_lognormal_q(x, zeta, sigma)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: zeta, sigma
    real(fgsl_double) :: fgsl_cdf_lognormal_q
    fgsl_cdf_lognormal_q = gsl_cdf_lognormal_q(x, zeta, sigma)
  end function fgsl_cdf_lognormal_q
  function fgsl_cdf_lognormal_pinv(p, zeta, sigma)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: zeta, sigma
    real(fgsl_double) :: fgsl_cdf_lognormal_pinv
    fgsl_cdf_lognormal_pinv = gsl_cdf_lognormal_pinv(p, zeta, sigma)
  end function fgsl_cdf_lognormal_pinv
  function fgsl_cdf_lognormal_qinv(q, zeta, sigma)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: zeta, sigma
    real(fgsl_double) :: fgsl_cdf_lognormal_qinv
    fgsl_cdf_lognormal_qinv = gsl_cdf_lognormal_qinv(q, zeta, sigma)
  end function fgsl_cdf_lognormal_qinv
  function fgsl_ran_chisq(r, nu)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: nu
    real(fgsl_double) :: fgsl_ran_chisq
    fgsl_ran_chisq = gsl_ran_chisq(r%gsl_rng, nu)
  end function fgsl_ran_chisq
  function fgsl_ran_chisq_pdf(x, nu)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: nu
    real(fgsl_double) :: fgsl_ran_chisq_pdf
    fgsl_ran_chisq_pdf = gsl_ran_chisq_pdf(x, nu)
  end function fgsl_ran_chisq_pdf
  function fgsl_cdf_chisq_p(x, nu)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: nu
    real(fgsl_double) :: fgsl_cdf_chisq_p
    fgsl_cdf_chisq_p = gsl_cdf_chisq_p(x, nu)
  end function fgsl_cdf_chisq_p
  function fgsl_cdf_chisq_q(x, nu)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: nu
    real(fgsl_double) :: fgsl_cdf_chisq_q
    fgsl_cdf_chisq_q = gsl_cdf_chisq_q(x, nu)
  end function fgsl_cdf_chisq_q
  function fgsl_cdf_chisq_pinv(p, nu)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: nu
    real(fgsl_double) :: fgsl_cdf_chisq_pinv
    fgsl_cdf_chisq_pinv = gsl_cdf_chisq_pinv(p, nu)
  end function fgsl_cdf_chisq_pinv
  function fgsl_cdf_chisq_qinv(q, nu)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: nu
    real(fgsl_double) :: fgsl_cdf_chisq_qinv
    fgsl_cdf_chisq_qinv = gsl_cdf_chisq_qinv(q, nu)
  end function fgsl_cdf_chisq_qinv
  function fgsl_ran_fdist(r, nu1, nu2)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: nu1, nu2
    real(fgsl_double) :: fgsl_ran_fdist
    fgsl_ran_fdist = gsl_ran_fdist(r%gsl_rng, nu1, nu2)
  end function fgsl_ran_fdist
  function fgsl_ran_fdist_pdf(x, nu1, nu2)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: nu1, nu2
    real(fgsl_double) :: fgsl_ran_fdist_pdf
    fgsl_ran_fdist_pdf = gsl_ran_fdist_pdf(x, nu1, nu2)
  end function fgsl_ran_fdist_pdf
  function fgsl_cdf_fdist_p(x, nu1, nu2)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: nu1, nu2
    real(fgsl_double) :: fgsl_cdf_fdist_p
    fgsl_cdf_fdist_p = gsl_cdf_fdist_p(x, nu1, nu2)
  end function fgsl_cdf_fdist_p
  function fgsl_cdf_fdist_q(x, nu1, nu2)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: nu1, nu2
    real(fgsl_double) :: fgsl_cdf_fdist_q
    fgsl_cdf_fdist_q = gsl_cdf_fdist_q(x, nu1, nu2)
  end function fgsl_cdf_fdist_q
  function fgsl_cdf_fdist_pinv(p, nu1, nu2)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: nu1, nu2
    real(fgsl_double) :: fgsl_cdf_fdist_pinv
    fgsl_cdf_fdist_pinv = gsl_cdf_fdist_pinv(p, nu1, nu2)
  end function fgsl_cdf_fdist_pinv
  function fgsl_cdf_fdist_qinv(q, nu1, nu2)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: nu1, nu2
    real(fgsl_double) :: fgsl_cdf_fdist_qinv
    fgsl_cdf_fdist_qinv = gsl_cdf_fdist_qinv(q, nu1, nu2)
  end function fgsl_cdf_fdist_qinv
  function fgsl_ran_tdist(r, nu)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: nu
    real(fgsl_double) :: fgsl_ran_tdist
    fgsl_ran_tdist = gsl_ran_tdist(r%gsl_rng, nu)
  end function fgsl_ran_tdist
  function fgsl_ran_tdist_pdf(x, nu)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: nu
    real(fgsl_double) :: fgsl_ran_tdist_pdf
    fgsl_ran_tdist_pdf = gsl_ran_tdist_pdf(x, nu)
  end function fgsl_ran_tdist_pdf
  function fgsl_cdf_tdist_p(x, nu)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: nu
    real(fgsl_double) :: fgsl_cdf_tdist_p
    fgsl_cdf_tdist_p = gsl_cdf_tdist_p(x, nu)
  end function fgsl_cdf_tdist_p
  function fgsl_cdf_tdist_q(x, nu)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: nu
    real(fgsl_double) :: fgsl_cdf_tdist_q
    fgsl_cdf_tdist_q = gsl_cdf_tdist_q(x, nu)
  end function fgsl_cdf_tdist_q
  function fgsl_cdf_tdist_pinv(p, nu)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: nu
    real(fgsl_double) :: fgsl_cdf_tdist_pinv
    fgsl_cdf_tdist_pinv = gsl_cdf_tdist_pinv(p, nu)
  end function fgsl_cdf_tdist_pinv
  function fgsl_cdf_tdist_qinv(q, nu)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: nu
    real(fgsl_double) :: fgsl_cdf_tdist_qinv
    fgsl_cdf_tdist_qinv = gsl_cdf_tdist_qinv(q, nu)
  end function fgsl_cdf_tdist_qinv
  function fgsl_ran_beta(r, a, b)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_beta
    fgsl_ran_beta = gsl_ran_beta(r%gsl_rng, a, b)
  end function fgsl_ran_beta
  function fgsl_ran_beta_pdf(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_beta_pdf
    fgsl_ran_beta_pdf = gsl_ran_beta_pdf(x, a, b)
  end function fgsl_ran_beta_pdf
  function fgsl_cdf_beta_p(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_beta_p
    fgsl_cdf_beta_p = gsl_cdf_beta_p(x, a, b)
  end function fgsl_cdf_beta_p
  function fgsl_cdf_beta_q(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_beta_q
    fgsl_cdf_beta_q = gsl_cdf_beta_q(x, a, b)
  end function fgsl_cdf_beta_q
  function fgsl_cdf_beta_pinv(p, a, b)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_beta_pinv
    fgsl_cdf_beta_pinv = gsl_cdf_beta_pinv(p, a, b)
  end function fgsl_cdf_beta_pinv
  function fgsl_cdf_beta_qinv(q, a, b)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_beta_qinv
    fgsl_cdf_beta_qinv = gsl_cdf_beta_qinv(q, a, b)
  end function fgsl_cdf_beta_qinv
  function fgsl_ran_logistic(r, a)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_ran_logistic
    fgsl_ran_logistic = gsl_ran_logistic(r%gsl_rng, a)
  end function fgsl_ran_logistic
  function fgsl_ran_logistic_pdf(x, a)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_ran_logistic_pdf
    fgsl_ran_logistic_pdf = gsl_ran_logistic_pdf(x, a)
  end function fgsl_ran_logistic_pdf
  function fgsl_cdf_logistic_p(x, a)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_cdf_logistic_p
    fgsl_cdf_logistic_p = gsl_cdf_logistic_p(x, a)
  end function fgsl_cdf_logistic_p
  function fgsl_cdf_logistic_q(x, a)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_cdf_logistic_q
    fgsl_cdf_logistic_q = gsl_cdf_logistic_q(x, a)
  end function fgsl_cdf_logistic_q
  function fgsl_cdf_logistic_pinv(p, a)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_cdf_logistic_pinv
    fgsl_cdf_logistic_pinv = gsl_cdf_logistic_pinv(p, a)
  end function fgsl_cdf_logistic_pinv
  function fgsl_cdf_logistic_qinv(q, a)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: a
    real(fgsl_double) :: fgsl_cdf_logistic_qinv
    fgsl_cdf_logistic_qinv = gsl_cdf_logistic_qinv(q, a)
  end function fgsl_cdf_logistic_qinv
  function fgsl_ran_pareto(r, a, b)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_pareto
    fgsl_ran_pareto = gsl_ran_pareto(r%gsl_rng, a, b)
  end function fgsl_ran_pareto
  function fgsl_ran_pareto_pdf(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_pareto_pdf
    fgsl_ran_pareto_pdf = gsl_ran_pareto_pdf(x, a, b)
  end function fgsl_ran_pareto_pdf
  function fgsl_cdf_pareto_p(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_pareto_p
    fgsl_cdf_pareto_p = gsl_cdf_pareto_p(x, a, b)
  end function fgsl_cdf_pareto_p
  function fgsl_cdf_pareto_q(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_pareto_q
    fgsl_cdf_pareto_q = gsl_cdf_pareto_q(x, a, b)
  end function fgsl_cdf_pareto_q
  function fgsl_cdf_pareto_pinv(p, a, b)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_pareto_pinv
    fgsl_cdf_pareto_pinv = gsl_cdf_pareto_pinv(p, a, b)
  end function fgsl_cdf_pareto_pinv
  function fgsl_cdf_pareto_qinv(q, a, b)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_pareto_qinv
    fgsl_cdf_pareto_qinv = gsl_cdf_pareto_qinv(q, a, b)
  end function fgsl_cdf_pareto_qinv
  subroutine fgsl_ran_dir_2d(r, x, y)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(out) :: x, y
    call gsl_ran_dir_2d(r%gsl_rng, x, y)
  end subroutine fgsl_ran_dir_2d
  subroutine fgsl_ran_dir_2d_trig_method(r, x, y)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(out) :: x, y
    call gsl_ran_dir_2d_trig_method(r%gsl_rng, x, y)
  end subroutine fgsl_ran_dir_2d_trig_method
  subroutine fgsl_ran_dir_3d(r, x, y, z)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(out) :: x, y, z
    call gsl_ran_dir_3d(r%gsl_rng, x, y, z)
  end subroutine fgsl_ran_dir_3d
  subroutine fgsl_ran_dir_nd(r, n, x)
    type(fgsl_rng), intent(in) :: r
    integer(fgsl_size_t), intent(in) :: n
    real(fgsl_double), intent(out) :: x
    call gsl_ran_dir_nd(r%gsl_rng, n, x)
  end subroutine fgsl_ran_dir_nd
  function fgsl_ran_weibull(r, a, b)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_weibull
    fgsl_ran_weibull = gsl_ran_weibull(r%gsl_rng, a, b)
  end function fgsl_ran_weibull
  function fgsl_ran_weibull_pdf(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_weibull_pdf
    fgsl_ran_weibull_pdf = gsl_ran_weibull_pdf(x, a, b)
  end function fgsl_ran_weibull_pdf
  function fgsl_cdf_weibull_p(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_weibull_p
    fgsl_cdf_weibull_p = gsl_cdf_weibull_p(x, a, b)
  end function fgsl_cdf_weibull_p
  function fgsl_cdf_weibull_q(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_weibull_q
    fgsl_cdf_weibull_q = gsl_cdf_weibull_q(x, a, b)
  end function fgsl_cdf_weibull_q
  function fgsl_cdf_weibull_pinv(p, a, b)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_weibull_pinv
    fgsl_cdf_weibull_pinv = gsl_cdf_weibull_pinv(p, a, b)
  end function fgsl_cdf_weibull_pinv
  function fgsl_cdf_weibull_qinv(q, a, b)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_weibull_qinv
    fgsl_cdf_weibull_qinv = gsl_cdf_weibull_qinv(q, a, b)
  end function fgsl_cdf_weibull_qinv
  function fgsl_ran_gumbel1(r, a, b)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_gumbel1
    fgsl_ran_gumbel1 = gsl_ran_gumbel1(r%gsl_rng, a, b)
  end function fgsl_ran_gumbel1
  function fgsl_ran_gumbel1_pdf(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_gumbel1_pdf
    fgsl_ran_gumbel1_pdf = gsl_ran_gumbel1_pdf(x, a, b)
  end function fgsl_ran_gumbel1_pdf
  function fgsl_cdf_gumbel1_p(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_gumbel1_p
    fgsl_cdf_gumbel1_p = gsl_cdf_gumbel1_p(x, a, b)
  end function fgsl_cdf_gumbel1_p
  function fgsl_cdf_gumbel1_q(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_gumbel1_q
    fgsl_cdf_gumbel1_q = gsl_cdf_gumbel1_q(x, a, b)
  end function fgsl_cdf_gumbel1_q
  function fgsl_cdf_gumbel1_pinv(p, a, b)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_gumbel1_pinv
    fgsl_cdf_gumbel1_pinv = gsl_cdf_gumbel1_pinv(p, a, b)
  end function fgsl_cdf_gumbel1_pinv
  function fgsl_cdf_gumbel1_qinv(q, a, b)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_gumbel1_qinv
    fgsl_cdf_gumbel1_qinv = gsl_cdf_gumbel1_qinv(q, a, b)
  end function fgsl_cdf_gumbel1_qinv
  function fgsl_ran_gumbel2(r, a, b)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_gumbel2
    fgsl_ran_gumbel2 = gsl_ran_gumbel2(r%gsl_rng, a, b)
  end function fgsl_ran_gumbel2
  function fgsl_ran_gumbel2_pdf(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_ran_gumbel2_pdf
    fgsl_ran_gumbel2_pdf = gsl_ran_gumbel2_pdf(x, a, b)
  end function fgsl_ran_gumbel2_pdf
  function fgsl_cdf_gumbel2_p(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_gumbel2_p
    fgsl_cdf_gumbel2_p = gsl_cdf_gumbel2_p(x, a, b)
  end function fgsl_cdf_gumbel2_p
  function fgsl_cdf_gumbel2_q(x, a, b)
    real(fgsl_double), intent(in) :: x
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_gumbel2_q
    fgsl_cdf_gumbel2_q = gsl_cdf_gumbel2_q(x, a, b)
  end function fgsl_cdf_gumbel2_q
  function fgsl_cdf_gumbel2_pinv(p, a, b)
    real(fgsl_double), intent(in) :: p
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_gumbel2_pinv
    fgsl_cdf_gumbel2_pinv = gsl_cdf_gumbel2_pinv(p, a, b)
  end function fgsl_cdf_gumbel2_pinv
  function fgsl_cdf_gumbel2_qinv(q, a, b)
    real(fgsl_double), intent(in) :: q
    real(fgsl_double), intent(in) :: a, b
    real(fgsl_double) :: fgsl_cdf_gumbel2_qinv
    fgsl_cdf_gumbel2_qinv = gsl_cdf_gumbel2_qinv(q, a, b)
  end function fgsl_cdf_gumbel2_qinv
  subroutine fgsl_ran_dirichlet(r, alpha, theta)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in), target, contiguous :: alpha(:)
    real(fgsl_double), intent(out), target, contiguous :: theta(:)
    !check dims
    if (size(theta) /= size(alpha)) then
      call fgsl_error('alpha and beta dimensions do not match', 'fgsl_rng', __LINE__, fgsl_ebadlen)
      return
    endif
    call gsl_ran_dirichlet(r%gsl_rng, size(alpha, kind=fgsl_size_t), &
    c_loc(alpha), c_loc(theta))
  end subroutine fgsl_ran_dirichlet
  function fgsl_ran_dirichlet_pdf(alpha, theta)
    real(fgsl_double), intent(in), target, contiguous :: alpha(:)
    real(fgsl_double), intent(in), target, contiguous :: theta(:)
    real(fgsl_double) :: fgsl_ran_dirichlet_pdf
    !check dims
    if (size(theta) /= size(alpha)) then
      call fgsl_error('alpha and beta dimensions do not match', 'fgsl_rng', __LINE__, fgsl_ebadlen)
      fgsl_ran_dirichlet_pdf = 0.0_fgsl_double
      return
    endif
    fgsl_ran_dirichlet_pdf = gsl_ran_dirichlet_pdf(size(alpha, kind=fgsl_size_t), &
    c_loc(alpha), c_loc(theta))
  end function fgsl_ran_dirichlet_pdf
  function fgsl_ran_dirichlet_lnpdf(alpha, theta)
    real(fgsl_double), intent(in), target, contiguous :: alpha(:)
    real(fgsl_double), intent(in), target, contiguous :: theta(:)
    real(fgsl_double) :: fgsl_ran_dirichlet_lnpdf
    !check dims
    if (size(theta) /= size(alpha)) then
      call fgsl_error('alpha and beta dimensions do not match', 'fgsl_rng', __LINE__, fgsl_ebadlen)
      fgsl_ran_dirichlet_lnpdf = 0.0_fgsl_double
      return
    endif
    fgsl_ran_dirichlet_lnpdf = gsl_ran_dirichlet_lnpdf(size(alpha, kind=fgsl_size_t), &
    c_loc(alpha), c_loc(theta))
  end function fgsl_ran_dirichlet_lnpdf
  function fgsl_ran_discrete_preproc(p)
    real(fgsl_double), intent(in), target, contiguous :: p(:)
    type(fgsl_ran_discrete_t) :: fgsl_ran_discrete_preproc
    fgsl_ran_discrete_preproc%gsl_ran_discrete_t = &
         gsl_ran_discrete_preproc(size(p, kind=fgsl_size_t), c_loc(p))
  end function fgsl_ran_discrete_preproc
  function fgsl_ran_discrete(r, g)
    type(fgsl_rng), intent(in) :: r
    type(fgsl_ran_discrete_t), intent(in) :: g
    integer(fgsl_size_t) :: fgsl_ran_discrete
    fgsl_ran_discrete = gsl_ran_discrete(r%gsl_rng, g%gsl_ran_discrete_t)
  end function fgsl_ran_discrete
  function fgsl_ran_discrete_pdf(k, g)
    integer(fgsl_size_t), intent(in) :: k
    type(fgsl_ran_discrete_t), intent(in) :: g
    real(fgsl_double) :: fgsl_ran_discrete_pdf
    fgsl_ran_discrete_pdf = gsl_ran_discrete_pdf(k, g%gsl_ran_discrete_t)
  end function fgsl_ran_discrete_pdf
  subroutine fgsl_ran_discrete_free(g)
    type(fgsl_ran_discrete_t), intent(inout) :: g
    call gsl_ran_discrete_free(g%gsl_ran_discrete_t)
  end subroutine fgsl_ran_discrete_free
  function fgsl_ran_poisson(r, mu)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: mu
    integer(fgsl_int) :: fgsl_ran_poisson
    fgsl_ran_poisson = gsl_ran_poisson(r%gsl_rng, mu)
  end function fgsl_ran_poisson
  function fgsl_ran_poisson_pdf(k, mu)
    integer(fgsl_int), intent(in) :: k
    real(fgsl_double), intent(in) :: mu
    real(fgsl_double) :: fgsl_ran_poisson_pdf
    fgsl_ran_poisson_pdf = gsl_ran_poisson_pdf(k, mu)
  end function fgsl_ran_poisson_pdf
  function fgsl_cdf_poisson_p(k, mu)
    integer(fgsl_int), intent(in) :: k
    real(fgsl_double), intent(in) :: mu
    real(fgsl_double) :: fgsl_cdf_poisson_p
    fgsl_cdf_poisson_p = gsl_cdf_poisson_p(k, mu)
  end function fgsl_cdf_poisson_p
  function fgsl_cdf_poisson_q(k, mu)
    integer(fgsl_int), intent(in) :: k
    real(fgsl_double), intent(in) :: mu
    real(fgsl_double) :: fgsl_cdf_poisson_q
    fgsl_cdf_poisson_q = gsl_cdf_poisson_q(k, mu)
  end function fgsl_cdf_poisson_q
  function fgsl_ran_bernoulli(r, p)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: p
    integer(fgsl_int) :: fgsl_ran_bernoulli
    fgsl_ran_bernoulli = gsl_ran_bernoulli(r%gsl_rng, p)
  end function fgsl_ran_bernoulli
  function fgsl_ran_bernoulli_pdf(k, p)
    integer(fgsl_int), intent(in) :: k
    real(fgsl_double), intent(in) :: p
    real(fgsl_double) :: fgsl_ran_bernoulli_pdf
    fgsl_ran_bernoulli_pdf = gsl_ran_bernoulli_pdf(k, p)
  end function fgsl_ran_bernoulli_pdf
  function fgsl_ran_binomial(r, p, n)
    type(fgsl_rng), intent(in) :: r
    integer(fgsl_int), intent(in) :: n
    real(fgsl_double), intent(in) :: p
    real(fgsl_double) :: fgsl_ran_binomial
    fgsl_ran_binomial = gsl_ran_binomial(r%gsl_rng, p, n)
  end function fgsl_ran_binomial
  function fgsl_ran_binomial_pdf(k, p, n)
    integer(fgsl_int), intent(in) :: k, n
    real(fgsl_double), intent(in) :: p
    real(fgsl_double) :: fgsl_ran_binomial_pdf
    fgsl_ran_binomial_pdf = gsl_ran_binomial_pdf(k, p, n)
  end function fgsl_ran_binomial_pdf
  function fgsl_cdf_binomial_p(k, p, n)
    integer(fgsl_int), intent(in) :: k, n
    real(fgsl_double), intent(in) :: p
    real(fgsl_double) :: fgsl_cdf_binomial_p
    fgsl_cdf_binomial_p = gsl_cdf_binomial_p(k, p, n)
  end function fgsl_cdf_binomial_p
  function fgsl_cdf_binomial_q(k, p, n)
    integer(fgsl_int), intent(in) :: k, n
    real(fgsl_double), intent(in) :: p
    real(fgsl_double) :: fgsl_cdf_binomial_q
    fgsl_cdf_binomial_q = gsl_cdf_binomial_q(k, p, n)
  end function fgsl_cdf_binomial_q
  subroutine fgsl_ran_multinomial(r, nn, p, n)
    type(fgsl_rng), intent(in) :: r
    integer(fgsl_int), intent(in) :: nn
    real(fgsl_double), intent(in), target, contiguous :: p(:)
    integer(fgsl_int), intent(out), target, contiguous :: n(:)
    !check dims
    if (size(p) /= size(n)) then
      call fgsl_error('p and n dimensions do not match', 'fgsl_rng', __LINE__, fgsl_ebadlen)
      return
    endif
    call gsl_ran_multinomial(r%gsl_rng, size(p, kind=fgsl_size_t), nn, c_loc(p), c_loc(n))
  end subroutine fgsl_ran_multinomial
  function fgsl_ran_multinomial_pdf(p, n)
    real(fgsl_double), intent(in), target, contiguous :: p(:)
    integer(fgsl_int), intent(in), target, contiguous :: n(:)
    real(fgsl_double) :: fgsl_ran_multinomial_pdf
    !check dims
    if (size(p) /= size(n)) then
      call fgsl_error('p and n dimensions do not match', 'fgsl_rng', __LINE__, fgsl_ebadlen)
      fgsl_ran_multinomial_pdf = 0.0_fgsl_double
      return
    endif
    fgsl_ran_multinomial_pdf = gsl_ran_multinomial_pdf(size(p, kind=fgsl_size_t), &
    c_loc(p), c_loc(n))
  end function fgsl_ran_multinomial_pdf
  function fgsl_ran_multinomial_lnpdf(p, n)
    real(fgsl_double), intent(in), target, contiguous :: p(:)
    integer(fgsl_int), intent(in), target, contiguous :: n(:)
    real(fgsl_double) :: fgsl_ran_multinomial_lnpdf
    !check dims
    if (size(p) /= size(n)) then
      call fgsl_error('p and n dimensions do not match', 'fgsl_rng', __LINE__, fgsl_ebadlen)
      fgsl_ran_multinomial_lnpdf = 0.0_fgsl_double
      return
    endif
    fgsl_ran_multinomial_lnpdf = gsl_ran_multinomial_lnpdf(&
    size(p, kind=fgsl_size_t), c_loc(p), c_loc(n))
  end function fgsl_ran_multinomial_lnpdf
  function fgsl_ran_negative_binomial(r, p, n)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: p, n
    integer(fgsl_int) :: fgsl_ran_negative_binomial
    fgsl_ran_negative_binomial = gsl_ran_negative_binomial(r%gsl_rng, p, n)
  end function fgsl_ran_negative_binomial
  function fgsl_ran_negative_binomial_pdf(k, p, n)
    integer(fgsl_int), intent(in) :: k
    real(fgsl_double), intent(in) :: p, n
    real(fgsl_double) :: fgsl_ran_negative_binomial_pdf
    fgsl_ran_negative_binomial_pdf = gsl_ran_negative_binomial_pdf(k, p, n)
  end function fgsl_ran_negative_binomial_pdf
  function fgsl_cdf_negative_binomial_p(k, p, n)
    integer(fgsl_int), intent(in) :: k
    real(fgsl_double), intent(in) :: p, n
    real(fgsl_double) :: fgsl_cdf_negative_binomial_p
    fgsl_cdf_negative_binomial_p = gsl_cdf_negative_binomial_p(k, p, n)
  end function fgsl_cdf_negative_binomial_p
  function fgsl_cdf_negative_binomial_q(k, p, n)
    integer(fgsl_int), intent(in) :: k
    real(fgsl_double), intent(in) :: p, n
    real(fgsl_double) :: fgsl_cdf_negative_binomial_q
    fgsl_cdf_negative_binomial_q = gsl_cdf_negative_binomial_q(k, p, n)
  end function fgsl_cdf_negative_binomial_q
  function fgsl_ran_pascal(r, p, n)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: p, n
    integer(fgsl_int) :: fgsl_ran_pascal
    fgsl_ran_pascal = gsl_ran_pascal(r%gsl_rng, p, n)
  end function fgsl_ran_pascal
  function fgsl_ran_pascal_pdf(k, p, n)
    integer(fgsl_int), intent(in) :: k
    real(fgsl_double), intent(in) :: p, n
    real(fgsl_double) :: fgsl_ran_pascal_pdf
    fgsl_ran_pascal_pdf = gsl_ran_pascal_pdf(k, p, n)
  end function fgsl_ran_pascal_pdf
  function fgsl_cdf_pascal_p(k, p, n)
    integer(fgsl_int), intent(in) :: k
    real(fgsl_double), intent(in) :: p, n
    real(fgsl_double) :: fgsl_cdf_pascal_p
    fgsl_cdf_pascal_p = gsl_cdf_pascal_p(k, p, n)
  end function fgsl_cdf_pascal_p
  function fgsl_cdf_pascal_q(k, p, n)
    integer(fgsl_int), intent(in) :: k
    real(fgsl_double), intent(in) :: p, n
    real(fgsl_double) :: fgsl_cdf_pascal_q
    fgsl_cdf_pascal_q = gsl_cdf_pascal_q(k, p, n)
  end function fgsl_cdf_pascal_q
  function fgsl_ran_geometric(r, p)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: p
    integer(fgsl_int) :: fgsl_ran_geometric
    fgsl_ran_geometric = gsl_ran_geometric(r%gsl_rng, p)
  end function fgsl_ran_geometric
  function fgsl_ran_geometric_pdf(k, p)
    integer(fgsl_int), intent(in) :: k
    real(fgsl_double), intent(in) :: p
    real(fgsl_double) :: fgsl_ran_geometric_pdf
    fgsl_ran_geometric_pdf = gsl_ran_geometric_pdf(k, p)
  end function fgsl_ran_geometric_pdf
  function fgsl_cdf_geometric_p(k, p)
    integer(fgsl_int), intent(in) :: k
    real(fgsl_double), intent(in) :: p
    real(fgsl_double) :: fgsl_cdf_geometric_p
    fgsl_cdf_geometric_p = gsl_cdf_geometric_p(k, p)
  end function fgsl_cdf_geometric_p
  function fgsl_cdf_geometric_q(k, p)
    integer(fgsl_int), intent(in) :: k
    real(fgsl_double), intent(in) :: p
    real(fgsl_double) :: fgsl_cdf_geometric_q
    fgsl_cdf_geometric_q = gsl_cdf_geometric_q(k, p)
  end function fgsl_cdf_geometric_q
  function fgsl_ran_hypergeometric(r, n1, n2, t)
    type(fgsl_rng), intent(in) :: r
    integer(fgsl_int), intent(in) :: n1, n2, t
    integer(fgsl_int) :: fgsl_ran_hypergeometric
    fgsl_ran_hypergeometric = gsl_ran_hypergeometric(r%gsl_rng, n1, n2, t)
  end function fgsl_ran_hypergeometric
  function fgsl_ran_hypergeometric_pdf(k, n1, n2, t)
    integer(fgsl_int), intent(in) :: k
    integer(fgsl_int), intent(in) :: n1, n2, t
    real(fgsl_double)  :: fgsl_ran_hypergeometric_pdf
    fgsl_ran_hypergeometric_pdf = gsl_ran_hypergeometric_pdf(k, n1, n2, t)
  end function fgsl_ran_hypergeometric_pdf
  function fgsl_cdf_hypergeometric_p(k, n1, n2, t)
    integer(fgsl_int), intent(in) :: k
    integer(fgsl_int), intent(in) :: n1, n2, t
    real(fgsl_double)  :: fgsl_cdf_hypergeometric_p
    fgsl_cdf_hypergeometric_p = gsl_cdf_hypergeometric_p(k, n1, n2, t)
  end function fgsl_cdf_hypergeometric_p
  function fgsl_cdf_hypergeometric_q(k, n1, n2, t)
    integer(fgsl_int), intent(in) :: k
    integer(fgsl_int), intent(in) :: n1, n2, t
    real(fgsl_double)  :: fgsl_cdf_hypergeometric_q
    fgsl_cdf_hypergeometric_q = gsl_cdf_hypergeometric_q(k, n1, n2, t)
  end function fgsl_cdf_hypergeometric_q
  function fgsl_ran_logarithmic(r, p)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: p
    integer(fgsl_int) :: fgsl_ran_logarithmic
    fgsl_ran_logarithmic = gsl_ran_logarithmic(r%gsl_rng, p)
  end function fgsl_ran_logarithmic
  function fgsl_ran_logarithmic_pdf(k, p)
    integer(fgsl_int), intent(in) :: k
    real(fgsl_double), intent(in) :: p
    real(fgsl_double) :: fgsl_ran_logarithmic_pdf
    fgsl_ran_logarithmic_pdf = gsl_ran_logarithmic_pdf(k, p)
  end function fgsl_ran_logarithmic_pdf
  function fgsl_ran_wishart(r, df, l, result, work)
    type(fgsl_rng), intent(in) :: r
    real(fgsl_double), intent(in) :: df
    type(fgsl_matrix), intent(in) :: l
    type(fgsl_matrix), intent(inout) :: result, work
    integer(fgsl_int) :: fgsl_ran_wishart
    fgsl_ran_wishart = gsl_ran_wishart(r%gsl_rng, df, l%gsl_matrix, &
         result%gsl_matrix, work%gsl_matrix)
  end function fgsl_ran_wishart
  function fgsl_ran_wishart_pdf(x, l_x, df, l, result, work) 
    type(fgsl_matrix), intent(in) :: x, l_x, l
    type(fgsl_matrix), intent(inout) :: work
    real(fgsl_double), intent(in) :: df
    real(fgsl_double), intent(inout) :: result
    integer(fgsl_int) :: fgsl_ran_wishart_pdf
    fgsl_ran_wishart_pdf = gsl_ran_wishart_pdf(x%gsl_matrix, l_x%gsl_matrix, &
         df, l%gsl_matrix, result, work%gsl_matrix)
  end function fgsl_ran_wishart_pdf
  function fgsl_ran_wishart_log_pdf(x, l_x, df, l, result, work) 
    type(fgsl_matrix), intent(in) :: x, l_x, l
    type(fgsl_matrix), intent(inout) :: work
    real(fgsl_double), intent(in) :: df
    real(fgsl_double), intent(inout) :: result
    integer(fgsl_int) :: fgsl_ran_wishart_log_pdf
    fgsl_ran_wishart_log_pdf = gsl_ran_wishart_log_pdf(x%gsl_matrix, &
         l_x%gsl_matrix, df, l%gsl_matrix, result, work%gsl_matrix)
  end function fgsl_ran_wishart_log_pdf
  subroutine fgsl_ran_shuffle(r, base, n, size)
    type(fgsl_rng), intent(in) :: r
    type(c_ptr), intent(in) :: base
    integer(fgsl_size_t), intent(in) :: n, size
    call gsl_ran_shuffle(r%gsl_rng, base, n, size)
  end subroutine fgsl_ran_shuffle
  subroutine fgsl_ran_shuffle_double(r, base, n)
    type(fgsl_rng), intent(in) :: r
    integer(fgsl_size_t), intent(in) :: n
    real(fgsl_double), target, intent(in) :: base(n)
!
    integer(fgsl_size_t) :: size
    type(c_ptr) :: ptr_base
    ptr_base = c_loc(base)
    size = fgsl_sizeof(1.0_fgsl_double)
    call gsl_ran_shuffle(r%gsl_rng, ptr_base, n, size)
  end subroutine fgsl_ran_shuffle_double
  subroutine fgsl_ran_shuffle_size_t(r, base, n)
    type(fgsl_rng), intent(in) :: r
    integer(fgsl_size_t), intent(in) :: n
    integer(fgsl_size_t), target, intent(in) :: base(n)
!
    integer(fgsl_size_t) :: size
    type(c_ptr) :: ptr_base
    ptr_base = c_loc(base)
    size = fgsl_sizeof(1_fgsl_size_t)
    call gsl_ran_shuffle(r%gsl_rng, ptr_base, n, size)
  end subroutine fgsl_ran_shuffle_size_t
  function fgsl_ran_choose(r, dest, k, src, n, size)
    type(fgsl_rng), intent(in) :: r
    type(c_ptr), intent(in) :: dest, src
    integer(fgsl_size_t), intent(in) :: k, n, size
    integer(fgsl_int) :: fgsl_ran_choose
    fgsl_ran_choose = gsl_ran_choose(r%gsl_rng, dest, k, src, n, size)
  end function fgsl_ran_choose
  subroutine fgsl_ran_sample(r, dest, k, src, n, size)
    type(fgsl_rng), intent(in) :: r
    type(c_ptr), intent(in) :: dest, src
    integer(fgsl_size_t), intent(in) :: k, n, size
    call gsl_ran_sample(r%gsl_rng, dest, k, src, n, size)
  end subroutine fgsl_ran_sample
! Add-ons



  function fgsl_ran_discrete_t_status(ran_discrete_t)
    type(fgsl_ran_discrete_t), intent(in) :: ran_discrete_t
    logical :: fgsl_ran_discrete_t_status
    fgsl_ran_discrete_t_status = .true.
    if (.not. c_associated(ran_discrete_t%gsl_ran_discrete_t)) &
         fgsl_ran_discrete_t_status = .false.
  end function fgsl_ran_discrete_t_status
end module fgsl_cdf
