AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}
AM_FCFLAGS = -I${top_srcdir} -I${top_builddir}
LDADD = ${top_builddir}/libfgsl.la $(gsl_LIBS)

fgsl_exampledir = $(datadir)/examples/fgsl



check_PROGRAMS = cdf cheb combination diff dwt \
		 error_1 error_2 error_3 error_4 \
		 fitting fitting3 fitting2 histogram \
		 histogram2d ieee ieeeround integration \
	         integration_fixed \
		 interp interpp intro matrix min monte \
                 multifit nlfit nlfit2 nlfit4 \
		 ntuplew ntupler \
		 linalglu eigen eigen_nonsymm fft \
		 ode-initval permutation permshuffle \
		 polyroots qrng randpoisson randwalk \
		 rng rngunif roots siman stat statsort \
		 sum vector vector_int vector_legacy spmatrix splinalg rstat \
		 movstat_1 movstat_2 movstat_3 \
		 gaussfilt impulse

cdf_SOURCES = cdf.f90

cheb_SOURCES = cheb.f90

combination_SOURCES = combination.F90

diff_SOURCES = diff.f90

dwt_SOURCES = dwt.F90
dwt_FCFLAGS = -DDWT_DAT="'$(srcdir)/dwt.dat'"

error_1_SOURCES = error_1.f90
error_2_SOURCES = error_2.f90
error_3_SOURCES = error_3.F90
error_4_SOURCES = error_4.f90

fitting_SOURCES = fitting.f90
fitting2_SOURCES = fitting2.f90
fitting3_SOURCES = fitting3.f90

histogram_SOURCES = histogram.F90
histogram_FCFLAGS = -DHISTOGRAM_DAT="'$(srcdir)/histogram.dat'" \
			-DCAUCHY_DAT="'$(srcdir)/cauchy.dat'"

histogram2d_SOURCES = histogram2d.f90

ieee_SOURCES = ieee.f90

ieeeround_SOURCES = ieeeround.f90

integration_SOURCES = integration.f90

integration_fixed_SOURCES = integration_fixed.f90

interp_SOURCES = interp.f90

interpp_SOURCES = interpp.f90

intro_SOURCES = intro.f90

matrix_SOURCES = matrix.f90

min_SOURCES = min.f90

monte_SOURCES = monte.f90

multifit_SOURCES = multifit.f90

nlfit_SOURCES = nlfit.f90

nlfit2_SOURCES = nlfit2.f90

nlfit4_SOURCES = nlfit4.f90

ntupler_SOURCES = ntupler.f90

ntuplew_SOURCES = ntuplew.f90

linalglu_SOURCES = linalglu.f90

eigen_SOURCES = eigen.f90

eigen_nonsymm_SOURCES = eigen_nonsymm.f90

fft_SOURCES = fft.f90

ode_initval_SOURCES = ode-initval.F90

permutation_SOURCES = permutation.f90

permshuffle_SOURCES = permshuffle.f90

polyroots_SOURCES = polyroots.f90

qrng_SOURCES = qrng.f90

randpoisson_SOURCES = randpoisson.f90

randwalk_SOURCES = randwalk.f90

rng_SOURCES = rng.f90

rngunif_SOURCES = rngunif.f90

roots_SOURCES = roots.f90

siman_SOURCES = siman.f90 siman_print.c

stat_SOURCES = stat.f90

statsort_SOURCES = statsort.f90

sum_SOURCES = sum.f90

vector_SOURCES = vector.f90

vector_int_SOURCES = vector_int.f90

vector_legacy_SOURCES = vector_legacy.f90

spmatrix_SOURCES = spmatrix.f90

splinalg_SOURCES = splinalg.f90

rstat_SOURCES = rstat.f90

movstat_1_SOURCES = movstat_1.f90

movstat_2_SOURCES = movstat_2.f90

movstat_3_SOURCES = movstat_3.f90

gaussfilt_SOURCES = gaussfilt.f90

impulse_SOURCES = impulse.f90

#integration_omp_SOURCES = integration_omp.f90
#integration_omp_FCFLAGS = @OPENMP_FCFLAGS@ -I${top_srcdir} -I${top_builddir}
#integration_omp_LDADD =  ${top_builddir}/libfgsl.la

data_files = dwt.dat histogram.dat cauchy.dat

dist_fgsl_example_DATA = $(data_files) cdf.f90 cheb.f90 combination.F90 diff.f90 dwt.F90 \
			error_1.f90 error_2.f90 error_3.F90 error_4.f90 \
			fitting.f90 fitting3.f90 fitting2.f90 histogram.F90 \
			histogram2d.f90 ieee.f90 ieeeround.f90 integration.f90 \
	                integration_fixed.f90 \
			interp.f90 interpp.f90 intro.f90 matrix.f90 min.f90 monte.f90 \
			nlfit.f90 nlfit2.f90 nlfit4.f90 ntuplew.f90 ntupler.f90 \
			linalglu.f90 eigen.f90 eigen_nonsymm.f90 fft.f90 \
			ode-initval.F90 permutation.f90 permshuffle.f90 \
			polyroots.f90 qrng.f90 randpoisson.f90 randwalk.f90 \
			rng.f90 rngunif.f90 roots.f90 siman.f90 siman_print.c \
			stat.f90 statsort.f90 sum.f90 vector.f90 vector_int.f90 \
			vector_legacy.f90 integration_omp.f90 \
			spmatrix.f90 splinalg.f90 rstat.f90 movstat_1.f90 movstat_2.f90 \
			movstat_3.f90 gaussfilt.f90 impulse.f90

TESTS = $(check_PROGRAMS)
XFAIL_TESTS = error_1

clean-local:
	rm -rf *mod fitting2.dat test.dat
