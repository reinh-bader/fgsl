AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}
AM_FCFLAGS = -I${top_srcdir} -I${top_builddir}

check_PROGRAMS = poly interp specfunc permutation integration \
	sort array linalg eigen fft rng histogram \
	statistics ntuple siman ode deriv roots \
	minim multiroots montecarlo bspline \
	multimin fit nlfit version \
	chebyshev error ieee_utils math interp2d

BUILT_SOURCES = mod_unit.mod

SHARED_LDADD = ../libfgsl.la $(gsl_LIBS)

poly_SOURCES = mod_unit.f90 mod_unit.mod poly.F90
poly_LDADD = $(SHARED_LDADD)

interp_SOURCES = mod_unit.f90 mod_unit.mod interp.f90
interp_LDADD = $(SHARED_LDADD)

specfunc_SOURCES = specfunc.f90 mod_unit.f90 mod_unit.mod
specfunc_LDADD = $(SHARED_LDADD)

permutation_SOURCES = permutation.F90 mod_unit.f90 mod_unit.mod
permutation_LDADD = $(SHARED_LDADD)

integration_SOURCES = integration.F90 mod_unit.f90 mod_unit.mod
integration_LDADD = $(SHARED_LDADD)

sort_SOURCES = sort.F90 mod_unit.f90 mod_unit.mod
sort_LDADD = $(SHARED_LDADD)

array_SOURCES = array.f90 mod_unit.f90 mod_unit.mod
array_LDADD = $(SHARED_LDADD)

linalg_SOURCES = linalg.F90 mod_unit.f90 mod_unit.mod
linalg_LDADD = $(SHARED_LDADD)

eigen_SOURCES = eigen.f90 mod_unit.f90 mod_unit.mod
eigen_LDADD = $(SHARED_LDADD)

fft_SOURCES = fft.f90 mod_unit.f90 mod_unit.mod
fft_LDADD = $(SHARED_LDADD)

rng_SOURCES = rng.f90 mod_unit.f90 mod_unit.mod
rng_LDADD = $(SHARED_LDADD)

histogram_SOURCES = histogram.f90 mod_unit.f90 mod_unit.mod
histogram_LDADD = $(SHARED_LDADD)

statistics_SOURCES = statistics.F90 mod_unit.f90 mod_unit.mod
statistics_LDADD = $(SHARED_LDADD)

ntuple_SOURCES = ntuple.f90 mod_unit.f90 mod_unit.mod
ntuple_LDADD = $(SHARED_LDADD)

siman_SOURCES = siman.f90 mod_unit.f90 mod_unit.mod
siman_LDADD = $(SHARED_LDADD)

ode_SOURCES = ode.f90 mod_unit.f90 mod_unit.mod
ode_LDADD = $(SHARED_LDADD)

deriv_SOURCES = deriv.f90 mod_unit.f90 mod_unit.mod
deriv_LDADD = $(SHARED_LDADD)

roots_SOURCES = roots.f90 mod_unit.f90 mod_unit.mod
roots_LDADD = $(SHARED_LDADD)

minim_SOURCES = minim.f90 mod_unit.f90 mod_unit.mod
minim_LDADD = $(SHARED_LDADD)

multiroots_SOURCES = multiroots.f90 mod_unit.f90 mod_unit.mod
multiroots_LDADD = $(SHARED_LDADD)

montecarlo_SOURCES = montecarlo.f90 mod_unit.f90 mod_unit.mod
montecarlo_LDADD = $(SHARED_LDADD)

bspline_SOURCES = bspline.f90 mod_unit.f90 mod_unit.mod
bspline_LDADD = $(SHARED_LDADD)

ieee_utils_SOURCES = ieee_utils.f90 mod_unit.f90 mod_unit.mod
ieee_utils_LDADD = $(SHARED_LDADD)

multimin_SOURCES = multimin.f90 mod_unit.f90 mod_unit.mod
multimin_LDADD = $(SHARED_LDADD)

fit_SOURCES = fit.F90 mod_unit.f90 mod_unit.mod
fit_LDADD = $(SHARED_LDADD)

nlfit_SOURCES = nlfit.F90 mod_unit.f90 mod_unit.mod
nlfit_LDADD = $(SHARED_LDADD)

version_SOURCES = version.f90 mod_unit.f90 mod_unit.mod
version_LDADD = $(SHARED_LDADD)

error_SOURCES = error.F90 mod_unit.f90 mod_unit.mod
error_LDADD = $(SHARED_LDADD)

chebyshev_SOURCES = chebyshev.f90 mod_unit.f90 mod_unit.mod
chebyshev_LDADD = $(SHARED_LDADD)

math_SOURCES = math.f90 mod_unit.f90 mod_unit.mod
math_LDADD = $(SHARED_LDADD)

interp2d_SOURCES = interp2d.f90 mod_unit.f90 mod_unit.mod
interp2d_LDADD = $(SHARED_LDADD)


TESTS = $(check_PROGRAMS)

#the error tests should end with a segfault
XFAIL_TESTS = error

mod_unit.mod:	mod_unit.o

clean-local:
	rm -f *.dat *.mod unit_debug
