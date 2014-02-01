AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}
AM_FCFLAGS = -I${top_srcdir} -I${top_builddir}

check_PROGRAMS = poly interp specfunc permutation integration \
	sort array linalg eigen fft rng histogram \
	statistics ntuple siman ode deriv roots \
	minim multiroots montecarlo bspline \
	multimin fit nlfit version \
	chebyshev error ieee_utils

poly_SOURCES = mod_unit.f90 mod_unit.mod poly.f90 
poly_LDADD = ../libfgsl.la

interp_SOURCES = mod_unit.f90 mod_unit.mod interp.f90 
interp_LDADD = ../libfgsl.la

specfunc_SOURCES = specfunc.f90 mod_unit.f90 mod_unit.mod
specfunc_LDADD = ../libfgsl.la

permutation_SOURCES = permutation.F90 mod_unit.f90 mod_unit.mod
permutation_LDADD = ../libfgsl.la

integration_SOURCES = integration.F90 mod_unit.f90 mod_unit.mod
integration_LDADD = ../libfgsl.la

sort_SOURCES = sort.F90 mod_unit.f90 mod_unit.mod
sort_LDADD = ../libfgsl.la

array_SOURCES = array.f90 mod_unit.f90 mod_unit.mod
array_LDADD = ../libfgsl.la

linalg_SOURCES = linalg.f90 mod_unit.f90 mod_unit.mod
linalg_LDADD = ../libfgsl.la

eigen_SOURCES = eigen.f90 mod_unit.f90 mod_unit.mod
eigen_LDADD = ../libfgsl.la

fft_SOURCES = fft.f90 mod_unit.f90 mod_unit.mod
fft_LDADD = ../libfgsl.la

rng_SOURCES = rng.f90 mod_unit.f90 mod_unit.mod
rng_LDADD = ../libfgsl.la

histogram_SOURCES = histogram.f90 mod_unit.f90 mod_unit.mod
histogram_LDADD = ../libfgsl.la

statistics_SOURCES = statistics.f90 mod_unit.f90 mod_unit.mod
statistics_LDADD = ../libfgsl.la

ntuple_SOURCES = ntuple.f90 mod_unit.f90 mod_unit.mod
ntuple_LDADD = ../libfgsl.la

siman_SOURCES = siman.f90 mod_unit.f90 mod_unit.mod
siman_LDADD = ../libfgsl.la

ode_SOURCES = ode.f90 mod_unit.f90 mod_unit.mod
ode_LDADD = ../libfgsl.la

deriv_SOURCES = deriv.f90 mod_unit.f90 mod_unit.mod
deriv_LDADD = ../libfgsl.la

roots_SOURCES = roots.f90 mod_unit.f90 mod_unit.mod
roots_LDADD = ../libfgsl.la

minim_SOURCES = minim.f90 mod_unit.f90 mod_unit.mod
minim_LDADD = ../libfgsl.la

multiroots_SOURCES = multiroots.f90 mod_unit.f90 mod_unit.mod
multiroots_LDADD = ../libfgsl.la

montecarlo_SOURCES = montecarlo.f90 mod_unit.f90 mod_unit.mod
montecarlo_LDADD = ../libfgsl.la

bspline_SOURCES = bspline.f90 mod_unit.f90 mod_unit.mod
bspline_LDADD = ../libfgsl.la

ieee_utils_SOURCES = ieee_utils.f90 mod_unit.f90 mod_unit.mod
ieee_utils_LDADD = ../libfgsl.la

multimin_SOURCES = multimin.f90 mod_unit.f90 mod_unit.mod
multimin_LDADD = ../libfgsl.la

fit_SOURCES = fit.F90 mod_unit.f90 mod_unit.mod
fit_LDADD = ../libfgsl.la

nlfit_SOURCES = nlfit.F90 mod_unit.f90 mod_unit.mod
nlfit_LDADD = ../libfgsl.la

version_SOURCES = version.f90 mod_unit.f90 mod_unit.mod
version_LDADD = ../libfgsl.la

error_SOURCES = error.F90 mod_unit.f90 mod_unit.mod
error_LDADD = ../libfgsl.la

chebyshev_SOURCES = chebyshev.f90 mod_unit.f90 mod_unit.mod
chebyshev_LDADD = ../libfgsl.la


TESTS = $(check_PROGRAMS)

#the error tests should end with a segfault
XFAIL_TESTS = error

mod_unit.mod:	mod_unit.o

clean-local:
	rm -f *.dat *.mod
