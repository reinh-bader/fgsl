SUBDIRS = . tests doc
ACLOCAL_AMFLAGS = -I m4
AUTOMAKE_OPTIONS=subdir-objects

AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

fgslincludedir=${includedir}/fgsl

BUILT_SOURCES = fgsl_base.mod \
		fgsl_errno.mod \
		fgsl_io.mod \
		fgsl_math.mod \
		fgsl_complex_math.mod \
		fgsl_poly.mod \
		fgsl_sf_types.mod \
		fgsl_sf_airy.mod \
		fgsl_sf_bessel.mod \
		fgsl_sf_claus.mod \
		fgsl_sf_coulomb.mod \
		fgsl_sf_coupling.mod \
		fgsl_sf_daws.mod \
		fgsl_sf_debye.mod \
		fgsl_sf_dilogarithm.mod \
		fgsl_sf_elementary.mod \
		fgsl_sf_ellint.mod \
		fgsl_sf_elljac.mod \
		fgsl_sf_errorfunc.mod \
		fgsl_sf_exponential.mod \
		fgsl_sf_expint.mod \
		fgsl_sf_fermi_dirac.mod \
		fgsl_sf_gam.mod \
		fgsl_sf_gegenbauer.mod \
		fgsl_sf_herm.mod \
		fgsl_sf_hyperg.mod \
		fgsl_sf_laguerre.mod \
		fgsl_sf_lambert.mod \
		fgsl_sf_legendre.mod \
		fgsl_sf_logarithm.mod \
		fgsl_sf_mathieu.mod \
		fgsl_sf_power.mod \
		fgsl_sf_polygamma.mod \
		fgsl_sf_synchrotron.mod \
		fgsl_sf_transport.mod \
		fgsl_sf_trig.mod \
		fgsl_sf_zetafun.mod \
		fgsl_sf.mod \
		fgsl_array.mod \
		fgsl_permutations.mod \
		fgsl_combinations.mod \
		fgsl_multisets.mod \
		fgsl.mod

NULL =

lib_LTLIBRARIES = libfgsl.la
libfgsl_la_SOURCES = \
	fgsl.F90 \
	modules/fgsl_base.f90 \
	modules/fgsl_errno.f90 \
	modules/fgsl_io.f90 \
	modules/fgsl_math.f90 \
	modules/fgsl_complex_math.f90 \
	modules/fgsl_poly.F90 \
	modules/fgsl_sf_types.f90 \
	modules/fgsl_sf_airy.f90 \
	modules/fgsl_sf_bessel.f90 \
	modules/fgsl_sf_claus.f90 \
	modules/fgsl_sf_coulomb.F90 \
	modules/fgsl_sf_coupling.f90 \
	modules/fgsl_sf_daws.f90 \
	modules/fgsl_sf_debye.f90 \
	modules/fgsl_sf_dilogarithm.f90 \
	modules/fgsl_sf_elementary.f90 \
	modules/fgsl_sf_ellint.f90 \
	modules/fgsl_sf_elljac.f90 \
	modules/fgsl_sf_errorfunc.f90 \
	modules/fgsl_sf_exponential.f90 \
	modules/fgsl_sf_expint.f90 \
	modules/fgsl_sf_fermi_dirac.f90 \
	modules/fgsl_sf_gam.f90 \
	modules/fgsl_sf_gegenbauer.f90 \
	modules/fgsl_sf_herm.f90 \
	modules/fgsl_sf_hyperg.f90 \
	modules/fgsl_sf_laguerre.f90 \
	modules/fgsl_sf_lambert.f90 \
	modules/fgsl_sf_legendre.F90 \
	modules/fgsl_sf_logarithm.f90 \
	modules/fgsl_sf_mathieu.f90 \
	modules/fgsl_sf_power.f90 \
	modules/fgsl_sf_polygamma.f90 \
	modules/fgsl_sf_synchrotron.f90 \
	modules/fgsl_sf_transport.f90 \
	modules/fgsl_sf_trig.f90 \
	modules/fgsl_sf_zetafun.f90 \
	modules/fgsl_sf.f90 \
	modules/fgsl_array.F90 \
	modules/fgsl_permutations.f90 \
	modules/fgsl_combinations.f90 \
	modules/fgsl_multisets.f90 \
	csub/fgsl_base_c.c \
	csub/fgsl_io_c.c \
	csub/fgsl_math_c.c \
	csub/fgsl_array_c.c \
	csub/fgsl_permutations_c.c \
	csub/fgsl_combinations_c.c \
	csub/fgsl_multisets_c.c \
	fgsl_utils.c \
	api/dht.finc \
	api/histogram.finc \
	api/linalg.finc \
	api/multifit.finc \
	api/sort.finc \
	api/bspline.finc \
	api/eigen.finc \
	api/ieee.finc \
	api/multimin.finc \
	api/chebyshev.finc \
	api/integration.finc \
	api/min.finc \
	api/multiroots.finc \
	api/rng.finc \
	api/statistics.finc \
	api/fft.finc \
	api/interp.finc \
	api/ntuple.finc \
	api/roots.finc \
	api/sum_levin.finc \
	api/deriv.finc \
	api/fit.finc \
	api/montecarlo.finc \
	api/ode.finc \
	api/siman.finc \
	api/wavelet.finc \
	api/multilarge.finc \
	api/spmatrix.finc \
	api/splinalg.finc \
	api/rstat.finc \
	api/nlfit.finc \
	api/movstat.finc \
	api/filter.finc \
	interface/eigen.finc \
	interface/ieee.finc \
	interface/linalg.finc \
	interface/multimin.finc \
	interface/rng.finc \
	interface/sum_levin.finc \
	interface/bspline.finc \
	interface/multiroots.finc \
	interface/roots.finc \
	interface/wavelet.finc \
	interface/chebyshev.finc \
	interface/fft.finc \
	interface/min.finc \
	interface/ntuple.finc \
	interface/siman.finc \
	interface/fit.finc \
	interface/integration.finc \
	interface/ode.finc \
	interface/sort.finc \
	interface/deriv.finc \
	interface/generics.finc \
	interface/interp.finc \
	interface/montecarlo.finc \
	interface/dht.finc \
	interface/histogram.finc \
	interface/multifit.finc \
	interface/statistics.finc \
	interface/multilarge.finc \
	interface/spmatrix.finc \
	interface/splinalg.finc \
	interface/rstat.finc \
	interface/nlfit.finc \
	interface/movstat.finc \
	interface/filter.finc \
	$(NULL)

libfgsl_la_DEPENDENCIES = $(libfgsl_la_SOURCES)

libfgsl_la_LDFLAGS = -version-info @LIB_CURRENT@:@LIB_REVISION@:@LIB_AGE@ @FGSL_LDFLAGS@
libfgsl_la_CFLAGS = @gsl_CFLAGS@ -I$(top_srcdir)
libfgsl_la_LIBADD = @gsl_LIBS@

fgsl.mod: fgsl.o libfgsl.la
fgsl_base.mod: modules/fgsl_base.o
fgsl_errno.mod: modules/fgsl_errno.o
fgsl_io.mod: modules/fgsl_io.o
fgsl_math.mod: modules/fgsl_math.o
fgsl_complex_math.mod: modules/fgsl_complex_math.o
fgsl_poly.mod: modules/fgsl_poly.o
fgsl_sf_types.mod: modules/fgsl_sf_types.o
fgsl_sf_airy.mod: modules/fgsl_sf_airy.o
fgsl_sf_bessel.mod: modules/fgsl_sf_bessel.o
fgsl_sf_claus.mod: modules/fgsl_sf_claus.o
fgsl_sf_coulomb.mod: modules/fgsl_sf_coulomb.o
fgsl_sf_coupling.mod: modules/fgsl_sf_coupling.o
fgsl_sf_daws.mod: modules/fgsl_sf_daws.o
fgsl_sf_debye.mod: modules/fgsl_sf_debye.o
fgsl_sf_dilogarithm.mod: modules/fgsl_sf_dilogarithm.o
fgsl_sf_elementary.mod: modules/fgsl_sf_elementary.o
fgsl_sf_ellint.mod: modules/fgsl_sf_ellint.o
fgsl_sf_elljac.mod: modules/fgsl_sf_elljac.o
fgsl_sf_errorfunc.mod: modules/fgsl_sf_errorfunc.o
fgsl_sf_exponential.mod: modules/fgsl_sf_exponential.o
fgsl_sf_expint.mod: modules/fgsl_sf_expint.o
fgsl_sf_fermi_dirac.mod: modules/fgsl_sf_fermi_dirac.o
fgsl_sf_gam.mod: modules/fgsl_sf_gam.o
fgsl_sf_gegenbauer.mod: modules/fgsl_sf_gegenbauer.o
fgsl_sf_herm.mod: modules/fgsl_sf_herm.o
fgsl_sf_hyperg.mod: modules/fgsl_sf_hyperg.o
fgsl_sf_laguerre.mod: modules/fgsl_sf_laguerre.o
fgsl_sf_lambert.mod: modules/fgsl_sf_lambert.o
fgsl_sf_legendre.mod: modules/fgsl_sf_legendre.o
fgsl_sf_logarithm.mod: modules/fgsl_sf_logarithm.o
fgsl_sf_mathieu.mod: modules/fgsl_sf_mathieu.o
fgsl_sf_power.mod: modules/fgsl_sf_power.o
fgsl_sf_polygamma.mod: modules/fgsl_sf_polygamma.o
fgsl_sf_synchrotron.mod: modules/fgsl_sf_synchrotron.o
fgsl_sf_transport.mod: modules/fgsl_sf_transport.o
fgsl_sf_trig.mod: modules/fgsl_sf_trig.o
fgsl_sf_zetafun.mod: modules/fgsl_sf_zetafun.o
fgsl_sf.mod: modules/fgsl_sf.o
fgsl_array.mod: modules/fgsl_array.o
fgsl_permutations.mod: modules/fgsl_permutations.o
fgsl_combinations.mod: modules/fgsl_combinations.o
fgsl_multisets.mod: modules/fgsl_multisets.o
fgsl.lo: $(libfgsl_la_SOURCES)

pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA = fgsl.pc
nodist_fgslinclude_HEADERS = fgsl.mod \
			     fgsl_base.mod \
			     fgsl_errno.mod \
			     fgsl_io.mod \
			     fgsl_math.mod \
			     fgsl_complex_math.mod \
			     fgsl_poly.mod \
			     fgsl_sf_types.mod \
			     fgsl_sf_airy.mod \
			     fgsl_sf_bessel.mod \
			     fgsl_sf_claus.mod \
			     fgsl_sf_coulomb.mod \
			     fgsl_sf_coupling.mod \
			     fgsl_sf_daws.mod \
			     fgsl_sf_debye.mod \
			     fgsl_sf_dilogarithm.mod \
			     fgsl_sf_elementary.mod \
			     fgsl_sf_ellint.mod \
			     fgsl_sf_elljac.mod \
			     fgsl_sf_errorfunc.mod \
			     fgsl_sf_exponential.mod \
			     fgsl_sf_expint.mod \
			     fgsl_sf_fermi_dirac.mod \
			     fgsl_sf_gam.mod \
			     fgsl_sf_gegenbauer.mod \
			     fgsl_sf_herm.mod \
			     fgsl_sf_hyperg.mod \
			     fgsl_sf_laguerre.mod \
			     fgsl_sf_lambert.mod \
			     fgsl_sf_legendre.mod \
			     fgsl_sf_logarithm.mod \
			     fgsl_sf_mathieu.mod \
			     fgsl_sf_power.mod \
			     fgsl_sf_polygamma.mod \
			     fgsl_sf_synchrotron.mod \
			     fgsl_sf_transport.mod \
			     fgsl_sf_trig.mod \
			     fgsl_sf_zetafun.mod \
			     fgsl_sf.mod \
			     fgsl_array.mod \
			     fgsl_permutations.mod \
			     fgsl_combinations.mod \
			     fgsl_multisets.mod 

EXTRA_DIST = Doxyfile

clean-local:
	rm -f *.mod 
