SUBDIRS = . tests doc
ACLOCAL_AMFLAGS = -I m4
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

fgslincludedir=${includedir}/fgsl

all-am: fgsl.mod

NULL =

lib_LTLIBRARIES = libfgsl.la
libfgsl_la_SOURCES = fgsl.F90 \
										 fgsl_utils.c \
										 api/array.finc \
										 api/dht.finc \
										 api/histogram.finc \
										 api/linalg.finc \
										 api/multifit.finc \
										 api/permutation.finc \
										 api/sort.finc \
										 api/bspline.finc \
										 api/eigen.finc \
										 api/ieee.finc \
										 api/math.finc \
										 api/multimin.finc \
										 api/poly.finc \
										 api/specfunc.finc \
										 api/chebyshev.finc \
										 api/error.finc \
										 api/integration.finc \
										 api/min.finc \
										 api/multiroots.finc \
										 api/rng.finc \
										 api/statistics.finc \
										 api/complex.finc \
										 api/fft.finc \
										 api/interp.finc \
										 api/misc.finc \
										 api/ntuple.finc \
										 api/roots.finc \
										 api/sum_levin.finc \
										 api/deriv.finc \
										 api/fit.finc \
										 api/io.finc \
										 api/montecarlo.finc \
										 api/ode.finc \
										 api/siman.finc \
										 api/wavelet.finc \
										 api/multilarge.finc \
										 api/spmatrix.finc \
										 api/splinalg.finc \
										 api/rstat.finc \
										 interface/array.finc \
										 interface/eigen.finc \
										 interface/ieee.finc \
										 interface/linalg.finc \
										 interface/multimin.finc \
										 interface/rng.finc \
										 interface/sum_levin.finc \
										 interface/bspline.finc \
										 interface/error.finc \
										 interface/math.finc \
										 interface/multiroots.finc \
										 interface/roots.finc \
										 interface/wavelet.finc \
										 interface/chebyshev.finc \
										 interface/fft.finc \
										 interface/min.finc \
										 interface/ntuple.finc \
										 interface/siman.finc \
										 interface/complex.finc \
										 interface/fit.finc \
										 interface/integration.finc \
										 interface/misc.finc \
										 interface/ode.finc \
										 interface/sort.finc \
										 interface/deriv.finc \
										 interface/generics.finc \
										 interface/interp.finc \
										 interface/montecarlo.finc \
										 interface/permutation.finc \
										 interface/specfunc.finc \
										 interface/dht.finc \
										 interface/histogram.finc \
										 interface/io.finc \
										 interface/multifit.finc \
										 interface/poly.finc \
										 interface/statistics.finc \
										 interface/multilarge.finc \
										 interface/spmatrix.finc \
										 interface/splinalg.finc \
										 interface/rstat.finc \
										 $(NULL)

libfgsl_la_DEPENDENCIES = $(libfgsl_la_SOURCES)

libfgsl_la_LDFLAGS = -version-info @LIB_CURRENT@:@LIB_REVISION@:@LIB_AGE@ @FGSL_LDFLAGS@
libfgsl_la_CFLAGS = @gsl_CFLAGS@
libfgsl_la_LIBADD = @gsl_LIBS@

fgsl.mod: fgsl.o libfgsl.la
fgsl.lo: $(libfgsl_la_SOURCES)

pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA = fgsl.pc
nodist_fgslinclude_HEADERS = fgsl.mod

EXTRA_DIST = Doxyfile

clean-local:
	rm -f fgsl.mod
