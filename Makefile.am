SUBDIRS = . tests api interface doc
ACLOCAL_AMFLAGS = -I m4
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

fgslincludedir=${includedir}/fgsl


lib_LTLIBRARIES = libfgsl.la
libfgsl_la_SOURCES = fgsl.F90 fgsl_utils.c
libfgsl_la_LDFLAGS = -version-info @LIB_CURRENT@:@LIB_REVISION@:@LIB_AGE@ @FGSL_LDFLAGS@
libfgsl_la_CFLAGS = @gsl_CFLAGS@
libfgsl_la_LIBADD = @gsl_LIBS@

fgsl.mod: fgsl.o

pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA = fgsl.pc
nodist_fgslinclude_HEADERS = fgsl.mod

#EXTRA_DIST = fgsl.pc.in fgsl.spec

EXTRA_DIST = Doxyfile

clean-local:
	rm -f fgsl.mod
