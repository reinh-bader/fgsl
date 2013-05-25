AC_INIT([fgsl],[1.0.0],[Bader.Reinhold@lrz.de])
AC_PREREQ([2.60])

AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([-Wall -Werror foreign])

AC_CANONICAL_HOST

LIB_CURRENT=0
LIB_REVISION=0
LIB_AGE=0
AC_SUBST(LIB_CURRENT)
AC_SUBST(LIB_REVISION)
AC_SUBST(LIB_AGE)

#m4_pattern_allow([AS_TR_SH])
AC_CONFIG_MACRO_DIR([m4])

AC_USE_SYSTEM_EXTENSIONS

m4_ifdef([AM_PROG_AR],[AM_PROG_AR])


LT_PREREQ([2.0.0])
LT_INIT()
AC_PROG_LIBTOOL

AC_PROG_CC
if test -z $CC ; then
        AC_MSG_ERROR([no C compiler was found on the system.])
fi
#initialize pkg-config
PKG_PROG_PKG_CONFIG

PKG_CHECK_MODULES([gsl],gsl >= 1.13)

local_gsl_version=`pkg-config --modversion gsl`
AC_SUBST(GSL_VERSION,$local_gsl_version)
AC_DEFINE_UNQUOTED([GSL_VERSION],['$local_gsl_version'],[gsl version])
AC_DEFINE_UNQUOTED([GSL_VERSION2],[$local_gsl_version],[gsl version without quotes])

AC_PROG_FC
if test -z $FC ; then
        AC_MSG_ERROR([Fortran 2003 compiler was not found])
fi
AC_FC_SRCEXT(f90)

AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([size_t])

AC_PROG_SED

gsl_version_major_fortran=`echo $local_gsl_version | $SED -e 's/\([[[:digit:]]]\+\)\.\([[[:digit:]]]\+\)/\1/'`
gsl_version_minor_fortran=`echo $local_gsl_version | $SED -e 's/\([[[:digit:]]]\+\)\.\([[[:digit:]]]\+\)/\2/'`
AC_DEFINE_UNQUOTED([GSL_VERSION_MAJOR_FORTRAN],[$gsl_version_major_fortran],[gsl version major for fortran code])
AC_DEFINE_UNQUOTED([GSL_VERSION_MINOR_FORTRAN],[$gsl_version_minor_fortran],[gsl version minor for fortran code])



AC_CONFIG_FILES([Makefile api/Makefile interface/Makefile tests/Makefile fgsl.pc fgsl.spec doc/Makefile])
AC_CONFIG_HEADERS([config.h])

AC_OUTPUT
