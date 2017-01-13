AC_INIT([fgsl],[1.2.0],[Reinhold.Bader@lrz.de])
AC_PREREQ([2.69])

AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([-Wall -Werror foreign])

AC_CANONICAL_HOST

LIB_CURRENT=1
LIB_REVISION=2
LIB_AGE=0
AC_SUBST(LIB_CURRENT)
AC_SUBST(LIB_REVISION)
AC_SUBST(LIB_AGE)

#m4_pattern_allow([AS_TR_SH])
AC_CONFIG_MACRO_DIR([m4])

AC_USE_SYSTEM_EXTENSIONS

m4_ifdef([AM_PROG_AR],[AM_PROG_AR])


LT_PREREQ([2.1.0])
LT_INIT([win32-dll])

AC_PROG_CC
if test -z $CC ; then
        AC_MSG_ERROR([no C compiler was found on the system.])
fi
#initialize pkg-config
PKG_PROG_PKG_CONFIG

PKG_CHECK_MODULES([gsl],gsl >= 2.3)

local_gsl_version=`$PKG_CONFIG --modversion gsl`
AC_SUBST(GSL_VERSION,$local_gsl_version)
AC_DEFINE_UNQUOTED([GSL_VERSION],['$local_gsl_version'],[gsl version])
AC_DEFINE_UNQUOTED([GSL_VERSION2],[$local_gsl_version],[gsl version without quotes])

AC_PROG_FC
if test -z $FC ; then
        AC_MSG_ERROR([Fortran 2003 compiler was not found])
fi


#check for ieee_exceptions support
AC_FC_SRCEXT([F90],[
	AC_MSG_CHECKING([whether the fortran compiler supports the 2003 feature: ieee_exceptions])
	AC_LANG_PUSH(Fortran)
	AC_COMPILE_IFELSE([[
PROGRAM main
USE, INTRINSIC :: ieee_exceptions
IMPLICIT NONE
INTEGER :: i = 0

	i = i+1
	

ENDPROGRAM
	]], [IEEE_EXCEPTIONS=yes], [IEEE_EXCEPTIONS=no])
	AC_LANG_POP(Fortran)
	AC_MSG_RESULT([$IEEE_EXCEPTIONS])
])

if test "x$IEEE_EXCEPTIONS" = xyes ; then
	AC_DEFINE([IEEE_EXCEPTIONS],[],[Fortran 2003 ieee_exceptions intrinsic module found])
fi

#check for target support
AC_FC_SRCEXT([F90],[
	AC_MSG_CHECKING([whether the fortran compiler supports the 2008 feature: c_loc for targeted arrays])
	AC_LANG_PUSH(Fortran)
	AC_COMPILE_IFELSE([[
SUBROUTINE test(c)
USE, INTRINSIC :: iso_c_binding
IMPLICIT NONE
INTEGER(c_size_t), DIMENSION(:), INTENT(inout), TARGET:: c
TYPE(C_PTR) :: ptr

ptr = C_LOC(c)

ENDSUBROUTINE
	]], [C_LOC_TARGET=yes], [C_LOC_TARGET=no])
	AC_LANG_POP(Fortran)
	AC_MSG_RESULT([$C_LOC_TARGET])
])

if test "x$C_LOC_TARGET" = xno; then
	AC_MSG_ERROR([Fortran compiler does not support a required feature from the 2008 standard.])
fi

case "$host" in
	*mingw*)
		FGSL_LDFLAGS="-no-undefined"
		AC_SUBST(FGSL_LDFLAGS)
		;;
esac


AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([size_t])

if test "x$ac_cv_sizeof_size_t" = "x4" ; then
	AC_MSG_NOTICE([32 bit architecture detected])
elif test "x$ac_cv_sizeof_size_t" = "x8" ; then
	AC_MSG_NOTICE([64 bit architecture detected])
else
	AC_MSG_ERROR([Unknown architecture detected])
fi

AC_PROG_SED

gsl_version_major_fortran=`echo $local_gsl_version | $SED -e 's/\([[[:digit:]]]\{1,\}\)\.\([[[:digit:]]]\{1,\}\)/\1/'`
gsl_version_minor_fortran=`echo $local_gsl_version | $SED -e 's/\([[[:digit:]]]\{1,\}\)\.\([[[:digit:]]]\{1,\}\)/\2/'`
AC_DEFINE_UNQUOTED([GSL_VERSION_MAJOR_FORTRAN],[$gsl_version_major_fortran],[gsl version major for fortran code])
AC_DEFINE_UNQUOTED([GSL_VERSION_MINOR_FORTRAN],[$gsl_version_minor_fortran],[gsl version minor for fortran code])

AC_LANG_PUSH(Fortran)
AC_OPENMP
AC_LANG_POP(Fortran)



AC_CONFIG_FILES([Makefile tests/Makefile fgsl.pc fgsl.spec doc/Makefile doc/examples/Makefile])
AC_CONFIG_HEADERS([config.h])

abs_top_builddir=`pwd -P`
AC_SUBST(abs_top_builddir)
abs_top_srcdir=`AS_DIRNAME([$0])`
cd $abs_top_srcdir
abs_top_srcdir=`pwd -P`
cd $abs_top_builddir
AC_SUBST(abs_top_srcdir)

AM_CONDITIONAL([ABS_SRC_BUILD_EQUAL],[test x$abs_top_builddir = x$abs_top_srcdir])

AC_OUTPUT
