SUBDIRS = examples

info_TEXINFOS = fgsl-ref.texi
fgsl_ref_TEXINFOS = intro.texi usage.texi err.texi math.texi complex.texi poly.texi \
			specfunc.texi vectors.texi permutation.texi sort.texi \
			integration.texi random.texi freemanuals.texi gpl.texi \
			fdl.texi

examples_src = codefrags/errhand.f90 codefrags/math_1.f90 codefrags/math_2.f90 \
		codefrags/math_3.f90 codefrags/math_4.f90 examples/intro.f90 \
		examples/intro.txt



noinst_DATA = $(examples_src)
EXTRA_DIST = $(noinst_DATA)
