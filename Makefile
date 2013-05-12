
-include make.inc

PKG=fgsl-0.9.4

ifeq ($(F90),)
all :
	@echo "Please run configure to set up build"
else
all : lib 
endif

test : lib
	@cd tests; $(MAKE) clean; $(MAKE) -j 8 run


lib : libfgsl_$(F90).a 

libfgsl_$(F90).a : fgsl.o fgsl_utils.o
	ar $(ARFLAGS) libfgsl_$(F90).a $^
	@echo "Build complete."

fgsl.o : fgsl.f90 api/*.finc interface/*.finc
	$(F90) $(FFLAGS) $(DEBUG) -c  fgsl.f90

%.o : %.f90
	$(F90) $(FFLAGS) $(DEBUG) -c -o $@ $^

%.o : %.c
	$(CC) -c $(GSL_INC) $(CFLAGS) $(CDEBUG) -o $@ $^

install : lib
	mkdir -p $(PREFIX)/$(LIB)
	cp -p libfgsl_$(F90).a $(PREFIX)/$(LIB)
	chmod ugo+r $(PREFIX)/$(LIB)/libfgsl_$(F90).a
	mkdir -p $(PREFIX)/include/$(F90)
	cp -p fgsl.mod $(PREFIX)/include/$(F90)
	chmod ugo+r $(PREFIX)/include/$(F90)/fgsl.mod
	cd doc; tar -xzf html.tgz; mkdir -p $(PREFIX)/fgsl_doc;\
	cp -a html $(PREFIX)/fgsl_doc

doc : 
	cd doc; $(MAKE)

clean :
	rm -f a.out *.exe *.o *.mod *~ *.a typesizes.out *.tar.gz
	cd tests; $(MAKE) clean

distclean : clean
	cd doc; $(MAKE) clean
	cd doc/examples; $(MAKE) clean
	cd tests; $(MAKE) clean
	rm -f make.inc integer.finc
	rm -f *~ */*~ */*/*~
	rm -rf doc/html doc/latex

.PHONY : html
html :
	@if [ -f Doxyfile ] ; then echo building documentation ...;\
	doxygen ; rm doc/html.tgz; \
	tar -czvf doc/html.tgz -C doc html; echo ... done; fi 

package :
	-ln -s . $(PKG)
	tar -czf $(PKG).tar.gz --exclude .svn --exclude forum --exclude .texi $(PKG)/*.f90 $(PKG)/*.c $(PKG)/Makefile $(PKG)/configure $(PKG)/README $(PKG)/NEWS $(PKG)/api $(PKG)/doc/html.tgz $(PKG)/doc/examples $(PKG)/interface $(PKG)/tests
	rm -f $(PKG)

count :
	@echo -n "Fortran code: "; cat f*.f90 */*.finc | grep -v "^\!" | wc -l
	@echo -n "C code:       "; cat fgsl_utils.c | wc -l
	@echo -n "Test code:    "; cat tests/*.f90  | grep -v "^\!" | wc -l
	@echo -n "Example code: "; cat doc/examples/*.f90  | grep -v "^\!" | wc -l
