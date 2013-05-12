-include ../make.inc

ifeq ($(F90),)
all :
	@echo "Please run configure to set up build"
else
all : mod_unit.o poly.exe interp.exe specfunc.exe permutation.exe integration.exe \
	sort.exe array.exe linalg.exe eigen.exe fft.exe rng.exe histogram.exe \
	statistics.exe ntuple.exe siman.exe ode.exe deriv.exe roots.exe \
	minim.exe multiroots.exe montecarlo.exe bspline.exe ieee_utils.exe \
	multimin.exe fit.exe nlfit.exe version.exe error.x
endif


INC=-I..
LIB=-L.. -lfgsl_$(F90) $(GSL_LIB)



run : all
	@rm -f unit_debug
	@./runtests.sh

mod_unit.o : mod_unit.f90
	$(F90) $(FPP) $(FFLAGS) $(DEBUG) -c  $(INC) $^ 

%.exe : %.f90 mod_unit.o
	$(F90) $(FPP) $(FFLAGS) $(DEBUG) -o $@ $(INC) $< mod_unit.o $(LIB) 
error.x : error.f90 mod_unit.o
	$(F90) $(FPP) $(FFLAGS) $(DEBUG) -o $@ $(INC) $< mod_unit.o $(LIB) 

clean :
	rm -f *~ *.exe error.x *.mod *.o *.dat
	rm -f unit_debug
