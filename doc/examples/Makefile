
-include ../../make.inc

INC=-I../..
LIB=-L../..  -lfgsl_$(F90) $(GSL_LIB)
OMP=-fopenmp


all : cdf.exe cheb.exe combination.exe diff.exe dwt.exe \
	error_1.exe error_2.exe error_3.exe error_4.exe \
	fitting.exe fitting2.exe fitting3.exe histogram.exe histogram2d.exe \
	ieee.exe ieeeround.exe integration.exe \
	interp.exe interpp.exe intro.exe matrix.exe \
	min.exe monte.exe nlfit.exe ntupler.exe ntuplew.exe \
	linalglu.exe eigen.exe eigen_nonsymm.exe fft.exe \
	ode-initval.exe permutation.exe permshuffle.exe polyroots.exe  qrng.exe \
	randpoisson.exe randwalk.exe \
	rng.exe rngunif.exe roots.exe \
	siman.exe stat.exe statsort.exe sum.exe vector.exe 

%.exe : %.f90
	$(F90) $(FFLAGS) $(FPP) -o $@ $(INC) $^ $(LIB) 

integration_omp.exe : integration_omp.f90
	$(F90) $(FFLAGS) $(OMP) $(FPP) -o $@ $(INC) $^ $(LIB) 

siman.exe : siman.f90 siman_print.c
	$(CC) -c siman_print.c
	$(F90) $(FFLAGS) $(FPP) -o $@ $(INC) siman.f90 siman_print.o $(LIB) 

clean :
	rm -f *~ *.exe *.mod *.o fitting2.dat test.dat
