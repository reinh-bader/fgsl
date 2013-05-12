
all : fgsl-ref.pdf

fgsl-ref.dvi : $(shell ls *.texi)
	texi2dvi  fgsl-ref.texi

fgsl-ref.pdf : fgsl-ref.dvi
	dvipdf $^

clean : 
	rm -f *.dvi *.pdf *.aux *.log *.pg *.fn *.ky  *.cp *.cps 
	rm -f *.toc *.tp *.vr *~ *.fns
