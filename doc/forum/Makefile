

pdf : fgsl_disc.pdf
dvi : fgsl_disc.dvi

fgsl_disc.pdf : fgsl_disc.tex
	pdflatex $^
	pdflatex $^

fgsl_disc.dvi : fgsl_disc.tex
	latex $^
	latex $^
clean :
	rm -f *.pdf *.dvi *.log *.aux *~
