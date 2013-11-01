# Makefile for LaTeX files

LATEX	= latex
BIBTEX	= bibtex
DVIPS	= dvips
PSTOPDF = ps2pdf
PDFLATEX	= pdflatex

SRC	= Xhyb.tex

all 	: Xhyb.pdf

%.pdf:	%.tex
#	$(LATEX) $*
#	$(BIBTEX) $*
#	$(LATEX) $*
#	$(LATEX) $*
#	$(DVIPS) $*
#	$(PSTOPDF) $*.ps
	$(PDFLATEX) $*
	$(PDFLATEX) $*
#	rm -f *.ps rq-* $*.dvi $*.log $*.bbl *.tex $*.aux $*.blg $*.log
	rm -f *.ps *.eps Xhyb-*.pdf $*.log $*.bbl *.tex *.aux $*.blg *.log
