FILE = specification
TEX = $(FILE).tex
PDF = $(FILE).pdf

all: $(PDF)

$(PDF): $(TEX)
	pdflatex $(TEX)

clean:
	rm -f $(PDF) *.log *.aux *.out *.pdf *.toc

