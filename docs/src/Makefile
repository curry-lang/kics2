# To format the KiCS2 User Manual

.PHONY: pdf
pdf : manual.pdf

manual.pdf: manual.tex currydoc.tex markdown_syntax.tex ../../lib/TEXDOC/LAST
	pdflatex manual
	bibtex manual
	pdflatex manual
	makeindex manual.idx
	pdflatex manual

../Manual.pdf: manual.pdf
	thumbpdf manual
	pdflatex manual
	rm -f ../Manual.pdf
	cp manual.pdf ../Manual.pdf

markdown_syntax.tex: markdown_syntax.txt
	../../bin/kics2 :add Markdown :eval 'readFile "markdown_syntax.txt" >>= writeFile "markdown_syntax.tex" . markdownText2LaTeX' :q

../markdown_syntax.html: markdown_syntax.txt
	../../bin/kics2 :add Markdown :eval 'readFile "markdown_syntax.txt" >>= writeFile "../markdown_syntax.html" . markdownText2CompleteHTML' :q

../../lib/TEXDOC/LAST:
	${MAKE} libdoc

# generate documentation for all libraries
.PHONY: libdoc
libdoc:
	@cd ../../lib && ${MAKE} texdoc
	@rm -f lib && ln -s ../../lib/TEXDOC lib

.PHONY: install
install:
	${MAKE} ../markdown_syntax.html
	${MAKE} libdoc
	${MAKE} ../Manual.pdf

.PHONY: clean
clean:
	/bin/mv manual.tex manual.texx
	/bin/mv manual.bbl manual.bblx
	/bin/rm -f manual.?? manual.???
	/bin/mv manual.texx manual.tex
	/bin/mv manual.bblx manual.bbl
	/bin/rm -f ../Manual.pdf thumb*

