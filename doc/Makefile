
all: doc

doc: manual.pdf html

manual.pdf:
	texi2pdf manual.texinfo

html: manual.html

manual.html:
	makeinfo --html --no-split manual.texinfo

clean:
	rm manual.aux  manual.cp  manual.fn  manual.ky  manual.log  manual.pdf  manual.pg  manual.toc  manual.tp  manual.vr
