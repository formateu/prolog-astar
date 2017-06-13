all: doc.pdf

%.pdf: %.md
	pandoc -s $< -o $@

view: doc.pdf
	zathura $<
