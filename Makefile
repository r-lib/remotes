
SRC := $(wildcard R/*R)

all: inst/install-github.R install-github.R

inst/install-github.R: inst/install-github.Rin $(SRC)
	Rscript -e 'library(brew);brew("$<", "$@")'

install-github.R: inst/install-github.R
	cp $< $@

clean:
	rm inst/install-github.R install-github.R
