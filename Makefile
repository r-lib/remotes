SRC := $(wildcard R/*R)

inst/install-github.R: inst/install-github.Rin $(SRC)
	Rscript -e 'brew::brew("$<", "$@")'
