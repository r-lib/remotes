SRC := $(wildcard R/*R)

install-github.R: install-github.Rin $(SRC)
	Rscript -e 'brew::brew("$<", "$@")'
