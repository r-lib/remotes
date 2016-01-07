
install-github.R: install-github.Rin
	Rscript -e 'brew::brew("$<", "$@")'
