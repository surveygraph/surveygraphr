install:
	R CMD install .

build:
	R CMD build .

check:
	R CMD check surveygraph_*.tar.gz

checkcran:
	R CMD check --as-cran surveygraph_*.tar.gz

clean:
	rm src/*.o src/*.so

run:
	@Rscript eg.R

doc:
	Rscript -e "library('roxygen2'); roxygenise()"
