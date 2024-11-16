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

test:
	Rscript -e "library('surveygraph'); library('testthat'); test_file('tests/testthat/test-projection.R')"

knit:
	@Rscript -e "rmarkdown::render('vignettes/datacleaning.Rmd', params=list(args = myarg))"
	@#Rscript -e "rmarkdown::render('vignettes/surveygraph.Rmd', params=list(args = myarg))"
	@#Rscript -e "rmarkdown::render('vignettes/syntheticdata.Rmd', params=list(args = myarg))"
	@#Rscript -e "rmarkdown::render('vignettes/projections.Rmd', params=list(args = myarg))"
