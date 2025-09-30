install:
	R CMD INSTALL .

build:
	R CMD build .

check:
	R CMD build .
	R CMD check surveygraph_*.tar.gz

cran:
	R CMD build .
	R CMD check --as-cran surveygraph_*.tar.gz

covr:
	Rscript -e "library('covr'); package_coverage()"

clean:
	rm src/*.o src/*.so

doc:
	Rscript -e "library('roxygen2'); roxygenise()"

test:
	Rscript -e "library('surveygraph'); library('testthat'); test_file('tests/testthat/test_make_threshold_profile_args.R')"
	Rscript -e "library('surveygraph'); library('testthat'); test_file('tests/testthat/test_make_threshold_profile.R')"
	Rscript -e "library('surveygraph'); library('testthat'); test_file('tests/testthat/test_make_projection_args.R')"
	Rscript -e "library('surveygraph'); library('testthat'); test_file('tests/testthat/test_make_projection_method_lcc.R')"
	Rscript -e "library('surveygraph'); library('testthat'); test_file('tests/testthat/test_make_projection_method_avgdegree.R')"
	Rscript -e "library('surveygraph'); library('testthat'); test_file('tests/testthat/test_make_projection_method_similarity.R')"
	Rscript -e "library('surveygraph'); library('testthat'); test_file('tests/testthat/test_make_projection_metric.R')"
	Rscript -e "library('surveygraph'); library('testthat'); test_file('tests/testthat/test_make_projection_comparisons.R')"
	Rscript -e "library('surveygraph'); library('testthat'); test_file('tests/testthat/test_make_projection_bootstrap.R')"
	Rscript -e "library('surveygraph'); library('testthat'); test_file('tests/testthat/test_data_preprocessing_args.R')"
	Rscript -e "library('surveygraph'); library('testthat'); test_file('tests/testthat/test_data_preprocessing.R')"
	Rscript -e "library('surveygraph'); library('testthat'); test_file('tests/testthat/test_make_synthetic_args.R')"
	Rscript -e "library('surveygraph'); library('testthat'); test_file('tests/testthat/test_make_synthetic.R')"

knit:
	Rscript -e "rmarkdown::render('vignettes/surveygraph.Rmd', params=list(args = myarg))"
