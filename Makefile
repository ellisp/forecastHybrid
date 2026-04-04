clean:
	rm -rf pkg/forecastHybrid_*.tar.gz pkg/tests/testthat/Rplots.pdf pkg/cache pkg/docs
test: clean
	cd pkg && R --vanilla -q -e 'devtools::test()'
check: clean
	cd pkg && R --vanilla -q -e 'devtools::check(run_dont_test = TRUE)'
check_win: clean document
	cd pkg && R --vanilla -q -e 'devtools::check_win_devel()'
lint:
	cd pkg && R --vanilla -q -e 'library(lintr);lint_package(linters = linters_with_defaults(line_length_linter(100), object_name_linter(styles = "camelCase")))'
full_lint:
	cd pkg && R --vanilla -q -e 'library(lintr);lint_package(linters = all_linters(line_length_linter(100), object_name_linter(styles = "camelCase"), implicit_integer_linter = NULL))'
vignette:
	cd pkg && R --vanilla -q -e 'devtools::build_vignettes("vignettes")'
document:
	cd pkg && R --vanilla -q -e 'devtools::document(".")'

build: clean
	cd pkg && R CMD build .
