clean:
	rm -rf pkg/forecastHybrid_*.tar.gz pkg/tests/testthat/Rplots.pdf
test: clean
	cd pkg && R --vanilla -q -e 'devtools::test()'
check: clean
	cd pkg && R --vanilla -q -e 'devtools::check(run_dont_test = TRUE)'
check_win: clean
	cd pkg && R --vanilla -q -e 'devtools::check_win_devel()'
lint:
	cd pkg && R --vanilla -q -e 'library(lintr);lint_package(linters = with_defaults(line_length_linter(100), object_name_linter(styles = "camelCase")))'

build: clean
	cd pkg && R CMD build .
