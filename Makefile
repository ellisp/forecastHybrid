clean:
	rm -rf pkg/forecastHybrid_*.tar.gz
test: clean
	cd pkg && R --vanilla -q -e 'devtools::test()'
check: clean
	cd pkg && R --vanilla -q -e 'devtools::check(run_dont_test = TRUE)'
check_win: clean
	cd pkg && R --vanilla -q -e 'devtools::check_win_devel()'
build: check
	cd pkg && R CMD build .
