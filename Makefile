install:
	eask install-deps --dev

tests:
	eask exec ert-runner

checkdoc:
	eask lint checkdoc

package-lint:
	eask lint package django-test-runner.el

lint: checkdoc package-lint
