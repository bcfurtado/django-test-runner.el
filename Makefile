install:
	cask install

tests:
	cask exec ert-runner -l django-test-runner.el test/django-test-runner-test.el
