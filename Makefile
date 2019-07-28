install:
	cask install

tests:
	cask exec ert-runner -l django-test.el test/django-test.el-test.el
