;;; django-test-runner-test.el --- Tests for django-test-runner.el

(require 'ert)
(require 'django-test-runner)


(ert-deftest django-test-runner-is-available ()
  (ert-info ("test django-test-runner is available")
    (should (featurep 'django-test-runner))))


;;; django-test.el-test.el ends here
