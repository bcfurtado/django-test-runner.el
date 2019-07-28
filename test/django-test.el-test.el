;;; django-test.el-test.el --- Tests for django-test.el

(require 'ert)
(require 'django-test)


(ert-deftest django-test-is-available ()
  (ert-info ("test django-test is available")
    (should (featurep 'django-test))))


;;; django-test.el-test.el ends here
