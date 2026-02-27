;;; django-test-runner-test.el --- Tests for django-test-runner.el
;;; Commentary:
;;; Code:

(require 'ert)
(require 'el-mock)
(require 'django-test-runner)

(ert-deftest django-test-runner-is-available ()
  (ert-info ("test django-test-runner is available")
    (should (featurep 'django-test-runner))))

(ert-deftest django-test-runner--convert-path-into-python-module ()
  (ert-info ("convert path into python module")
    (should (equal
	     (django-test-runner--convert-path-into-python-module "project/file/test.py")
	     "project.file.test"))))

(ert-deftest django-test-runner--current-class ()
  (ert-info ("return the current class when currently at a function")
    (with-mock
     (stub python-info-current-defun => "TestCase1.test1")
     (should (equal (django-test-runner--current-class) "TestCase1"))))

  (ert-info ("return the current class when currently at a class")
    (with-mock
     (stub python-info-current-defun => "TestCase1")
     (should (equal (django-test-runner--current-class) "TestCase1"))))

  (ert-info ("return nil when currently not at a class or function")
    (with-mock
     (stub python-info-current-defun => nil)
     (should (eq (django-test-runner--current-class) nil)))))

;;; django-test-runner-test.el ends here
