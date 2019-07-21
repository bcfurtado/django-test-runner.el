;;; django-test.el --- Quickly execute django tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Bruno Furtado

;; Author: Bruno Furtado <bruno@bcfurtado.com>
;; Keywords: convenience

;;; Commentary:

;; Package to help you to quickly execute django tests using
;; compilation mode

;;; Code:

(require 'compile)
(require 'python)
(require 'subr-x)

(defconst django-test-manage-py "manage.py")
(defconst django-test-command "test")
(defconst django-test-command-params-no-input "--no-input")

(defgroup django-test nil
  "Quickly execute django tests"
  :group 'convenience)

(defcustom django-test-settings-module nil
  "Specifies the settings module to use."
  :group 'django-test
  :type 'stringp)

(defcustom django-test-keepdb nil
  "Specifies the settings module to use."
  :group 'django-test
  :type 'booleanp)

(defun is-nil (element)
  "Check if ELEMENT is nil."
  (eq element nil))

(defun django-test-project-folder ()
  "Return Django root project path.
Currently, we are assuming that the root folder is the one that
contains the manage.py."
  (locate-dominating-file (buffer-file-name) django-test-manage-py))

(defun django-test-file-path ()
  "Return the path of the file to be tested relative to the project root directory."
  (file-relative-name (buffer-file-name) (django-test-project-folder)))

(defun django-test-convert-path-into-python-module (path)
  "Convert PATH into python module."
  (replace-regexp-in-string "/" "."
    (string-join
      (list
        (file-name-directory path)
        (file-name-base path)))))

(defun django-test-current-module ()
  "Return the current python module based on file path."
  (django-test-convert-path-into-python-module (django-test-file-path)))

(defun django-test-generate-python-module-at-point ()
  "Generate python module at the point."
  (let ((full-module (seq-map 'cdr
                      (list
                        (cons 'module (django-test-current-module))
                        (cons 'function (python-info-current-defun))))))
    (string-join (seq-remove 'is-nil full-module) ".")))

(defun django-test-generate-settings-module ()
  "Generate settings option."
  (when django-test-settings-module
    (string-join
      (list
        "--settings"
        django-test-settings-module)
      "=")))

(defun django-test-generate-keepdb ()
  "Generate settings option."
  (when django-test-keepdb
    "--keepdb"))

(defun django-test-generate-test-command ()
  "Generate the test command."
  (let ((command (seq-map 'cdr
                   (list
                     (cons 'python-interpreter python-shell-interpreter)
                     (cons 'manage-py django-test-manage-py)
                     (cons 'command django-test-command)
                     (cons 'module (django-test-generate-python-module-at-point))
                     (cons 'noinput django-test-command-params-no-input)
		     (cons 'keepdb (django-test-generate-keepdb))
                     (cons 'settings-module (django-test-generate-settings-module))))))
    (string-trim (string-join command " "))))


(defun django-test-run-test-at-point ()
  "Run django test at the point.
Invoke this function and you'll be promoted with the exact command to
run only the tests for the specific file.
Keep your cursor under the class name or the function and the command
will be even more specific."
  (interactive)
  (let* ((command (django-test-generate-test-command)))
    (save-excursion
      (let* ((project-root-folder (find-file-noselect (django-test-project-folder))))
        (setq compilation-read-command t)
        (set-buffer project-root-folder)
        (setq compile-command command)
        (call-interactively 'compile)
        (kill-buffer project-root-folder)))))

(provide 'django-test)
;;; django-test ends here
