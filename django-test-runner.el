;;; django-test-runner.el --- Quickly execute django tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Bruno Furtado

;; Author: Bruno Furtado <bruno@bcfurtado.com>
;; Keywords: convenience
;; URL: https://github.com/bcfurtado/django-test-runner.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (transient "0.1.0"))

;;; Commentary:

;; Package to help you to quickly execute django tests using
;; compilation mode

;;; Code:

(require 'compile)
(require 'python)
(require 'subr-x)
(require 'transient)

(defconst django-test-manage-py "manage.py")
(defconst django-test-command "test")

(defun django-test--project-folder ()
  "Return Django root project path.
Currently, we are assuming that the root folder is the one that
contains the manage.py."
  (locate-dominating-file (buffer-file-name) django-test-manage-py))

(defun django-test--file-path ()
  "Return the path of the file to be tested relative to the project root directory."
  (file-relative-name (buffer-file-name) (django-test--project-folder)))

(defun django-test--convert-path-into-python-module (path)
  "Convert PATH into python module."
  (replace-regexp-in-string "/" "."
    (string-join
      (list
        (file-name-directory path)
        (file-name-base path)))))

(defun django-test--current-module ()
  "Return the current python module based on file path."
  (django-test--convert-path-into-python-module (django-test--file-path)))

(defun django-test--current-class ()
  "Return ."
  (let ((current-defun (python-info-current-defun)))
    (when current-defun
      (car (split-string current-defun "\\.")))))

(defun django-test--generate-python-module-with-function ()
  "Generate python module with current function."
  (let ((full-module (seq-map 'cdr
                      (list
                        (cons 'module (django-test--current-module))
                        (cons 'function (python-info-current-defun))))))
    (string-join (delq nil full-module) ".")))

(defun django-test--generate-python-module-with-class ()
  "Generate python module with current class."
  (let ((full-module (seq-map 'cdr
                      (list
                        (cons 'module (django-test--current-module))
                        (cons 'function (django-test--current-class))))))
    (string-join (delq nil full-module) ".")))

(defun django-test--generate-function-test-command ()
  "Generate function test command."
  (let ((command (seq-map 'cdr
                   (list
                     (cons 'python-interpreter python-shell-interpreter)
                     (cons 'manage-py django-test-manage-py)
                     (cons 'command django-test-command)
                     (cons 'module (django-test--generate-python-module-with-function))))))
    (string-trim (string-join command " "))))

(defun django-test--generate-class-test-command ()
  "Generate function test command."
  (let ((command (seq-map 'cdr
                   (list
                     (cons 'python-interpreter python-shell-interpreter)
                     (cons 'manage-py django-test-manage-py)
                     (cons 'command django-test-command)
                     (cons 'module (django-test--generate-python-module-with-class))))))
    (string-trim (string-join command " "))))

(defun django-test--generate-module-test-command ()
  "Generate module test command."
  (let ((command (seq-map 'cdr
                   (list
                     (cons 'python-interpreter python-shell-interpreter)
                     (cons 'manage-py django-test-manage-py)
                     (cons 'command django-test-command)
                     (cons 'module (django-test--current-module))))))
    (string-trim (string-join command " "))))

(defun django-test--generate-project-test-command ()
  "Generate project test command."
  (let ((command (seq-map 'cdr
                   (list
                     (cons 'python-interpreter python-shell-interpreter)
                     (cons 'manage-py django-test-manage-py)
                     (cons 'command django-test-command)))))
    (string-trim (string-join command " "))))

(defun django-test--run-test-command (prefix-command &optional args)
  "Invoke the compile mode with the test PREFIX-COMMAND and ARGS if provided.
When '--no-input' parameter is not available, execute tests are
executed with `comint-mode', otherwise with `compile-mode'."
  (save-excursion
    (let* ((project-root-folder (find-file-noselect (django-test--project-folder)))
          (arguments (string-join args " "))
          (command (string-join (list prefix-command arguments) " ")))
      (setq compilation-read-command t)
      (set-buffer project-root-folder)
      (setq compile-command command)
      (if (member "--no-input" args)
          (call-interactively 'compile)
        (let ((current-prefix-arg '(4)))
          (call-interactively 'compile)))
      (kill-buffer project-root-folder))))

(defun django-test-run-test-function (&optional args)
  "Run django test at the point."
  (interactive (list (django-test-arguments)))
  (django-test--run-test-command (django-test--generate-function-test-command) args))

(defun django-test-run-test-class (&optional args)
  "Run django test at the point."
  (interactive (list (django-test-arguments)))
  (django-test--run-test-command (django-test--generate-class-test-command) args))

(defun django-test-run-test-module (&optional args)
  "Run django test from the current module."
  (interactive (list (django-test-arguments)))
  (django-test--run-test-command (django-test--generate-module-test-command) args))

(defun django-test-run-test-project (&optional args)
  "Run all the tests of the current project."
  (interactive (list (django-test-arguments)))
  (django-test--run-test-command (django-test--generate-project-test-command) args))

(define-infix-argument django-test-runner:--settings ()
  :description "Run with a custom settings module"
  :class 'transient-option
  :shortarg "-s"
  :argument "--settings=")

;;;###autoload
(define-transient-command django-test-runner ()
  "Open django test pop up."
  ["Arguments"
   ("-k" "Preserves the test DB between runs."   ("-k" "--keepdb"))
   ("-n" "Do NOT prompt any user input."         "--no-input")
   ("-f" "Stop at the first failed test."        "--failfast")
   (django-test-runner:--settings)]
  [["Test"
    ("f" "Function"       django-test-run-test-function)
    ("c" "Class"          django-test-run-test-class)
    ("m" "Module"         django-test-run-test-module)
    ("p" "Project"        django-test-run-test-project)]]
  (interactive)
  (transient-setup 'django-test-runner))

(defun django-test-arguments nil
  (transient-args 'django-test-runner))

(provide 'django-test-runner)
;;; django-test-runner.el ends here
