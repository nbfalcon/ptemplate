;;; ptemplate-test.el --- ptemplate's ert test suite -*- lexical-binding: t -*-

;;; Commentary:
;; ptemplate's ert-based test suite. Run with 'cask exec ert-runner'.

;;; Code:

(require 'ptemplate)
(require 'ert)
(require 'cl-lib)

;;; `defvar' declare
(defvar ert-runner-test-path)

(defun ptemplate-test--rsc (path)
  "Expand PATH relative to \"test/rsc\"."
  (expand-file-name (concat (file-name-as-directory "rsc") path)
                    ert-runner-test-path))

(defun ptemplate-test--list-test-templates ()
  "List the test template directory."
  (ptemplate-list-template-dir (ptemplate-test--rsc "test-templates")))

(ert-deftest ptemplate-template-dir-with-file ()
  "Template directories can contain files."
  (ptemplate-test--list-test-templates))

(defmacro ptemplate-test--with-temp-dir (&rest body)
  "Execute BODY in a temporary directory.
Create a temporary directory and evaluate BODY in it. The
temporary directory is bound to `default-directory' and safely
deleted after executing BODY \(recursively\), even if it throws
an error. Return the result of the last BODY form."
  (declare (indent 0))
  `(let ((--ptemplate-test-temp-dir-- (make-temp-file "ptemplate-test" t)))
     (unwind-protect
         (let ((default-directory --ptemplate-test-temp-dir--)) ,@body)
       ;; This should be safe, as --ptemplate-test-temp-dir-- wouldn't ever be
       ;; modified; however, `delete-directory' recursively is still scary.
       (delete-directory --ptemplate-test-temp-dir-- t))))

(defun ptemplate-test--cmpdir (a b)
  "Check if directories A and B are recursively equal.
Return t if that is the case and nil otherwise."
  (= 0 (call-process-shell-command
        (format "%s --recursive --brief -- %s %s"
                (or (getenv "PTEMPLATE_TEST_DIFF_CMD") "diff")
                (shell-quote-argument a) (shell-quote-argument b)))))

(defun ptemplate-test--expansion (dir)
  "Ensure that the template in DIR expands properly.
DIR shall specify a directory consisting of two directories:
template/ and result/. If template/ expands to a directory not
equivalent to result/, return nil and t otherwise."
  (let ((template (expand-file-name
                   (concat (file-name-as-directory dir) "template/")))
        (result (expand-file-name
                 (concat (file-name-as-directory dir) "result/")))
        (expand-dir "./expansion"))
    (ptemplate-test--with-temp-dir
      (ptemplate-expand-template template expand-dir)
      (ptemplate-test--cmpdir expand-dir result))))

(defun ptemplate-test--lwarn-fail (type level message &rest args)
  "An `lwarn' replacement that fails the current `ert-deftest'.
TYPE, LEVEL, MESSAGE and ARGS are the same as in `lwarn', except
that they are used to generate a failure message instead."
  (ert-fail (format "`lwarn' called: %S %s: %s" type level
                    (apply #'format message args))))

(defmacro ptemplate-test--with-fatal-warnings (&rest body)
  "Execute BODY and fail the test if it throws warnings.
Return the result of the last BODY expression.

Currently, only `lwarn' is handled."
  `(cl-letf (((symbol-function #'lwarn) #'ptemplate-test--lwarn-fail))
     ,@body))

(ert-deftest ptemplate-expansion ()
  "Verify that the expansion-test templates expand correctly.
See test/rsc/expansion-tests/README.md for details."
  (let* ((test-templates
          (cl-delete-if-not
           #'file-directory-p
           (ptemplate--list-dir-dirs (ptemplate-test--rsc "expansion-tests"))))
         (failed-templates
          (ptemplate-test--with-fatal-warnings
           (cl-delete-if #'ptemplate-test--expansion test-templates)))
         (failed-expansions (mapcar #'file-name-nondirectory failed-templates)))
    (should (eq failed-expansions nil))))

(defconst ptemplate-test-prompt-template-functions
  '(ptemplate-prompt-template-completing-read ptemplate-prompt-template-helm)
  "A list of functions for prompting templates.
See the test `ptemplate-prompt' for details.")

(ert-deftest ptemplate-prompt ()
  "A smoketest for the prompt functions.
Call each function in `ptemplate-test-prompt-template-functions'
with a list of templates and virtually simulate RET after each
call, which should hopefully not result in an error."
  (let ((test-templates (ptemplate-test--list-test-templates)))
    (dolist (prompt-fn ptemplate-test-prompt-template-functions)
      (execute-kbd-macro
       (kbd "RET") 1 (lambda () (funcall prompt-fn test-templates))))))

(provide 'ptemplate-test)
;;; ptemplate-test.el ends here
