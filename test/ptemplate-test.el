;;; ptemplate-test.el --- ptemplate's ert test suite -*- lexical-binding: t -*-

;;; Commentary:
;; ptemplate's ert-based test suite. Run with 'cask exec ert-runner'.

;;; Code:

(require 'ptemplate)
(require 'ert)
(eval-when-compile (require 'cl-lib))

;;; `defvar' declare
(defvar ert-runner-test-path)

(defun ptemplate-test--rsc (path)
  "Yield PATH relative to test/rsc."
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
       ;; NOTE this should be safe, as --ptemplate-test-temp-dir-- wouldn't ever
       ;; be modified; however, `delete-directory' recursively is still scary.
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

(defmacro ptemplate-test--with-fatal-dup-warning (&rest body)
  "Fail if a template expansion has duplicate warnings.
Execute BODY and fail the current `ert-deftest' if a template
expansion within it produces warnings about duplicate file
mappings. Return the value of the last BODY form."
  `(cl-letf (((symbol-function #'ptemplate--warn-dup-mapping)
              (lambda (prev cur)
                (ert-fail (format "duplicate mapping: \"%S\" before \"%S\""
                                  prev cur)))))
     ,@body))

(ert-deftest ptemplate-expansion ()
  "Verify that the expansion-test templates expand correctly.
See test/rsc/expansion-tests/README.md for details."
  (let* ((test-templates
          (cl-delete-if-not
           #'file-directory-p
           (ptemplate--list-dir-dirs (ptemplate-test--rsc "expansion-tests"))))
         (failed-templates (ptemplate-test--with-fatal-dup-warning
                            (cl-delete-if
                             #'ptemplate-test--expansion test-templates)))
         (failed-expansions (mapcar #'file-name-nondirectory failed-templates)))
    (should (eq failed-expansions nil))))

;; (ert-deftest ptemplate-prompt ()
;;   "Test the prompt functions.
;; The prompt functions are currently
;; `ptemplate-prompt-template-completing-read' and
;; `ptemplate-prompt-template-helm'."
;;   (let ((test-templates (ptemplate-test--list-test-templates)))
;;     (funcall #'ptemplate-prompt-template-helm test-templates)
;;     (funcall #'ptemplate-prompt-template-completing-read test-templates)))

(provide 'ptemplate-test)
;;; ptemplate-test.el ends here
