;;; ptemplate-test.el --- ptemplate's ert test suite -*- lexical-binding: t -*-

;;; Commentary:
;; ptemplate's ert-based test suite. Run with 'cask exec ert-runner'.

;;; Code:

(require 'ptemplate)
(require 'ert)

;;; `defvar'
(defvar ert-runner-test-path)

(defun ptemplate-test-rsc (path)
  "Yield PATH relative to this module."
  (expand-file-name path ert-runner-test-path))

(defun ptemplate-test-list-test-templates ()
  "List the test template directory."
  (ptemplate-list-template-dir (ptemplate-test-rsc "rsc/test-templates")))

(ert-deftest ptemplate-template-dir-with-file ()
  "Template directories can contain files."
  (ptemplate-test-list-test-templates))

;; (ert-deftest ptemplate-prompt ()
;;   "Test the prompt functions.
;; The prompt functions are currently
;; `ptemplate-prompt-template-completing-read' and
;; `ptemplate-prompt-template-helm'."
;;   (let ((test-templates (ptemplate-test-list-test-templates)))
;;     (funcall #'ptemplate-prompt-template-helm test-templates)
;;     (funcall #'ptemplate-prompt-template-completing-read test-templates)))

(provide 'ptemplate-test)
;;; ptemplate-test.el ends here
