;;; .ptemplate.el --- Test `ptemplate-automap' -*- lexical-binding: t -*-

;;; Commentary:
;; Ensure that `ptemplate!' + :automap works correctly by ignoring root and
;; automapping a file.

;;; Code:

(require 'ptemplate)

(ptemplate! :ignore "/" :automap "/file")

;;; .ptemplate.el ends here
