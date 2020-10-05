;;; .ptemplate.el --- Simply inherit test template -*- lexical-binding: t -*-

;;; Commentary:
;; This expanion test tests ptemplate's inheritance facility.

;;; Code:

(require 'ptemplate)

(ptemplate! :inherit (ptemplate-source "../../basic/template/"))

;;; .ptemplate.el ends here
