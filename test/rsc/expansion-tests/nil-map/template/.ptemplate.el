;;; .ptemplate.el --- Test nil maps -*- lexical-binding: t -*-

;;; Commentary:
;; This test ensures that `ptemplate-map' to nil works correctly, even with
;; inheritance.

;;; Code:

(require 'ptemplate)

(ptemplate! :map (nil "/file")
            :inherit (ptemplate-source "../../basic/template/"))

;;; .ptemplate.el ends here
