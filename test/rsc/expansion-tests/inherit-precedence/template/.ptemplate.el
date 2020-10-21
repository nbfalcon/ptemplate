;;; .ptemplate.el --- Test inherit precdence -*- lexical-binding: t -*-

;;; Commentary:
;; Ensure that mappings inherited with `ptemplate-inherit' have a lower
;; precedence than ones in the current template. NOTE that "template/file" has
;; text.

;;; Code:

(require 'ptemplate)

(ptemplate! :inherit-rel "../../basic/template/")

;;; .ptemplate.el ends here
