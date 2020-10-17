;;; .ptemplate.el --- Test `ptemplate-remap-rec' -*- lexical-binding: t -*-

;;; Commentary:
;; Test `ptemplate-remap-rec' by remapping root ("/") to a different directory.

;;; Code:

(require 'ptemplate)

(ptemplate! :init (ptemplate-remap-rec "/" "/dir"))

;;; .ptemplate.el ends here
