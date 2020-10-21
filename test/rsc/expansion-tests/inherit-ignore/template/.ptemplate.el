;;; .ptemplate.el --- `ptemplate-ignore' + inherit -*- lexical-binding: t -*-

;;; Commentary:
;; Test `ptemplate-ignore' after a nested `ptemplate-inherit'.

;;; Code:

(require 'ptemplate)

(ptemplate! :inherit-rel "../../inherit/template"
            :late (ptemplate-ignore "file"))

;;; .ptemplate.el ends here
