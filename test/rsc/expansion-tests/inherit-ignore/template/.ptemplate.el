;;; .ptemplate.el --- `ptemplate-ignore' + inherit -*- lexical-binding: t -*-

;;; Commentary:
;; Test `ptemplate-ignore' after a nested `ptemplate-inherit'.

;;; Code:

(require 'ptemplate)

(ptemplate! :init
            (ptemplate-inherit (ptemplate-source "../../inherit/template"))
            (ptemplate-ignore "\\.gitkeep"))

;;; .ptemplate.el ends here
