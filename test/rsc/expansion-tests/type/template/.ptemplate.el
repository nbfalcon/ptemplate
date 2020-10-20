;;; .ptemplate.el --- Test `ptemplate-map' + type -*- lexical-binding: t -*-

;;; Commentary:
;; Ensure that `ptemplate-map''s type specification is respected and that
;; ignoring root works.

;;; Code:

(require 'ptemplate)

(ptemplate! :ignore "/" :map ("/nonsnippet.autoyas" "/snippet.autoyas" :copy))

;;; .ptemplate.el ends here
