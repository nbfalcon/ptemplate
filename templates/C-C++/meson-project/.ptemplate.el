;;; .ptemplate.el --- C/C++ Meson project .ptemplate -*- lexical-binding: t -*-

;;; Commentary:

;; C/C++ (prompted) meson project template.

;;; Code:

;; Silence warnings: 'ptemplate is always provided before expansion. Also needed
;; for byte-compilation.
(require 'ptemplate)

(ptemplate!
 :ignore "\\.gitkeep" "/README.md"
 :snippet-let
 (ptemplate-var-language
  (completing-read "Select a language: " '("c" "cpp") nil t))
 (ptemplate-var-main-file (format "src/main.%s" ptemplate-var-language))
 :remap ("/src/main.c.yas" ptemplate-var-main-file)
 :open ptemplate-var-main-file)

;;; .ptemplate.el ends here
