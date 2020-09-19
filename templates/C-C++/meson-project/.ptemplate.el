;; silence warnings; 'ptemplate is always provide'd before expansion; also, this
;; is needed for compiling.
(require 'ptemplate)

(ptemplate!
 :ignore "\\.gitkeep" "/README.md"
 :snippet-let
 (ptemplate-var-language
  (completing-read "Select a language: " '("c" "cpp") nil t))
 (ptemplate-var-main-file (format "src/main.%s" ptemplate-var-language))
 :remap ("/src/main.c.yas" ptemplate-var-main-file)
 :finalize (find-file (ptemplate-target ptemplate-var-main-file)))
