;;; helm-ptemplate.el --- Ptemplate helm interface -*- lexical-binding: t -*-

;; Copyright (C) 2020  Nikita Bloshchanevich

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Nikita Bloshchanevich <nikblos@outlook.com>
;; URL: https://github.com/nbfalcon/ptemplate
;; Package-Requires: ((emacs "25.1") (helm "3.0.0"))
;; Version: 0.1

;;; Commentary:
;; Provides a helm interface for creating projects based on templates.

;;; Code:

(require 'ptemplate)
(require 'helm)

(defun helm-ptemplate-make-helm-sources ()
  "Make a list of helm sources from the user's templates.
Gather a list of the user's templates using
`ptemplate-list-templates' and convert each TYPE . TEMPLATES pair
into a helm source with TYPE as its header. Each helm source's
action is to create a new project in a directory prompted from
the user (see `ptemplate-exec-template')."
  (cl-loop for entry in (ptemplate-list-templates) collect
           (helm-build-sync-source (car entry) :candidates (cdr entry)
                                   :action #'ptemplate-exec-template)))

;;;###autoload
(defun helm-ptemplate-new-project ()
  "Create a new project using a template that is prompted."
  (interactive)
  (helm :sources (helm-ptemplate-make-helm-sources)
        :buffer "*helm ptemplate*"))

(provide 'helm-ptemplate)
;;; helm-ptemplate.el ends here
