;;; ptemplate.el --- Project templates -*- lexical-binding: t -*-

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
;; Package-Requires: ((emacs "25.1") (yasnippet "0.13.0"))
;; Version: 0.1

;;; Commentary:
;; Creating projects can be a lot of work. Cask files need to be set up, a
;; License file must be added, maybe build system files need to be created. A
;; lot of that can be automated, which is what ptemplate does. You can create a
;; set of templates categorized by type/template like in eclipse, and ptemplate
;; will then initialize the project for you. In the template you can have any
;; number of yasnippets or normal files.

;; Security note: yasnippets allow arbitrary code execution, as do .ptemplate.el
;; files. DO NOT EXPAND UNTRUSTED PTEMPLATES. Ptemplate DOES NOT make ANY
;; special effort to protect against malicious templates.

;;; Code:

(require 'yasnippet)
(require 'cl-lib)

;;; (ptemplate--read-file :: String -> String)
(defun ptemplate--read-file (file)
  "Read FILE and return its contents a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;; (ptemplate--snippet-chain :: (Cons String String) | Buffer)
(defvar ptemplate--snippet-chain nil
  "List of (SNIPPET . TARGET) or BUFFER.
Template directories can have any number of yasnippet files.
These need to be filled in by the user. To do this, there is a
snippet chain: a list of snippets and their target files or
buffers. During expansion of a template directory, first all
snippets are gathered into a list, the first snippet of which is
then shown to the user. If the user presses
\\<ptemplate--snippet-chain-mode-map>
\\[ptemplate-snippet-chain-next], the next item in the snippet
chain is displayed. Buffers are appended to this list when the
user presses \\<ptemplate--snippet-chain-mode-map>
\\[ptemplate-snippet-chain-later].")

(defun ptemplate--snippet-chain-continue ()
  "Make the next snippt/buffer in the snippet chain current."
  (when-let ((next (pop ptemplate--snippet-chain)))
    (if (bufferp next)
        (switch-to-buffer next)
      (find-file (cdr next))
      (ptemplate--snippet-chain-mode 1)
      (yas-minor-mode 1)
      (yas-expand-snippet (ptemplate--read-file (car next))))))

(defun ptemplate-snippet-chain-next ()
  "Save the current buffer and continue in the snippet chain.
The buffer is killed after calling this. If the snippet chain is
empty, do nothing."
  (interactive)
  (save-buffer 0)
  (let ((old-buf (current-buffer)))
    (ptemplate--snippet-chain-continue)
    (kill-buffer old-buf)))

(defun ptemplate-snippet-chain-later ()
  "Save the current buffer to be expanded later.
Use this if you are not sure yet what expansions to use in the
current snipept and want to decide later, after looking at
others."
  (interactive)
  (unless ptemplate--snippet-chain
    (user-error "No more snippets to expand"))
  (nconc ptemplate--snippet-chain (list (current-buffer)))
  (ptemplate--snippet-chain-continue))

(define-minor-mode ptemplate--snippet-chain-mode
  "Minor mode for template directory snippets.
This mode is only for keybindings."
  :init-value nil
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'ptemplate-snippet-chain-next)
            (define-key map (kbd "C-c C-l") #'ptemplate-snippet-chain-later)
            map))

;;; (ptemplate--start-snippet-chain :: [Cons String String | Buffer])
(defun ptemplate--start-snippet-chain (snippets)
  "Start a snippet chain with SNIPPETS.
For details, see `ptemplate--snippet-chain'."
  (let ((first (pop snippets)))
    (find-file (cdr first))
    (yas-minor-mode 1)
    (yas-expand-snippet (ptemplate--read-file (car first)))
    (setq ptemplate--snippet-chain snippets))
  (ptemplate--snippet-chain-mode 1))

(defvar ptemplate-target-directory nil
  "Target directory of ptemplate expansion.
You can use this in templates. This variable always ends in the
platform-specific directory separator, so you can use this with
concat to build file paths.")

(defvar ptemplate-source-directory nil
  "Source directory of ptemplate expansion.
Akin to `ptemplate-source-directory'.")

(defmacro ptemplate! (&rest args)
  "Define a smart ptemplate with elisp.
For use in .ptemplate.el files. ARGS is a plist-like list with
any number of sections, specfied as :<section name> FORM... (like
in `use-package'). Sections can appear multiple times: you could,
for example, have multiple :init sections, the FORMs of which
would get evaluated in sequence. Supported keyword are:

:init FORMs to run before expansion.

:before-snippets FORMs to run before expanding yasnippets. Use
                 this if you need to ask the user questions that
                 could influence yasnippet expansion, but that
                 shouldn't block file copying.

:after FORMs to run after all files have been copied. The
             ptemplate's snippets need not have been expanded
             already.

Note that because .ptemplate.el files execute arbitrary code, you
could write them entirely without using this macro (e.g. by
modifying hooks directly, ...). However, you should still use
`ptemplate!', as this makes templates more future-proof and
readable."
  (let ((cur-keyword)
        (result)
        (before-yas-eval)
        (after-expand-eval))
    (dolist (arg args)
      (if (keywordp arg)
          (setq cur-keyword arg)
        (pcase cur-keyword
          (:init (push arg result))
          (:before-snippets (push arg before-yas-eval))
          (:after (push arg after-expand-eval)))))
    (macroexp-progn
     (nconc (nreverse result)
            (when before-yas-eval
              `((setq ptemplate--before-yas-eval
                      ',(macroexp-progn (nreverse before-yas-eval)))))
            (when after-expand-eval
              `((setq ptemplate--after-expand-eval
                      ',(macroexp-progn (nreverse after-expand-eval)))))))))

;;; (ptemplate--yasnippet-p :: String -> Bool)
(defun ptemplate--yasnippet-p (file)
  "Check if FILE has a yasnippet extension and nil otherwise."
  (string= (file-name-extension file) "yas"))

(defvar ptemplate--before-yas-eval nil
  "Expression `eval'ed before expanding yasnippets.")

(defvar ptemplate--after-expand-eval nil
  "Expression `eval'ed after all files have been copied.
The user probably won't have filled in all snippets before
this is expanded.")

;;; (ptemplate-expand-template :: String -> String)
(defun ptemplate-expand-template (dir target)
  "Expand the template in DIR to TARGET."
  (when (file-directory-p target)
    (user-error "Directory %s already exists" target))
  (make-directory target t)

  (setq target (file-name-as-directory target))
  (setq dir (file-name-as-directory dir))

  (setq ptemplate-target-directory target)
  (setq ptemplate-source-directory dir)
  ;; arbitrary code execution: don't expand untrusted templates
  (let ((dotptemplate (concat dir ".ptemplate.el"))
        (ptemplate--before-yas-eval)
        (ptemplate--after-expand-eval))
    (when (file-exists-p dotptemplate)
      (load-file dotptemplate))

    (with-temp-buffer
      ;; this way, all template files will begin with ./, making them easier to
      ;; copy to target (just concat target file).
      (cd dir)
      (let ((files (directory-files-recursively "." "" t)))
        ;; make directories
        (cl-loop for file in files do
                 (unless (file-directory-p file)
                   (setq file (file-name-directory file)))
                 (make-directory (concat target file) t))
        (setq files (cl-delete-if #'file-directory-p files))
        (setq files (cl-delete "./.ptemplate.el" files :test #'string=))

        (let ((yasnippets
               (cl-loop
                for file in files if (ptemplate--yasnippet-p file) collect
                (cons (concat dir file)
                      (concat target (file-name-sans-extension file)))))
              (normal-files (cl-delete-if #'ptemplate--yasnippet-p files)))
          (eval ptemplate--before-yas-eval)
          (when yasnippets
            (ptemplate--start-snippet-chain yasnippets))

          (dolist (file normal-files)
            (if (string-suffix-p ".keep" file)
                (copy-file file (concat target (file-name-sans-extension file)))
              (copy-file file (concat target file))))))
      (eval ptemplate--after-expand-eval))))

;;; (ptemplate-template-dirs :: [String])
(defcustom ptemplate-template-dirs '()
  "List of directories containing templates.
Analagous to `yas-snippet-dirs'."
  :group 'ptemplate
  :type '(repeat string))

;;; (ptemplate-find-template :: String -> [String])
(defun ptemplate-find-templates (template)
  "Find TEMPLATE in `ptemplate-template-dirs'.
Template shall be a path of the form \"category/type\". Returns a
list of full paths to the template directory specified by
TEMPLATE. Returns the empty list if TEMPLATE cannot be found."
  (let ((template (file-name-as-directory template))
        (result))
    (dolist (dir ptemplate-template-dirs)
      (let ((template-dir (concat (file-name-as-directory dir) template)))
        (when (file-directory-p template-dir)
          (push template-dir result))))
    (nreverse result)))

(defun ptemplate-find-template (template)
  "Find TEMPLATE in `ptemplate-template-dirs'.
Unlike `ptemplate-find-templates', this function does not return
all occurrences, but only the first."
  (catch 'result
    (dolist (dir ptemplate-template-dirs)
      (let ((template-dir (concat (file-name-as-directory dir) template)))
        (when (file-directory-p template-dir)
          (throw 'result template-dir))))))

(defun ptemplate--list-dir (dir)
  "List DIR, including directories.
A list of the full paths of each element is returned. The special
directories \".\" and \"..\" are ignored."
  (cl-delete-if (lambda (f) (or (string-suffix-p "/." f)
                                (string-suffix-p "/.." f)))
                (directory-files dir t)))

(defun ptemplate-list-template-dir (dir)
  "List all templates in directory DIR.
The result is of the form (TYPE ((NAME . PATH)...))...."
  (let* ((type-dirs (ptemplate--list-dir dir))
         (types (mapcar #'file-name-base type-dirs))
         (name-dirs (cl-loop for tdir in type-dirs collect
                             (ptemplate--list-dir tdir)))
         (name-dir-pairs (cl-loop for name-dir in name-dirs collect
                                  (cl-loop for dir in name-dir collect
                                           (cons (file-name-base dir) dir)))))
    (cl-mapcar #'cons types name-dir-pairs)))

(defun ptemplate-list-templates ()
  "List all templates that user has stored.
The result is an alist ((TYPE (NAME . PATH)...)...)."
  (mapcan #'ptemplate-list-template-dir ptemplate-template-dirs))

(defcustom ptemplate-workspace-alist '()
  "Alist mapping between template types and workspace folders."
  :group 'ptemplate
  :type '(alist :key-type (string :tag "Type")
                :value-type (string :tag "Workspace")))

(defun ptemplate-exec-template (template)
  "Expand TEMPLATE in a user-selected directory.
The initial directory is looked up based on
`ptemplate-workspace-alist'. TEMPLATE's type is deduced from its
path, which means that it should have been obtained using
`ptemplate-list-templates', or at least be in a template
directory."
  (let* ((base (directory-file-name template))
         (type (file-name-nondirectory (directory-file-name
                                        (file-name-directory base))))
         (workspace (alist-get type ptemplate-workspace-alist nil nil
                               #'string=))
         (target (read-file-name "Create project: " workspace workspace)))
    (ptemplate-expand-template template target)))

(provide 'ptemplate)
;;; ptemplate.el ends here
