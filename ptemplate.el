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

(require 'cl-lib)

;;; (ptemplate-template-dirs :: [String])
(defcustom ptemplate-template-dirs '()
  "List of directories containing templates.
Analagous to the variable `yas-snippet-dirs'."
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
  "List all templates in `ptemplate-template-dirs'.
The result is an alist ((TYPE (NAME . PATH)...)...)."
  (mapcan #'ptemplate-list-template-dir ptemplate-template-dirs))

(defun ptemplate--list-templates-helm ()
  "Make a list of helm sources from the user's templates.
Gather a list of the user's templates using
`ptemplate-list-templates' and convert each TYPE . TEMPLATES pair
into a helm source with TYPE as its header. Each helm source's
action is to create a new project in a directory prompted from
the user (see `ptemplate-exec-template').

Helm (in particular, helm-source.el) must already be loaded when
this function is called."
  (declare-function helm-make-source "helm" (name class &rest args))
  (cl-loop for entry in (ptemplate-list-templates) collect
           (helm-make-source (car entry) 'helm-source-sync
             :candidates (cdr entry))))

(defun ptemplate-prompt-template-helm ()
  "Prompt for a template using `helm'.
The prompt is a `helm' prompt where all templates are categorized
under their types (as `helm' sources). The return value is the
path to the template, as a string."
  (declare-function helm "helm")
  (require 'helm)
  (helm :sources (ptemplate--list-templates-helm) :buffer "*helm ptemplate*"))

(defface ptemplate-type-face '((t :inherit font-lock-function-name-face))
  "Face used to show template types in for the :completing-read backend.
When :completing-read is used as backend in
`ptemplate-template-prompt-function', all entries have a (TYPE)
STRING appended to it. That TYPE is propertized with this face."
  :group 'ptemplate-faces)

(defun ptemplate--list-templates-completing-read ()
  "Make a `completing-read' collection."
  (cl-loop for heading in (ptemplate-list-templates) nconc
           (let ((category (propertize (format "(%s)" (car heading))
                                       'face 'ptemplate-type-face)))
             (cl-loop for template in (cdr heading) collect
                      (cons (concat (car template) " " category)
                            (cdr template))))))

(defvar ptemplate--completing-read-history nil
  "History variable for `completing-read'-based template prompts.
If :completing-read is set as `ptemplate-template-prompt-function',
pass this variable as history argument to `completing-read'.")

(defun ptemplate-prompt-template-completing-read ()
  "Prompt for a template using `completing-read'.
The prompt is a list of \"NAME (TYPE)\". The return value is the
path to the template, as a string."
  (let ((ptemplates (ptemplate--list-templates-completing-read)))
    (alist-get (completing-read "Select template: " ptemplates
                                nil t nil 'ptemplate--completing-read-history)
               ptemplates nil nil #'string=)))

(defcustom ptemplate-template-prompt-function
  #'ptemplate-prompt-template-completing-read
  "Prompting method to use to read a template from the user.
The function shall take no arguments and return the path to the
template as a string."
  :group 'ptemplate
  :type '(radio
          (const :tag "completing-read (ivy, helm, ...)"
                 #'ptemplate-prompt-template-completing-read)
          (const :tag "helm" #'ptemplate-prompt-template-helm)
          (function :tag "Custom function")))

(defcustom ptemplate-workspace-alist '()
  "Alist mapping between template types and workspace folders."
  :group 'ptemplate
  :type '(alist :key-type (string :tag "Type")
                :value-type (string :tag "Workspace")))

(defcustom ptemplate-default-workspace nil
  "Default workspace for `ptemplate-workspace-alist'.
If looking up a template's type in `ptemplate-workspace-alist'
fails, because there is no corresponding entry, use this as a
workspace instead."
  :group 'ptemplate
  :type 'string)

(defun ptemplate--prompt-target (template)
  "Prompt the user to supply a project directory for TEMPLATE.
The initial directory is looked up based on
`ptemplate-workspace-alist'. TEMPLATE's type is deduced from its
path, which means that it should have been obtained using
`ptemplate-list-templates', or at least be in a template
directory."
  (let* ((base (directory-file-name template))
         (type (file-name-nondirectory (directory-file-name
                                        (file-name-directory base))))
         (workspace (alist-get type ptemplate-workspace-alist
                               ptemplate-default-workspace nil #'string=)))
    (read-file-name "Create project: " workspace workspace)))

;;; (ptemplate--snippet-chain :: (Cons String String) | Buffer)
(defvar-local ptemplate--snippet-chain nil
  "Cons pointer to list of (SNIPPET . TARGET) or BUFFER.
Template directories can have any number of yasnippet files.
These need to be filled in by the user. To do this, there is a
snippet chain: a list of snippets and their target files or
buffers. During expansion of a template directory, first all
snippets are gathered into a list, the first snippet of which is
then shown to the user. If the user presses
\\<ptemplate-snippet-chain-mode-map>
\\[ptemplate-snippet-chain-next], the next item in the snippet
chain is displayed. Buffers are appended to this list when the
user presses \\<ptemplate-snippet-chain-mode-map>
\\[ptemplate-snippet-chain-later].

To facilitate the expansion of multiple templates at once, the
snippet chain must be buffer-local. However, if each buffer has
its own list, updates to it wouldn't be synced across buffers
stored for later finalization. Such buffers would contain already
finalized filenames in their snippet chain. Because of this, a
solution needs to be devised to share a buffer local value
between multiple buffers, and `ptemplate--snippet-chain' works as
follows: This variable actually stores a cons, the `cdr' of which
points to the actual snippet chain, as described above, the `car'
always being ignored. This way (pop (cdr
`ptemplate--snippet-chain')) modifies it in a way that is shared
between all buffers.

See also `ptemplate--snippet-chain-start'.")

(defvar-local ptemplate--snippet-chain-finalize-hook nil
  "Hook to run after the snippet chain finishes.
Each function therein takes no arguments.")

(defvar-local ptemplate--snippet-chain-inherit nil
  "List of variables to pass on to a snippet-chain buffers.
`ptemplate--snippet-chain-finalize-hook',
`ptemplate--snippet-chain-inherit', ... should not be included.")

;;; (ptemplate--read-file :: String -> String)
(defun ptemplate--read-file (file)
  "Read FILE and return its contents a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(define-minor-mode ptemplate-snippet-chain-mode
  "Minor mode for template directory snippets.
This mode is only for keybindings."
  :init-value nil
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'ptemplate-snippet-chain-next)
            (define-key map (kbd "C-c C-l") #'ptemplate-snippet-chain-later)
            map))

(defun ptemplate--snippet-chain-continue ()
  "Make the next snippt/buffer in the snippet chain current."
  (require 'yasnippet)
  (declare-function yas-minor-mode "yasnippet" (&optional arg))
  (declare-function yas-expand-snippet "yasnippet" (s &optional start end env))
  ;; the snippet chain is a cons abused as a pointer: car is never used, while
  ;; cdr is modified; the cons can be shared between multiple buffers, sharing
  ;; the actual payload (which is always in the cdr). (See
  ;; `ptemplate--snippet-chain' for details).
  (let ((next (pop (cdr ptemplate--snippet-chain))))
    (cond
     ((null next) (run-hooks 'ptemplate--snippet-chain-finalize-hook))
     ((bufferp next) (switch-to-buffer next))
     ((consp next)
      (let ((oldbuf (current-buffer))
            (next-file (cdr next))
            (source-file (car next)))
        (find-file next-file)

        ;; inherit snippet chain variables

        ;; NOTE: `ptemplate--snippet-chain', ... are `defvar-local', so need not
        ;; be made buffer-local.
        (mapc #'make-variable-buffer-local ptemplate--snippet-chain-inherit)
        (dolist (sym (append '(ptemplate--snippet-chain
                               ptemplate--snippet-chain-inherit
                               ptemplate--snippet-chain-finalize-hook)
                             ptemplate--snippet-chain-inherit))
          (set sym (buffer-local-value sym oldbuf)))

        (ptemplate-snippet-chain-mode 1)
        (yas-minor-mode 1)
        (yas-expand-snippet (ptemplate--read-file source-file)))))))

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
  ;; snippet chain cannot be nil, so nconc will append to it, modifying it
  ;; across all buffers.
  (nconc ptemplate--snippet-chain (list (current-buffer)))
  (ptemplate--snippet-chain-continue))

(defun ptemplate--snippet-chain-start (snippets &optional inherit finalize-hook)
  "Start a snippet chain with SNIPPETS.
For details, see `ptemplate--snippet-chain'.

INHERIT (a list of symbols) specifies the variables to set in
each new buffer. Their values are taken from the current
environment (you can let-bind them) and passed on to each new
buffer.

FINALIZE-HOOK is called when the snippet chain finishes (see
`ptemplate--snippet-chain-finalize-hook')."
  (let ((ptemplate--snippet-chain (cons 'snippet-chain snippets))
        (ptemplate--snippet-chain-inherit inherit)
        (ptemplate--snippet-chain-finalize-hook finalize-hook))
    (ptemplate--snippet-chain-continue)))

(defvar ptemplate--before-expand-hook nil
  "Hook run before expanding yasnippets.
Each function therein shall take no arguments.

These variables are hooks to allow multiple ptemplate! blocks
that specify :before-yas, :after, ....")

(defvar ptemplate--after-copy-hook nil
  "Hook run after copying files.

See also `ptemplate--before-expand-hook'.")

(defvar ptemplate--finalize-hook nil
  "Hook to run after template expansion finishes.
At this point, no more files need to be copied and no more
snippets need be expanded.

See also `ptemplate--before-expand-hooks'.")

(defvar-local ptemplate--snippet-env nil
  "Environment used for snippet expansion.
This variable has the form of expand-env in
`yas-expand-snippet'.")

(defvar-local ptemplate-target-directory nil
  "Target directory of ptemplate expansion.
You can use this in templates. This variable always ends in the
platform-specific directory separator, so you can use this with
`concat' to build file paths.")

(defvar-local ptemplate-source-directory nil
  "Source directory of ptemplate expansion.
Akin to `ptemplate-source-directory'.")
;;; (ptemplate--yasnippet-p :: String -> Bool)
(defun ptemplate--yasnippet-p (file)
  "Check if FILE has a yasnippet extension and nil otherwise."
  (string-suffix-p ".yas" file))

;;; (ptemplate-expand-template :: String -> String)
;;;###autoload
(defun ptemplate-expand-template (source target)
  "Expand the template in SOURCE to TARGET.
If called interactively, SOURCE is prompted using
`ptemplate-template-prompt-function'. TARGET is prompted using
`read-file-name', with the initial directory looked up in
`ptemplate-workspace-alist' using SOURCE's type, defaulting to
`ptemplate-default-workspace'. If even that is nil, use
`default-directory'."
  (interactive (let ((template (funcall ptemplate-template-prompt-function)))
                 (list template (ptemplate--prompt-target template))))
  (when (file-directory-p target)
    ;; NOTE: the error message should mention the user-supplied target (not
    ;; necessarily with a slash at the end), so do this buffer
    ;; (file-name-as-directory).
    (user-error "Directory %s already exists" target))
  (make-directory target t)

  (setq target (file-name-as-directory target))
  (setq source (file-name-as-directory source))

  (let ((dotptemplate (concat source ".ptemplate.el"))
        (ptemplate--before-expand-hook)
        (ptemplate--after-copy-hook)
        (ptemplate--finalize-hook)
        (ptemplate--snippet-env)

        ;; the dotptemplate file should know about source and target.
        (ptemplate-source-directory source)
        (ptemplate-target-directory target))
    (when (file-exists-p dotptemplate)
      ;; NOTE: arbitrary code execution
      (load-file dotptemplate))

    ;; This way, all template files will begin with ./, making them easier to
    ;; copy to target (just concat target file).

    ;; We can't merge the two lets because the dotptemplate file must be eval'd
    ;; in the context of the calling buffer, without default-directory being
    ;; modified.
    (let* ((default-directory source)
           (files (directory-files-recursively "." "" t)))
      ;; make directories
      (cl-loop for file in files do
               (unless (file-directory-p file)
                 (setq file (file-name-directory file)))
               (make-directory (concat target file) t))

      ;; directories were already made
      (setq files (cl-delete-if #'file-directory-p files))
      ;; don't copy the dotptemplate file
      (setq files (cl-delete "./.ptemplate.el" files :test #'string=))

      (let ((yasnippets
             (cl-loop
              for file in files if (ptemplate--yasnippet-p file) collect
              (cons (concat source file)
                    (concat target (file-name-sans-extension file)))))
            (normal-files (cl-delete-if #'ptemplate--yasnippet-p files)))
        (dolist (file normal-files)
          (cond ((string-suffix-p ".keep" file)
                 (copy-file
                  file (concat target (file-name-sans-extension file))))
                (t (copy-file file (concat target file)))))
        (run-hooks 'ptemplate--after-copy-hook)

        (run-hooks 'ptemplate--before-expand-hook)
        (when yasnippets
          (let ((snippet-env-vars (mapcar #'car ptemplate--snippet-env))
                (snippet-env-values (mapcar #'cdr ptemplate--snippet-env)))
            (cl-progv (mapcar #'car ptemplate--snippet-env) snippet-env-values
              (ptemplate--snippet-chain-start
               yasnippets
               (nconc snippet-env-vars
                      '(ptemplate-source-directory ptemplate-target-directory))
               ptemplate--finalize-hook))))))))

(defmacro ptemplate! (&rest args)
  "Define a smart ptemplate with elisp.
For use in .ptemplate.el files. ARGS is a plist-like list with
any number of sections, specfied as :<section name> FORM... (like
in `use-package'). Sections can appear multiple times: you could,
for example, have multiple :init sections, the FORMs of which
would get evaluated in sequence. Supported keyword are:

:init FORMs to run before expansion. This is the default when no
      section is specified.

:before-snippets FORMs to run before expanding yasnippets.

:after-copy FORMs to run after all files have been copied. The
            ptemplate's snippets need not have been expanded
            already.

:finalize FORMs to run after expansion finishes.

:snippet-env variables to make available in snippets. Their
             values are examined at the end of `ptemplate!' and
             stored. Each element after :env shall be a symbol or
             a list of the form (SYMBOL VALUEFORM), like in
             `let'. the SYMBOLs should not be quoted.

Note that because .ptemplate.el files execute arbitrary code, you
could write them entirely without using this macro (e.g. by
modifying hooks directly, ...). However, you should still use
`ptemplate!', as this makes templates more future-proof and
readable."
  (let ((cur-keyword :init)
        (result)
        (before-yas-eval)
        (after-copy-eval)
        (finalize-eval)
        (snippet-env))
    (dolist (arg args)
      (if (keywordp arg)
          (setq cur-keyword arg)
        (pcase cur-keyword
          (:init (push arg result))
          (:before-snippets (push arg before-yas-eval))
          (:after-copy (push arg after-copy-eval))
          (:finalize (push arg finalize-eval))
          (:snippet-env (push arg snippet-env)))))
    (macroexp-progn
     (nconc
      (nreverse result)
      (when before-yas-eval
        `((add-hook 'ptemplate--before-expand-hook
                    (lambda () "Run before expanding snippets."
                      ,@(nreverse before-yas-eval)))))
      (when after-copy-eval
        `((add-hook 'ptemplate--after-copy-hook
                    (lambda () "Run after copying files."
                      ,@(nreverse after-copy-eval)))))
      (when finalize-eval
        `((add-hook 'ptemplate--finalize-hook
                    (lambda () "Run after template expansion finishes."
                      ,@(nreverse finalize-eval)))))
      (when snippet-env
        `((setq
           ptemplate--snippet-env
           (nconc
            ptemplate--snippet-env
            (list ,@(cl-loop
                     for var in snippet-env collect
                     (if (listp var)
                         (list #'cons (macroexp-quote (car var)) (cadr var))
                       `(cons ',var ,var))))))))))))

(provide 'ptemplate)
;;; ptemplate.el ends here
