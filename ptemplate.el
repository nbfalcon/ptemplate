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
(require 'subr-x)

;;; snippet-chain subsystem
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
Each function therein gets called without arguments.

This hook is a snippet-env variable and not simply appended to
the list, as it would be executed *before* the end of snippet
expansion if `ptemplate-snippet-chain-later' is called during
expansion.")

(defvar-local ptemplate--snippet-chain-env nil
  "List of variables to set in snippet-chain buffers.
Alist of (SYMBOL . VALUE).
`ptemplate--snippet-chain-finalize-hook',
`ptemplate--snippet-chain-env', ... should not be included. All
variables will be made buffer-local before being set, so
`defvar-local' is not necessary.")

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

(defun ptemplate--setup-snippet-env (snippet-env)
  "Set all \(SYMBOL . VALUE\) pairs in SNIPPET-ENV.
Variables are set buffer-locally."
  ;; setup snippet env
  (cl-loop for (sym . val) in snippet-env do
           (set (make-local-variable sym) val)))

(defun ptemplate--snippet-chain-continue ()
  "Make the next snippt/buffer in the snippet chain current."
  (require 'yasnippet)
  (declare-function yas-minor-mode "yasnippet" (&optional arg))
  (declare-function yas-expand-snippet "yasnippet" (s &optional start end env))

  ;; the actual payload (which is always in the cdr). (See
  ;; `ptemplate--snippet-chain' for details).
  (let* ((realchain (cdr ptemplate--snippet-chain))
         (next (car realchain)))
    (when realchain
      ;; if the snippet chain is empty, pop fails.
      (pop (cdr ptemplate--snippet-chain)))

    (cond
     ((null next) (run-hooks 'ptemplate--snippet-chain-finalize-hook))
     ((bufferp next) (switch-to-buffer next))
     ((consp next)
      (let ((oldbuf (current-buffer))
            (next-file (cdr next))
            (source-file (car next)))
        (with-current-buffer (find-file-noselect next-file)
          ;; Inherit snippet chain variables. NOTE: `ptemplate--snippet-chain',
          ;; ... are `defvar-local', so need not be made buffer-local.
          (dolist (sym '(ptemplate--snippet-chain
                         ptemplate--snippet-chain-env
                         ptemplate--snippet-chain-finalize-hook))
            (set sym (buffer-local-value sym oldbuf)))
          (ptemplate--setup-snippet-env ptemplate--snippet-chain-env)

          ;; "yasnippet needs a properly set-up `yas-minor-mode'"
          (yas-minor-mode 1)
          (yas-expand-snippet (ptemplate--read-file source-file) nil nil)

          (ptemplate-snippet-chain-mode 1)
          (pop-to-buffer-same-window (current-buffer))))))))

(defun ptemplate-snippet-chain-next ()
  "Save the current buffer and continue in the snippet chain.
The buffer is killed after calling this. If the snippet chain is
empty, do nothing."
  (interactive)
  (save-buffer 0)
  (if (cdr ptemplate--snippet-chain)
      (let ((old-buf (current-buffer)))
        ;; mitigate "flickering" to old buffer; first, acquire the new one and
        ;; then kill the old one.
        (ptemplate--snippet-chain-continue)
        (kill-buffer old-buf))
    ;; EDGE CASE: if no buffer follows, `ptemplate--finalize-hook' must be
    ;; run, but at the *very* end, meaning this buffer must already be dead by
    ;; then and nothing must happen after. The following issue prompted this:
    ;; if the last snippet chain buffer's target file is opened in `find-file'
    ;; in `ptemplate!''s :finalize block, it would not show up, as `find-file'
    ;; will find the snippet chain buffer, which gets killed. (in: C/C++ meson
    ;; project).
    (let ((finalize ptemplate--snippet-chain-finalize-hook)
          (env ptemplate--snippet-chain-env))
      (kill-buffer)
      ;; we cannot use `ptemplate--setup-snippet-env', since this isn't a
      ;; snippet chain buffer, so we must resort to abusing `cl-progv'.
      (cl-progv (mapcar #'car env) (mapcar #'cdr env)
        ;; override in the context of the *new* buffer; the previous had been
        ;; killed, and it isn't part of the snippet chain, so
        ;; `ptemplate--snippet-chain-finalize-hook' is nil for it
        ;; (buffer-local). This means that the hook won't run, so override it
        ;; *again*, buffer-locally, for the pre-snippet-chain buffer. This is
        ;; horrible, but luckily confined to the snippet-chain subsystem.
        (let ((ptemplate--snippet-chain-finalize-hook finalize))
          (ptemplate--snippet-chain-continue))))))

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

(defun ptemplate--snippet-chain-start (snippets &optional env finalize-hook)
  "Start a snippet chain with SNIPPETS.
For details, see `ptemplate--snippet-chain'.

ENV (alist of (SYMBOL . VALUE)) specifies the variables to set in
each new buffer.

FINALIZE-HOOK is called when the snippet chain finishes (see
`ptemplate--snippet-chain-finalize-hook')."
  (let ((ptemplate--snippet-chain (cons 'ptemplate-snippet-chain snippets))
        (ptemplate--snippet-chain-env env)
        (ptemplate--snippet-chain-finalize-hook finalize-hook))
    (ptemplate--snippet-chain-continue)))

;;; utility functions
(defun ptemplate--unix-to-native-path (path)
  "Replace slashes in PATH with the platform's directory separator.
PATH is a file path, as a string, assumed to use slashses as
directory separators. On platform's where that character is
different \(MSDOS, Windows\), replace such slashes with the
platform's equivalent."
  (declare (side-effect-free t))
  (if (memq system-type '(msdos windows-nt))
      (replace-regexp-in-string "/" "\\" path nil t)
    path))

(defun ptemplate--dir-find-relative (path)
  "List all files in PATH recursively.
The list is a string of paths beginning with ./ \(or the
platform's equivalent\) of all files and directories within it.
Unlike `directory-files-recursively', directories end in the
platform's directory separator. \".\" and \"..\" are not
included."
  (setq path (file-name-as-directory path))
  (cl-loop for file in (let ((default-directory path))
                         (directory-files-recursively "." "" t))
           collect (if (file-directory-p (concat path file))
                       (file-name-as-directory file) file)))

(defun ptemplate--auto-map-file (file)
  "Map FILE to its target, removing special extensions.
See `ptemplate--template-files'."
  (if (member (file-name-extension file) '("keep" "yas" "autoyas"))
      (file-name-sans-extension file)
    file))

(defun ptemplate--list-template-dir-files (path)
  "`ptemplate--list-template-files', but include .ptemplate.el.
PATH specifies the path to examine."
  (cl-loop for file in (ptemplate--dir-find-relative path)
           unless (string-suffix-p ".nocopy" file)
           collect (cons file (ptemplate--auto-map-file file))))

(defun ptemplate--list-template-files (path)
  "Find all files in ptemplate PATH.
Associates each file with its target \(alist (SRC . TARGET)\),
removing the extension of special files \(e.g. .nocopy, .yas\).
Directories are included. .ptemplate.el and .ptemplate.elc are
removed."
  (cl-delete-if
   (lambda (f)
     (string-match-p
      (ptemplate--unix-to-native-path "\\`\\./\\.ptemplate\\.elc?") (car f)))
   (ptemplate--list-template-dir-files path)))

(defun ptemplate--autoyas-expand (src target &optional expand-env)
  "Expand yasnippet in file SRC to file TARGET.
Expansion is done \"headless\", that is without any UI.
EXPAND-ENV is an environment alist like in
`ptemplate--snippet-env'."
  (with-temp-file target
    (ptemplate--setup-snippet-env expand-env)

    (yas-minor-mode 1)
    (yas-expand-snippet (ptemplate--read-file src))))

(defun ptemplate--list-dir (dir)
  "List DIR, including directories.
A list of the full paths of each element is returned. The special
directories \".\" and \"..\" are ignored."
  (cl-delete-if (lambda (f) (or (string= (file-name-base f) ".")
                                (string= (file-name-base f) "..")))
                (directory-files dir t)))

;;; `ptemplate!' variables
;; HACKING NOTE: since ptemplate supports scripting within .ptemplate.el files,
;; certain variables need to be made available to that file for use with the
;; `ptemplate!' macro to hook into expansion. These variables should be defined
;; within this block, and made available by using `let' within
;; `ptemplate-expand-template'. `let' is used instead of `setq', as ptemplate
;; supports the expansion of multiple templates at once. This means that these
;; variables need to be overriden in separate contexts, potentially at once,
;; which was traditionally implemented using dynamic-binding. However, using
;; dynamic binding is recommended against; to still support the features of the
;; latter, Emacs allows `let' to override global variables in dynamic-binding
;; style, a feature made use of in `ptemplate-expand-template'.
(defvaralias 'ptemplate--after-copy-hook 'ptemplate--before-snippet-hook
  "Hook run after copying files.

Currently, this hook is always run before snippet expansion, and
is as such an alias for `ptemplate--before-snippet-hook', which
see.")

(defvar ptemplate--before-snippet-hook nil
  "Hook run before expanding yasnippets.
Each function therein shall take no arguments.

These variables are hooks to allow multiple ptemplate! blocks
that specify :before-yas, :after, ....")

(defvar ptemplate--finalize-hook nil
  "Hook to run after template expansion finishes.
At this point, no more files need to be copied and no more
snippets need be expanded.

See also `ptemplate--before-expand-hooks'.")

(defvar-local ptemplate--snippet-env nil
  "Environment used for snippet expansion.
Alist of (SYMBOL . VALUE), like `ptemplate--snippet-chain-env'
but for the entire template.")

(defvar ptemplate--template-files nil
  "Alist mapping template source files to their targets.
Alist (SRC . TARGET), where SRC and TARGET are strings (see
`ptemplate-map' for details). This variable is always
`let'-bound.")

(defvar-local ptemplate-target-directory nil
  "Target directory of ptemplate expansion.
You can use this in templates. This variable always ends in the
platform-specific directory separator, so you can use this with
`concat' to build file paths.")

(defvar-local ptemplate-source-directory nil
  "Source directory of ptemplate expansion.
Akin to `ptemplate-source-directory'.")

;;; `ptemplate--copy-context'
(defmacro ptemplate--define-copy-context (name docstring &rest fields)
  "Helper macro to generate the `ptemplate--copy-context'.
NAME is the name of the struct to be generated and DOCSTRING its
docstring.

Each field in FIELDS shall be a list, the first element of which
is the name of the corresponding struct field followed by a
plist. It must have a :var key, which specifies the global
variable this field corresponds to, and is used to generate the
<name><-from-env function. The struct's constructor is
<name><-new and its copier is <name><-copy.

This macro generates three functions:

<name><-from-env: Yield a copy context from the global
environment, with each field acquired from the corresponding
global variable, as specified in :var.

<name>->to-env: Set the global environment based on the copy
context. The opposite of <name><-from-env.

<name>->merge-hooks: Taking any number of copy contexts
\(&rest\), yields a new copy context by merging each of their
fields in order \(with `nconc'\), except for those that have the
:merge-hooks key explicitly set to nil \(defaults to t\)."
  (let ((constructor (intern (format "%s<-new" name)))
        (copier (intern (format "%s<-copy" name)))
        (from-env (intern (format "%s<-from-env" name)))
        (to-env (intern (format "%s->to-env" name)))
        (merge-hooks (intern (format "%s<-merge-hooks" name))))
    `(progn
       ;;; `ptemplate--copy-context'
       (cl-defstruct (,name (:constructor ,constructor)
                            (:copier ,copier)) ,docstring
         ,@(cl-loop for (fname . props) in fields
                    for var = (plist-get props :var)
                    for field-doc = (format "Corresponds to variable `%s'."
                                            var)
                    collect `(,fname nil :documentation ,field-doc)))
       ;;; `ptemplate--copy-context<-from-env'
       (defun ,from-env ()
         ,(format
           "Make a `%s' from the current environment.
All fields are initialized from their corresponding globals, as
bound in the current environment, which could be `let'-bound.

The opposite of `%s->to-env'." name name)
         (,constructor ,@(cl-loop for (fname . props) in fields
                                  collect (intern (format ":%s" fname))
                                  collect (plist-get props :var))))
       ;;; `ptemplate--copy-context->to-env'
       (defun ,to-env (--context--)
         ,(format
           "Elevate --CONTEXT-- into the caller's environment.
The opposite of `%s<-from-env'. Globals corresponding to
--CONTEXT--'s fields are set to their values, in the current
environment, which could be \(`let'-bound\).

--CONTEXT-- shall be a %s." name name)
         (setq
          ,@(cl-loop for (fname . props) in fields
                     collect (plist-get props :var)
                     collect `(,(intern (format "%s-%s" name fname)) --context--))))
       ;;; `ptemplate--copy-context<-merge-hooks'
       (defun ,merge-hooks (&rest --contexts--)
         ,(format
           "Merge --CONTEXTS--'s \"hook\" members.
CONTEXTS is a list of `%s's, whose fields are merged using
`nconc', but only those that don't specify :merge-hooks nil in
`ptemplate--define-copy-context'.

Return the result, which is a `%s'.

Fields not merged are: %s."
           ;; XXX: if there are many :merge-hooks nil fields, the docstring
           ;; will not be filled properly. This won't be a problem for
           ;; `ptemplate' though.
           name name (string-join
                      (cl-loop for (fname . props) in fields
                               if (and (plist-member props :merge-hooks)
                                       (not (plist-get props :merge-hooks)))
                               collect (symbol-name fname)) ", "))
         (,constructor
          ,@(cl-loop for (fname . props) in fields
                     collect (intern (format ":%s" fname))
                     collect
                     (if
                         (if (plist-member props :merge-hooks)
                             ;; NOTE: by default, *do* merge hooks; it is
                             ;; usually only file-map that shouldn't be merged.
                             (plist-get props :merge-hooks) t)
                         `(mapcan #',(intern (format "%s-%s" name fname)) --contexts--)
                       'nil))))
       ;;; HACKING: add new to-be generated copy-context functions here
       )))

(ptemplate--define-copy-context ptemplate--copy-context
  "Holds data needed by ptemplate's copy phase.
To acquire this state, a template's file need to be listed and
the .ptemplate.el needs to be evaluated against it.
`ptemplate--template-context->execute'"
  (before-snippets :var ptemplate--before-snippet-hook)
  (finalize-hook :var ptemplate--finalize-hook)
  (snippet-env :var ptemplate--snippet-env)
  (file-map :var ptemplate--template-files :merge-hooks nil))

;; (defun ptemplate--copy-context->to-env (context)
;;   "Elevate CONTEXT into the caller's environment.
;; The opposite of `ptemplate--copy-context<-from-env'. All global
;; template variables, as bound in the current environment, are
;; overridden with their counterparts in CONTEXT, which is a
;; `ptemplate--copy-context'."
;;   (setq ptemplate--before-snippet-hook
;;         (ptemplate--copy-context-before-snippets context)
;;         ptemplate--finalize-hook
;;         (ptemplate--copy-context-finalize-hook context)
;;         ptemplate--snippet-env
;;         (ptemplate--copy-context-snippet-env context)

;;         ptemplate--template-files (ptemplate--copy-context-file-map context)))

;; (defun ptemplate--copy-context<-from-env ()
;;   "Make a `ptemplate--copy-context' from the current environment.
;; During expansion, global ptemplate variables \(e.g.
;; `ptemplate--template-files'\) are `let'-bound and modified. This
;; function acquires a `ptemplate--copy-context' from such
;; variables, as bound in the current environment, and returns it."
;;   (ptemplate--copy-context<-new
;;    :file-map ptemplate--template-files
;;    :before-snippets ptemplate--before-snippet-hook
;;    :finalize-hook ptemplate--finalize-hook
;;    :snippet-env ptemplate--snippet-env))

;; (defun ptemplate--copy-context<-merge-hooks (&rest contexts)
;;   "Merge CONTEXTS' non-file-map members.
;; CONTEXTS is a list of `ptemplate--copy-context's.

;; Return the result, which is a `ptemplate--copy-context'.

;; Note that non-file-map members \(like before-snippets\) are
;; merged using `nconc' and as such may be altered destructively."
;;   (ptemplate--copy-context<-new
;;    :before-snippets (mapcan #'ptemplate--copy-context-before-snippets contexts)
;;    :finalize-hook (mapcan #'ptemplate--copy-context-finalize-hook contexts)
;;    :snippet-env (mapcan #'ptemplate--copy-context-snippet-env contexts)
;;    :file-map nil))

(defun ptemplate--eval-template (source &optional target)
  "Evaluate the template given by SOURCE.
Gather all of its files, execute the .ptemplate.el file in it and
return a `ptemplate--copy-context' for it. TARGET specifies where
the template should be expanded to and may be left out for
templates that don't make use of `ptemplate-target-directory' in
:init. Both SOURCE and TARGET are directories, with an optional
trailing slash."
  (setq target (file-name-as-directory target))
  (setq source (file-name-as-directory source))

  (let ((dotptemplate (concat source ".ptemplate.el"))
        ptemplate--before-snippet-hook
        ptemplate--finalize-hook
        ptemplate--snippet-env

        ;; the dotptemplate file should know about source and target.
        (ptemplate-source-directory source)
        (ptemplate-target-directory target)

        ;; all template files should start with a ., which makes them source and
        ;; target agnostic. `concat' source/target + file will yield a correct
        ;; path because of this. NOTE: we mustn't override `default-directory'
        ;; for .ptemplate.el, as it should have access to the entire context of
        ;; the current buffer.
        (ptemplate--template-files (ptemplate--list-template-files source)))
    ;;; load .ptemplate.el
    (when (file-exists-p dotptemplate)
      ;; NOTE: arbitrary code execution
      (load-file dotptemplate))

    (ptemplate--copy-context<-from-env)))

(defun ptemplate--copy-context->execute (context source target)
  "Copy all files in CONTEXT's file-map.
SOURCE specifies the template's source directory and TARGET the
target directory, which are needed since all paths in CONTEXT are
relative and not stored in it. Note that executing this with a
different SOURCE and TARGET might lead to issues if the template
manually copies files around in its .ptemplate.el :init block.
\(`ptemplate-copy-target' is okay in :finalize\)."
  ;; EDGE CASE: empty templates should yield a directory
  (make-directory target t)

  (setq source (file-name-as-directory source)
        target (file-name-as-directory target))
  (cl-loop with snippet-env = (nconc `((ptemplate-source-directory . ,source)
                                       (ptemplate-target-directory . ,target))
                                     (ptemplate--copy-context-snippet-env context))
           for (src . targetf) in (ptemplate--copy-context-file-map context)
           for realsrc = (concat source src)
           for realtarget = (when targetf (concat target targetf))
           ;; NOTE: all files from `ptemplate--list-template-files' end in a slash.
           for dir? = (directory-name-p realsrc)

           ;;; `ptemplate--copy-context->execute' support nil maps
           if targetf do
           (make-directory
            ;; directories need to be created "as-is" (they may potentially
            ;; be empty); files must not be created as directories however
            ;; but their containing directories instead. This avoids
            ;; prompts asking the user if they really want to save a file
            ;; even though its containing directory was not made yet.
            (if dir? (concat target src)
              (concat target (file-name-directory src)))
            t)

           and unless dir?
           if (string-suffix-p ".yas" src)
           collect (cons realsrc realtarget) into yasnippets
           else if (string-suffix-p ".autoyas" src)
           do (ptemplate--autoyas-expand realsrc realtarget snippet-env)
           else do (copy-file realsrc realtarget)

           finally do
           (run-hooks 'ptemplate--before-snippet-hook)
           (ptemplate--snippet-chain-start
            yasnippets snippet-env
            (ptemplate--copy-context-finalize-hook context))))

;;; Public API
(defcustom ptemplate-template-dirs '()
  "List of directories containing templates.
Analagous to the variable `yas-snippet-dirs'."
  :group 'ptemplate
  :type '(repeat string))

(defun ptemplate-find-templates (template)
  "Find TEMPLATE in `ptemplate-template-dirs'.
Template shall be a path of the form \"category/type\". Returns a
list of full paths to the template directory specified by
TEMPLATE. Returns the empty list if TEMPLATE cannot be found."
  (let ((template (file-name-as-directory template))
        result)
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
  (require 'helm)
  (declare-function helm "helm")
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
  (let ((context (ptemplate--eval-template source target)))
    (ptemplate--copy-context->execute context source target)))

(defun ptemplate-new-project (source target)
  "Create a new project based on a template.
Like `ptemplate-expand-template', but ensure that TARGET doesn't
exist. SOURCE and TARGET are passed to
`ptemplate-expand-template' unmodified."
  (interactive (let ((template (funcall ptemplate-template-prompt-function)))
                 (list template (ptemplate--prompt-target template))))
  (when (file-directory-p target)
    ;; NOTE: the error message should mention the user-supplied target (not
    ;; necessarily with a slash at the end), so do this buffer
    ;; (file-name-as-directory).
    (user-error "Directory %s already exists" target))
  (ptemplate-expand-template source target))

;;; auxiliary functions for the .ptemplate.el API
(defun ptemplate--make-basename-regex (file)
  "Return a regex matching FILE as a basename.
FILE shall be a regular expressions matching a path, separated
using slashes, which will be converted to the platform-specific
directory separator. The returned regex will match if FILE
matches at the start of some string or if FILE matches after a
platform-specific directory separator. The returned regexes can
be used to remove files with certain filenames from directory
listings.

Note that . or .. path components are not handled at all, meaning
that \"\(string-match-p (ptemplate--make-basename-regex
\"tmp/foo\") \"tmp/foo/../foo\"\)\" will yield nil."
  (declare (side-effect-free t))
  (concat (ptemplate--unix-to-native-path "\\(?:/\\|\\`\\)")
          (ptemplate--unix-to-native-path file) "\\'"))

(defun ptemplate--make-path-regex (path)
  "Make a regex matching PATH if some PATH is below it.
The resulting regex shall match if some other path starts with
PATH. Slashes should be used to separate directories in PATH, the
necessary conversion being done for windows and msdos. The same
caveats apply as for `ptemplate--make-basename-regex'."
  (declare (side-effect-free t))
  (concat "\\`" (regexp-quote (ptemplate--unix-to-native-path path))
          (ptemplate--unix-to-native-path "\\(?:/\\|\\'\\)")))

(defun ptemplate--simplify-user-path (path)
  "Make PATH a template-relative path without any prefix.
PATH's slashes are converted to the native directory separator
and prefixes like ./ and / are removed. Note that directory
separator conversion is not performed."
  (declare (side-effect-free t))
  (let* ((paths (split-string path "/"))
         (paths (cl-delete-if #'string-empty-p paths))
         (paths (cl-delete-if (apply-partially #'string= ".") paths)))
    (string-join paths "/")))

(defun ptemplate--normalize-user-path (path)
  "Make PATH usable to query template files.
PATH shall be a user-supplied template source/target relative
PATH, which will be normalized and whose directory separators
will be converted to the platform's native ones."
  (declare (side-effect-free t))
  (ptemplate--unix-to-native-path
   (concat "./" (ptemplate--simplify-user-path path))))

(defun ptemplate--make-ignore-regex (regexes)
  "Make a delete regex for `ptemplate-ignore'.
REGEXES is a list of strings as described there."
  (declare (side-effect-free t))
  (string-join
   (cl-loop for regex in regexes collect
            (if (string-prefix-p "/" regex)
                (ptemplate--make-path-regex
                 (ptemplate--normalize-user-path regex))
              (ptemplate--make-basename-regex regex)))
   "\\|"))

(defun ptemplate--prune-template-files (regex)
  "Remove all template whose source files match REGEX.
This function is only supposed to be called from `ptemplate!'."
  (setq ptemplate--template-files
        (cl-delete-if
         (lambda (src-targetf) (string-match-p regex (car src-targetf)))
         ptemplate--template-files)))

(defun ptemplate--prune-duplicate-files (files dup-cb)
  "Find and remove duplicates in FILES.
FILES shall be a list of template file mappings \(see
`ptemplate--template-files'\). If a duplicate is encountered,
call DUP-CB using `funcall' and pass to it the (SRC . TARGET)
cons that was encountered later.

Return a new list of mappings with all duplicates removed.

This function uses a hashmap and is as such efficient for large
lists, but doesn't use constant memory."
  ;; hashmap of all target files mapped to `t'
  (cl-loop with known-targets = (make-hash-table :test 'equal)
           for file in files for target = (cdr file)
           ;; already encountered? call DUP-CB
           if (gethash target known-targets) do (funcall dup-cb file)
           ;; remember it as encountered and collect it, since it was first
           else do (puthash target t known-targets) and collect file))

(defun ptemplate--override-files (base-files override)
  "Override all mappings in BASE-FILES with those in OVERRIDE.
Both of them shall be mappings like `ptemplate--template-files'.
BASE-FILES and OVERRIDE may be altered destructively.

Return the new mapping alist, with files from OVERRIDE having
taken precedence. In either of those parameters, files mapped
earlier win.

Note that because duplicate mappings might silently be deleted,
you should call `ptemplate--prune-duplicate-files' with a warning
callback first, to report such duplicates to the user."
  (ptemplate--prune-duplicate-files
   (nconc override base-files)
   ;; duplicates are normal (mappings from OVERRIDE).
   #'ignore))

(defun ptemplate--inherit-templates (srcs)
  "Inherit the hooks of all templates in SRCS.
This functions evaluates all templates in the template path array
SRCS and prepends their hooks \(as defined by
`ptemplate--copy-context<-merge-hooks'\) to the current global
ones. The files of SRCS are not imported though, to do that being
left to the caller. Returns a list of template contexts
corresponding to each template in SRCS.

See also `ptemplate-inherit' and `ptemplate-inherit-overriding'."
  (let ((inherit-contexts (mapcar #'ptemplate--eval-template srcs)))
    (ptemplate--copy-context->to-env
     (apply #'ptemplate--copy-context<-merge-hooks
            (nconc inherit-contexts (ptemplate--copy-context<-from-env))))
    inherit-contexts))

;;; .ptemplate.el api
(defun ptemplate-map (src target)
  "Map SRC to TARGET for expansion.
SRC is a path relative to the ptemplate being expanded and
TARGET is a path relative to the expansion target."
  (add-to-list 'ptemplate--template-files
               (cons (ptemplate--normalize-user-path src)
                     (when target (ptemplate--normalize-user-path target)))))

(defun ptemplate-remap (src target)
  "Remap template file SRC to TARGET.
SRC shall be a template-relative path separated by slashes
\(conversion is done for windows\). Using .. in SRC will not work.
TARGET shall be the destination, relative to the expansion
target. See `ptemplate--normalize-user-path' for SRC name rules.

Note that directories are not recursively remapped, which means
that all files contained within them retain their original
\(implicit?\) mapping. This means that nonempty directories whose
files haven't been remapped will still be created.

See also `ptemplate-remap-rec'."
  (ptemplate--prune-template-files
   (ptemplate--unix-to-native-path
    (format "\\`%s/?\\'" (ptemplate--normalize-user-path src))))
  (ptemplate-map src target))

(defun ptemplate-remap-rec (src target)
  "Like `ptemplate-remap', but handle directories recursively instead.
For each directory that is mapped to a directory within SRC,
remap it to that same directory relative to TARGET."
  (let ((remap-regex (ptemplate--make-path-regex
                      (ptemplate--normalize-user-path src)))
        (target (when target (ptemplate--normalize-user-path target))))
    (dolist (file ptemplate--template-files)
      (when (string-match-p remap-regex (car file))
        (setcdr file (when target (replace-regexp-in-string
                                   remap-regex target file nil t)))))))

(defun ptemplate-copy-target (src target)
  "Copy SRC to TARGET, both relative to the expansion target.
Useful if a single template expansion needs to be mapped to two
files, in the :finalize block of `ptemplate!'."
  (copy-file (concat ptemplate-target-directory src)
             (concat ptemplate-target-directory target)))

(defun ptemplate-ignore (&rest regexes)
  "REGEXES specify template files to ignore.
See `ptemplate--make-basename-regex' for details. As a special
case, if a REGEX starts with /, it is interpreted as a template
path to ignore \(see `ptemplate--make-path-regex'\)."
  (ptemplate--prune-template-files
   (ptemplate--make-ignore-regex regexes)))

(defun ptemplate-include (&rest dirs)
  "Use all files in DIRS for expansion.
The files are added as if they were part of the current template
being expanded, except that .ptemplate.el and .ptemplate.elc are
valid filenames and are not interpreted.

The files defined in the template take precedence. To get the
other behaviour, use `ptemplate-include-override' instead."
  (ptemplate--override-files (mapcan #'ptemplate--list-template-dir-files dirs)
                             ptemplate--template-files))

(defun ptemplate-include-override (&rest dirs)
  "Like `ptemplate-include', but files in DIRS override."
  (ptemplate--override-files
   ptemplate--template-files
   (mapcan #'ptemplate--list-template-dir-files dirs)))

(defun ptemplate-inherit (&rest srcs)
  "Inherit all templates in SRCS.
The hooks of all templates in SRCS are run before the current
template's ones and the files from SRCS are added for expansion.
File maps defined in the current template take precedence, so can
be used to override mappings from SRCS. Mappings from templates
that come earlier in SRCS take precedence over those from later
templates. To ignore files from SRCS, map them to nil using :map
or `ptemplate-map' before calling this function."
  ;; TODO: map nil needed? @@docstring
  (let* ((contexts (ptemplate--inherit-templates srcs))
         ;; NOTE: templates that come later in DIRS are overriden.
         (to-inherit (mapcan #'ptemplate--copy-context-file-map contexts)))
    (ptemplate--override-files to-inherit ptemplate--template-files)))

(defun ptemplate-inherit-overriding (&rest srcs)
  "Like `ptemplate-inherit', but files in SRCS take precedence.
Files from templates that come later in SRCS take precedence."
  (let* ((contexts (ptemplate--inherit-templates srcs))
         (to-inherit (mapcan #'ptemplate--copy-context-file-map
                             (nreverse contexts))))
    (ptemplate--override-files ptemplate--template-files to-inherit)))

(defun ptemplate-source (dir)
  "Return DIR as if relative to `ptemplate-source-directory'."
  (concat ptemplate-source-directory dir))

(defun ptemplate-target (dir)
  "Return DIR as if relative to `ptemplate-target-directory'."
  (concat ptemplate-target-directory dir))

;; NOTE: ;;;###autoload is unnecessary here, as `ptemplate!' is only useful in
;; .ptemplate.el files, which are only ever loaded from
;; `ptemplate-expand-template', at which point `ptemplate' is already loaded.
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

:snippet-env VARIABLES to make available in snippets. Their
             values are examined at the end of `ptemplate!' and
             stored. Each element after :env shall be a symbol or
             a list of the form (SYMBOL VALUEFORM), like in
             `let'. The SYMBOLs should not be quoted. If only a
             symbol is specified, its value is taken from the
             current environment. This way, variables can be
             let-bound outside of `ptemplate!' and used in
             snippets. Leaving out VALUEFORM makes it nil.

:snippet-let VARIABLES to `let*'-bind around the `ptemplate!'
             block and to include in the snippet environment.
             Each ARG shall be a list (SYMBOL VALUEFORM) or just
             SYMBOL. If specified as SYMBOL, the variable is
             initialized to nil. Otherwise, VALUEFORM is used to
             initialize the variable. Note that the value of
             snippet-let blocks can be changed in :init.

:ignore See `ptemplate-ignore'. Files are pruned before
        :init.

:subdir Make some template-relative paths appear to be in the
        root. Practically, this means not adding its files and
        including it. Evaluated before :init.

:remap ARG shall be of the form \(SRC TARGET\); calls
       `ptemplate-remap' on the results of evaluating SRC and
       TARGET. Run after :init.

:remap-rec Like :remap, but call `ptemplate-remap-rec' instead.
           Note that it is undefined whether :remap-rec or :remap
           is executed firsts and their order of appearance is
           insignificant.

:map Syntax sugar for `ptemplate-map'. ARG must be of the form
     \(SRC TARGET\), both of which are ordinary LISP expressions.
     Run after :remap and :remap-rec.

:inherit Syntax sugar for `ptemplate-inherit'. FORMs may be
         arbitrary Lisp expressions \(not just strings\).
         Executed after :map.

:open-bg Expressions yielding files \(target-relative\) to open
         with `find-file-noselect' at the very end of expansion.

:open Like :open-bg, but using `find-file'.

Note that because .ptemplate.el files just execute arbitrary
code, you could write them entirely without using this
macro (e.g. by modifying hooks directly, ...). However, you
should still use `ptemplate!', as this makes templates more
future-proof and readable."
  (let ((cur-keyword :init)
        init-forms
        before-yas-eval
        after-copy-eval
        finalize-eval open-eval open-bg-eval
        snippet-env
        around-let
        ignore-regexes
        inherited-templates
        include-dirs
        remap-eval map-eval)
    (dolist (arg args)
      (if (keywordp arg)
          (setq cur-keyword arg)
        (pcase cur-keyword
          (:init (push arg init-forms))
          (:before-snippets (push arg before-yas-eval))
          (:after-copy (push arg after-copy-eval))
          (:finalize (push arg finalize-eval))
          (:open (push `(find-file (ptemplate-target ,arg)) open-eval))
          (:open-bg (push `(find-file (ptemplate-target ,arg)) open-bg-eval))
          (:snippet-env (push arg snippet-env))
          (:snippet-let (push (if (consp arg) (car arg) arg) snippet-env)
                        (push arg around-let))
          (:ignore (push arg ignore-regexes))
          (:inherit (push arg inherited-templates))
          (:subdir (let ((simplified-path (ptemplate--simplify-user-path arg)))
                     (push (concat (ptemplate--unix-to-native-path "/")
                                   simplified-path)
                           ignore-regexes)
                     (push simplified-path include-dirs)))
          (:remap (push `(ptemplate-remap ,(car arg) ,(cadr arg)) remap-eval))
          (:remap-rec (push `(ptemplate-remap-rec ,(car arg) ,(cadr arg))
                            remap-eval))
          (:map (push `(ptemplate-map ,(car arg) ,(cadr arg)) map-eval)))))
    (macroexp-let*
     (nreverse around-let)
     (macroexp-progn
      (nconc
       (when ignore-regexes
         `((ptemplate--prune-template-files
            ,(ptemplate--make-ignore-regex ignore-regexes))))
       (when include-dirs
         ;; include dirs specified first take precedence
         `((ptemplate-include
            ,@(cl-loop for dir in (nreverse include-dirs)
                       collect (list #'ptemplate-source dir)))))
       (nreverse init-forms)
       (nreverse remap-eval)
       (nreverse map-eval)
       ;; execute late to give the user the chance to map files in a way that
       ;; overrides first.
       (when inherited-templates
         `((ptemplate-inherit ,@inherited-templates)))
       (when before-yas-eval
         `((add-hook 'ptemplate--before-snippet-hook
                     (lambda () "Run before expanding snippets."
                       ,@(nreverse before-yas-eval)))))
       (when after-copy-eval
         `((add-hook 'ptemplate--after-copy-hook
                     (lambda () "Run after copying files."
                       ,@(nreverse after-copy-eval)))))
       (when (or finalize-eval open-bg-eval open-eval)
         `((add-hook 'ptemplate--finalize-hook
                     (lambda () "Run after template expansion finishes."
                       ,@(nreverse finalize-eval)
                       ,@(nreverse open-bg-eval)
                       ,@(nreverse open-eval)))))
       (when snippet-env
         `((setq
            ptemplate--snippet-env
            (nconc
             ptemplate--snippet-env
             (list ,@(cl-loop
                      for var in snippet-env collect
                      (if (listp var)
                          (list #'cons (macroexp-quote (car var)) (cadr var))
                        `(cons ',var ,var)))))))))))))

(provide 'ptemplate)
;;; ptemplate.el ends here
