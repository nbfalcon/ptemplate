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
;; Version: 1.2.0

;;; Commentary:
;; Creating projects can be a lot of work. Cask files need to be set up, a
;; License file must be added, maybe build system files need to be created. A
;; lot of that can be automated, which is what ptemplate does. You can create a
;; set of templates categorized by type/template like in eclipse, and ptemplate
;; will then initialize the project for you. In the template you can have any
;; number of yasnippets or normal files.

;; Security note: yasnippets allow arbitrary code execution, as do
;; .ptemplate.el files. DO NOT EXPAND UNTRUSTED TEMPLATES. ptemplate DOES NOT
;; make ANY special effort to protect against malicious templates.

;;; Code:

(require 'cl-lib)
(require 'subr-x)                       ; `string-join'

;;; global `declare-function'
(declare-function yas-minor-mode "yasnippet" (&optional arg))
(declare-function yas-expand-snippet "yasnippet" (s &optional start end env))

;;; snippet-chain subsystem

(cl-defstruct (ptemplate--snippet-chain-mapping
               (:constructor ptemplate--snippet-chain-mapping<-new)
               (:copier ptemplate--snippet-chain<-copy))
  "Maps a SRC to snippet expansion TARGET."
  (src
   nil :documentation
   "Path to the snippet being expanded.")
  (target
   nil :documentation
   "Where SRC is expanded to.")
  (setup-hook
   nil :documentation
   "Run before inserting each snippet.
Each function therein is called without arguments."))

(cl-defstruct (ptemplate--snippet-chain
               (:constructor ptemplate--snippet-chain<-new))
  "Holds all state needed for executing a snippet chain.
An instance of this, `ptemplate--snippet-chain-context', is
passed trough each buffer so that they can all share some state."
  (snippets
   nil :documentation
   "List of `ptemplate--snippet-chain-mapping'.
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

See also `ptemplate--snippet-chain->start'.")
  (env
   nil :documentation
   "List of variables to set in snippet-chain buffers.
Alist of (SYMBOL . VALUE).

All variables will be made buffer-local before being set in each
snippet-chain buffer. `ptemplate--snippet-chain-context' shouldn't be
included.")
  (finalize-hook
   nil :documentation
   "Hook to run after the snippet chain finishes.
Each function therein gets called without arguments.

This hook needs to be a separate variable and cannot be
implemented by simply appending it to the SNIPPETS field, because
in that case it would get run too early if
`ptemplate--snippet-chain-later' were called at least once, as
then it wouldn't be the last element anymore.")
  (newbuf-hook
   nil :documentation
   "Hook run after each snippet-chain buffer is created.
Can be used to configure `ptemplate--snippet-chain-nokill'. Each
function therein is called with no arguments."))

(defvar-local ptemplate--snippet-chain-nokill nil
  "If set in a snippet-chain buffer, don't kill it.
Normally, `ptemplate--snippet-chain->continue' kills the buffer
when moving on. If this variable is set, don't do that. Useful
when one wants to keep the cursor when reopening a snippet chain
file.")

(defvar-local ptemplate--snippet-chain-next-hook nil
  "Run at the very start of `ptemplate-snippet-chain-next'.
Can be used to modify the buffer before it is saved, add a
`kill-buffer-hook', ....")

(defvar ptemplate--snippet-chain-context nil
  "The instance of `ptemplate--snippet-chain'.
facilitate parallel template expansion. All snippet-chain This
variable is always either `let' bound or buffer-local, to
functions operate on this variable, as defined in their calling
environment.")

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
  "Set all (SYMBOL . VALUE) pairs in SNIPPET-ENV.
Variables are set buffer-locally."
  (let ((symbols (mapcar #'car snippet-env))
        (values (mapcar #'cdr snippet-env)))
    (mapc #'make-local-variable symbols)
    (cl-mapcar #'set symbols values)))

(defun ptemplate--snippet-chain->continue ()
  "Make the next snippet/buffer in the snippet chain current."
  (let* ((context ptemplate--snippet-chain-context)
         (realchain (ptemplate--snippet-chain-snippets context))
         (next (car realchain)))
    (when realchain
      ;; if the snippet chain is empty, pop fails.
      (pop (ptemplate--snippet-chain-snippets context)))

    (cond
     ((null next)
      (mapc #'funcall (ptemplate--snippet-chain-finalize-hook context)))
     ((bufferp next) (switch-to-buffer next))
     ((ptemplate--snippet-chain-mapping-p next)
      (let ((target (ptemplate--snippet-chain-mapping-target next))
            (source-file (ptemplate--snippet-chain-mapping-src next))
            (setup-hook (ptemplate--snippet-chain-mapping-setup-hook next)))
        (require 'yasnippet)
        (with-current-buffer (find-file-noselect target)
          (make-local-variable 'ptemplate--snippet-chain-context)

          (ptemplate--setup-snippet-env (ptemplate--snippet-chain-env context))
          ;; let the user configure the buffer, with the snippet-env already
          ;; bound.
          (mapc #'funcall (ptemplate--snippet-chain-newbuf-hook context))
          (mapc #'funcall setup-hook)

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
  (run-hooks 'ptemplate--snippet-chain-next-hook)
  (save-buffer 0)
  (let ((context ptemplate--snippet-chain-context))
    (if ptemplate--snippet-chain-nokill
        (ptemplate-snippet-chain-mode -1)
      (kill-buffer))
    ;; the `let' needs to be here, as otherwise it would override the
    ;; buffer-local binding
    (let ((ptemplate--snippet-chain-context context))
      (ptemplate--snippet-chain->continue))))

(defun ptemplate-snippet-chain-later ()
  "Save the current buffer to be expanded later.
Use this if you are not sure yet what expansions to use in the
current snippet and want to decide later, after looking at
others."
  (interactive)
  (unless ptemplate--snippet-chain-context
    (user-error "No more snippets to expand"))
  ;; snippet chain cannot be nil, so `nconc' will append to it, modifying it
  ;; across all buffers.
  (nconc (ptemplate--snippet-chain-snippets
          ptemplate--snippet-chain-context)
         (list (current-buffer)))
  (ptemplate--snippet-chain->continue))

(defun ptemplate--snippet-chain->start
    (snippets &optional env finalize-hook newbuf-hook)
  "Start a snippet chain with SNIPPETS.
For details, see `ptemplate--snippet-chain-context'.

ENV (alist of (SYMBOL . VALUE)) specifies the variables to set in
each new buffer.

FINALIZE-HOOK is run when the snippet chain finishes. Corresponds
to `ptemplate--snippet-chain-finalize-hook'.

NEWBUF-HOOK is run each time a new snippet chain buffer is
created. Corresponds to `ptemplate--snippet-chain-newbuf-hook'"
  (let ((ptemplate--snippet-chain-context
         (ptemplate--snippet-chain<-new
          :snippets snippets :env env :finalize-hook finalize-hook
          :newbuf-hook newbuf-hook)))
    (ptemplate--snippet-chain->continue)))

;;; common utility functions
(defun ptemplate--unix-to-native-path (path)
  "Replace slashes in PATH with the platform's directory separator.
PATH is a file path, as a string, assumed to use slashes as
directory separators. On platform's where that character is
different (MS DOS, Windows), replace such slashes with the
platform's equivalent."
  (declare (side-effect-free t))
  (if (memq system-type '(msdos windows-nt))
      (replace-regexp-in-string "/" "\\" path nil t)
    path))

(defun ptemplate--dir-find-relative (path)
  "List all files in PATH recursively.
The list is a string of paths beginning with ./ (or the
platform's equivalent) of all files and directories within it.
Unlike `directory-files-recursively', directories end in the
platform's directory separator. \".\" and \"..\" are not
included."
  (setq path (file-name-as-directory path))
  (cl-loop for file in (let ((default-directory (expand-file-name path)))
                         (directory-files-recursively "." "" t))
           collect (if (file-directory-p (concat path file))
                       (file-name-as-directory file) file)))

(defun ptemplate--auto-map-file (file)
  "Associate FILE to a target file name.
File specifies the template-relative path to a file in a
template."
  (if (member (file-name-extension file) '("keep" "yas" "autoyas"))
      (file-name-sans-extension file)
    file))

(defun ptemplate--file-mapping<-auto (file)
  "Map FILE using `ptemplate--auto-map-file'.
Return a `ptemplate--file-mapping'."
  (ptemplate--file-mapping<-new
   :src file :target (ptemplate--auto-map-file file)))

(defun ptemplate--list-template-dir-files (path)
  "`ptemplate--list-template-files', but include .ptemplate.el.
PATH specifies the path to examine."
  (mapcar #'ptemplate--file-mapping<-auto
          (cl-delete-if (apply-partially #'string-suffix-p ".nocopy")
                        (ptemplate--dir-find-relative path))))

(defun ptemplate--list-template-dir-files-abs (path)
  "Like `ptemplate--list-template-dir-files'.
The difference is that this version yields an absolute mapping
instead (see `ptemplate--file-map->absoluteify').

PATH specifies that path to the template."
  (ptemplate--file-map->absoluteify
   path (ptemplate--list-template-dir-files path)))

(defun ptemplate--list-template-files (path)
  "Find all files in ptemplate PATH.
The result is a list file-map, as in `ptemplate--file-map'.

Associates each file with its target, removing the extension of
special files (e.g. .nocopy, .yas). Directories are included.
.ptemplate.el and .ptemplate.elc are removed."
  (cl-delete-if
   (lambda (mapping)
     (member (ptemplate--file-mapping-src mapping)
             ;; NOTE: these may be directories; don't ignore them in that case
             '("./.ptemplate.el" "./.ptemplate.elc")))
   (ptemplate--list-template-dir-files path)))

(defun ptemplate--autoyas-expand
    (src target &optional expand-env snippet-setup-hook)
  "Expand yasnippet in file SRC to file TARGET.
Expansion is done \"headless\", that is without showing buffers.
EXPAND-ENV is an environment alist like in
`ptemplate--snippet-chain-env'. Execute each function in
SNIPPET-SETUP-HOOK before expanding the snippet."
  (with-temp-file target
    (require 'yasnippet)

    (ptemplate--setup-snippet-env expand-env)
    (mapc #'funcall snippet-setup-hook)

    (yas-minor-mode 1)
    (yas-expand-snippet (ptemplate--read-file src))))

(defun ptemplate--list-dir (dir)
  "List DIR, including directories.
A list of the full paths of each element is returned. The special
directories \".\" and \"..\" are ignored."
  (cl-delete-if (lambda (f) (or (string= (file-name-base f) ".")
                           (string= (file-name-base f) "..")))
                (directory-files dir t)))

(defmacro ptemplate--prependlf (newels place)
  "Prepend list NEWELS to PLACE.
There is no single-element version of this, because `cl-push'
does that already."
  (declare (debug (form gv-place)))
  (macroexp-let2 macroexp-copyable-p x newels
    (gv-letplace (getter setter) place
      (funcall setter `(nconc ,x ,getter)))))

(defmacro ptemplate--appendlf (newels place)
  "Add list NEWELS to the end of list PLACE."
  (declare (debug (form gv-place)))
  (macroexp-let2 macroexp-copyable-p x newels
    (gv-letplace (getter setter) place
      (funcall setter `(nconc ,getter ,x)))))

(defmacro ptemplate--appendf (newelt place)
  "Add NEWELT to the end of list PLACE."
  (declare (debug (form gv-place)))
  `(ptemplate--appendlf (list ,newelt) ,place))

;;; copy context
(cl-defstruct (ptemplate--file-mapping
               (:constructor ptemplate--file-mapping<-new)
               (:copier ptemplate--file-mapping<-copy))
  "Holds a file mapping, as can be found in file-maps.
The underlying data-structure of
`ptemplate--copy-context-file-map'."
  (prefix
   nil :documentation
   "Prepended to SRC to yield the actual location.")
  (src
   nil :documentation
   "See PREFIX.")
  (target
   nil :documentation
   "Where the SRC is mapped to.
Relative to the expansion target.")
  (snippet-setup-hook
   nil :documentation
   "List of functions run to set up this snippet.
Only makes sense in snippet-chain files. See
`ptemplate-snippet-setup'."))

(defun ptemplate--file-map->absoluteify (path file-map)
  "Make each relative mapping in FILE-MAP refer to PATH.
A relative mapping is one whose PREFIX is nil. For each of them,
set the PREFIX to PATH, modifying FILE-MAP. Return the result."
  (dolist (mapping file-map)
    (setf (ptemplate--file-mapping-prefix mapping)
          (or (ptemplate--file-mapping-prefix mapping) path)))
  file-map)

(cl-defstruct (ptemplate--copy-context
               (:constructor ptemplate--copy-context<-new)
               (:copier ptemplate--copy-context<-copy))
  "Holds data needed by ptemplate's copy phase.
To acquire this state, a template's file need to be listed and
the .ptemplate.el needs to be evaluated against it.
`ptemplate--copy-context->execute'. A `let'-bound, global
instance of this that is used by the .ptemplate.el API is in
`ptemplate--cur-copy-context'."
  (file-map
   nil :documentation
   "List of `ptemplate--file-mapping.'")
  (snippet-env
   nil :documentation
   "Environment used for snippet expansion.
Alist of (SYMBOL . VALUE), like `ptemplate--snippet-chain-env'
but for the entire template.")
  (snippet-conf-hook
   nil :documentation
   "Hook run when creating snippet-chain buffers.
Corresponds to `ptemplate--snippet-chain-newbuf-hook'.")
  (before-snippets
   nil :documentation
   "Hook run before expanding yasnippets.
Each function therein shall take no arguments.

These variables are hooks to allow multiple ptemplate! blocks
that specify :before-yas, :after, ....")
  (finalize-hook
   nil :documentation
   "Hook to run after template expansion finishes.
At this point, no more files need to be copied and no more
snippets need be expanded.

See also `ptemplate--before-expand-hooks'."))

(defun ptemplate--copy-context<-merge-hooks (contexts)
  "Merge all hook-fields of CONTEXTS.
CONTEXTS is a list of `ptemplate--copy-context's. The fields that
are not FILE-MAP of each CONTEXT are concatenated using `nconc',
and the result, also a `ptemplate--copy-context', returned."
  (ptemplate--copy-context<-new
   :snippet-env (mapcan #'ptemplate--copy-context-snippet-env contexts)
   :snippet-conf-hook
   (mapcan #'ptemplate--copy-context-snippet-conf-hook contexts)
   :before-snippets (mapcan #'ptemplate--copy-context-before-snippets contexts)
   :finalize-hook (mapcan #'ptemplate--copy-context-finalize-hook contexts)))

(defvar ptemplate--cur-copy-context nil
  "Current instance of the `ptemplate--copy-context'.
This variable is `let'-bound when evaluating a template and
modified by the helper functions defined in the \".ptemplate.el
API\" section.")

(defvar ptemplate-target-directory nil
  "Target directory of ptemplate expansion.
You can use this in templates. This variable always ends in the
platform-specific directory separator, so you can use this with
`concat' to build file paths.")

(defvar ptemplate-source-directory nil
  "Source directory of ptemplate expansion.")

(defun ptemplate--eval-template (source &optional target)
  "Evaluate the template given by SOURCE.
Gather all of its files, execute the .ptemplate.el file in it and
return a `ptemplate--copy-context' for it. TARGET specifies where
the template should be expanded to and may be left out for
templates that don't make use of `ptemplate-target-directory' in
:init. Both SOURCE and TARGET are directories, with an optional
trailing slash."
  (setq source (file-name-as-directory source))
  (when target (setq target (file-name-as-directory target)))
  (let ((ptemplate-source-directory source)
        (ptemplate-target-directory target)
        (ptemplate--cur-copy-context
         (ptemplate--copy-context<-new
          :file-map (ptemplate--list-template-files source)))
        (dotptemplate (concat source ".ptemplate.el")))
    (when (file-exists-p dotptemplate)
;;; load .ptemplate.el
      ;; NOTE: arbitrary code execution
      (load-file dotptemplate))
    ptemplate--cur-copy-context))

(defun ptemplate--remove-duplicate-files (files dup-cb)
  "Find and remove duplicates in FILES.
FILES shall be a list of template file mappings (see
`ptemplate--copy-context-file-map'\). If a duplicate is
encountered, call DUP-CB using `funcall' and pass to it `car' of
the mapping that came earlier and the (SRC . TARGET\) cons that
was encountered later.

Return a new list of mappings with all duplicates removed
\(non-destructively\).

This function uses a hashmap and is as such efficient for large
lists, but doesn't use constant memory."
  ;; hashmap of all target files mapped to `t'
  (cl-loop with known-targets = (make-hash-table :test 'equal)
           for file in files
           for target = (ptemplate--file-mapping-target file)

           for prev-source = (gethash target known-targets)
           if prev-source
           ;; already encountered? call DUP-CB
           do (funcall dup-cb prev-source file)
           ;; remember it as encountered and collect it, since it was first
           else do (puthash target file known-targets) and collect file))

(defun ptemplate--warn-dup-mapping (earlier current)
  "Warn that a file was already mapped earlier.
EARLIER should be the mapping found earlier, while CURRENT is the
one that caused the warning.

For use with `ptemplate--remove-duplicate-files'."
  (lwarn
   '(ptemplate ptemplate-expand-template) :warning
   "duplicate mappings encountered: \"%s\" came before \"%s\""
   earlier current))

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
  (cl-loop with snippet-env = `((ptemplate-source-directory . ,source)
                                (ptemplate-target-directory . ,target)
                                ,@(ptemplate--copy-context-snippet-env context))
           with dup-file-map = (ptemplate--copy-context-file-map context)
           with mappings = (ptemplate--remove-duplicate-files
                            dup-file-map #'ptemplate--warn-dup-mapping)
           for mapping in mappings
           for src = (ptemplate--file-mapping-src mapping)
           for targetf = (ptemplate--file-mapping-target mapping)

           for realsrc =
           (and src
                (concat (or (ptemplate--file-mapping-prefix mapping) source)
                        src))
           ;; all directories from `ptemplate--list-template-files' end in '/'
           for src-dir? = (and src (directory-name-p realsrc))
           for realtarget = (concat target targetf)

           for snippet-setup-hook =
           (ptemplate--file-mapping-snippet-setup-hook mapping)

;;; `ptemplate--copy-context->execute': support nil maps
           if src do
           (make-directory
            ;; directories need to be created "as-is" (they may potentially be
            ;; empty); files must not be created as directories however but
            ;; their containing directories instead. This avoids prompts asking
            ;; the user if they really want to save a file even though its
            ;; containing directory was not made yet.
            (if src-dir? realtarget (file-name-directory realtarget))
            t)

           and unless src-dir?
           if (string-suffix-p ".yas" src)
           collect (ptemplate--snippet-chain-mapping<-new
                    :src realsrc :target realtarget
                    :setup-hook snippet-setup-hook) into yasnippets
           else if (string-suffix-p ".autoyas" src)
           do (ptemplate--autoyas-expand realsrc realtarget snippet-env
                                         snippet-setup-hook)
           else do (copy-file realsrc realtarget)

           finally do
           (run-hooks 'ptemplate--before-snippet-hook)
           (ptemplate--snippet-chain->start
            yasnippets snippet-env
            (ptemplate--copy-context-finalize-hook context)
            (ptemplate--copy-context-snippet-conf-hook context))))

;;; Public API
(defun ptemplate--list-dir-dirs (dir)
  "Like `ptemplate-list-dir', but only include directories.
DIR specifies the path to the directory to list."
  (cl-delete-if-not #'file-directory-p (ptemplate--list-dir dir)))

(defun ptemplate-list-template-dir (dir)
  "List all templates in directory DIR.
The result is of the form (TYPE ((NAME . PATH)...))...."
  (let* ((type-dirs (ptemplate--list-dir-dirs dir))
         (types (mapcar #'file-name-base type-dirs))
         (name-dirs (cl-loop for tdir in type-dirs collect
                             (ptemplate--list-dir-dirs tdir)))
         (name-dir-pairs (cl-loop for name-dir in name-dirs collect
                                  (cl-loop for dir in name-dir collect
                                           (cons (file-name-base dir) dir)))))
    (cl-mapcar #'cons types name-dir-pairs)))

(defun ptemplate-list-templates (template-dirs)
  "List all templates in TEMPLATE-DIRS.
The result is an alist ((TYPE (NAME . PATH)...)...)."
  (mapcan #'ptemplate-list-template-dir template-dirs))

(defcustom ptemplate-project-template-dirs '()
  "List of directories containing project templates.
Each directory therein shall be a directory of directories, the
latter specifying the types of templates and the former the names
of the templates.

The templates defined by this list are used in
`ptemplate-new-project', when called interactively. Analogous to
the variable `yas-snippet-dirs'."
  :group 'ptemplate
  :type '(repeat string))

(defun ptemplate-list-project-templates ()
  "List all templates in `ptemplate-project-template-dirs'."
  (ptemplate-list-templates ptemplate-project-template-dirs))

(defcustom ptemplate-directory-template-dirs '()
  "List of directories containing directory templates.
Like `ptemplate-project-template-dirs', but for
`ptemplate-expand-template'."
  :group 'ptemplate
  :type '(repeat string))

(defun ptemplate-list-directory-templates ()
  "List all templates in `ptemplate-project-template-dirs'."
  (ptemplate-list-templates ptemplate-directory-template-dirs))

(defun ptemplate--list-templates-helm (templates)
  "Make a list of helm sources from the user's templates.
TEMPLATES specifies the list of templates for the user to select
from, and is as returned by `ptemplate-list-templates'.

Convert each (TYPE . TEMPLATES) pair into a helm source with TYPE
as its header.

Helm (in particular, helm-source.el) must already be loaded when
this function is called."
  (declare-function helm-make-source "helm" (name class &rest args))
  (cl-loop for (type . templates) in templates collect
           (helm-make-source type 'helm-source-sync :candidates templates)))

(defun ptemplate-prompt-template-helm (templates)
  "Prompt for a template using `helm'.
TEMPLATES is as returned by `ptemplate-list-templates'. The
prompt is a `helm' prompt where all templates are categorized
under their types (as `helm' sources). The return value is the
path to the template, as a string.

This function's API is not stable, and it is only for use in
`ptemplate-template-prompt-function'."
  (require 'helm)
  (declare-function helm "helm")
  (helm :sources (ptemplate--list-templates-helm templates)
        :buffer "*helm ptemplate*"))

(defface ptemplate-type-face '((t :inherit font-lock-function-name-face))
  "Face used to show template types in for the :completing-read backend.
When :completing-read is used as backend in
`ptemplate-template-prompt-function', all entries have a (TYPE)
STRING appended to it. That TYPE is propertized with this face."
  :group 'ptemplate-faces)

(defun ptemplate--list-templates-completing-read (templates)
  "`ptemplate--list-templates-helm', but for `completing-read'.
Returns an alist mapping (propertized) strings, of the form
\"<name> <type>\", to template paths. TEMPLATES is a list of
templates, as returned by `ptemplate-list-templates'."
  (cl-loop for (type . templates) in templates nconc
           (let ((category (propertize (format "(%s)" type)
                                       'face 'ptemplate-type-face)))
             (cl-loop for (name . path) in templates collect
                      (cons (concat name " " category) path)))))

(defvar ptemplate-completing-read-history nil
  "History variable for `completing-read'-based template prompts.
Used by `ptemplate-prompt-template-completing-read' to remember
the history.")

(defun ptemplate-prompt-template-completing-read (templates)
  "Prompt for a template using `completing-read'.
The prompt is a list of \"NAME (TYPE)\" and uses
`completing-read', so can be used with anything that isn't helm.

TEMPLATES is as returned by `ptemplate-list-templates'.

This function's API is not stable, and it for use only for use in
`ptemplate-template-prompt-function' and conforms to its API."
  (let ((ptemplates (ptemplate--list-templates-completing-read templates)))
    (or
     (alist-get (completing-read "Select template: " ptemplates
                                 nil t nil 'ptemplate-completing-read-history)
                ptemplates nil nil #'string=)
     (user-error "Please select a template"))))

(defcustom ptemplate-template-prompt-function
  #'ptemplate-prompt-template-completing-read
  "Prompting method to use to read a template from the user.
The function shall take a single argument, the list of templates
\(as returned by `ptemplates-list-templates'\) and return the
path to the template as a string."
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

(defcustom ptemplate-post-expand-hook '()
  "Hook run after expanding a template finishes.
All snippet variables are still bound when this is executed, and
you can acquire the expansion source and target using
`ptemplate-source-directory' and `ptemplate-target-directory'.
The functions therein are called without arguments."
  :group 'ptemplate
  :type 'hook)

;;;###autoload
(defun ptemplate-expand-template (source target)
  "Expand the template in SOURCE to TARGET.
If called interactively, SOURCE is prompted using
`ptemplate-template-prompt-function' and TARGET using
`read-file-name'"
  (interactive (list (funcall ptemplate-template-prompt-function
                              (ptemplate-list-directory-templates))
                     (read-file-name "Expand to: ")))
  (let ((context (ptemplate--eval-template source target)))
    ;; ensure `ptemplate-post-expand-hook' is run
    (ptemplate--appendlf ptemplate-post-expand-hook
                         (ptemplate--copy-context-finalize-hook context))
    (ptemplate--copy-context->execute context source target)))

;;;###autoload
(defun ptemplate-new-project (source target)
  "Create a new project based on a template.
Like `ptemplate-expand-template', but ensure that TARGET doesn't
exist and prompt for TARGET differently. SOURCE and TARGET are
passed to `ptemplate-expand-template' unmodified. If called
interactively, TARGET is prompted using `read-file-name', with
the initial directory looked up in `ptemplate-workspace-alist'
using SOURCE's type, defaulting to `ptemplate-default-workspace'.
If even that is nil, use `default-directory'."
  (interactive (let ((template (funcall ptemplate-template-prompt-function
                                        (ptemplate-list-project-templates))))
                 (list template (ptemplate--prompt-target template))))
  (when (file-exists-p target)
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
that \"(string-match-p (ptemplate--make-basename-regex
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
  (setf (ptemplate--copy-context-file-map ptemplate--cur-copy-context)
        (cl-delete-if
         (lambda (src-targetf)
           (string-match-p regex (ptemplate--file-mapping-src src-targetf)))
         (ptemplate--copy-context-file-map ptemplate--cur-copy-context))))

(defun ptemplate--puthash-filemap (file-map table)
  "Insert FILE-MAP's targets into the hash table TABLE.
FILE-MAP shall be a `ptemplate--copy-context-file-map' and TABLE
a hash-table with :test `equal'.

Insert each TARGET of FILE-MAP into TABLE as the KEY, with the
VALUE t."
  (dolist (mapping file-map)
    (puthash (ptemplate--file-mapping-target mapping) t table)))

(defun ptemplate--override-filemap (table file-map)
  "Remove all entries from FILE-MAP already in TABLE non-destructively.
Return the result. See `ptemplate--puthash-filemap' for details."
  (cl-remove-if (lambda (x) (gethash (ptemplate--file-mapping-target x) table))
                file-map))

(defun ptemplate--merge-filemaps (file-maps)
  "Merge FILE-MAPS non-destructively into a single file-map.
Each entry in FILE-MAPS shall be a
`ptemplate--copy-context-file-map'.

Remove each entry whose TARGET was mapped in an earlier FILE-MAP,
meaning that earlier FILE-MAPs take precedence. Do not remove
duplicates in any FILE-MAP."
  (cl-loop with visited-targets = (make-hash-table :test #'equal)
           for file-map in file-maps
           ;; we must use `append' here, as otherwise a wrong FILE-MAP will be
           ;; inserted into VISITED-TARGETS
           append (ptemplate--override-filemap visited-targets file-map)
           do (ptemplate--puthash-filemap file-map visited-targets)))

(defun ptemplate--override-files (file-maps)
  "Override all mappings in FILE-MAPS and apply the result.
Store the result in `ptemplate--copy-context-file-map'.

See `ptemplate--merge-filemaps' for details."
  (setf (ptemplate--copy-context-file-map ptemplate--cur-copy-context)
        (ptemplate--merge-filemaps file-maps)))

(defun ptemplate--normalize-user-path-dir (path)
  "`ptemplate--normalize-user-path', but yield a directory.
PATH is transformed according to it and the result made a
directory path."
  (file-name-as-directory (ptemplate--normalize-user-path path)))

;;; .ptemplate.el API
(defun ptemplate-map (src target)
  "Map SRC to TARGET for expansion.
SRC is a path relative to the ptemplate being expanded and TARGET
is a path relative to the expansion target.

SRC can also be nil, in which case nothing would be copied, but
TARGET would shadow mappings from inherited or included
templates."
  (cl-pushnew
   (ptemplate--file-mapping<-new
    :src (and src (ptemplate--normalize-user-path src))
    :target (ptemplate--normalize-user-path target))
   (ptemplate--copy-context-file-map ptemplate--cur-copy-context)))

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
  (let ((remap-regex
         (concat "\\`" (regexp-quote (ptemplate--normalize-user-path-dir src))))
        (target (ptemplate--normalize-user-path-dir target)))
    (dolist (mapping (ptemplate--copy-context-file-map
                      ptemplate--cur-copy-context))
      (setf (ptemplate--file-mapping-target mapping)
            (replace-regexp-in-string
             remap-regex target (ptemplate--file-mapping-target mapping)
             nil t)))))

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
path to ignore (see `ptemplate--make-path-regex'\)."
  (ptemplate--prune-template-files
   (ptemplate--make-ignore-regex regexes)))

(defun ptemplate-include (&rest dirs)
  "Use all files in DIRS for expansion.
The files are added as if they were part of the current template
being expanded, except that .ptemplate.el and .ptemplate.elc are
valid filenames and are not interpreted.

Mappings from the current template have the highest precedence,
followed by the mappings from DIRS, where mappings from earlier
DIRS win.

To get the opposite behaviour, use `ptemplate-include-override'."
  (ptemplate--override-files
   (cons (ptemplate--copy-context-file-map ptemplate--cur-copy-context)
         (mapcar #'ptemplate--list-template-dir-files-abs dirs))))

(defun ptemplate-include-override (&rest dirs)
  "The opposite of `ptemplate-include' in terms of behaviour.
Later DIRS take precedence. See `ptemplate-include' for details."
  (ptemplate--override-files
   (nconc
    ;; mappings from later DIRs should win.
    (nreverse (mapcar #'ptemplate--list-template-dir-files-abs dirs))
    ;; the current template's mappings
    (list (ptemplate--copy-context-file-map ptemplate--cur-copy-context)))))

(defun ptemplate--inherit-templates (srcs)
  "Inherit the hooks of all templates in SRCS.
This functions evaluates all templates in the template path list
SRCS and prepends their hooks (as defined by
`ptemplate--copy-context<-merge-hooks'\) to the current global
ones.

Return a list of path mappings corresponding to SRCS, each of
which refer to their corresponding sources (see
`ptemplate--file-map->absoluteify'\).

See also `ptemplate-inherit' and `ptemplate-inherit-overriding'."
  (let* ((inherit-contexts (mapcar #'ptemplate--eval-template srcs))
         (merged-context
          (ptemplate--copy-context<-merge-hooks
           (append inherit-contexts (list ptemplate--cur-copy-context)))))
    ;; don't truncate the file-map
    (setf (ptemplate--copy-context-file-map merged-context)
          (ptemplate--copy-context-file-map ptemplate--cur-copy-context))
    (setq ptemplate--cur-copy-context merged-context)
    (cl-mapcar #'ptemplate--file-map->absoluteify srcs
               (mapcar #'ptemplate--copy-context-file-map inherit-contexts))))

(defun ptemplate-inherit (&rest srcs)
  "Like `ptemplate-include', but also evaluate the .ptemplate.el file.
The hooks of all templates in SRCS are run before the current
template's ones and the files from SRCS are added for expansion.
File maps defined in the current template take precedence, so can
be used to override mappings from SRCS. Mappings from templates
that come earlier in SRCS take precedence over those from later
templates. To ignore files from SRCS, map them from nil using
:map or `ptemplate-map' before calling this function."
  ;; NOTE: templates that come later in DIRS are overridden.
  (ptemplate--override-files
   (cons (ptemplate--copy-context-file-map ptemplate--cur-copy-context)
         (ptemplate--inherit-templates srcs))))

(defun ptemplate-inherit-overriding (&rest srcs)
  "The opposite of `ptemplate-inherit' in terms of behaviour.
To `ptemplate-inherit' what `ptemplate-include-override' is to
`ptemplate-include'. Files from templates that come later in SRCS
take precedence."
  (ptemplate--override-files
   (nconc
    (nreverse (ptemplate--inherit-templates srcs))
    (list (ptemplate--copy-context-file-map ptemplate--cur-copy-context)))))

(defun ptemplate-source (dir)
  "Return DIR as if relative to `ptemplate-source-directory'."
  (concat ptemplate-source-directory dir))

(defun ptemplate-target (dir)
  "Return DIR as if relative to `ptemplate-target-directory'."
  (concat ptemplate-target-directory dir))

(defmacro ptemplate-with-file-sandbox (&rest body)
  "Execute BODY with an isolated file map.
The file map in which BODY is executed is empty and appended to
the global one afterwards.

Return the result of the last BODY form."
  (declare (indent 0))
  `(let ((--ptemplate-old-filemap--
          (ptemplate--copy-context-file-map ptemplate--cur-copy-context)))
     (setf (ptemplate--copy-context-file-map ptemplate--cur-copy-context) nil)
     (prog1
         (progn ,@body)
       (ptemplate--prependlf
        --ptemplate--old-filemap--
        (ptemplate--copy-context-file-map ptemplate--cur-copy-context)))))

;;; snippet configuration
(defun ptemplate-target-relative ()
  "Get the expansion-target relative path of the buffer.
Only for use in `ptemplate-snippet-setup'"
  (concat (ptemplate--unix-to-native-path "./")
          (file-relative-name buffer-file-name ptemplate-target-directory)))

(defun ptemplate--snippet-name-p (f)
  "Check if F names an interactive or non-interactive snippet."
  (member (file-name-extension f) '("autoyas" "yas")))

(defun ptemplate-snippet-setup (snippets callback)
  "Run CALLBACK to configure SNIPPETS.
SNIPPETS is a list of target-relative file snippet targets
\(.yas, .autoyas\). If a yasnippet expands to them, CALLBACK is
called and can configure them further. All variables defined in
:snippet-env, :snippet-let, ... are available to it."
  (cl-loop with snippets = (mapcar #'ptemplate--normalize-user-path snippets)
           for mapping in (ptemplate--copy-context-file-map
                           ptemplate--cur-copy-context)
           for src = (ptemplate--file-mapping-src mapping)
           for target = (ptemplate--file-mapping-target mapping)
           if (member target snippets)
           if (ptemplate--snippet-name-p src) do
           (ptemplate--appendf
            callback (ptemplate--file-mapping-snippet-setup-hook mapping))
           else do
           (lwarn '(ptemplate-snippet-setup) :error
                  "trying to configure non-snippet mapping %S" mapping)))

(defmacro ptemplate-snippet-setup! (snippets &rest body)
  "Like `ptemplate--snippet-setup', but as a macro.
SNIPPETS shall be an expression yielding a list snippet-chain
  (declare (indent 1))
expansion targets. BODY will be executed for each of them."
  `(ptemplate-snippet-setup
    ,snippets (lambda () "Run to configure snippet-chain buffers." ,@body)))

(defun ptemplate-add-snippet-next-hook (&rest functions)
  "Add `ptemplate-snippet-chain-next' functions.
Each function in FUNCTIONS is run at the start of the above
function.

Only for use with `ptemplate-snippet-setup'."
  (dolist (fn functions)
    (add-hook 'ptemplate--snippet-chain-next-hook fn nil t)))

(defmacro ptemplate-add-snippet-next-hook! (&rest body)
  "Like `ptemplate-add-snippet-next-hook', but as a macro.
Add a lambda containing each form in BODY."
  `(ptemplate-add-snippet-next-hook
    (lambda () "Run in `ptemplate-snippet-chain-next'." ,@body)))

(defun ptemplate-set-snippet-kill-p (&optional kill-p)
  "Set whether the snippet buffer should be killed.
When continuing the snippet-chain with
`ptemplate-snippet-chain-next', snippet chain buffers would
usually be killed. Use this function to change that to KILL-P.
Only for use in `ptemplate-snippet-setup'."
  (setq ptemplate--snippet-chain-nokill (not kill-p)))

(defun ptemplate-nokill-snippets (snippets)
  "Don't kill SNIPPETS after expansion.
SNIPPETS is a list of target-relative file snippets (.yas) whose
buffers should not be killed in `ptemplate-snippet-chain-next'."
  (ptemplate-snippet-setup snippets #'ptemplate-set-snippet-kill-p))

(defun ptemplate-nokill-snippets! (&rest snippets)
  "Like `ptemplate-nokill-snippets', but SNIPPETS is &rest."
  (ptemplate-nokill-snippets snippets))

;; NOTE: ;;;###autoload is unnecessary here, as `ptemplate!' is only useful in
;; .ptemplate.el files, which are only ever loaded from
;; `ptemplate-expand-template', at which point `ptemplate' is already loaded.
(defmacro ptemplate! (&rest args)
  "Define a smart ptemplate with Elisp.
For use in .ptemplate.el files. ARGS is a plist-like list with
any number of sections, specified as :<section name>
FORM... (like in `use-package'). Sections can appear multiple
times: you could, for example, have multiple :init sections, the
FORMs of which would get evaluated in sequence. Supported keyword
are:

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

:ignore Syntax sugar for `ptemplate-ignore'. Files are pruned
        before :init.

:subdir Make some template-relative paths appear to be in the
        root. Practically, this means not adding its files and
        including it. Evaluated before :init.

:remap ARG shall be of the form (SRC TARGET\; calls
       `ptemplate-remap' on the results of evaluating SRC and
       TARGET. Run after :init.

:remap-rec Like :remap, but call `ptemplate-remap-rec' instead.
           Note that it is undefined whether :remap-rec or :remap
           is executed firsts and their order of appearance is
           insignificant.

:map Syntax sugar for `ptemplate-map'. ARG must be of the form
     (SRC TARGET), both of which are ordinary LISP expressions.
     Run after :remap and :remap-rec.

:inherit Syntax sugar for `ptemplate-inherit'. FORMs may be
         arbitrary Lisp expressions (not just strings). Executed
         after :map.

:open-bg Expressions yielding files (target-relative) to open
         with `find-file-noselect' at the very end of expansion.

:open Like :open-bg, but open the last file using `find-file'.
      Files that are not the last one will be opened using
      `find-file-noselect'.

:late Executed after :inherit.

Note that because .ptemplate.el files just execute arbitrary
code, you could write them entirely without using this
macro (e.g. by modifying hooks directly, ...). However, you
should still use `ptemplate!', as it abstracts away those
internal details, which are subject to change at any time."
  (declare (indent 0))
  (let ((cur-keyword :init)
        init-forms
        before-yas-forms
        after-copy-forms
        finalize-forms open-fg open-bg
        snippet-env
        around-let
        ignore-regexes ignore-expressions
        inherited-templates
        include-dirs
        remap-forms map-forms
        late-forms
        nokill-buffers snippet-setup-forms)
    (dolist (arg args)
      (if (keywordp arg)
          (setq cur-keyword arg)
        (pcase cur-keyword
          (:init (push arg init-forms))
          (:before-snippets (push arg before-yas-forms))
          (:after-copy (push arg after-copy-forms))
          (:finalize (push arg finalize-forms))
          (:open (push arg open-fg))
          (:open-bg (push arg open-bg))
          (:snippet-env (push arg snippet-env))
          (:snippet-let
           (push (if (consp arg) (car arg) arg) snippet-env)
           (push arg around-let))
          (:ignore (push arg (if (stringp arg) ignore-regexes
                               ignore-expressions)))
          (:inherit (push arg inherited-templates))
          (:subdir (let ((simplified-path (ptemplate--simplify-user-path arg)))
                     (push (concat (ptemplate--unix-to-native-path "/")
                                   simplified-path)
                           ignore-regexes)
                     (push simplified-path include-dirs)))
          (:remap (push `(ptemplate-remap ,(car arg) ,(cadr arg)) remap-forms))
          (:remap-rec (push `(ptemplate-remap-rec ,(car arg) ,(cadr arg))
                            remap-forms))
          (:map (push `(ptemplate-map ,(car arg) ,(cadr arg)) map-forms))
          (:late (push arg late-forms))
          (:nokill (push arg nokill-buffers))
          (:snippet-setup
           (push `(ptemplate-snippet-setup (lambda () ,@(cdr arg)) ,@(car arg))
                 snippet-setup-forms))
;;; HACKING: add new `ptemplate' blocks before here

;;; `ptemplate!' error handling
          (_ (error "`ptemplate!': unknown keyword '%s'" cur-keyword)))))
    (macroexp-let*
     (nreverse around-let)
     (macroexp-progn
      (nconc
       (when ignore-regexes
         `((ptemplate-ignore ,@ignore-regexes)))
       (when ignore-expressions
         `((ptemplate-ignore ,@ignore-expressions)))
       (when include-dirs
         ;; include dirs specified first take precedence
         `((ptemplate-include
            ,@(cl-loop for dir in (nreverse include-dirs)
                       collect (list #'ptemplate-source dir)))))
       (nreverse init-forms)
       (nreverse remap-forms)
       (nreverse map-forms)
       ;; execute late to give the user the chance to map files in a way that
       ;; overrides first.
       (when inherited-templates
         `((ptemplate-inherit ,@inherited-templates)))
       (nreverse late-forms)
       (when before-yas-forms
         `((add-hook 'ptemplate--before-snippet-hook
                     (lambda () "Run before expanding snippets."
                       ,@(nreverse before-yas-forms)))))
       (when after-copy-forms
         `((add-hook 'ptemplate--after-copy-hook
                     (lambda () "Run after copying files."
                       ,@(nreverse after-copy-forms)))))
       (when (or finalize-forms open-bg open-fg)
         `((add-hook 'ptemplate--finalize-hook
                     (lambda () "Run after template expansion finishes."
                       ,@(nreverse finalize-forms)
;;; :open-bg, :open
                       ;; First open all files from :open-bg and then all files
                       ;; except the last :open file (push prepends to the list,
                       ;; so they are in reverse) using `find-file-noselect'.
                       ;; Edge case: open-fg is nil; however, `cdr' nil -> nil.
                       ,@(cl-loop for bg-f in (nconc (nreverse open-bg)
                                                     (nreverse (cdr open-fg)))
                                  collect `(find-file-noselect (ptemplate-target ,bg-f)))
                       ;; Are there any files to be opened in the foreground? If
                       ;; yes, open only the first that way.
                       ,@(when open-fg
                           (list (car open-fg)))))))
       (when snippet-env
         `((ptemplate--appendlf
            (list ,@(cl-loop
                     for var in snippet-env collect
                     (if (listp var)
                         (list #'cons (macroexp-quote (car var)) (cadr var))
                       `(cons ',var ,var))))
            (ptemplate--copy-context-snippet-env ptemplate--cur-copy-context))))
       (nreverse snippet-setup-forms)
       (when nokill-buffers
         ;; `nreverse' is technically unnecessary here, but it looks better in
         ;; the template expansion.
         `((ptemplate-nokill-snippets! ,@(nreverse nokill-buffers)))))))))

(provide 'ptemplate)
;;; ptemplate.el ends here

;;; Fix spell checking
;; LocalWords: UNTRUSTED
;; LocalWords: Nikita
;; LocalWords: Bloshchanevich
;; LocalWords: emacs
;; LocalWords: Elisp
;; LocalWords: ispell
;; LocalWords: ptemplate
;; LocalWords: ptemplate's
;; LocalWords: yasnippet
;; LocalWords: yasnippets
;; LocalWords: yas
;; LocalWords: autoyas
;; LocalWords: nocopy
;; LocalWords: el
;; LocalWords: elc
;; LocalWords: Alist
;; LocalWords: alist
;; LocalWords: plist
;; LocalWords: ENV
;; LocalWords: NEWBUF
;; LocalWords: env
;; LocalWords: DIRS
;; LocalWords: SRC
;; LocalWords: SRCS
;; LocalWords: RSRC
;; LocalWords: FSRC
;; LocalWords: DUP
;; LocalWords: FORMs
;; LocalWords: CONTEXT's
;; LocalWords: init

;; Local Variables:
;; fill-column: 79
;; ispell-dictionary: "en"
;; End:
