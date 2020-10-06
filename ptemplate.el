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
;; Version: 0.2

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
(require 'subr-x)                       ; `string-join'

;;; `declare-function'
(declare-function yas-minor-mode "yasnippet" (&optional arg))
(declare-function yas-expand-snippet "yasnippet" (s &optional start end env))

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
always being ignored. This way \(`pop' (`cdr'
`ptemplate--snippet-chain')\) modifies it in a way that is shared
between all buffers.

See also `ptemplate--snippet-chain-start'.")

(defvar-local ptemplate--snippet-chain-finalize-hook nil
  "Hook to run after the snippet chain finishes.
Each function therein gets called without arguments.

This hook needs to be a separate variable and cannot be
implemented by simply appending it to `ptemplate--snippet-chain'.
This is because in that case it would get executed too early if
`ptemplate--snippet-chain-later' were called at least once, as
then it wouldn't be the last element anymore.")

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
  "Make the next snippet/buffer in the snippet chain current."
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
        (require 'yasnippet)
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
      ;; HACK we cannot use `ptemplate--setup-snippet-env', since this isn't a
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

;;; common utility functions
(defun ptemplate--unix-to-native-path (path)
  "Replace slashes in PATH with the platform's directory separator.
PATH is a file path, as a string, assumed to use slashes as
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
  (cl-loop for file in (let ((default-directory (expand-file-name path)))
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

(defun ptemplate--file-map-absolute (src file-map)
  "Make FILE-MAP refer to SRC.
Each mapping \(FSRC . TARGET\) is transformed into \((SRC . FSRC)
. TARGET\), unless it already is of that form \(needed for nested
inheritance\).

Return the result.

See `ptemplate--template-files' for a description of FILE-MAP."
  (cl-loop with src = (file-name-as-directory src)
           for (fsrc . target) in file-map
           collect (cons (if (consp fsrc) fsrc (cons src fsrc)) target)))

(defun ptemplate--list-template-dir-files-abs (path)
  "Like `ptemplate--list-template-dir-files'.
The difference is that this version yields an absolute mapping
instead \(see `ptemplate--file-map-absolute'\).

PATH specifies that path to the template."
  (ptemplate--file-map-absolute path (ptemplate--list-template-dir-files path)))

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
    (require 'yasnippet)

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

(defvar ptemplate--snippet-env nil
  "Environment used for snippet expansion.
Alist of (SYMBOL . VALUE), like `ptemplate--snippet-chain-env'
but for the entire template.")

(defvar ptemplate--template-files nil
  "Alist mapping template source files to their targets.
Alist \(SRC . TARGET\), where SRC and TARGET are strings (see
`ptemplate-map' for details). Additionally, SRC may be a cons of
the form \(PREFIX . SRC\), in which case the source path becomes
PREFIX + SRC. TARGET may be nil, in which case nothing shall be
copied.

This variable is always `let'-bound.")

(defvar ptemplate-target-directory nil
  "Target directory of ptemplate expansion.
You can use this in templates. This variable always ends in the
platform-specific directory separator, so you can use this with
`concat' to build file paths.")

(defvar ptemplate-source-directory nil
  "Source directory of ptemplate expansion.")

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
  (declare (doc-string 1))
  (let ((constructor (intern (format "%s<-new" name)))
        (copier (intern (format "%s<-copy" name)))
        (from-env (intern (format "%s<-from-env" name)))
        (to-env (intern (format "%s->to-env" name)))
        (merge-hooks (intern (format "%s<-merge-hooks" name)))
        (with-vars-nil (intern (format "%s->with-vars-nil" name))))
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
       (defmacro ,with-vars-nil (&rest body)
         "`let'-bind the copy context's variables to nil.
Execute BODY in an environment where the variables to which the
fields correspond are bound to nil. Return the result of the last
expression."
         (declare (debug t))
         (backquote
          (let ,(cl-loop for (_ . props) in fields
                         for var = (plist-get props :var)
                         collect var)
            ,',@body)))
;;; HACKING: add new to-be-generated copy-context functions before here
       )))

(ptemplate--define-copy-context ptemplate--copy-context
  "Holds data needed by ptemplate's copy phase.
To acquire this state, a template's file need to be listed and
the .ptemplate.el needs to be evaluated against it.
`ptemplate--copy-context->execute'"
  (before-snippets :var ptemplate--before-snippet-hook)
  (finalize-hook :var ptemplate--finalize-hook)
  (snippet-env :var ptemplate--snippet-env)
  (file-map :var ptemplate--template-files :merge-hooks nil))

(defun ptemplate--eval-template (source &optional target)
  "Evaluate the template given by SOURCE.
Gather all of its files, execute the .ptemplate.el file in it and
return a `ptemplate--copy-context' for it. TARGET specifies where
the template should be expanded to and may be left out for
templates that don't make use of `ptemplate-target-directory' in
:init. Both SOURCE and TARGET are directories, with an optional
trailing slash."
  (setq source (file-name-as-directory source))
  (ptemplate--copy-context->with-vars-nil
   (let ((dotptemplate (concat source ".ptemplate.el"))
         ;; the dotptemplate file should know about source and target.
         (ptemplate-source-directory source)
         (ptemplate-target-directory
          (and target (file-name-as-directory target))))
     (setq ptemplate--template-files (ptemplate--list-template-files source))
;;; load .ptemplate.el
     (when (file-exists-p dotptemplate)
       ;; NOTE: arbitrary code execution
       (load-file dotptemplate))

     (ptemplate--copy-context<-from-env))))

(defun ptemplate--prune-duplicate-files (files dup-cb)
  "Find and remove duplicates in FILES.
FILES shall be a list of template file mappings \(see
`ptemplate--template-files'\). If a duplicate is encountered,
call DUP-CB using `funcall' and pass to it `car' of the mapping
that came earlier and the \(SRC . TARGET\) cons that was
encountered later.

Return a new list of mappings with all duplicates removed
\(non-destructively\).

This function uses a hashmap and is as such efficient for large
lists, but doesn't use constant memory."
  ;; hashmap of all target files mapped to `t'
  (cl-loop with known-targets = (make-hash-table :test 'equal)
           for file in files
           for target = (cdr file)

           for prev-source = (gethash target known-targets)
           if prev-source
           ;; already encountered? call DUP-CB
           do (funcall dup-cb prev-source file)
           ;; remember it as encountered and collect it, since it was first
           else do (puthash target file known-targets) and collect file))

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
           with file-map =
           (ptemplate--prune-duplicate-files
            dup-file-map
            (lambda (prev cur)
              (lwarn '(ptemplate ptemplate-expand-template) :error
                     "duplicate mappings encountered: \"%s\" before \"%s\""
                     prev cur)))
           for (srcpair . targetf) in file-map
           ;; NOTE: If SRCPAIR is nil, SRC becomes nil (no error), because nil
           ;; is `consp' and `cdr' nil is nil.
           for src = (if (consp srcpair) (cdr srcpair) srcpair)
           for realsrc = (when src (concat (if (consp srcpair)
                                               (car srcpair) source) src))
           ;; NOTE: all directories from `ptemplate--list-template-files' end in
           ;; a slash.
           for dir? = (when src (directory-name-p realsrc))

           for realtarget = (concat target targetf)

;;; `ptemplate--copy-context->execute': support nil maps
           if src do
           (make-directory
            ;; directories need to be created "as-is" (they may potentially be
            ;; empty); files must not be created as directories however but
            ;; their containing directories instead. This avoids prompts asking
            ;; the user if they really want to save a file even though its
            ;; containing directory was not made yet.
            (if dir? realtarget (file-name-directory realtarget))
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
Returns an alist mapping \(propertized\) strings, of the form
\"<name> <type>\", to template paths. TEMPLATES is a list of
templates, as returned by `ptemplate-list-templates'."
  (cl-loop for (type . templates) in templates nconc
           (let ((category (propertize (format "(%s)" type)
                                       'face 'ptemplate-type-face)))
             (cl-loop for (name . path) in templates collect
                      (cons (concat name " " category) path)))))

(defvar ptemplate--completing-read-history nil
  "History variable for `completing-read'-based template prompts.
If :completing-read is set as `ptemplate-template-prompt-function',
pass this variable as history argument to `completing-read'.")

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
                                 nil t nil 'ptemplate--completing-read-history)
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
    (setf (ptemplate--copy-context-finalize-hook context)
          (nconc (ptemplate--copy-context-finalize-hook context)
                 ptemplate-post-expand-hook))
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

(defun ptemplate--map-relsrc (file-map)
  "Get the relative source from FILE-MAP.
FILE-MAP shall be a file mapping, as can be found in
`ptemplate--template-files' \((SRC . TARGET)\). If SRC is a cons
that also stores the path to the file, return only the relative
part: \(((TEMPLATE . RSRC) . TARGET\) -> RSRC, otherwise yield
SRC."
  (let ((src (car file-map)))
    (or (cdr-safe src) src)))

(defun ptemplate--prune-template-files (regex)
  "Remove all template whose source files match REGEX.
This function is only supposed to be called from `ptemplate!'."
  (setq ptemplate--template-files
        (cl-delete-if
         (lambda (src-targetf)
           (string-match-p regex (ptemplate--map-relsrc src-targetf)))
         ptemplate--template-files)))

(defun ptemplate--override-files (base-files override)
  "Override all mappings in BASE-FILES with those in OVERRIDE.
Both of them shall be mappings like `ptemplate--template-files'.
BASE-FILES and OVERRIDE may be altered destructively.

Store the result in `ptemplate--template-files'."
  (let ((mapped-targets (make-hash-table :test #'equal)))
    ;; set up hash table: all targets from OVERRIDE are to be remembered, and
    ;; not to be taken from BASE-FILES.
    (dolist (target override)
      (puthash (cdr target) t mapped-targets))
    (setq ptemplate--template-files
          ;; OVERRIDE + all files not mapped to in OVERRIDE from BASE-FILES
          (nconc override (cl-delete-if
                           (lambda (m) (gethash (cdr m) mapped-targets))
                           base-files)))))

;;; .ptemplate.el api
(defun ptemplate-map (src target)
  "Map SRC to TARGET for expansion.
SRC is a path relative to the ptemplate being expanded and TARGET
is a path relative to the expansion target.

SRC can also be nil, in which case nothing would be copied, but
TARGET would shadow mappings from inherited or included
templates."
  (add-to-list 'ptemplate--template-files
               (cons (when src (ptemplate--normalize-user-path src))
                     (ptemplate--normalize-user-path target))))

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
        (target (ptemplate--normalize-user-path target)))
    (dolist (file ptemplate--template-files)
      (when (string-match-p remap-regex (car file))
        (setcdr file (replace-regexp-in-string
                      remap-regex target file nil t))))))

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
  (ptemplate--override-files
   (mapcan #'ptemplate--list-template-dir-files-abs dirs)
   ptemplate--template-files))

(defun ptemplate-include-override (&rest dirs)
  "Like `ptemplate-include', but files in DIRS override."
  (ptemplate--override-files
   ptemplate--template-files
   (mapcan #'ptemplate--list-template-dir-files-abs dirs)))

(defun ptemplate--inherit-templates (srcs)
  "Inherit the hooks of all templates in SRCS.
This functions evaluates all templates in the template path list
SRCS and prepends their hooks \(as defined by
`ptemplate--copy-context<-merge-hooks'\) to the current global
ones.

Return a list of path mappings corresponding to SRCS, each of
which refer to their corresponding sources \(see
`ptemplate--file-map-absolute'\).

See also `ptemplate-inherit' and `ptemplate-inherit-overriding'."
  (let ((inherit-contexts (mapcar #'ptemplate--eval-template srcs))
        ;; prevent `ptemplate--copy-context->to-env' from overriding the global
        ;; file-map (set to nil, as `ptemplate--copy-context<-merge-hooks' leaves
        ;; :file-map nil)
        ptemplate--template-files)
    (ptemplate--copy-context->to-env
     (apply #'ptemplate--copy-context<-merge-hooks
            (append inherit-contexts (list (ptemplate--copy-context<-from-env)))))
    (cl-mapcar #'ptemplate--file-map-absolute srcs
               (mapcar #'ptemplate--copy-context-file-map inherit-contexts))))

(defun ptemplate-inherit (&rest srcs)
  "Inherit all templates in SRCS.
The hooks of all templates in SRCS are run before the current
template's ones and the files from SRCS are added for expansion.
File maps defined in the current template take precedence, so can
be used to override mappings from SRCS. Mappings from templates
that come earlier in SRCS take precedence over those from later
templates. To ignore files from SRCS, map them from nil using
:map or `ptemplate-map' before calling this function."
  ;; NOTE: templates that come later in DIRS are overriden.
  (let ((to-inherit (apply #'nconc (ptemplate--inherit-templates srcs))))
    (ptemplate--override-files to-inherit ptemplate--template-files)))

(defun ptemplate-inherit-overriding (&rest srcs)
  "Like `ptemplate-inherit', but files in SRCS take precedence.
Files from templates that come later in SRCS take precedence."
  (let ((to-inherit
         (apply #'nconc (nreverse (ptemplate--inherit-templates srcs)))))
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

:open Like :open-bg, but open the last file using `find-file'.
      Files that are not the last one will be opened using
      `find-file-noselect'.

Note that because .ptemplate.el files just execute arbitrary
code, you could write them entirely without using this
macro (e.g. by modifying hooks directly, ...). However, you
should still use `ptemplate!', as it abstracts away those
internal details, which are subject to change at any time."
  (declare (indent 0))
  (let ((cur-keyword :init)
        init-forms
        before-yas-eval
        after-copy-eval
        finalize-eval open-fg open-bg
        snippet-env
        around-let
        ignore-regexes ignore-expressions
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
          (:remap (push `(ptemplate-remap ,(car arg) ,(cadr arg)) remap-eval))
          (:remap-rec (push `(ptemplate-remap-rec ,(car arg) ,(cadr arg))
                            remap-eval))
          (:map (push `(ptemplate-map ,(car arg) ,(cadr arg)) map-eval)))))
    (macroexp-let*
     (nreverse around-let)
     (macroexp-progn
      (nconc
       (when ignore-regexes
         `((ptemplate-ignore ,ignore-regexes)))
       (when ignore-expressions
         `((ptemplate-ignore ,@ignore-expressions)))
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
       (when (or finalize-eval open-bg open-fg)
         `((add-hook 'ptemplate--finalize-hook
                     (lambda () "Run after template expansion finishes."
                       ,@(nreverse finalize-eval)
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
