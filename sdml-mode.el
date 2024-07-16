;;; sdml-mode.el --- Major mode for SDML -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Version: 0.1.9
;; Package-Requires: ((emacs "28.1") (tree-sitter "0.18.0") (tree-sitter-indent "0.3"))
;; URL: https://github.com/johnstonskj/emacs-sdml-mode
;; Keywords: languages tools

;;; License:

;; Copyright (c) 2023, 2024 Simon Johnston
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;;
;; This package provides a tree-sitter based major mode for SDML.

;;
;;         ___          _____          ___
;;        /  /\        /  /::\        /__/\
;;       /  /:/_      /  /:/\:\      |  |::\
;;      /  /:/ /\    /  /:/  \:\     |  |:|:\    ___     ___
;;     /  /:/ /::\  /__/:/ \__\:|  __|__|:|\:\  /__/\   /  /\
;;    /__/:/ /:/\:\ \  \:\ /  /:/ /__/::::| \:\ \  \:\ /  /:/
;;    \  \:\/:/~/:/  \  \:\  /:/  \  \:\~~\__\/  \  \:\  /:/
;;     \  \::/ /:/    \  \:\/:/    \  \:\         \  \:\/:/
;;      \__\/ /:/      \  \::/      \  \:\         \  \::/
;;        /__/:/        \__\/        \  \:\         \__\/
;;        \__\/          Domain       \__\/          Language
;;         Simple                      Modeling
;;

;;
;; Installing
;;
;; `(use-package sdml-mode
;;    :ensure t)'

;;
;; Usage
;;
;; Once installed the major mode should be used for any file ending in `.sdm'
;; or `.sdml' with highlighting and indentation support.

;; Debug
;;
;; `\\[tree-sitter-debug-mode]' -- open tree-sitter debug view
;; `\\[tree-sitter-query-builder]' -- open tree-sitter query builder

;; Abbreviations and Skeletons
;;
;; This package creates a new `abbrev-table', named `sdml-mode-abbrev-table', which
;; provides a number of useful skeletons for the following.  `abbrev-mode' is enabled
;; by `sdml-mode' and when typing one of the abbreviations below type space to
;; expand.
;;
;; Typing `d t SPCA' will prompt for a name and expand into the SDML declaration
;; `datatype MyName ← opaque _' where the underscore character represents the new
;; cursor position.
;;
;; Declarations: mo=module, dt=datatype, en=enum, ev=event, pr=property,
;;   st=structure, un=union
;;
;; Annotation Properties: pal=skos:altLabel, pdf=skos:definition,
;;   ped=skos:editorialNote, ppl=skos:prefLabel, pco=rdfs:comment
;;
;; Constraints: ci=informal, cf=formal, all=universal, any=existential
;;
;; Datatypes: db=boolean, dd=decimal, df=double, dh=binary, di=integer,
;;   sd=string, du=unsigned
;;

;; Interactive Commands
;;

;; `sdml-mode-validate-current-buffer' (\\[sdml-mode-validate-current-buffer]) to
;; validate and show errors for the current buffer's module.
;;
;; Adding this as a save-hook allows validation on every save of a buffer.
;;
;; `(add-hook 'after-save-hook 'sdml-validate-current-buffer)'
;;
;; `sdml-mode-validate-file' (\\[sdml-mode-validate-file]) to
;; validate and show errors for a specified file name.
;;
;; `sdml-mode-current-buffer-dependency-tree' (\\[sdml-mode-current-buffer-dependency-tree])
;; to display the dependencies of the current buffer's module as a textual tree.
;;
;; `sdml-mode-current-buffer-dependency-graph' (\\[sdml-mode-current-buffer-dependency-graph])
;; to display the dependencies of the current buffer's module as an SVG directed graph.
;;

;; Extensions
;;
;; `flycheck-sdml' -- Integrate the lisp-based linter with sdml-mode.
;; `ob-sdml' -- Support SDML org-mode Babel blocks
;; `sdml-fold' -- Provide code-folding for SDML source.
;; `sdml-ispell' -- Provide spell checking for specific SDML Tree-Sitter nodes.
;;

;;; Code:

(eval-when-compile
  (require 'rx)) ;; built-in

(require 'tree-sitter)

(require 'ansi-color) ;; built-in
(require 'compile) ;; built-in

(require 'sdml-mode-abbrev)
(require 'sdml-mode-cli)
(require 'sdml-mode-ctags)
(require 'sdml-mode-hl)
(require 'sdml-mode-indent)

;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(defgroup sdml nil
  "SDML language support."
  :tag "SDML"
  :prefix "sdml-"
  :group 'languages)

(defcustom sdml-mode-prettify-symbols-alist
  '(("->" . ?→) ("<-" . ?←) ("forall" ?∀) ("exists" ?∃) ("in" ?∈) (":=" ?≔))
  "An alist of symbol replacements used for `prettify-symbols-alist'."
  :tag "Symbol mapping for prettify"
  :type '(repeat (cons string character))
  :group 'sdml)

(defcustom sdml-mode-validation-level 'warnings
  "The level of information to provide during validation."
  :tag "Validation level"
  :type '(choice (const :tag "None" none)
                 (const :tag "Bugs" bugs)
                 (const :tag "Errors" errors)
                 (const :tag "Warnings" warnings)
                 (const :tag "Notes" notes)
                 (const :tag "Help" help)
                 (const :tag "All" all))
  :group 'sdml)

;; --------------------------------------------------------------------------
;; Tree-Sitter  Library Setup
;; --------------------------------------------------------------------------

(defun sdml-mode--tree-sitter-setup (&optional dylib-file)
  "Internal: Load and connect the parser library in DYLIB-FILE.

Load the dynamic library, either with the explicit path
in DYLIB-FILE, or by searching the path in `tree-sitter-load-path'.
The parser library will be named  `tree-sitter-sdml' with a
platform-specific extension in `tree-sitter-load-suffixes'."
  (unless (assoc 'sdml tree-sitter-languages)
    ;; Load the dynamic library from the search path.
    (tree-sitter-load 'sdml dylib-file))

  (unless (assoc 'sdml-mode tree-sitter-major-mode-language-alist)
    ;; Connect the major mode to the loaded library. This is how the key
    ;; variable `tree-sitter-language' gets set which is used in highlighting
    ;; and more.
    (add-to-list 'tree-sitter-major-mode-language-alist
                 '(sdml-mode . sdml))))

;; --------------------------------------------------------------------------
;; Buffer Commands  Validation
;; --------------------------------------------------------------------------

(defconst sdml-mode-validation-error-regexp
  (rx bol
      (or (group (or "bug" "error")) (group "warning") (group (or "help" "note")))
      ?\[ (char "BEWI") (= 4 digit) ?\] ?: (* (char " \t"))
      (group (+ (not ?\n))) ?\n
      "   ┌─ "
      (group (+ (not ?:)))
      ?:
      (group (+ digit))
      ?:
      (group (+ digit))
      ?\n))

(defun sdml-mode--command-setup ()
  "Internal: Setup buffer commands."
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (setq-local ansi-color-for-compilation-mode t)
  (add-to-list 'compilation-error-regexp-alist-alist
               `(sdml ,sdml-mode-validation-error-regexp 5 6 7 (2 . 3)))
  (add-to-list 'compilation-error-regexp-alist 'sdml))

(defun sdml-mode--exec-validator (file-name)
  "Internal: execute the command-line validator for FILE-NAME."
  (let ((cmd-line (sdml-mode-cli-make-command
                   "validate"
                   (sdml-mode-cli-make-arg 'level sdml-mode-validation-level)
                   (sdml-mode-cli-make-arg 'input file-name))))
    (when cmd-line
      (compile cmd-line))))

(defun sdml-mode-validate-file (file-name)
  "Validate FILE-NAME using the `compile' command.

This command executes the SDML command-line tool's validation
tool, on the file FILE-NAME, using the value of
`sdml-mode-validation-level' to determine the level of messages
output. The command uses the `compile' function and the resulting
window supports error navigation and source highlighting as
usual."
  (interactive "fSDML File name: ")
  (sdml-mode--exec-validator file-name))

(defun sdml-mode-validate-current-buffer ()
  "Validate the current buffer using the `compile' command.

This command executes the SDML command-line tool's validation
tool, on the current buffer's underlying file, using the value of
`sdml-mode-validation-level' to determine the level of messages
output. The command uses the `compile' function and the resulting
window supports error navigation and source highlighting as
usual."
  (interactive nil sdml-mode)
  (sdml-mode--exec-validator (buffer-file-name)))


;; --------------------------------------------------------------------------
;; Buffer Commands  Dependencies
;; --------------------------------------------------------------------------


(defun sdml-mode-current-buffer-dependency-graph ()
  "Show full dependency graph in SVG of the current buffer.

This command generates an SVG representing the current buffer's
dependencies as a directed graph. The command uses the SDML
command-line tool to generate a temporary file which is then
opened in a new window. The resulting image window may be dismissed
using the key `q'."
  (interactive nil sdml-mode)
  (cond
   (window-system
    (let* ((output-file-name (concat (make-temp-file "sdml-mode") ".svg"))
           (cmd-line (sdml-mode-cli-make-command
                      "deps"
                      (sdml-mode-cli-make-arg 'output output-file-name)
                      (sdml-mode-cli-make-arg 'output-format 'graph)
                      (sdml-mode-cli-make-arg 'depth 0)
                      'current-buffer)))
      (when cmd-line
        (sdml-mode-cli-run-command cmd-line)
        (find-file-other-window output-file-name))))
   (t (message "Command only available if window-system is set"))))

(defun sdml-mode-current-buffer-dependency-tree (depth)
  "Show the dependency tree of the current buffer, to a max DEPTH.

This command generates a textual tree representing the current
buffer's dependencies. The command uses the SDML command-line
tool to generate the tree, using DEPTH to denote how many levels
of dependencies to display. The resulting window may be dismissed
using the key `q', and it's content may be refreshed with the key
`g'."
  (interactive "nMax depth of tree (0=all): " sdml-mode)
  (let ((cmd-line (sdml-mode-cli-make-command
                     "deps"
                     (sdml-mode-cli-make-arg 'output-format 'tree)
                     (sdml-mode-cli-make-arg 'depth depth)
                     'current-buffer)))
      (when cmd-line
        (sdml-mode-cli-run-command cmd-line "*SDML Dependencies*" nil t))))


;; --------------------------------------------------------------------------
;; Key Bindings
;; --------------------------------------------------------------------------

(defvar sdml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s d") 'tree-sitter-debug-mode)
    (define-key map (kbd "C-c C-s q") 'tree-sitter-query-builder)
    (define-key map (kbd "C-c C-s v") 'sdml-mode-validate-current-buffer)
    (define-key map (kbd "C-c C-s M-v") 'sdml-mode-validate-file)
    (define-key map (kbd "C-c C-s t") 'sdml-mode-current-buffer-dependency-tree)
    (define-key map (kbd "C-c C-s M-t") 'sdml-mode-current-buffer-dependency-graph)
    map)
  "Keymap for SDML major mode.")


;; --------------------------------------------------------------------------
;; Emacs  Syntax
;; --------------------------------------------------------------------------

(defvar sdml-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- ". 12b" table)
    table))


;; --------------------------------------------------------------------------
;; Mode Definition
;; --------------------------------------------------------------------------

;;;###autoload
(define-derived-mode
  sdml-mode
  prog-mode
  "SDML"
  "A major mode for editing SDML (Simple Domain Modeling Language) files.

This major mode will, by default, enable the following minor modes:

- `abbrev-mode'
- `prettify-symbols-mode' (see `sdml-mode-prettify-symbols-alist')
- `tree-sitter-mode'
- `sdml-mode-hl-mode'
- `sdml-mode-indent-mode'
- `sdml-mode-ctags-mode'

  Key bindings:
  \\{sdml-mode-map}"

  :group 'sdml

  :syntax-table sdml-mode-syntax-table

  :abbrev-table sdml-mode-abbrev-table

  ;; Only the basic font-lock, the rest is taken care of by tree-sitter-hl-mode
  (unless font-lock-defaults
    (setq font-lock-defaults '(nil)))

  ;; Comment handling
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-multi-line t)

  ;; Prettify (prettify-symbols-mode)
  (setq prettify-symbols-alist sdml-mode-prettify-symbols-alist)
  (prettify-symbols-mode 1)

  (abbrev-mode 1)

  ;; connect the parser library
  (sdml-mode--tree-sitter-setup)

  ;; tree-sitter debug and query
  (setq-local tree-sitter-debug-jump-buttons t)
  (setq-local tree-sitter-debug-highlight-jump-region t)
  (tree-sitter-mode 1)

  ;; tree-sitter highlighting support
  (sdml-mode-hl-mode 1)

  ;; tree-sitter indentation support
  (sdml-mode-indent-mode 1)

  ;; add Universal Ctags support
  (sdml-mode-ctags-mode 1)

  ;; enable the validation command based on the builtin `compile'.
  (sdml-mode--command-setup))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sdml?\\'" . sdml-mode))

(provide 'sdml-mode)

;;; sdml-mode.el ends here
