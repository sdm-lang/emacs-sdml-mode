;;; sdml-mode.el --- Major mode for SDML -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Version: 0.1.8snapshot
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
;; Typing `d t SPC' will prompt for a name and expand into the SDML declaration
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
;; validate and show errors for the buffer's current module.
;;
;; Adding this as a save-hook allows  validation on every save of a buffer.
;;
;; `(add-hook 'after-save-hook 'sdml-validate-current-buffer)'
;;
;; `sdml-mode-current-buffer-dependencies' (\\[sdml-mode-current-buffer-dependencies])
;; to display the dependencies of the curtent buffer's module.
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
  "An alist of symbol prettifications used for `prettify-symbols-alist'."
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
  "Load and connect the parser library in DYLIB-FILE.

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
  "^\\(?:\\(bug\\|error\\)\\|\\(warning\\)\\|\\(help\\|note\\)\\)\\[\\([BEIW][[:digit:]]\\{4\\}\\)]: .*
\\(?:^  ┌─ \\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)\\)")

(defun sdml-mode-validate-current-buffer ()
  "Validate the current buffer using the `compile' command."
  (interactive)
  (when (eq major-mode 'sdml-mode)
    (let ((cmd-line (sdml-mode-cli-make-command
                     "validate"
                     :log-filter sdml-mode-cli-log-filter
                     :validation-level sdml-mode-validation-level
                     :input-file (buffer-file-name (current-buffer)))))
      (when (not (null cmd-line))
        (compile cmd-line)))))


;; --------------------------------------------------------------------------
;; Buffer Commands  Dependencies
;; --------------------------------------------------------------------------

(defun sdml-mode-current-buffer-dependencies ()
  "Dependencies of the current buffer."
  (interactive)
  (when (eq major-mode 'sdml-mode)
    (let ((cmd-line (sdml-mode-cli-make-command
                     "deps"
                     :log-filter sdml-mode-cli-log-filter
                     :input-file (buffer-file-name (current-buffer)))))
      (when (not (null cmd-line))
        (sdml-mode-cli-run-command cmd-line "*SDML Dependencies*")))))

(defun sdml-mode-buffer-commands-setup ()
  "Setup buffer commands."
  (setq-local ansi-color-for-compilation-mode t)
  (add-to-list 'compilation-error-regexp-alist-alist
               `(sdml ,sdml-mode-validation-error-regexp 5 6 7 (2 . 3)))
  (add-to-list 'compilation-error-regexp-alist  'sdml))


;; --------------------------------------------------------------------------
;; Key Bindings
;; --------------------------------------------------------------------------

(defvar sdml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s d") 'tree-sitter-debug-mode)
    (define-key map (kbd "C-c C-s q") 'tree-sitter-query-builder)
    (define-key map (kbd "C-c C-s v") 'sdml-validate-current-buffer)
    (define-key map (kbd "C-c C-s t") 'sdml-current-buffer-dependencies)
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
  "Major mode for editing SDML files.

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

  ;; enable the validation command based on the builtin `compile'.
  (sdml-mode-buffer-commands-setup))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sdml?\\'" . sdml-mode))

(provide 'sdml-mode)

;;; sdml-mode.el ends here
