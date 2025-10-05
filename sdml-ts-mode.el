;;; sdml-ts-mode.el --- Major mode for SDML -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (treesit-fold "0.2.1"))
;; URL: https://github.com/johnstonskj/emacs-sdml-mode
;; Keywords: languages tools

;;; License:

;; Copyright (c) 2023, 2025 Simon Johnston
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
;; Install is easiest from MELPA, here's how with `use-package`.
;;
;; `(use-package sdml-ts-mode)'
;;
;; Or, interactively; `M-x package-install RET sdml-ispell RET'

;;
;; Usage
;;
;; Once installed the major mode should be used for any file ending in `.sdm'
;; or `.sdml' with highlighting and indentation support.

;; Abbreviations and Skeletons
;;
;; This package creates a new `abbrev-table', named `sdml-ts-mode-abbrev-table', which
;; provides a number of useful skeletons for the following.  `abbrev-mode' is enabled
;; by `sdml-ts-mode' and when typing one of the abbreviations below type space to
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

;; Interactive Commands
;;

;; `sdml-ts-mode-validate-current-buffer' (\\[sdml-ts-mode-validate-current-buffer]) to
;; validate and show errors for the current buffer's module.
;;
;; Adding this as a save-hook allows validation on every save of a buffer.
;;
;; `(add-hook 'after-save-hook 'sdml-validate-current-buffer)'
;;
;; `sdml-ts-mode-validate-file' (\\[sdml-ts-mode-validate-file]) to
;; validate and show errors for a specified file name.
;;
;; `sdml-ts-mode-current-buffer-dependency-tree' (\\[sdml-ts-mode-current-buffer-dependency-tree])
;; to display the dependencies of the current buffer's module as a textual tree.
;;
;; `sdml-ts-mode-current-buffer-dependency-graph' (\\[sdml-ts-mode-current-buffer-dependency-graph])
;; to display the dependencies of the current buffer's module as an SVG directed graph.
;;

;; Configuration
;;

;; This mode provides the customization group sdml accessed in the usual manner:
;;
;; \[customize-group RET sdml RET]
;;
;; The following properties are available:
;;
;; `sdml-ts-mode-indent-offset': Number of spaces for each indentation step in SDML source.
;;
;; `sdml-ts-mode-prettify-symbols-alist': An alist of symbol replacements used for
;; `prettify-symbols-alist'.
;;
;; `sdml-ts-mode-validation-level': The level of information to provide during validation.
;;
;; `sdml-ts-mode-enable-folding': Enable support for`treesit-fold-mode' and
;; `treesit-fold-indicators-mode' in all SDML buffers.
;;

;;; Code:

(eval-when-compile
  (require 'rx))
(require 'treesit)
(require 'ansi-color)
(require 'compile)

(require 'sdml-ts-mode-abbrev)

;(require 'sdml-mode-cli)
;(require 'sdml-mode-ctags)

(declare-function json-mode "json")

(unless (assoc 'sdml treesit-language-source-alist)
  (add-to-list 'treesit-language-source-alist
               '(sdml . ("https://github.com/sdm-lang/tree-sitter-sdml"))))

(defvar sdml-ts-mode--debug-mode
  (let ((debug-mode (getenv "DEBUG_SDML_TS_MODE")))
    (and debug-mode (string-equal debug-mode "1"))))

;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(defgroup sdml nil
  "SDML language support."
  :tag "SDML"
  :prefix "sdml-ts-mode-"
  :group 'languages)

(defcustom sdml-ts-mode-prettify-symbols-alist
  '(("->" . ?→) ("<-" . ?←) ("forall" ?∀) ("exists" ?∃) ("in" ?∈) (":=" ?≔))
  "An alist of symbol replacements used for `prettify-symbols-alist'."
  :tag "Symbol mapping for prettify"
  :type '(repeat (cons string character))
  :group 'sdml)

(defcustom sdml-ts-mode-validation-level 'warnings
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
;; Commands ❱ Validation
;; --------------------------------------------------------------------------

(defconst sdml-ts-mode-validation-error-regexp
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

(defun sdml-ts-mode--command-setup ()
  "Internal: Setup buffer commands."
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (setq-local ansi-color-for-compilation-mode t)
  (add-to-list 'compilation-error-regexp-alist-alist
               `(sdml ,sdml-ts-mode-validation-error-regexp 5 6 7 (2 . 3)))
  (add-to-list 'compilation-error-regexp-alist 'sdml))

(defun sdml-ts-mode--exec-validator (file-name)
  "Internal: execute the command-line validator for FILE-NAME."
  (let ((cmd-line (sdml-ts-mode-cli-make-command
                   "validate"
                   (sdml-ts-mode-cli-make-arg 'level sdml-ts-mode-validation-level)
                   (sdml-ts-mode-cli-make-arg 'input file-name))))
    (when cmd-line
      (compile cmd-line))))

(defun sdml-ts-mode-validate-file (file-name)
  "Validate FILE-NAME using the `compile' command.

This command executes the SDML command-line tool's validation
tool, on the file FILE-NAME, using the value of
`sdml-ts-mode-validation-level' to determine the level of messages
output.  The command uses the `compile' function and the resulting
window supports error navigation and source highlighting as
usual."
  (interactive "fSDML File name: ")
  (sdml-ts-mode--exec-validator file-name))

(defun sdml-ts-mode-validate-current-buffer ()
  "Validate the current buffer using the `compile' command.

This command executes the SDML command-line tool's validation
tool, on the current buffer's underlying file, using the value of
`sdml-ts-mode-validation-level' to determine the level of messages
output.  The command uses the `compile' function and the resulting
window supports error navigation and source highlighting as
usual."
  (interactive nil sdml-ts-mode)
  (sdml-ts-mode--exec-validator (buffer-file-name)))


;; --------------------------------------------------------------------------
;; Commands ❱ Dependencies
;; --------------------------------------------------------------------------


(defun sdml-ts-mode-current-buffer-dependency-graph ()
  "Show full dependency graph in SVG of the current buffer.

This command generates an SVG representing the current buffer's
dependencies as a directed graph.  The command uses the SDML
command-line tool to generate a temporary file which is then
opened in a new window.  The resulting image window may be dismissed
using the key `q'."
  (interactive nil sdml-ts-mode)
  (cond
   (window-system
    (let* ((output-file-name (concat (make-temp-file "sdml-ts-mode") ".svg"))
           (cmd-line (sdml-ts-mode-cli-make-command
                      "deps"
                      (sdml-ts-mode-cli-make-arg 'output output-file-name)
                      (sdml-ts-mode-cli-make-arg 'output-format 'graph)
                      (sdml-ts-mode-cli-make-arg 'depth 0)
                      'current-buffer)))
      (when cmd-line
        (sdml-ts-mode-cli-run-command cmd-line)
        (find-file-other-window output-file-name))))
   (t (message "Command only available if window-system is set"))))

(defun sdml-ts-mode-current-buffer-dependency-tree (depth)
  "Show the dependency tree of the current buffer, to a max DEPTH.

This command generates a textual tree representing the current
buffer's dependencies.  The command uses the SDML command-line
tool to generate the tree, using DEPTH to denote how many levels
of dependencies to display.  The resulting window may be dismissed
using the key `q', and it's content may be refreshed with the key
`g'."
  (interactive "nMax depth of tree (0=all): " sdml-ts-mode)
  (let ((cmd-line (sdml-ts-mode-cli-make-command
                     "deps"
                     (sdml-ts-mode-cli-make-arg 'output-format 'tree)
                     (sdml-ts-mode-cli-make-arg 'depth depth)
                     'current-buffer)))
      (when cmd-line
        (sdml-ts-mode-cli-run-command cmd-line "*SDML Dependencies*" nil t))))


;; --------------------------------------------------------------------------
;; Commands ❱ Placeholders
;; --------------------------------------------------------------------------

(defun sdml-ts-mode-document-module ()
  "Generate standard documentation for module in current buffer."
  (interactive nil sdml-ts-mode)
  (let* ((inp-file (buffer-file-name))
         (out-file (if (null inp-file) "-"
                     (concat (file-name-sans-extension inp-file) ".org")))
         (out-buffer (if (null inp-file) "*SDML Documentation*" nil)))
    (let ((cmd-line (sdml-ts-mode-cli-make-command
                     "doc"
                     (sdml-ts-mode-cli-make-arg 'output-format 'org-mode)
                     (sdml-ts-mode-cli-make-arg 'output out-file)
                     'current-buffer)))
      (when cmd-line
        (sdml-ts-mode-cli-run-command cmd-line out-buffer nil t)
        (cond
         ((not (null out-buffer))
          (with-current-buffer out-buffer
            (org-mode)))
         ((not (eq out-file "-"))
          (find-file out-file)))))))

(defun sdml-ts-mode-document-project ()
  "Do something."
  (interactive nil sdml-ts-mode)
  t)

(defun sdml-ts-mode-draw-concept ()
  "Do something."
  (interactive nil sdml-ts-mode)
  t)

(defun sdml-ts-mode-draw-entities ()
  "Do something."
  (interactive nil sdml-ts-mode)
  t)

(defun sdml-ts-mode-draw-uml ()
  "Do something."
  (interactive nil sdml-ts-mode)
  t)

(defun sdml-ts-mode-generate-rdf ()
  "Do something."
  (interactive nil sdml-ts-mode)
  t)

(defun sdml-ts-mode-generate-scheme ()
  "Generate Scheme representation of the current buffer."
  (interactive nil sdml-ts-mode)
  (let* ((inp-file (buffer-file-name))
         (out-file (if (null inp-file) "-"
                     (concat (file-name-sans-extension inp-file) ".scm")))
         (out-buffer (if (null inp-file) "*SDML Parse-tree Scheme*" nil)))
    (let ((cmd-line (sdml-ts-mode-cli-make-command
                     "convert"
                     (sdml-ts-mode-cli-make-arg 'output-format 's-expr)
                     (sdml-ts-mode-cli-make-arg 'output out-file)
                     'current-buffer)))
      (when cmd-line
        (sdml-ts-mode-cli-run-command cmd-line out-buffer nil t)
        (cond
         ((and (not (null out-buffer)) (featurep 'scheme-mode))
          (with-current-buffer out-buffer
            (scheme-mode)))
         ((not (eq out-file "-"))
          (find-file out-file)))))))

(defun sdml-ts-mode-generate-json ()
  "Generate JSON representation of current buffer."
  (interactive nil sdml-ts-mode)
  (let* ((inp-file (buffer-file-name))
         (out-file (if (null inp-file) "-"
                     (concat (file-name-sans-extension inp-file) ".json")))
         (out-buffer (if (null inp-file) "*SDML Parse-tree JSON*" nil)))
    (let ((cmd-line (sdml-ts-mode-cli-make-command
                     "convert"
                     (sdml-ts-mode-cli-make-arg 'output-format 'json-pretty)
                     (sdml-ts-mode-cli-make-arg 'output out-file)
                     'current-buffer)))
      (when cmd-line
        (sdml-ts-mode-cli-run-command cmd-line out-buffer nil t)
        (cond
         ((and (not (null out-buffer)) (featurep 'json-mode))
          (with-current-buffer out-buffer
            (json-mode)))
         ((not (eq out-file "-"))
          (find-file out-file)))))))

(defun sdml-ts-mode-generate-with-tera ()
  "Do something."
  (interactive nil sdml-ts-mode)
  t)

;; --------------------------------------------------------------------------
;; Key Bindings
;; --------------------------------------------------------------------------

(defvar sdml-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s v") 'sdml-ts-mode-validate-current-buffer)
    (define-key map (kbd "C-c C-s M-v") 'sdml-ts-mode-validate-file)
    (define-key map (kbd "C-c C-s t") 'sdml-ts-mode-current-buffer-dependency-tree)
    (define-key map (kbd "C-c C-s M-t") 'sdml-ts-mode-current-buffer-dependency-graph)
    map)
  "Keymap for SDML major mode.")


;; --------------------------------------------------------------------------
;; Syntax Table
;; --------------------------------------------------------------------------

(defvar sdml-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- ". 12b" table)
    table))

;; --------------------------------------------------------------------------
;; Mode Definition
;; --------------------------------------------------------------------------

;;;###autoload
(define-derived-mode
  sdml-ts-mode
  prog-mode
  "SDML-ts"
  "A major mode for editing SDML (Simple Domain Modeling Language) files.

This major mode will, by default, enable the following minor modes:

- `abbrev-mode' (see `sdml-ts-mode-abbrev-table')
- `prettify-symbols-mode' (see `sdml-ts-mode-prettify-symbols-alist')
- `treesit-mode'
- `treesit-fold-mode'
- `treesit-fold-indicators-mode'

  Key bindings:
  \\{sdml-ts-mode-map}"

  :group 'sdml

  :syntax-table sdml-ts-mode-syntax-table

  :abbrev-table sdml-ts-mode-abbrev-table

  (setq-local comment-start-skip ";+")

  (when (treesit-ready-p 'sdml)
    (message "Setting up tree-sitter for SDML")
    (treesit-parser-create 'sdml)

    (require 'sdml-ts-mode-imenu)
    (sdml-ts-mode-imenu-setup)

    (require 'sdml-ts-mode-font-lock)
    (sdml-ts-mode-font-lock-setup)

    (require 'sdml-ts-mode-indent)
    (sdml-ts-mode-indent-setup)

    (require 'sdml-ts-mode-fold)
    (sdml-ts-mode-fold-setup)

    (treesit-major-mode-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sdml?\\'" . sdml-ts-mode))

(provide 'sdml-ts-mode)

;;; sdml-ts-mode.el ends here
