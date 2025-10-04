;;; sdml-ts-mode.el --- Major mode for SDML -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Version: 0.2.1
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/johnstonskj/emacs-sdml-ts-mode
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

;;; Code:

(eval-when-compile
  (require 'rx)) ;; built-in

(require 'treesit)

(require 'ansi-color) ;; built-in
(require 'compile) ;; built-in

(require 'sdml-mode-abbrev)
(require 'sdml-mode-cli)
(require 'sdml-mode-ctags)

(declare-function json-mode "json")

(unless (assoc 'sdml treesit-language-source-alist)
  (add-to-list 'treesit-language-source-alist
               '(sdml . ("https://github.com/sdm-lang/tree-sitter-sdml"))))

;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(defgroup sdml-ts nil
  "SDML language support."
  :tag "SDML"
  :prefix "sdml-ts-"
  :group 'languages)

(defcustom sdml-ts-mode-indent-offset
  2
  "The number of characters to indent source by, recommended is 2."
  :tag "Indentation offset"
  :type 'integer
  :safe 'natnump
  :group 'sdml)

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
;; Commands  Validation
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
;; Commands  Dependencies
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
;; Commands  Placeholders
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
;; Emacs ❱ Syntax
;; --------------------------------------------------------------------------

(defvar sdml-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- ". 12b" table)
    table))


;; --------------------------------------------------------------------------
;; Tree-Sitter ❱ Font Lock Faces
;; --------------------------------------------------------------------------

(defface sdml-ts-comment-face
  '((t (:inherit font-lock-comment-face :weight light)))
  "Face used for SDML line comments."
  :group 'sdml-ts)

;; --------------------------------------------------------------------------

(defface sdml-ts-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face used for SDML reserved keywords."
  :group 'sdml-ts)

(defface sdml-ts-keyword-facet-face
  '((t (:inherit sdml-ts-keyword-face :slant italic)))
  "Face used for SDML datatype facet keywords."
  :group 'sdml-ts)

;; --------------------------------------------------------------------------

(defface sdml-ts-constant-builtin-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for SDML built-in constant values."
  :group 'sdml-ts)

(defface sdml-ts-boolean-face
  '((t (:inherit sdml-ts-constant-builtin-face)))
  "Face used for SDML boolean literals."
  :group 'sdml-ts)

(defface sdml-ts-number-face
  '((t (:inherit font-lock-number-face)))
  "Face used for SDML numeric literals."
  :group 'sdml-ts)

(defface sdml-ts-string-face
  '((t (:inherit font-lock-string-face)))
  "Face used for SDML string, and string-like, literals."
  :group 'sdml-ts)

(defface sdml-ts-binary-face
  '((t (:inherit sdml-ts-string-face)))
  "Face used for SDML binary literals."
  :group 'sdml-ts)

(defface sdml-ts-iri-face
  '((t (:inherit sdml-ts-string-face :slant italic)))
  "Face used for SDML IRI literals."
  :group 'sdml-ts)

;; --------------------------------------------------------------------------

(defface sdml-ts-operator-face
  '((t (:inherit font-lock-operator-face :weight bold)))
  "Face used for operators in the SDML grammar."
  :group 'sdml-ts)

(defface sdml-ts-operator-assignment-face
  '((t (:inherit sdml-ts-operator-face)))
  "Face used for SDML assignment operators."
  :group 'sdml-ts)

(defface sdml-ts-operator-arithmetic-face
  '((t (:inherit sdml-ts-operator-face)))
  "Face used for SDML arithmetic operators."
  :group 'sdml-ts)

(defface sdml-ts-operator-logical-face
  '((t (:inherit sdml-ts-operator-face)))
  "Face used for SDML logical operators."
  :group 'sdml-ts)

(defface sdml-ts-operator-logical-negation-face
  '((t (:inherit sdml-ts-operator-logical-face)))
  "Face used for the SDML logical negation operator."
  :group 'sdml-ts)

(defface sdml-ts-operator-relational-face
  '((t (:inherit sdml-ts-operator-face)))
  "Face used for SDML inequality relation operators."
  :group 'sdml-ts)

(defface sdml-ts-operator-set-face
  '((t (:inherit sdml-ts-operator-face)))
  "Face used for SDML set operators."
  :group 'sdml-ts)

(defface sdml-ts-operator-type-face
  '((t (:inherit sdml-ts-operator-face)))
  "Face used for SDML type-level operators."
  :group 'sdml-ts)

(defface sdml-ts-operator-function-face
  '((t (:inherit sdml-ts-operator-face)))
  "Face used for SDML function-level operators."
  :group 'sdml-ts)

;; --------------------------------------------------------------------------

(defface sdml-ts-type-face
  '((t (:inherit font-lock-type-face)))
  "Face used for type reference identifiers in SDML."
  :group 'sdml-ts)

(defface sdml-ts-type-def-face
  '((t (:inherit sdml-ts-type-face :weight bold)))
  "Face used for type definition identifiers in SDML."
  :group 'sdml-ts)

(defface sdml-ts-type-builtin-face
  '((t (:inherit sdml-ts-type-face)))
  "Face used for built-in type reference in SDML."
  :group 'sdml-ts)

;; --------------------------------------------------------------------------

(defface sdml-ts-module-face
  '((t (:inherit font-lock-type-face)))
  "Face used for module reference identifiers in SDML."
  :group 'sdml-ts)

(defface sdml-ts-module-def-face
  '((t (:inherit sdml-ts-module-face :weight bold)))
  "Face used for module definition identifiers in SDML."
  :group 'sdml-ts)

;; --------------------------------------------------------------------------

(defface sdml-ts-member-face
  '((t (:inherit font-lock-property-use-face)))
  "Face used for member (abstract) definitions in SDML."
  :group 'sdml-ts)

(defface sdml-ts-member-def-face
  '((t (:inherit font-lock-property-name-face :weight bold)))
  "Face used for member (abstract) definitions in SDML."
  :group 'sdml-ts)

(defface sdml-ts-field-def-face
  '((t (:inherit sdml-ts-member-def-face :weight bold)))
  "Face used for field definitions in SDML."
  :group 'sdml-ts)

(defface sdml-ts-field-face
  '((t (:inherit sdml-ts-member-face)))
  "Face used for field references in SDML."
  :group 'sdml-ts)

(defface sdml-ts-value-variant-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for value variants, defined within enum definitions, in SDML."
  :group 'sdml-ts)

(defface sdml-ts-type-variant-face
  '((t (:inherit font-lock-function-name-face)))
  "Face used for type variants, defined within union definitions, in SDML."
  :group 'sdml-ts)

;; --------------------------------------------------------------------------

(defface sdml-ts-annotation-property-face
  '((t (:inherit font-lock-builtin-face :foreground "red3")))
  "Face used for annotation property assertions in SDML."
  :group 'sdml-ts)

(defface sdml-ts-annotation-property-def-face
  '((t (:inherit sdml-ts-annotation-property-face :weight bold)))
  "Face used for annotation property definitions in SDML."
  :group 'sdml-ts)

(defface sdml-ts-annotation-constraint-face
  '((t (:inherit sdml-ts-annotation-property-face  :foreground "red3")))
  "Face used for annotation constraint names in SDML."
  :group 'sdml-ts)

(defface sdml-ts-informal-constraint-face
  '((t (:inherit font-lock-doc-face)))
  "Face used for informal constraint strings in SDML."
  :group 'sdml-ts)

(defface sdml-ts-function-def-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face used for function definitions in SDML."
  :group 'sdml-ts)

(defface sdml-ts-function-call-face
  '((t (:inherit font-lock-function-call-face)))
  "Face used for function calls in SDML."
  :group 'sdml-ts)

(defface sdml-ts-variable-face
  '((t (:inherit font-variable-use-face)))
  "Face used for variable names in SDML."
  :group 'sdml-ts)

(defface sdml-ts-variable-def-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for variable definitions in SDML."
  :group 'sdml-ts)

;; TODO: formal constraints ...

;; --------------------------------------------------------------------------

(defface sdml-ts-bracket-face
  '((t (:inherit font-lock-bracket-face)))
  "Face used for paired bracket forms in the SDML grammar."
  :group 'sdml-ts)

(defface sdml-ts-bracket-restriction-face
  '((t (:inherit sdml-ts-bracket-face)))
  "Face used for type and cardinality restriction braces, \"{\" and \"}\", in SDML."
  :group 'sdml-ts)

(defface sdml-ts-bracket-sequence-face
  '((t (:inherit sdml-ts-bracket-face)))
  "Face used for sequence brackets, \"[\" and \"]\", in SDML."
  :group 'sdml-ts)

(defface sdml-ts-bracket-parameters-face
  '((t (:inherit sdml-ts-bracket-face)))
  "Face used for parameter list parenthesis, \"(\" and \")\", in SDML."
  :group 'sdml-ts)

;; --------------------------------------------------------------------------

(defface sdml-ts-separator-face
  '((t (:inherit font-lock-delimiter-face)))
  "Face used for separator characters and strings in the SDML grammar."
  :group 'sdml-ts)

(defface sdml-ts-separator-module-path-face
  '((t (:inherit sdml-ts-separator-face)))
  "Face used for import path separators, \"::\", in SDML."
  :group 'sdml-ts)

(defface sdml-ts-separator-qualified-sentence-face
  '((t (:inherit sdml-ts-separator-face)))
  "Face used for qualified sentence separators, \",\", in SDML."
  :group 'sdml-ts)

(defface sdml-ts-separator-sequence-builder-face
  '((t (:inherit sdml-ts-separator-face)))
  "Face used for sequence builder separators, \"|\", in SDML."
  :group 'sdml-ts)

;; --------------------------------------------------------------------------
;; Tree-Sitter ❱ Font Lock Rules
;; --------------------------------------------------------------------------

(makunbound 'sdml-ts-mode--font-lock-settings)
(defvar sdml-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :feature 'comments
   :language 'sdml
   '((line_comment) @sdml-ts-comment-face)

   :feature 'global-keywords
   :language 'sdml
   '([ "as" "assert" "class" "datatype" "dimension" "end" "entity" "enum" "event"
       "from" "import" "is" "module" "of" "property" "rdf" "structure" "union"
       (unknown_type) ]
     @sdml-ts-keyword-face)

   :feature 'binaries
   :language 'sdml
   '((binary) @sdml-ts-binary-face)

   :feature 'booleans
   :language 'sdml
   '((boolean) @sdml-ts-boolean-face)

   :feature 'iris
   :language 'sdml
   '((iri) @sdml-ts-iri-face)

   :feature 'numbers
   :language 'sdml
   '([(rational)
      (decimal)
      (double)
      (integer)
      (unsigned)]
     @sdml-ts-number-face)

   :feature 'strings
   :language 'sdml
   '((quoted_string) @sdml-ts-string-face
     (language_tag) @sdml-ts-string-face)

   :feature 'annotations
   :language 'sdml
   '((annotation_property "@" @sdml-ts-annotation-property-face)
     (annotation_property name: (identifier_reference) @sdml-ts-annotation-property-face))

   :feature 'constraints
   :language 'sdml
   '((constraint "assert" @sdml-ts-keyword-face)
     (constraint name: (identifier) @sdml-ts-annotation-constraint-face)
     (informal_constraint value: (quoted_string) @sdml-ts-informal-constraint-face)
     (informal_constraint language: (controlled_language_tag) @sdml-ts-informal-constraint-face))

   :feature 'formals
   :language 'sdml
   '((constraint_environment "with" @sdml-ts-keyword-face)
     (function_signature name: (identifier) @sdml-ts-function-def-face)
     (function_signature "def" @sdml-ts-keyword-face)
     (function_signature target: (_) @sdml-ts-type-reference-face)
     (cardinality_reference_expression (sequence_ordering) @sdml-ts-keyword-face)
     (cardinality_reference_expression (sequence_uniqueness) @sdml-ts-keyword-face)
     (function_composition subject: (reserved_self) @sdml-ts-constant-builtin-face)
     (function_composition name: (identifier) @sdml-ts-function-call-face)
     (atomic_sentence predicate: (term (identifier_reference) @sdml-ts-function-call-face))
     (quantified_variable
      (variable name: (identifier) @sdml-ts-variable-face)
      ((set_op_is_member) @sdml-ts-operator-logical-face))
     (sequence_builder variable: (variable name: (identifier) @sdml-ts-variable-face))
     )

   :feature 'modules
   :language 'sdml
   '((module name: (identifier) @sdml-ts-module-def-face)
     (module_version "version" @sdml-ts-keyword-face)
     (module_path_absolute segment: (identifier) @sdml-ts-module-face)
     (module_path_relative segment: (identifier) @sdml-ts-module-face)
     (member_import name: (qualified_identifier) @sdml-ts-type-face)
     (module_import name: (identifier) @sdml-ts-module-face))

   :feature 'facets
   :language 'sdml
   '((length_restriction_facet
      [ "length" "maxLength" "minLength" ]
      @sdml-ts-keyword-facet-face)
     (digit_restriction_facet
      [ "fractionDigits" "totalDigits" ]
      @sdml-ts-keyword-facet-face)
     (value_restriction_facet
      [ "maxExclusive" "maxInclusive" "minExclusive" "minInclusive" ]
      @sdml-ts-keyword-facet-face)
     (tz_restriction_facet "explicitTimezone"  @sdml-ts-keyword-facet-face)
     (tz_restriction_facet (tz_restriction_value) @sdml-ts-constant-builtin-face)
     (pattern_restriction_facet "pattern" @sdml-ts-keyword-facet-face)
     ((kw_is_fixed) @sdml-ts-keyword-face))

   :feature 'definitions
   :language 'sdml
   '((from_definition_clause from: (identifier_reference) @sdml-ts-type-face)
     (from_definition_with
      "with" @sdml-ts-keyword-face
      member: (identifier) @sdml-ts-member-face)
     (datatype_def name: (identifier) @sdml-ts-type-def-face)
     (datatype_def base: (identifier_reference) @sdml-ts-type-face)
     (datatype_def base: (builtin_types) @sdml-ts-builtin-type-face)
     (datatype_def opaque: (opaque) @sdml-ts-keyword-facet-face)
     (dimension_def name: (identifier) @sdml-ts-type-def-face)
     (entity_def name: (identifier) @sdml-ts-type-def-face)
     (enum_def name: (identifier) @sdml-ts-type-def-face)
     (event_def name: (identifier) @sdml-ts-type-def-face)
     (rdf_def name: (identifier) @sdml-ts-type-def-face)
     (rdf_def [ "a" "type" ] @sdml-ts-keyword-facet-face)
     (rdf_def type: (identifier_reference) @sdml-ts-type-def-face)
     (structure_def name: (identifier) @sdml-ts-type-def-face)
     (type_class_def name: (identifier) @sdml-ts-type-def-face)
     (type_parameter name: (identifier) @sdml-ts-type-def-face)
     (type_parameter_restriction class: (identifier_reference) @sdml-ts-type-face)
     (type_restriction_argument (identifier) @sdml-ts-type-face)
     (union_def name: (identifier) @sdml-ts-type-def-face)
     (cardinality_reference_expression [(sequence_ordering) (sequence_uniqueness)] @sdml-ts-keyword-face))

   :feature 'fields
   :language 'sdml
   '((member_def name: (identifier) @sdml-ts-field-definition-face)
     (member_def target: (type_reference) @sdml-ts-type-reference-face)
     (annotation_member_def
      "@" @sdml-ts-annotation-property-def-face
      (member_def name: (identifier) @sdml-ts-annotation-property-def-face))
     (property_ref "ref" @sdml-ts-keyword-face)
     (property_ref property: (identifier_reference) @sdml-ts-field-face)
     (entity_identity "identity" @sdml-ts-keyword-face)
     (dimension_parent "parent" @sdml-ts-keyword-face)
     (dimension_parent name: (identifier) @sdml-ts-field-def-face)
     (dimension_parent parent: (identifier_reference) @sdml-ts-type-face)
     (source_entity "source" @sdml-ts-keyword-face)
     (source_entity entity: (identifier_reference) @sdml-ts-type-face)
     (source_entity "with" @sdml-ts-keyword-face)
     (source_entity member: (identifier) @sdml-ts-field-def-face)
     (cardinality_expression [(sequence_ordering) (sequence_uniqueness)] @sdml-ts-keyword-face))

   :feature 'variants
   :language 'sdml
   '((value_variant name: (identifier) @sdml-ts-value-variant-face)
     (type_variant (identifier_reference) @sdml-ts-type-face)
     (type_variant name: (_) @sdml-ts-type-variant-face)
     (type_variant rename: (_) @sdml-ts-type-face))

   :feature 'operators
   :language 'sdml
   '((length_restriction_facet "=" @sdml-ts-operator-assignment-face)
     (digit_restriction_facet "=" @sdml-ts-operator-assignment-face)
     (value_restriction_facet "=" @sdml-ts-operator-assignment-face)
     (tz_restriction_facet "=" @sdml-ts-operator-assignment-face)
     (pattern_restriction_facet "=" @sdml-ts-operator-assignment-face)
     (annotation_property "=" @sdml-ts-operator-assignment-face)
     (informal_constraint "=" @sdml-ts-operator-assignment-face)
     (function_body [ ":=" "≔" ] @sdml-ts-operator-assignment-face)
     (member_def [ "->" "→" ] @sdml-ts-operator-type-face)
     (datatype_def [ "<-" "←" ] @sdml-ts-operator-type-face)
     (type_parameter [ "<-" "←" ] @sdml-ts-operator-type-face)
     (function_composition [ "." "∘" ] @sdml-ts-operator-function-face)
     ((type_op_combiner) @sdml-ts-operator-type-face)
     ([ (op_equality)
        (inequality_relation) ]
      @sdml-ts-operator-relational-face)
     ([ (logical_connective)
        (logical_quantifier) ]
      @sdml-ts-operator-logical-face)
     ((logical_op_negation) @sdml-ts-operator-logical-negation-face)
     ((set_operator) @sdml-ts-operator-set-face)
     ((math_operator) @sdml-ts-operator-arithmetic-face)
     (cardinality_range ".." @sdml-ts-operator-face))

   :feature 'sequences
   :language 'sdml
   '((import_statement [ "[" "]" ] @sdml-ts-bracket-sequence-face)
     (sequence_of_predicate_values  [ "[" "]" ] @sdml-ts-bracket-sequence-face)
     (pattern_restriction_facet [ "[" "]" ] @sdml-ts-bracket-sequence-face)
     (from_definition_with [ "[" "]" ] @sdml-ts-bracket-sequence-face)
     (from_definition_without [ "[" "]" ] @sdml-ts-bracket-sequence-face)
     (source_entity [ "[" "]" ] @sdml-ts-bracket-sequence-face)
     (rdf_def [ "[" "]" ] @sdml-ts-bracket-sequence-face)
     (sequence_of_values  [ "[" "]" ] @sdml-ts-bracket-sequence-face))

   :feature 'parameters
   :language 'sdml
   '((function_signature [ "(" ")" ] @sdml-ts-bracket-parameters-face)
     (constraint_sentence [ "(" ")" ] @sdml-ts-bracket-parameters-face)
     (atomic_sentence [ "(" ")" ] @sdml-ts-bracket-parameters-face)
     (functional_term [ "(" ")" ] @sdml-ts-bracket-parameters-face)
     (type_class_def [ "(" ")" ] @sdml-ts-bracket-parameters-face)
     (type_parameter_restriction [ "(" ")" ] @sdml-ts-bracket-parameters-face)
     (value_constructor [ "(" ")" ] @sdml-ts-bracket-parameters-face))

   :feature 'restrictions
   :language 'sdml
   '((cardinality_reference_expression [ "{" "}" ] @sdml-ts-bracket-restriction-face)
     (datatype_def_restriction [ "{" "}" ] @sdml-ts-bracket-restriction-face)
     (sequence_of_predicate_values [ "{" "}" ] @sdml-ts-bracket-restriction-face)
     (datatype_def_restriction [ "{" "}" ] @sdml-ts-bracket-restriction-face)
     (cardinality_expression [ "{" "}" ] @sdml-ts-bracket-restriction-face)
     (sequence_of_values [ "{" "}" ]) @sdml-ts-bracket-restriction-face)

   :feature 'delimiters
   :language 'sdml
   '((module_path_relative "::" @sdml-ts-separator-module-path-face)
     (module_path_absolute "::" @sdml-ts-separator-module-path-face)
     (quantified_sentence "," @sdml-ts-separator-qualified-sentence-face)
     ((seq_builder_separator) @sdml-ts-separator-sequence-builder-face))))

;; --------------------------------------------------------------------------
;; Tree-Sitter ❱ Font Lock Features
;; --------------------------------------------------------------------------

(makunbound 'sdml-ts-mode--font-lock-feature-list)
(defvar sdml-ts-mode--font-lock-feature-list
  '((comments global-keywords)
    (modules definitions fields variants strings booleans numbers binaries iris)
    (facets annotations constraints formals)
    (operators sequences parameters restrictions delimiters)))

;; --------------------------------------------------------------------------
;; Tree-Sitter ❱ Indentation Rules
;; --------------------------------------------------------------------------

(makunbound 'sdml-ts-mode--indent-rules)
(defvar sdml-ts-mode--indent-rules
  (let ((offset sdml-ts-mode-indent-offset)
        (no-offset 0))
    `(;; These are simply line-up rules, for the most part they're simple
      ;; except for weird Emacs regex escaping.
      ((node-is "\\[") parent-bol ,no-offset)
      ((node-is "]") parent-bol ,no-offset)
      ((node-is "\\\\(") parent-bol ,no-offset)
      ((node-is ")") parent-bol ,no-offset)
      ((node-is "{") parent-bol ,no-offset)
      ((and (node-is "}") (parent-is "datatype_def_restriction")) grand-parent ,no-offset)
      ((node-is "}") parent-bol ,no-offset)

      ;; --------------------------------------------------------------------
      ;; These are relatively obvious and universal, except for the
      ;; top-level rule for the "is" opening a module.
      ((and (node-is "is") (parent-is "module_body")) parent-bol ,offset)
      ;;((and (node-is "is") (not (parent-is "module_body"))) grand-parent ,offset)
      ((node-is "end") grand-parent ,no-offset)
      ((node-is "line_comment") parent-bol ,offset)

      ;; --------------------------------------------------------------------
      ;; Rules for all annotation types; formal constraint sentences are
      ;; described separately below.
      ((node-is "annotation") parent-bol ,offset)
      ((parent-is "annotation_property") parent-bol ,offset)
      ((node-is ,(rx (or "formal_constraint" "informal_constraint"))) parent-bol ,offset)
      ((parent-is "informal_constraint") parent-bol ,no-offset)

      ;; --------------------------------------------------------------------
      ;; Modules and bodies.
      ((node-is "module") column-0)
      ((node-is "module_body") parent-bol ,no-offset)
      ((match ,(rx (or "identifier" "iri" "quoted_string")) "module") parent-bol ,offset)
      ((match ,(rx (or "import_statement" "definition")) "module_body") parent-bol ,offset)
      ((match ,(rx (or "member_import" "module_import")) "import_statement") parent-bol ,offset)

      ;; --------------------------------------------------------------------
      ;; Definitions and their top-level properties.
      ((parent-is "definition") parent-bol ,offset)
      ((match "identifier"
              ,(rx (or "data_type_def"
                       "dimension_def"
                       "entity_def"
                       "enum_def"
                       "event_def"
                       "property_def"
                       "rdf_def"
                       "structure_def"
                       "type_class_def"
                       "union_def"))
              "name")
       parent-bol ,offset)
      ((match ,(rx (or "<-"
                       "←"
                       "opaque"
                       "identifier_reference"
                       "builtin_simple_type"))
              "data_type_def")
       parent-bol ,offset)
      ((parent-is "datatype_def_restriction") parent-bol ,offset)

      ;; --------------------------------------------------------------------
      ;; Definition bodies
      ((node-is
        ,(rx (or "annotation_only_body"
                 "dimension_body"
                 "enum_body"
                 "event_body"
                 "structure_body"
                 "type_class_body"
                 "union_body")))
       parent-bol ,no-offset)
      ((match ,(rx (or "source_entity"
                       "entity_identity"
                       "dimension_parent"))
              "dimension_body")
       parent-bol ,offset)
      ((match "identity" "entity_body") parent-bol ,offset)
      ((match "source_entity" "event_body") parent-bol ,offset)

      ((match ,(rx (or "identifier_reference"
                       "identifier"))
              "source_entity")

      ;; --------------------------------------------------------------------
      ;; Definition Members.
       ((match "member" ,(rx (or "dimension_body"
                                 "entity_body"
                                 "event_body"
                                 "structure_body")))
        parent-bol ,offset)
      ((match  "type_variant" "union_body") parent-bol ,offset)
      ((match  "value_variant" "enum_body") parent-bol ,offset)
      ((match ,(rx (or "as" "identifier")) "type_variant") parent-bol ,offset)

      ;; --------------------------------------------------------------------
      ;; Formal Constraint Sentences

      ;; --------------------------------------------------------------------
      ;; Values
      ((match "simple_value" "value_constructor") parent-bol ,offset)
      ))))

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

- `abbrev-mode'
- `prettify-symbols-mode' (see `sdml-ts-mode-prettify-symbols-alist')
- `treesit-mode'
- `sdml-ts-mode-ctags-mode'

  Key bindings:
  \\{sdml-ts-mode-map}"

  :group 'sdml

  :syntax-table sdml-ts-mode-syntax-table

  :abbrev-table sdml-mode-abbrev-table

  (setq-local comment-start-skip ";+")

  (when (treesit-ready-p 'sdml)
    (message "Setting up tree-sitter for SDML")
    (treesit-parser-create 'sdml)
    (setq-local treesit-font-lock-settings sdml-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list sdml-ts-mode--font-lock-feature-list)
    (setq-local treesit-simple-indent-rules `((sdml . ,sdml-ts-mode--indent-rules)))

    ;; imenu

    ;; type-regexp

    ;; name-function

    (treesit-major-mode-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sdml?\\'" . sdml-ts-mode))

(provide 'sdml-ts-mode)

;;; sdml-ts-mode.el ends here
