;;; sdml-mode.el --- Major mode for SDML -*- lexical-binding: t; -*-

;; Copyright (c) 2023, 2024 Simon Johnston

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Version: 0.1.6
;; Package-Requires: ((emacs "28.2") (tree-sitter "0.18.0") (tree-sitter-indent "0.3"))
;; URL: https://github.com/johnstonskj/emacs-sdml-mode
;; Keywords: languages tools

;;; License:

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
;;    :ensure t
;;    :config (sdml-mode-setup))'

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
;; provides a number of useful skeletons for the following. `abbrev-mode' is enabled
;; by `sdml-mode' and when typing one of the abbreviations below type space to
;; expand.
;;
;; Typing `d t SPC' will prompt for a name and expand into the SDML declaration
;; `datatype MyName ← opaque _' where the underscore character represents the new
;; cursor position.
;;
;; Declarations: `mo'=module, `dt'=datatype, `en'=enum, `ev'=event, `pr'=property,
;;   `st'=structure, `un'=union
;;
;; Annotation Properties: `pal'=skos:altLabel, `pdf'=skos:definition,
;;   `ped'=skos:editorialNote, `ppl'=skos:prefLabel, `pco'=rdfs:comment
;;
;; Constraints: `ci'=informal, `cf'=formal, `all'=universal, `any'=existential
;;
;; Datatypes: `db'=boolean, `dd'=decimal, `df'=double, `dh'=binary, `di'=integer,
;;   `sd'=string, `du'=unsigned
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
;; `flycheck-sdml' --
;; `ob-sdml' --
;; `sdml-fold' --
;; `sdml-ispell' --
;;


;;; Code:

(eval-when-compile
  (require 'rx)) ;; built-in

(require 'tree-sitter)
(require 'tree-sitter-hl) ;; included in above
(require 'tree-sitter-indent)

(require 'ansi-color) ;; built-in
(require 'compile) ;; built-in

(require 'sdml-cli)

;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(defgroup sdml nil
  "SDML language support."
  :tag "SDML"
  :prefix "sdml-"
  :group 'languages)

(defcustom sdml-mode-indent-offset 2
  "Number of spaces for each indentation step."
  :tag "Indentation number of spaces"
  :type 'natnum
  :group 'sdml)

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
;; Tree-Sitter  Library
;; --------------------------------------------------------------------------

(defun sdml-mode--tree-sitter-setup (&optional dylib-file)
  "Load and connect the parser library in DYLIB-FILE."
  (unless (assoc 'sdml tree-sitter-languages)
    ;; Load the dynamic library from the search path.
    (tree-sitter-load 'sdml dylib-file))

  (unless (assoc 'sdml-mode tree-sitter-major-mode-language-alist)
    ;; Connect the major mode to the loaded library.
    (add-to-list 'tree-sitter-major-mode-language-alist
                 '(sdml-mode . sdml))))


;; --------------------------------------------------------------------------
;; Tree-Sitter  Additional Faces
;; --------------------------------------------------------------------------

;; The `tree-sitter-hl' package expects to map highlight scopes to faces named
;; `tree-sitter-hl-face:{{scope}}', which plays havoc with package-lint.
(defface tree-sitter-hl-face:type.scope
  '((default :inherit tree-sitter-hl-face:type))
  "Face for type scopes, or namespaces, in definitions and type constraints."
  :group 'tree-sitter-hl-faces)

;; See:
;; - tree-sitter-hl-face-mapping-function
;; - tree-sitter-hl-add-patterns
;; - tree-sitter-hl-default-patterns


;; --------------------------------------------------------------------------
;; Tree-Sitter  Highlighting
;; --------------------------------------------------------------------------

(defconst sdml-mode-tree-sitter-hl-patterns
  [
   ;; -------------------------------------------------------------------
   ;; Comments
   ;; -------------------------------------------------------------------

   (line_comment) @comment

   ;; -------------------------------------------------------------------
   ;; Reserved Keywords
   ;; -------------------------------------------------------------------

   [
    "module"
    "import"
    "assert"
    "class"
    "datatype"
    "entity"
    "enum"
    "event"
    "property"
    "rdf"
    "structure"
    "union"
    "is"
    "of"
    "end"
    ] @keyword

   ;; -------------------------------------------------------------------
   ;; Operators
   ;; -------------------------------------------------------------------

   [
    "="
    ":="
    "≔"
    "¬"
    "∧"
    "∨"
    "⊻"
    "==>"
    "⇒"
    "<==>"
    "⇔"
    "∀"
    "∃"
    "∈"
    "->"
    "→"
    "<-"
    "←"
    ".."
    ] @operator

   ;; -------------------------------------------------------------------
   ;; Module & Imports (Note module => type; definition => scope)
   ;; -------------------------------------------------------------------

   (module name: (identifier) @type.scope)
   (module "version" @keyword)

   (import_statement [ "[" "]" ] @punctuation.bracket)

   (member_import name: (qualified_identifier) @type)

   (module_import name: (identifier) @type.scope)

   ;; -------------------------------------------------------------------
   ;; Annotations and Constraints (Note property => label)
   ;; -------------------------------------------------------------------

   (annotation_property
    "@" @label
    name: (identifier_reference) @label)

   (annotation_property value: (value (identifier_reference) @type))

   (constraint name: (identifier) @label)

   (informal_constraint (quoted_string) @embedded)
   (informal_constraint language: (controlled_language_tag) @property)

   (constraint_environment (constraint_environment_end) @keyword)

   (environment_def "def" @keyword)
   (environment_def (identifier) @function.definition \. (function_def))
   (environment_def (identifier) @constant \. (constant_def))

   (function_signature target: (_) @type)
   (function_signature [ "(" ")" ] @punctuation.bracket)

   (function_parameter name: (identifier) @variable.parameter)
   (function_parameter target: (_) @type)

   (optional) @operator

   (function_cardinality_expression (sequence_ordering) @keyword)
   (function_cardinality_expression (sequence_uniqueness) @keyword)
   (function_cardinality_expression [ "{" "}" ] @punctuation.bracket)

   (function_composition subject: (reserved_self) @variable.builtin)
   (function_composition name: (identifier) @function.call)
   (function_composition "." @punctuation.delimiter)

   (constraint_sentence [ "(" ")" ] @punctuation.bracket)

   (atomic_sentence
    predicate: (term (identifier_reference) @function.call))

   (actual_arguments [ "(" ")" ] @punctuation.bracket)
   (actual_arguments
    argument: (term (identifier_reference (identifier) @variable)))

   (term (reserved_self) @variable.builtin)

   (equation lhs: (term (identifier_reference) @variable))

   (equation rhs: (term (identifier_reference) @variable))

   (quantified_sentence "," @punctuation.separator)

   (quantified_variable source: (reserved_self) @variable.builtin)
   (quantified_variable name: (identifier) @variable.parameter)
   (quantified_variable "in" @keyword)

   (functional_term
    function: (term (identifier_reference) @function.call))

   (sequence_builder [ "{" "}" ] @punctuation.bracket
                     "|" @punctuation.separator)

   (named_variable_set (identifier) @variable)

   (mapping_variable
    domain: (identifier) @variable range: (identifier) @variable)

   (sequence_builder_body [ "(" ")" ] @punctuation.bracket)

   (sequence_of_predicate_values (identifier_reference) @type)
   (sequence_of_predicate_values [ "[" "]" ] @punctuation.bracket)

   (negation "not" @keyword)
   (conjunction "and" @keyword)
   (disjunction "or" @keyword)
   (exclusive_disjunction "xor" @keyword)
   (implication "implies" @keyword)
   (biconditional "iff" @keyword)

   (universal "forall" @keyword)
   (existential "exists" @keyword)

   ;; -------------------------------------------------------------------
   ;; Types
   ;; -------------------------------------------------------------------

   [
    (builtin_simple_type)
    (unknown_type)
    ] @type.builtin

   (data_type_def name: (identifier) @type)
   (data_type_def base: (identifier_reference) @type)
   (data_type_def opaque: (opaque) @keyword)

   (entity_def name: (identifier) @type)

   (enum_def name: (identifier) @type)

   (event_def "source" @keyword)
   (event_def
    name: (identifier) @type
    source: (identifier_reference) @type)

   (structure_def name: (identifier) @type)

   (union_def name: (identifier) @type)

   ;; -------------------------------------------------------------------
   ;; RDF Definitions
   ;; -------------------------------------------------------------------

   (rdf_def name: (identifier) @type)

   ;; -------------------------------------------------------------------
   ;; Type Classes
   ;; -------------------------------------------------------------------

   (type_class_def name: (identifier) @type)
   (type_class_def [ "(" ")" ] @punctuation.bracket)

   (type_variable name: (identifier) @type)
   (type_variable "+" @operator)

   (type_class_reference name: (identifier_reference) @type)

   (type_class_arguments [ "(" ")" ] @punctuation.bracket)

   (method_def "def" @keyword)
   (method_def name: (identifier) @method.definition)

   (wildcard) @type.builtin

   ;; -------------------------------------------------------------------
   ;; Members
   ;; -------------------------------------------------------------------

   (entity_identity "identity" @keyword)
   (entity_identity name: (identifier) @variable.field)
   (entity_identity property: (identifier_reference) @variable.field)
   (entity_identity target: (type_reference) @type)
   (entity_identity "in" @keyword)

   (member name: (identifier) @variable.field)
   (member property: (identifier_reference) @variable.field)
   (member target: (type_reference) @type)
   (member
    feature: (feature_reference
              "features" @keyword
              target: (identifier_reference) @type))
   (member "in" @keyword)

   (member_inverse_name
    "(" @punctuation.bracket
    (identifier) @variable.field
    ")" @punctuation.bracket)

   (value_variant name: (identifier) @constant)

   (type_variant (identifier_reference) @type)
   (type_variant rename: (identifier) @type)
   (type_variant "as" @keyword)

   (property_def name: (identifier) @variable.field)

   (identity_role "identity" @keyword)
   (identity_role
    name: (identifier) @variable.field
    target: (type_reference) @type)

   (member_role
    name: (identifier) @variable.field
    target: (type_reference) @type)
   (member_role feature: (feature_reference) @keyword)

   (cardinality_expression (sequence_ordering) @keyword)
   (cardinality_expression (sequence_uniqueness) @keyword)
   (cardinality_expression [ "{" "}" ] @punctuation.bracket)

   ;; -------------------------------------------------------------------
   ;; Values
   ;; -------------------------------------------------------------------

   (string (quoted_string) @string)
   (string language: (language_tag) @property)

   (iri) @string.special

   (binary) @string.special

   [
    (decimal)
    (double)
    (integer)
    (unsigned)
    ] @number

   (boolean) @constant.builtin

   (value_constructor name: (identifier_reference) @function.call)
   (value_constructor [ "(" ")" ] @punctuation.bracket)

   (value (identifier_reference) @type)

   (sequence_of_values (identifier_reference) @type)
   (sequence_of_values  [ "[" "]" ] @punctuation.bracket)

   ;; -------------------------------------------------------------------
   ;; Errors
   ;; -------------------------------------------------------------------

   ;; Highlight errors in red. This is not very useful in practice, as text will
   ;; be highlighted as user types, and the error could be elsewhere in the code.
   (ERROR) @warning
   ])


;; --------------------------------------------------------------------------
;; Tree-Sitter  Indentation
;; --------------------------------------------------------------------------

;; The `tree-sitter-indent' package expects to find a function named
;; `tree-sitter-indent-{{language}}-scopes', which plays havoc with
;; package-lint.
(defconst tree-sitter-indent-sdml-scopes
  '(;; These nodes are always indented
    (indent-all . (member
                   entity_identity
                   function_body
                   constant_def
                   informal_constraint
                   constraint_sentence))

    ;; If parent node is one of this and current node is not first → indent
    (indent-rest . ())

    ;; If parent node is one of this and current node is in middle → indent
    (indent-body . (module_body
                    import_statement
                    annotation_only_body
                    ;; entity_body << this double indents.
                    enum_body
                    property_body
                    structure_body
                    union_body
                    type_class_body
                    function_body
                    entity_identity
                    sequence_of_values
                    sequence_of_predicate_values
                    sequence_builder
                    sequence_builder_body
                    constraint
                    informal_constraint
                    formal_constraint
                    constraint_sentence
                    function_signature
                    actual_arguments))

    ;; If parent node is one of these → indent to paren opener
    (paren-indent . ())

    ;; Chaining char → node types we move parentwise to find the first chaining char
    (align-char-to . ())

    ;; Siblings (nodes with same parent) should be aligned to the first child
    (aligned-siblings . ())

    ;; if node is one of this, then don't modify the indent
    ;; this is basically a peaceful way out by saying "this looks like something
    ;; that cannot be indented using AST, so best I leave it as-is"
    (multi-line-text . (quoted_string))

    ;; These nodes always outdent (1 shift in opposite direction)
    (outdent . (constraint_environment_end))))


;; --------------------------------------------------------------------------
;; Emacs  Syntax
;; --------------------------------------------------------------------------

(defvar sdml-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- ". 12b" table)
    table))


;; --------------------------------------------------------------------------
;; Compile-like Actions
;; --------------------------------------------------------------------------

(defconst sdml-mode-validation-error-regexp
  "^\\(?:\\(bug\\|error\\)\\|\\(warning\\)\\|\\(help\\|note\\)\\)\\[\\([BEIW][[:digit:]]\\{4\\}\\)]: .*
\\(?:^  ┌─ \\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)\\)")

(defun sdml-mode-current-buffer-dependencies ()
  "Dependencies of the current buffer."
  (interactive)
  (when (eq major-mode 'sdml-mode)
    (let ((cmd-line (sdml-cli-make-command
                     "deps"
                     :log-filter sdml-cli-log-filter
                     :input-file (buffer-file-name (current-buffer)))))
      (when (not (null cmd-line))
        (sdml-cli-run-command cmd-line "*SDML Dependencies*")))))

(defun sdml-mode-validate-current-buffer ()
  "Validate the current buffer using the `compile' command."
  (interactive)
  (when (eq major-mode 'sdml-mode)
    (let ((cmd-line (sdml-cli-make-command
                     "validate"
                     :log-filter sdml-cli-log-filter
                     :validation-level sdml-mode-validation-level
                     :input-file (buffer-file-name (current-buffer)))))
      (when (not (null cmd-line))
        (compile cmd-line)))))


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
;; Abbrev Table
;; --------------------------------------------------------------------------

(define-skeleton sdml-mode--new-module
  "New module."
  "Module name: "
  > "module " str | "new_module"
  > " base <https://example.org/vocabulary/" str | "new_module" "> is" \n
  > "" \n
  > "  import [ dc skos rdfs xsd ]" \n
  > "" \n
  > "  @skos:prefLabel = \"" str | "new_module" "\"@" locale-language \n
  > "  @dc:version = xsd:integer(1)" \n
  > "" \n
  > "  " _ \n
  > "" \n
  > "end" \n)

(define-skeleton sdml-mode--new-datatype
  "New datatype."
  "Datatype name: "
  > "datatype " str | "NewDatatype" " ← opaque " _ \n)

(define-skeleton sdml-mode--new-entity
  "New entity."
  "Entity name: "
  > "entity " str | "NewEntity" " is" \n
  > "  identity id → unknown" \n
  > "  " _ \n
  > "end" \n)

(define-skeleton sdml-mode--new-structure
  "New structure."
  "Structure name: "
  > "structure " str | "NewStructure" " is" \n
  > "  name → Type" \n
  > "  " _ \n
  > "end" \n)

(define-skeleton sdml-mode--new-property
  "New property."
  "Property name: "
  > "property " str | "NewProperty" " is" \n
  > "  role_name → Type" \n
  > "  " _ \n
  > "end" \n)

(define-skeleton sdml-mode--new-event
  "New event."
  "Event name: "
  > "event " str | "NewEvent" " source " _ " is" \n
  > "  name → Type" \n
  > "" \n
  > "end" \n)

(define-skeleton sdml-mode--new-enum
  "New enum."
  "Enum name: "
  > "enum " str | "NewEnum" " of" \n
  > "  ValueOne" \n
  > "  " _ \n
  > "end" \n)

(define-skeleton sdml-mode--new-union
  "New discriminated union."
  "Union name: "
  > "union " str | "NewUnion" " of" \n
  > "  TypeOne as One" \n
  > "  " _ \n
  > "end" \n)

(define-skeleton sdml-mode--new-constraint
  "New informal constraint."
  "Constraint name: "
  > "assert " str | "invariant" " = \"" _ "\"@" locale-language \n)

(define-skeleton sdml-mode--new-formal-constraint
  "New formal constraint."
  "Constraint name: "
  > "assert " str | "invariant" " is" \n
  > "  ∀ self, " _ \n
  > "end" \n)

;; --------------------------------------------------------------------------

(define-skeleton sdml-mode--new-constraint-def
  "New formal constraint definition."
  "Definition name: "
  > "def " str | "defn" "() ≔ " _ \n)

(define-skeleton sdml-mode--new-constraint-forall
  "Universal quantified sentence."
  "Variable name: "
  > "∀ " str | "self" ", " _ \n)

(define-skeleton sdml-mode--new-constraint-exists
  "Existential quantified sentence."
  "Variable name: "
  > "∃ " str | "self" ", " _ \n)

;; --------------------------------------------------------------------------

(define-skeleton sdml-mode--new-ann-altlabel
  "SKOS alternate label." nil
  > "@skos:altLabel = \"" _ "\"@" locale-language \n)

(define-skeleton sdml-mode--new-ann-definition
  "SKOS definition." nil
  > "@skos:definition = \"" _ "\"@" locale-language \n)

(define-skeleton sdml-mode--new-ann-editorial
  "SKOS editorial note." nil
  > "@skos:editorialNote = \"" _ "\"@" locale-language \n)

(define-skeleton sdml-mode--new-ann-preflabel
  "SKOS preferred label." nil
  > "@skos:prefLabel = \"" _ "\"@" locale-language \n)

(define-skeleton sdml-mode--new-ann-comment
  "RDFS comment." nil
  > "@rdfs:comment = \"" _ "\"@" locale-language \n)

;; --------------------------------------------------------------------------

;; Note you still need a replacement string "" when using
;;      skeletons or the original text isn't removed.
(define-abbrev-table 'sdml-mode-abbrev-table
  '(;; Declaration - Types
    ("mo" "" sdml-mode--new-module)
    ("dt" "" sdml-mode--new-datatype)
    ("en" "" sdml-mode--new-enum)
    ("ev" "" sdml-mode--new-event)
    ("pr" "" sdml-mode--new-property)
    ("st" "" sdml-mode--new-structure)
    ("un" "" sdml-mode--new-union)
    ;; Declaration - Members
    ;; Annotation - Constraints
    ("ci" "" sdml-mode--new-constraint)
    ("cf" "" sdml-mode--new-formal-constraint)
    ("all" "" sdml-mode--new-constraint-forall)
    ("any" "" sdml-mode--new-constraint-exists)
    ;; Annotation - Properties
    ("pal" "" sdml-mode--new-ann-altlabel)
    ("ppl" "" sdml-mode--new-ann-preflabel)
    ("pdf" "" sdml-mode--new-ann-definition)
    ("ped" "" sdml-mode--new-ann-editorial)
    ("pco" "" sdml-mode--new-ann-comment)
    ;; data types
    ("uk" "-> unknown")
    ("db" "boolean")
    ("dd" "decimal")
    ("df" "double")
    ("dh" "binary")
    ("di" "integer")
    ("ds" "string")
    ("du" "unsigned")))

;; --------------------------------------------------------------------------
;; Mode Definition
;; --------------------------------------------------------------------------

(defun sdml-mode-setup (&optional dylib-file)
  "Call internal setup functions for tree-sitter and validation.

* Tree-sitter

Load the dynamic library, either with the explicit path
in DYLIB-FILE, or by searching the path in `tree-sitter-load-path'.
The parser library will be named  `sdml' with a
platform-specific extension in `tree-sitter-load-suffixes'."
  (sdml-mode--tree-sitter-setup dylib-file))

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

  ;; Only the basic font-lock, taken care of by tree-sitter-hl-mode
  (unless font-lock-defaults
    (setq font-lock-defaults '(nil)))

  ;; Comment handling
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-multi-line t)

  ;; Prettify (prettify-symbols-mode)
  (setq prettify-symbols-alist sdml-mode-prettify-symbols-alist)
  (prettify-symbols-mode)

  (abbrev-mode)

  ;; tree-sitter debug and query
  (setq-local tree-sitter-debug-jump-buttons t)
  (setq-local tree-sitter-debug-highlight-jump-region t)
  (tree-sitter-mode)

  ;; tree-sitter highlighting capabilities
  (setq-local tree-sitter-hl-default-patterns sdml-mode-tree-sitter-hl-patterns)
  (tree-sitter-hl-mode)

  ;; enable the validation command based on the builtin `compile'.
  (setq-local ansi-color-for-compilation-mode t)
  (add-hook 'compilation-filter-hook 'sdml--colorize-compilation-buffer nil t)
  (add-to-list 'compilation-error-regexp-alist-alist
               `(sdml ,sdml-mode-validation-error-regexp 5 6 7 (2 . 3)))
  (add-to-list 'compilation-error-regexp-alist  'sdml))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sdml?\\'" . sdml-mode))

;; --------------------------------------------------------------------------
;; Tree-sitter integration
;; --------------------------------------------------------------------------

(provide 'sdml-mode)

;;; sdml-mode.el ends here

