;;; sdml-mode-hl.el --- Highlighting Support -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>

;;; License:

;; Copyright (c) 2024 Simon Johnston

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Internal module.

;;; Code:

(require 'tree-sitter)
(require 'tree-sitter-hl) ;; included in above

;; --------------------------------------------------------------------------
;; Highlighting  Patterns
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

   (module name: (identifier) @module.definition)
   (module "version" @keyword)

   (import_statement [ "[" "]" ] @punctuation.bracket)

   (member_import name: (qualified_identifier) @type)

   (module_import name: (identifier) @module)

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

   (data_type_def name: (identifier) @type.definition)
   (data_type_def base: (identifier_reference) @type)
   (data_type_def opaque: (opaque) @keyword)

   (entity_def name: (identifier) @type.definition)

   (enum_def name: (identifier) @type.definition)

   (event_def "source" @keyword)
   (event_def
    name: (identifier) @type.definition
    source: (identifier_reference) @type)

   (property_def name: (identifier) @variable.field)

   (structure_def name: (identifier) @type.definition)

   (union_def name: (identifier) @type.definition)

   ;; -------------------------------------------------------------------
   ;; RDF Definitions
   ;; -------------------------------------------------------------------

   (rdf_def name: (identifier) @type.definition)

   ;; -------------------------------------------------------------------
   ;; Type Classes
   ;; -------------------------------------------------------------------

   (type_class_def name: (identifier) @type.definition)
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
;; Highlighting  Additional Faces
;; --------------------------------------------------------------------------

(defface sdml-mode-hl-face-module
  '((default :inherit tree-sitter-hl-face:type :foreground "seagreen"))
  "Face for module references."
  :group 'tree-sitter-hl-faces)

(defface sdml-mode-hl-face-module-definition
  '((default :inherit sdml-mode-hl-face-module :weight semi-bold))
  "Face for module definitions."
  :group 'tree-sitter-hl-faces)

(defface sdml-mode-hl-face-type-definition
  '((default :inherit tree-sitter-hl-face:type :weight semi-bold))
  "Face for module definitions."
  :group 'tree-sitter-hl-faces)

(defun sdml-mode-hl-face-mapping-funtion (capture-name)
  "Add to the core mapping, if CAPTURE-NAME is 'module[.*]'."
  (cond
   ((string= capture-name "module") 'sdml-mode-hl-face-module)
   ((string= capture-name "module.definition") 'sdml-mode-hl-face-module-definition)
   ((string= capture-name "type.definition") 'sdml-mode-hl-face-type-definition)
   (t nil)))


;; --------------------------------------------------------------------------
;; Highlighting  Setup Function
;; --------------------------------------------------------------------------

(defun sdml-mode-hl-setup ()
  "Setup tree-sitter-hl with custom face mapping function."
  ;; Doc: This should be set by major modes ...
  (setq-local tree-sitter-hl-default-patterns sdml-mode-tree-sitter-hl-patterns)
  (add-function :before-until
                tree-sitter-hl-face-mapping-function
                #'sdml-mode-hl-face-mapping-funtion)
  (tree-sitter-hl-mode))


(provide 'sdml-mode-hl)

;;; sdml-mode-hl.el ends here
