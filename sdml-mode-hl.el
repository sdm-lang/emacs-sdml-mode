;;; sdml-mode-hl.el --- Highlighting Support -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>

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

;; Minor mode to provide highlighting of SDML (sdml-mode) source.

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
    "is"
    "of"
    "end"
   ] @keyword

   ;; -------------------------------------------------------------------
   ;; Operators
   ;; -------------------------------------------------------------------

   [
    "="
    "->"
    "→"
    "<-"
    "←"
    ".."
    ] @operator

   ;; -------------------------------------------------------------------
   ;; Module & Imports (Note module => type; definition => scope)
   ;; -------------------------------------------------------------------

   (module "module" @keyword name: (identifier) @module.definition)
   (module "version" @keyword)

   (from_clause "from" @keyword)

   (module_path_root_only "::" @punctuation.separator)
   (module_path_relative "::" @punctuation.separator)
   (module_path_relative segment: (identifier) @module.special)
   (module_path_absolute "::" @punctuation.separator
                         segment: (identifier) @module.special)

   (import_statement "import" @keyword)
   (import_statement [ "[" "]" ] @punctuation.bracket)

   (member_import name: (qualified_identifier) @type)
   (member_import "as" @keyword rename: (identifier) @type)

   (module_import name: (identifier) @module)
   (module_import "as" @keyword rename: (identifier) @module)

   ;; -------------------------------------------------------------------
   ;; Annotations and Constraints (Note property => label)
   ;; -------------------------------------------------------------------

   (annotation_property
    "@" @label
    name: (identifier_reference) @label)

   (annotation_property value: (value (identifier_reference) @type))

   (constraint "assert" @keyword name: (identifier) @label)

   (informal_constraint (quoted_string) @embedded)
   (informal_constraint language: (controlled_language_tag) @label)

   (constraint_environment "with" @keyword)

   (function_def
    (function_signature name: (identifier) @function.definition))

   (function_signature "def" @keyword)
   (function_signature target: (_) @type)
   (function_signature [ "(" ")" ] @punctuation.bracket)

   (function_parameter name: (identifier) @variable.parameter)
   (function_parameter target: (_) @type)

   (function_cardinality_expression (sequence_ordering) @keyword)
   (function_cardinality_expression (sequence_uniqueness) @keyword)
   (function_cardinality_expression [ "{" "}" ] @punctuation.bracket)

   (function_body (function_op_by_definition) @operator)

   (function_composition subject: (reserved_self) @variable.builtin)
   (function_composition name: (identifier) @function.call)
   (function_composition [ "·" "." ] @operator)

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

   (variable (identifier) @variable)

   (functional_term
    function: (term (identifier_reference) @function.call))

   (sequence_builder [ "{" "}" ] @punctuation.bracket
                     (set_op_builder) @punctuation.separator)

   (sequence_of_predicate_values [ "{" "}" ] @punctuation.bracket)
   (sequence_of_predicate_values (sequence_ordering) @keyword)
   (sequence_of_predicate_values (sequence_uniqueness) @keyword)
   (sequence_of_predicate_values  [ "[" "]" ] @punctuation.bracket)
   (sequence_of_predicate_values (identifier_reference) @type)

   (unary_boolean_sentence
    [ (logical_op_negation "¬" @operator)
      (logical_op_negation "not" @keyword) ])

   (binary_boolean_sentence
    [ (logical_op_conjunction "∧" @operator)
      (logical_op_conjunction "and" @keyword)
      (logical_op_disjunction "∨" @operator)
      (logical_op_disjunction "or" @keyword)
      (logical_op_exclusive_disjunction "⊻" @operator)
      (logical_op_exclusive_disjunction "xor" @keyword)
      (logical_op_implication [ "==>" "⇒" ] @operator)
      (logical_op_implication "implies" @keyword)
      (logical_op_biconditional [ "<==>" "⇔" ] @operator)
      (logical_op_biconditional "iff" @keyword) ])

   (quantified_variable_binding
    [ (logical_quantifier_universal "∀" @operator)
      (logical_quantifier_universal "forall" @keyword)
      (logical_quantifier_existential "∃" @operator)
      (logical_quantifier_existential "exists" @keyword) ])

   (quantified_variable
    [ (set_op_membership "∈" @operator)
      (set_op_membership "in" @keyword) ])

   ;; -------------------------------------------------------------------
   ;; Types
   ;; -------------------------------------------------------------------

   [
    (builtin_simple_type)
    (unknown_type)
    ] @type.builtin

   (data_type_def
    "datatype" @keyword
    name: (identifier) @type.definition)
   (data_type_def base: (identifier_reference) @type)
   (data_type_def base: (builtin_simple_type) @type.builtin)
   (data_type_def opaque: (opaque) @keyword)

   (datatype_def_restriction [ "{" "}" ] @punctuation.bracket)
   (length_restriction_facet
    [ "length"
      "maxLength"
      "minLength" ] @property
    "=" @operator)
   (digit_restriction_facet
    [ "fractionDigits"
      "totalDigits" ] @property
    "=" @operator)
   (value_restriction_facet
    [ "maxExclusive"
      "maxInclusive"
      "minExclusive"
      "minInclusive" ] @property
    "=" @operator)
   (tz_restriction_facet
    "explicitTimezone" @property
    "=" @operator)
   (pattern_restriction_facet [ "[" "]" ] @punctuation.bracket)
   (pattern_restriction_facet
    "pattern" @property
    "=" @operator
    (quoted_string) @string)

   (kw_is_fixed) @keyword

   (tz_restriction_value) @keyword

   (dimension_def "dimension" @keyword name: (identifier) @type.definition)

   (entity_def "entity" @keyword name: (identifier) @type.definition)

   (enum_def "enum" @keyword name: (identifier) @type.definition)

   (event_def "event" @keyword name: (identifier) @type.definition)

   (property_def "property" @keyword)

   (structure_def "structure" @keyword name: (identifier) @type.definition)

   (union_def "union" @keyword name: (identifier) @type.definition)

   (source_entity "source" @keyword entity: (identifier_reference) @type)
   (source_entity "with" @keyword)
   (source_entity [ "[" "]" ] @punctuation.bracket)
   (source_entity member: (identifier) @variable.field)

   ;; -------------------------------------------------------------------
   ;; RDF Definitions
   ;; -------------------------------------------------------------------

   (rdf_def "rdf" @keyword name: (identifier) @type.definition)
   (rdf_types "type" @keyword type: (identifier_reference) @type)
   (rdf_types [ "[" "]" ] @punctuation.bracket)

   ;; -------------------------------------------------------------------
   ;; Type Classes
   ;; -------------------------------------------------------------------

   (type_class_def "class" @keyword name: (identifier) @type.definition)
   (type_class_def [ "(" ")" ] @punctuation.bracket)

   (type_variable name: (identifier) @type)
   (type_variable (type_op_combiner) @operator)

   (type_class_reference name: (identifier_reference) @type)

   (type_class_arguments [ "(" ")" ] @punctuation.bracket)

   (method_def
    (function_signature name: (identifier) @method.definition))

   (wildcard) @type.builtin

   ;; -------------------------------------------------------------------
   ;; Members
   ;; -------------------------------------------------------------------

   (entity_identity "identity" @keyword)

   (member_def
    name: (identifier) @variable.field
    target: (type_reference) @type)

   (property_ref
    "ref" @keyword
    property: (identifier_reference) @variable.field)

   (dimension_parent
    "parent" @keyword
    name: (identifier) @variable.field
    entity: (identifier_reference) @type)

   (value_variant name: (identifier) @constant)

   (type_variant (identifier_reference) @type)
   (type_variant "as" @keyword rename: (identifier) @type)

   (cardinality_expression ordering: (sequence_ordering) @keyword)
   (cardinality_expression uniqueness: (sequence_uniqueness) @keyword)
   (cardinality_expression [ "{" "}" ] @punctuation.bracket)

   ;; -------------------------------------------------------------------
   ;; Values
   ;; -------------------------------------------------------------------

   (string (quoted_string) @string)
   (string language: (language_tag) @property)

   (iri) @string.special

   (binary) @string.special

   [
    (rational)
    (decimal)
    (double)
    (integer)
    (unsigned)
    ] @number

   (boolean) @constant.builtin

   (value_constructor name: (identifier_reference) @function.call)
   (value_constructor [ "(" ")" ] @punctuation.bracket)

   (value (identifier_reference) @type)

   (sequence_of_values [ "{" "}" ] @punctuation.bracket)
   (sequence_of_values (sequence_ordering) @keyword)
   (sequence_of_values (sequence_uniqueness) @keyword)
   (sequence_of_values  [ "[" "]" ] @punctuation.bracket)
   (sequence_of_values (identifier_reference) @type)

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
  "Add to the core mapping, if CAPTURE-NAME is \\='module[.*]\\='."
    (cond
   ((string= capture-name "module") 'sdml-mode-hl-face-module)
   ((string= capture-name "module.definition") 'sdml-mode-hl-face-module-definition)
   ((string= capture-name "type.definition") 'sdml-mode-hl-face-type-definition)
   (t nil)))

;; --------------------------------------------------------------------------
;; Highlighting Minor Mode
;; --------------------------------------------------------------------------

;;;###autoload
(define-minor-mode
  sdml-mode-hl-mode
  "Minor mode to provide highlighting of SDML source."

  :group 'sdml

  :tag "Enable SDML highlighting minor mode"

  :lighter nil

  (setq-local tree-sitter-hl-default-patterns sdml-mode-tree-sitter-hl-patterns)
  (add-function :before-until
                tree-sitter-hl-face-mapping-function
                #'sdml-mode-hl-face-mapping-funtion)
  (tree-sitter-hl-mode))


(provide 'sdml-mode-hl)

;;; sdml-mode-hl.el ends here
