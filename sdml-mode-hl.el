;;; sdml-mode-hl.el --- Highlighting Support -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>

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

;; Copyright 2023-2025 Simon Johnston <johnstonskj@gmail.com>
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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
   (module_path_absolute
    "::" @punctuation.separator
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

   (annotation_property "@" @label name: (identifier_reference) @label)

   (annotation_property value: (value (identifier_reference) @type))

   (constraint "assert" @keyword name: (identifier) @label)

   (informal_constraint (quoted_string) @embedded)
   (informal_constraint language: (controlled_language_tag) @label)

   (constraint_environment "with" @keyword)

   (function_def (function_signature name: (identifier) @function.definition))

   (function_signature "def" @keyword)
   (function_signature target: (_) @type)
   (function_signature [ "(" ")" ] @punctuation.bracket)

   (function_parameter name: (identifier) @variable.parameter)
   (function_parameter target: (_) @type)

   (cardinality_reference_expression (sequence_ordering) @keyword)
   (cardinality_reference_expression (sequence_uniqueness) @keyword)
   (cardinality_reference_expression [ "{" "}" ] @punctuation.bracket)

   (function_body [ ":=" "≔" ] @operator)

   (function_composition subject: (reserved_self) @variable.builtin)
   (function_composition name: (identifier) @function.call)
   (function_composition [ "." ] @operator)

   (constraint_sentence [ "(" ")" ] @punctuation.bracket)

   (atomic_sentence
    predicate: (term (identifier_reference) @function.call))
   (atomic_sentence [ "(" ")" ] @punctuation.bracket)
   (atomic_sentence
    argument: (term (identifier_reference (identifier) @variable)))

   (term (reserved_self) @variable.builtin)

   (equation lhs: (term (identifier_reference) @variable))

   (equation rhs: (term (identifier_reference) @variable))

   (quantified_sentence "," @punctuation.separator)

   (variable (identifier) @variable)
   (variable range: (identifier_reference) @type)

   (functional_term
    function: (term (identifier_reference) @function.call))
   (functional_term [ "(" ")" ] @punctuation.bracket)
   (functional_term
    argument: (term (identifier_reference (identifier) @variable)))

   (sequence_builder
    [ "{" "}" ] @punctuation.bracket
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

   (set_expression_sentence
    [ (set_op_union "∪" @operator)
      (set_op_union "union" @keyword)
      (set_op_intersection "∩" @operator)
      (set_op_intersection "intersection" @keyword)
      (set_op_complement "∖" @operator)
      (set_op_complement "complement" @keyword)
      (set_op_subset "⊂" @operator)
      (set_op_subset "subset" @keyword)
      (set_op_subset_or_equal "⊆" @operator)
      (set_op_subset_or_equal "subseteq" @keyword)
      (set_op_supset "⊃" @operator)
      (set_op_supset "supset" @keyword)
      (set_op_supset_or_equal "⊇" @operator)
      (set_op_supset_or_equal "supseteq" @keyword)
      (set_op_product "⨉" @operator)
      (set_op_product "product" @keyword)
      (set_op_membership "∈" @operator)
      (set_op_membership "in" @keyword) ])

   (arithmetic_expression_sentence
    [ (math_op_multiply [ "*" "✕" ] @operator)
      (math_op_divide [ "/" "÷" ] @operator)
      (math_op_modulo "mod" @keyword)
      (math_op_modulo "%" @operator)
      (math_op_add "+" @operator)
      (math_op_subtract "-" @operator) ])

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
    [ "length" "maxLength" "minLength" ] @property
    "=" @operator)
   (digit_restriction_facet
    [ "fractionDigits" "totalDigits" ] @property
    "=" @operator)
   (value_restriction_facet
    [ "maxExclusive" "maxInclusive" "minExclusive" "minInclusive" ] @property
    "=" @operator)
   (tz_restriction_facet
    "explicitTimezone" @property
    "=" @operator
    (tz_restriction_value) @keyword)
   (pattern_restriction_facet
    "pattern" @property
    "=" @operator
    (quoted_string) @string)
   (pattern_restriction_facet [ "[" "]" ] @punctuation.bracket)

   (kw_is_fixed) @keyword

   (dimension_def "dimension" @keyword name: (identifier) @type.definition)

   (entity_def "entity" @keyword name: (identifier) @type.definition)

   (enum_def "enum" @keyword name: (identifier) @type.definition)

   (event_def "event" @keyword name: (identifier) @type.definition)

   (property_def "property" @keyword)

   (structure_def "structure" @keyword name: (identifier) @type.definition)

   (union_def "union" @keyword name: (identifier) @type.definition)

   (from_definition_clause "from" @keyword from: (identifier_reference) @type)

   (from_definition_with "with" @keyword)
   (from_definition_with wildcard: (_)  @type.builtin)
   (from_definition_with member: (identifier)  @variable)
   (from_definition_with [ "[" "]" ] @punctuation.bracket)

   (from_definition_without "without" @keyword)
   (from_definition_without member: (identifier)  @variable)
   (from_definition_without [ "[" "]" ] @punctuation.bracket)

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

   (type_parameter name: (identifier) @type.definition)
   (type_parameter (type_op_combiner) @operator)

   (type_parameter_restriction class: (identifier_reference) @type)
   (type_parameter_restriction [ "(" ")" ] @punctuation.bracket)

   ;;(type_restriction_argument (identifier) @type.definition)

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

   [
    (binary)
    (iri) ] @string.special

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
