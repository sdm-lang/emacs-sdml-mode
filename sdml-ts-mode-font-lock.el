;;; sdml-ts-mode-font-lock.el --- Internal font-lock support -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; License: see sdml-ts-mode.el

;;; Commentary:

;; Internal module to organize all font-lock related functionality.
;;
;; Use `treesit-font-lock-level' to determine the amount of detail shown.

;;; Code:

(require 'treesit)

(require 'sdml-ts-mode-query)

;; --------------------------------------------------------------------------
;; Font Lock ❱ Faces
;; --------------------------------------------------------------------------

(defface sdml-ts-comment-face
  '((t (:inherit font-lock-comment-face :weight light)))
  "Face used for SDML line comments."
  :group 'sdml-ts)

(defface sdml-ts-warning-face
  '((t (:inherit font-lock-warning-face :weight normal :slant italic)))
  "Face used for syntax and other warnings."
  :group 'sdml-ts)

(defface sdml-ts-error-face
  '((t (:inherit font-lock-warning-face :underline (:style wave :position t))))
  "Face used for syntax and other errors."
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
  '((t (:inherit font-lock-builtin-face :foreground "DarkGoldenrod4")))
  "Face used for annotation property assertions in SDML."
  :group 'sdml-ts)

(defface sdml-ts-annotation-property-def-face
  '((t (:inherit sdml-ts-annotation-property-face :weight bold)))
  "Face used for annotation property definitions in SDML."
  :group 'sdml-ts)

(defface sdml-ts-annotation-constraint-face
  '((t (:inherit sdml-ts-annotation-property-face)))
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
  '((t (:inherit font-lock-variable-use-face)))
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
  '((t (:inherit font-lock-delimiter-face :foreground "DarkSlateGray4")))
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
;; Tree-Sitter ❱ Font Lock ❱ Rules
;; --------------------------------------------------------------------------

(when sdml-ts-mode--debug-mode
  (makunbound 'sdml-ts-mode-font-lock--settings))

(defvar sdml-ts-mode-font-lock--settings
  (treesit-font-lock-rules
   :feature 'comments
   :language 'sdml
   `((,sdml-ts-mode-query--any-comment @sdml-ts-comment-face))

   :feature 'global-keywords
   :language 'sdml
   `((,sdml-ts-mode-query--global-keywords @sdml-ts-keyword-face)
     (builtin_types
      [ "anyURI"
        "base64Binary"
        "binary"
        "boolean"
        "byte"
        "date"
        "dateTime"
        "dateTimeStamp"
        "dayTimeDuration"
        "decimal"
        "double"
        "duration"
        "float"
        "gDay"
        "gMonth"
        "gMonthDay"
        "gYear"
        "gYearMonth"
        "hexBinary"
        "int"
        "integer"
        "iri"
        "language"
        "long"
        "negativeInteger"
        "nonNegativeInteger"
        "nonPositiveInteger"
        "normalizedString"
        "Nothing"
        "positiveInteger"
        "rational"
        "real"
        "short"
        "string"
        "Thing"
        "time"
        "token"
        "unsigned"
        "unsignedByte"
        "unsignedInt"
        "unsignedLong"
        "unsignedShort"
        "yearMonthDuration" ]
      @sdml-ts-type-builtin-face))

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
   '((sentence_with_environment [ "with" "for" ] @sdml-ts-keyword-face)
     (keyword_function_def "def" @sdml-ts-keyword-face)
     (function_signature name: (identifier) @sdml-ts-function-def-face)
     (function_signature target: (_) @sdml-ts-type-face)
     (function_parameter name: (identifier) @sdml-ts-field-face)
     (function_type_reference (identifier_reference) @sdml-ts-type-face)
     (cardinality_reference_expression (sequence_ordering) @sdml-ts-keyword-face)
     (cardinality_reference_expression (sequence_uniqueness) @sdml-ts-keyword-face)
     (atomic_sentence predicate: (term (identifier_reference) @sdml-ts-function-call-face))
     (atomic_sentence argument: (term (identifier_reference) @sdml-ts-variable-face))
     (equation (term (identifier_reference) @sdml-ts-variable-face))
     (inequation (term (identifier_reference) @sdml-ts-variable-face))
     (set_expression_sentence (term (identifier_reference) @sdml-ts-variable-face))
     (arithmetic_expression_sentence (term (identifier_reference) @sdml-ts-variable-face))
     (quantified_variable
      (variable name: (identifier) @sdml-ts-variable-face)
      ((set_op_is_member) @sdml-ts-operator-logical-face))
     ;; terms
     (sequence_builder variable: (variable name: (identifier) @sdml-ts-variable-face))
     (functional_term
      function: (term (identifier_reference) @sdml-ts-function-call-face))
     (functional_term
      argument: (term (identifier_reference) @sdml-ts-variable-face))
     (function_composition subject: (reserved_self) @sdml-ts-constant-builtin-face)
     (function_composition subject: (reserved_event) @sdml-ts-constant-builtin-face) ;; TODO: scope this
     (function_composition subject: (identifier) @sdml-ts-variable-face)
     (function_composition name: (identifier) @sdml-ts-function-call-face)
     (term (reserved_self) @sdml-ts-constant-builtin-face)
     (term (reserved_event) @sdml-ts-constant-builtin-face)
     (reserved_event "event" @sdml-ts-constant-builtin-face))

   :feature 'modules
   :language 'sdml
   '((module name: (identifier) @sdml-ts-module-def-face)
     (module_version "version" @sdml-ts-keyword-face)
     (module_path_absolute segment: (identifier) @sdml-ts-module-face)
     (module_path_relative segment: (identifier) @sdml-ts-module-face)
     (member_import name: (qualified_identifier) @sdml-ts-type-face)
     (member_import rename: (identifier) @sdml-ts-type-face)
     (module_import name: (identifier) @sdml-ts-module-face)
     (module_import rename: (identifier) @sdml-ts-module-face))

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
     (datatype_base_type_reference (identifier_reference) @sdml-ts-type-face)
     (datatype_def opaque: (opaque) @sdml-ts-keyword-facet-face)
     (dimension_def name: (identifier) @sdml-ts-type-def-face)
     (entity_def name: (identifier) @sdml-ts-type-def-face)
     (enum_def name: (identifier) @sdml-ts-type-def-face)
     (event_def name: (identifier) @sdml-ts-type-def-face)
     (event_def name: (identifier) @sdml-ts-type-def-face)
     (event_def name: (identifier) @sdml-ts-type-def-face)
     (function_signature name: (identifier) @sdml-ts-function-def-face)
     (function_signature target: (_) @sdml-ts-type-face)
     (function_parameter name: (identifier) @sdml-ts-field-face)
     (function_type_reference (identifier_reference) @sdml-ts-type-face)
     (cardinality_reference_expression (sequence_ordering) @sdml-ts-keyword-face)
     (cardinality_reference_expression (sequence_uniqueness) @sdml-ts-keyword-face)
     (metric_group_def name: (identifier) @sdml-ts-type-def-face)
     (metric_event_binding "on" @sdml-ts-keyword-face)
     (metric_event_binding (identifier_reference) @sdml-ts-type-face)
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
   '((member_def name: (identifier) @sdml-ts-field-def-face)
     (type_reference (identifier_reference) @sdml-ts-type-face)
     (annotation_member_def
      "@" @sdml-ts-annotation-property-def-face
      (member_def name: (identifier) @sdml-ts-annotation-property-def-face))
     (property_ref "ref" @sdml-ts-keyword-face)
     (property_ref property: (identifier_reference) @sdml-ts-field-face)
     (metric_ref "ref" @sdml-ts-keyword-face)
     (metric_ref referent: (identifier_reference) @sdml-ts-field-face)
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
     (function_signature [ "->" "→" ] @sdml-ts-operator-type-face)
     (function_parameter [ "->" "→" ] @sdml-ts-operator-type-face)
     (mapping_type [ "->" "→" ] @sdml-ts-operator-type-face)
     (mapping_value [ "->" "→" ] @sdml-ts-operator-type-face)
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
     (mapping_type [ "(" ")" ] @sdml-ts-bracket-parameters-face)
     (type_class_def [ "(" ")" ] @sdml-ts-bracket-parameters-face)
     (type_parameter_restriction [ "(" ")" ] @sdml-ts-bracket-parameters-face)
     (value_constructor [ "(" ")" ] @sdml-ts-bracket-parameters-face))

   :feature 'restrictions
   :language 'sdml
   '((cardinality_reference_expression [ "{" "}" ] @sdml-ts-bracket-restriction-face)
     (datatype_type_restrictions [ "{" "}" ] @sdml-ts-bracket-restriction-face)
     (sequence_of_predicate_values [ "{" "}" ] @sdml-ts-bracket-restriction-face)
     (cardinality_expression [ "{" "}" ] @sdml-ts-bracket-restriction-face)
     (sequence_of_values [ "{" "}" ]) @sdml-ts-bracket-restriction-face)

   :feature 'delimiters
   :language 'sdml
   '((module_path_relative "::" @sdml-ts-separator-module-path-face)
     (module_path_absolute "::" @sdml-ts-separator-module-path-face)
     (quantified_sentence "," @sdml-ts-separator-qualified-sentence-face)
     ((seq_builder_separator) @sdml-ts-separator-sequence-builder-face))

   :feature 'error
   :language 'sdml
   '((ERROR) @sdml-ts-error-face)))

;; --------------------------------------------------------------------------
;; Tree-Sitter ❱ Font Lock ❱ Features
;; --------------------------------------------------------------------------

(when sdml-ts-mode--debug-mode
  (makunbound 'sdml-ts-mode-font-lock--feature-list))

(defvar sdml-ts-mode-font-lock--feature-list
  '(;; Level-1:
    (comments)
    ;; Level-2 adds:
    (global-keywords strings binaries iris)
    ;; Level-3 (default) adds:
    (modules definitions fields variants booleans numbers)
    ;; Level-4 adds:
    (operators error sequences parameters restrictions delimiters
               facets annotations constraints formals)))

;; ------------------------------------------------------------sdml-ts-mode--defun-name--------------
;; Tree-Sitter ❱ Font Lock ❱ Setup function
;; --------------------------------------------------------------------------

;;;###autoload
(defun sdml-ts-mode-font-lock-setup ()
  "Setup `treesit'-based font-lock highlighting."
  (message "Setting up tree-sitter/font-lock for SDML")
  (setq-local
   treesit-font-lock-settings
   sdml-ts-mode-font-lock--settings)
  (setq-local
   treesit-font-lock-feature-list
   sdml-ts-mode-font-lock--feature-list))

(provide 'sdml-ts-mode-font-lock)

;;; sdml-ts-mode-font-lock.el ends here
