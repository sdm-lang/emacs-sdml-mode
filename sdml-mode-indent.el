;;; sdml-mode-indent.el --- Indentation Support -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Minor mode to provide indentation when editing SDML (sdml-mode) source.

;;; Code:

(require 'tree-sitter-indent)

;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(defcustom sdml-mode-indent-offset 2
  "Number of spaces for each indentation step."
  :tag "Indentation number of spaces"
  :type 'natnum
  :group 'sdml)


;; --------------------------------------------------------------------------
;; Indentation
;; --------------------------------------------------------------------------

(defconst sdml-mode-indent--tree-sitter-scopes
  '(;; These nodes are always indented
    (indent-all . (member
                   entity_identity
                   function_def
                   function_body
                   informal_constraint
                   constraint_sentence
                   from_definition_clause
                   source_entity
                   dimension_parent))

    ;; If parent node is one of this and current node is not first → indent
    (indent-rest . ())

    ;; If parent node is one of this and current node is in middle → indent
    (indent-body . (module_body
                    import_statement
                    annotation_only_body
                    datatype_def_restriction
                    ;; entity_body << this double indents.
                    dimension_body
                    enum_body
                    structure_body
                    union_body
                    type_class_body
                    function_body
                    from_definition_clause
                    source_entity
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
    (outdent . (set_op_builder))))


;; --------------------------------------------------------------------------
;; Indentation Minor Mode
;; --------------------------------------------------------------------------

;;;###autoload
(define-minor-mode
  sdml-mode-indent-mode
  "Minor mode to provide indentation when editing SDML source."

  :group 'sdml

  :tag "Enable SDML indentation minor mode"

  :lighter nil

  (setq-local sdml-indent-offset sdml-mode-indent-offset)
  (setq-local tree-sitter-indent-sdml-scopes sdml-mode-indent--tree-sitter-scopes)
  (tree-sitter-indent-mode))


(provide 'sdml-mode-indent)

;;; sdml-mode-indent.el ends here
