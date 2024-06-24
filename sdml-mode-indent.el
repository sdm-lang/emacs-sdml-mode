;;; sdml-mode-indent.el --- Indentation Support -*- lexical-binding: t; -*-

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
;; Indentation  Setup Function
;; --------------------------------------------------------------------------

(defun sdml-mode-indent-setup ()
  "Setup `tree-sitter-indent' to do auto indentation."
  (setq-local sdml-indent-offset sdml-mode-indent-offset)
  (setq-local tree-sitter-indent-sdml-scopes sdml-mode-indent--tree-sitter-scopes)
  (tree-sitter-indent-mode))


(provide 'sdml-mode-indent)

;;; sdml-mode-indent.el ends here
