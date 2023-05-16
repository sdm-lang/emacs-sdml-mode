;;; sdml-mode.el --- Major mode for SDML -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Keywords: language
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.2") (tree-sitter "0.18.0") (tree-sitter-indent "0.3") (tree-sitter-ispell "0.1.0"))
;;
;;; License:

;; Copyright (c) 2023 Simon Johnston

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

;;
;; This package provides a tree-sitter based major mode for SDML - Simple
;; Domain Modeling Language.

;;
;; Installing
;;
;; `(use-package sdml-mode :ensure t)'

;;
;; Usage
;;
;; Once installed the major mode should be used for any file ending in `.sdm'
;; or `.sdml' with highlighting and indentation support.

;;
;; This package depends upon the following packages:
;;
;; - `tree-sitter' :: the core parser support.
;; - `tree-sitter-hl' :: font-lock highlighting based on `tree-sitter'.
;; - `tree-sitter-indent' :: indentation support based on `tree-sitter'.
;; - `tree-sitter-ispell' :: spell checking for text content.

;;; Code:

(eval-when-compile
  (require 'rx))

(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-indent)
(require 'tree-sitter-ispell)

;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(defgroup sdml-mode nil
  "SDML language support."
  :tag "sdml"
  :prefix "sdml-"
  :group 'languages)

(defcustom sdml-indent-offset 2
  "Number of spaces for each indentation step"
  :type 'natnum
  :group 'sdml-mode)

(defcustom sdml-auto-indent-flag t
  "If non-nil indent current line when certain words or
  characters are inserted."
  :type 'boolean
  :group 'sdml-mode)


;; --------------------------------------------------------------------------
;; Prettify
;; --------------------------------------------------------------------------

(defcustom sdml-prettify-symbols-alist
  '(("->" . ?→) ("<-" . ?←))
  "An alist of symbol prettifications used for `prettify-symbols-alist'."
  :type '(repeat (cons string character))
  :group 'sdml-mode)


;; --------------------------------------------------------------------------
;; Highlighting
;; --------------------------------------------------------------------------

(defface tree-sitter-hl-face:type.scope
  '((default :inherit tree-sitter-hl-face:type))
  "Face for type scopes, or namespaces, in definitions and type constraints."
  :group 'tree-sitter-hl-faces)

;; See:
;; - tree-sitter-hl-face-mapping-function
;; - tree-sitter-hl-add-patterns
;; - tree-sitter-hl-default-patterns

(defconst sdml-mode-tree-sitter-hl-patterns
  [
   ;; Comments
   (line_comment) @comment

   [
    "module"
    "is"
    "import"
    "identity"
    "ref"
    "entity"
    "structure"
    "event"
    "source"
    "structure"
    "enum"
    "datatype"
    "end"
    ] @keyword

   ;; Module & Imports
   (module name: (identifier) @type.scope)

   (member_import
    name: (qualified_identifier) @type)

   (module_import
    name: (identifier) @type.scope)

   ;; Annotations
   (annotation
    "@" @label)
   (annotation
     name: (identifier_reference) @label)
   ;;(annotation
   ;; target: (identifier_reference) @type)

   ;; Types
   (entity_def
    name: (identifier) @type)

   (structure_def
    name: (identifier) @type)

   (enum_def
    name: (identifier) @type)

   (event_def
    name: (identifier) @type
    source: (identifier_reference) @type)

   (data_type_def
    name: (identifier) @type
    base: (identifier_reference) @type)

   ;; Members
   (identity_member
    name: (identifier) @variable
    target: (identifier_reference) @type)

   (member_by_value
    name: (identifier) @variable
    target: (identifier_reference) @type)

   (member_by_reference
    name: (identifier) @variable
    target: (identifier_reference) @type)

   ;; Values
   (quoted_string) @string
   (iri_reference) @string.special
   (language_tag) @property

   [
    (double)
    (decimal)
    (integer)
    (unsigned)
    ] @number

    (boolean) @constant.builtin

    (value_constructor
    name: (identifier_reference)) @function.special

    ;; Punctuation
   "(" @punctuation.bracket
   ")" @punctuation.bracket
   "[" @punctuation.bracket
   "]" @punctuation.bracket
   "{" @punctuation.bracket
   "}" @punctuation.bracket

   ;; Highlight errors in red. This is not very useful in practice, as text will
   ;; be highlighted as user types, and the error could be elsewhere in the code.
   (ERROR) @warning
   ])


;; --------------------------------------------------------------------------
;; Indentation
;; --------------------------------------------------------------------------

(defcustom tree-sitter-indent-sdml-scopes
  '(;; These nodes are always indented
    (indent-all . ())

    ;; If parent node is one of this and node is not first → indent
    (indent-rest . ())

    ;; If parent node is one of this and current node is in middle → indent
    (indent-body . (module_body
                    entity_body
                    structure_body
                    enum_body
                    annotation_only_body
                    list_of_values))

    ;; If parent node is one of these → indent to paren opener
    (paren-indent . ())

    ;; Chaining char → node types we move parentwise to find the first chaining char
    (align-char-to . ())

    ;; Siblings (nodes with same parent) should be aligned to the first child
    (aligned-siblings . (enum_variant))

    ;; if node is one of this, then don't modify the indent
    ;; this is basically a peaceful way out by saying "this looks like something
    ;; that cannot be indented using AST, so best I leave it as-is"
    (multi-line-text . (quoted_string))

    ;; These nodes always outdent (1 shift in opposite direction)
    (outdent . ()))
  "Scopes for indenting in SDML."
  :type 'sexp
  :group 'sdml-mode)


;; --------------------------------------------------------------------------
;; Spelling
;; --------------------------------------------------------------------------

(defcustom tree-sitter-ispell-sdml-text-mapping
  '(quoted_string comment)
  "Nodes to be spell checked in SDML."
  :type '(repeat symbol)
  :group 'sdml-mode)


;; --------------------------------------------------------------------------
;; Key Mapping
;; --------------------------------------------------------------------------

;; tree-sitter-ispell-run-buffer
;; tree-sitter-debug-mode
;; tree-sitter-query-builder

;; --------------------------------------------------------------------------
;; Mode
;; --------------------------------------------------------------------------

(defun sdml--load-language ()
  "Load and connect the parser library.
Load the dynamic library, this should be named `sdml.dylib' or `sdml.so',
and associate it with the major mode."
  (tree-sitter-load 'sdml)

  ;; Connect the major mode to the loaded library.
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(sdml-mode . sdml)))

;;;###autoload
(define-derived-mode
  sdml-mode
  prog-mode
  "SDML"
  "Major mode for editing SDML files."

  :group 'sdml-mode

  (sdml--load-language)

  (unless font-lock-defaults
    (setq font-lock-defaults '(nil)))
  (setq-local tree-sitter-hl-default-patterns sdml-mode-tree-sitter-hl-patterns)

  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-multi-line t)

  (setq prettify-symbols-alist sdml-prettify-symbols-alist)

  (add-to-list 'tree-sitter-ispell-grammar-text-mapping
               `(sdml-mode . ,tree-sitter-ispell-sdml-text-mapping))

  (tree-sitter-mode)
  (tree-sitter-hl-mode)
  (tree-sitter-indent-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sdml?\\'" . sdml-mode))

;; --------------------------------------------------------------------------
;; Tree-sitter integration
;; --------------------------------------------------------------------------

(provide 'sdml-mode)

;;; sdml-mode.el ends here
