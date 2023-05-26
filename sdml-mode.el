;;; sdml-mode.el --- Major mode for SDML -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Simon Johnston

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Version: 0.1.3
;; Package-Requires: ((emacs "28.2") (tree-sitter "0.18.0") (tree-sitter-indent "0.3"))
;; URL: https://github.com/johnstonskj/emacs-sdml-mode
;; Keywords: languages tools

;;; License:

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

;; Folding
;;
;; This uses a package `ts-fold' which is not packaged for a repository and must
;; therefore be installed and required PRIOR to this package if you want to enable
;; folding.
;;
;; `\\[ts-fold-close]' -- fold item
;; `\\[ts-fold-open]' -- unfold item
;; `\\[ts-fold-close-all]' -- fold all items in buffer
;; `\\[ts-fold-open-all]' -- unfold all items in buffer
;; `\\[ts-fold-open-recursively]' -- unfold item and all children
;; `\\[ts-fold-toggle]' -- toggle fold/unfold state
;;

;; Fold Indicators
;;
;; This is only enabled if folding is enabled (see above) and running in GUI mode.
;;
;; To switch to left/right fringe: (Default is left-fringe)
;;
;; `(setq ts-fold-indicators-fringe 'right-fringe)'
;;
;; To lower/higher the fringe overlay's priority: (Default is 30)
;;
;; `(setq ts-fold-indicators-priority 30)'
;;

;;; Code:

(eval-when-compile
  (require 'rx)) ;; built-in

(require 'tree-sitter)
(require 'tree-sitter-hl) ;; included in above
(require 'tree-sitter-indent)

;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(defgroup sdml nil
  "SDML language support."
  :tag "SDML"
  :prefix "sdml-"
  :group 'languages)

(defcustom sdml-indent-offset 2
  "Number of spaces for each indentation step."
  :tag "Indentation number of spaces"
  :type 'natnum
  :group 'sdml)

(defcustom sdml-prettify-symbols-alist
  '(("->" . ?→) ("<-" . ?←))
  "An alist of symbol prettifications used for `prettify-symbols-alist'."
  :tag "Symbol mapping for prettify"
  :type '(repeat (cons string character))
  :group 'sdml)


;; --------------------------------------------------------------------------
;; Additional Faces
;; --------------------------------------------------------------------------

;; Unfortunately the name prefix and use of ":" comes from the `tree-sitter-hl'
;; package and is required.
(defface tree-sitter-hl-face:type.scope
  '((default :inherit tree-sitter-hl-face:type))
  "Face for type scopes, or namespaces, in definitions and type constraints."
  :group 'tree-sitter-hl-faces)

;; See:
;; - tree-sitter-hl-face-mapping-function
;; - tree-sitter-hl-add-patterns
;; - tree-sitter-hl-default-patterns


;; --------------------------------------------------------------------------
;; Syntax
;; --------------------------------------------------------------------------

(defvar sdml-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- ". 12b" table)
    table))


;; --------------------------------------------------------------------------
;; Highlighting
;; --------------------------------------------------------------------------

;; Unfortunately the name prefix and use of ":" comes from the `tree-sitter-hl'
;; package and is required.
(defconst sdml-mode-tree-sitter-hl-patterns
  [
   ;; Comments
   (line_comment) @comment

   [
    "datatype"
    "end"
    "entity"
    "enum"
    "event"
    "group"
    "identity"
    "import"
    "is"
    "module"
    "ref"
    "source"
    "structure"
    (unknown_type)
    ] @keyword

   ;; Module & Imports
   (module name: (identifier) @type.scope)

   (member_import
    name: (qualified_identifier) @type)

   (module_import
    name: (identifier) @type.scope)

   ;; Annotations
   (annotation
    "@" @label
    name: (identifier_reference) @label
    "=" @operator)

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
    "<-" @operator
    base: (identifier_reference) @type)

   ;; Members
   (identity_member
    name: (identifier) @variable
    "->" @operator
    target: (_) @type)

   (member_by_value
    name: (identifier) @variable
    "->" @operator
    target: (_) @type)

   (member_by_reference
    name: (identifier) @variable
    "->" @operator
    target: (_) @type)

   (cardinality_range ".." @operator)

   ;; Values
   (string
    (quoted_string) @string
    language: (language_tag) @property)

   (iri_reference) @string.special

   [
    (decimal)
    (double)
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

;; Unfortunately the name prefix and use of ":" here comes from the
;; `tree-sitter-indent' package and is required.
(defconst tree-sitter-indent-sdml-scopes
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
                    list_of_values
                    entity_group
                    structure_group))

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
    (outdent . ())))


;; --------------------------------------------------------------------------
;; Folding
;; --------------------------------------------------------------------------

(when (featurep 'ts-fold)
  (defconst sdml-mode-folding-definitions
    '((data_type_def . (ts-fold-range-seq 7 2))
      (entity_def . (ts-fold-range-seq 5 2))
      (enum_def . (ts-fold-range-seq 3 2))
      (event_def . (ts-fold-range-seq 4 2))
      (entity_body . (ts-fold-range-seq 1 -2))
      (structure_body . (ts-fold-range-seq 1 -2))
      (enum_body . (ts-fold-range-seq 1 -2))
      (annotation_only_body . (ts-fold-range-seq 1 -2))
      (entity_group . (ts-fold-range-seq 4 -2))
      (structure_group . (ts-fold-range-seq 4 -2))
      (list_of_values . ts-fold-range-seq)
      (line_comment . (lambda (node offset) (ts-fold-range-line-comment node offset ";;"))))))


;; --------------------------------------------------------------------------
;; Key Bindings
;; --------------------------------------------------------------------------

(defvar sdml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s d") 'tree-sitter-debug-mode)
    (define-key map (kbd "C-c C-s q") 'tree-sitter-query-builder)
    (define-key map (kbd "C-c C-s -") 'ts-fold-close)
    (define-key map (kbd "C-c C-s +") 'ts-fold-open)
    (define-key map (kbd "C-c C-s C--") 'ts-fold-close-all)
    (define-key map (kbd "C-c C-s C-+") 'ts-fold-open-all)
    (define-key map (kbd "C-c C-s /") 'ts-fold-open-recursively)
    (define-key map (kbd "C-c C-s .") 'ts-fold-toggle)
    map)
  "Keymap for SDML major mode.")


;; --------------------------------------------------------------------------
;; Abbrev Table
;; --------------------------------------------------------------------------

(define-skeleton sdml-mode--new-module
  "New module." nil
  > "module " _ " is" \n
  > "end")

(define-skeleton sdml-mode--new-entity
  "New entity." nil
  > "entity " _ " is" \n
  > "identity id -> unknown" \n
  > "" \n
  > "end")

(define-skeleton sdml-mode--new-structure
  "New structure." nil
  > "structure " _ " is" \n
  > "" \n
  > "end")

(define-skeleton sdml-mode--new-event
  "New event." nil
  > "event " _ " source Entity is" \n
  > "" \n
  > "end")

(define-skeleton sdml-mode--new-enum
  "New enum." nil
  > "enum " _ " is" \n
  > "VariantOne = 1" \n
  > "VariantTwo = 2" \n
  > "end")

(define-abbrev-table 'sdml-abbrev-table
  '(("uk" "-> unknown")
    ("mod" "" 'sdml-mode--new-module)
    ("ent" "" 'sdml-mode--new-entity)
    ("str" "" 'sdml-mode--new-structure)
    ("evt" "" 'sdml-mode--new-event)
    ("enu" "" 'sdml-mode--new-enum)
    ("xs" "xsd:string")
    ("xi" "xsd:integer")
    ("xb" "xsd:boolean")
    ("xu" "xsd:anyURI")))

;; --------------------------------------------------------------------------
;; Mode Definition
;; --------------------------------------------------------------------------

(defun sdml-mode-setup (&optional dylib-file)
  "Load and connect the parser dynamic library.

  Load the dynamic library, either with the explicit path
  in DYLIB-FILE, or by searching the path in `tree-sitter-load-path'.
  The parser library will be named  `sdml' with a
  platform-specific extension in `tree-sitter-load-suffixes'."
  (unless (assoc 'sdml tree-sitter-languages)
    ;; Load the dynamic library from the search path.
    (tree-sitter-load 'sdml dylib-file))

  (unless (assoc 'sdml-mode tree-sitter-major-mode-language-alist)
    ;; Connect the major mode to the loaded library.
    (add-to-list 'tree-sitter-major-mode-language-alist
                 '(sdml-mode . sdml))))

;;;###autoload
(define-derived-mode
  sdml-mode
  prog-mode
  "SDML"
  "Major mode for editing SDML files.

  Key bindings:
  \\{sdml-mode-map}"

  :group 'sdml

  :syntax-table sdml-syntax-table

  :abbrev-table sdml-abbrev-table

  ;; Only the basic font-lock, taken care of by tree-sitter-hl-mode
  (unless font-lock-defaults
    (setq font-lock-defaults '(nil)))

  ;; Comment handling
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-multi-line t)

  ;; Prettify (prettify-symbols-mode)
  (setq prettify-symbols-alist sdml-prettify-symbols-alist)
  (prettify-symbols-mode)

  (abbrev-mode)

  ;; tree-sitter debug and query
  (setq-local tree-sitter-debug-jump-buttons t)
  (setq-local tree-sitter-debug-highlight-jump-region t)
  (tree-sitter-mode)

  ;; tree-sitter highlighting capabilities
  (setq-local tree-sitter-hl-default-patterns sdml-mode-tree-sitter-hl-patterns)
  (tree-sitter-hl-mode)
  (tree-sitter-indent-mode)

  ;; Additional tree-sitter capabilities
  (when (featurep 'ts-fold)

    (add-to-list 'ts-fold-range-alist `(sdml-mode . ,sdml-mode-folding-definitions))
    (ts-fold-mode)
    (ts-fold-line-comment-mode)

    (when (and window-system (featurep ts-fold-indicators))
      (ts-fold-indicators-mode))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sdml?\\'" . sdml-mode))

;; --------------------------------------------------------------------------
;; Tree-sitter integration
;; --------------------------------------------------------------------------

(provide 'sdml-mode)

;;; sdml-mode.el ends here
