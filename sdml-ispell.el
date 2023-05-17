;;; sdml-ispell.el --- Spell check text content -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Keywords: sdml spelling
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.2") (tree-sitter-ispell "0.1.0") (sdml-mode "0.1.0"))

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

;; Complete description goes here.

;;
;; This package depends upon the following packages:
;;
;; - `sdml-mode' :: the core parser support.
;; - `tree-sitter-ispell' :: spell checking for text content.

;;; Code:

(require 'sdml-mode)
(require 'tree-sitter-ispell)

;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(defcustom tree-sitter-ispell-sdml-text-mapping
  '(quoted_string comment)
  "Tree-sitter node types to be spell checked with ispell."
  :tag "Node types for spell checking"
  :type '(repeat symbol)
  :group 'sdml)

;; --------------------------------------------------------------------------
;; Mode Definition
;; --------------------------------------------------------------------------

;;;###autoload
(define-minor-mode
  sdml-ispell-mode
  "Minor mode to allow ispell checking in SDML text content."
  :group 'sdml
  :tag "Enable SDML ispell minor mode"
  :keymap '(([C-c C-s s] . tree-sitter-ispell-run-at-point)
            ([C-c C-s C-s] . tree-sitter-ispell-run-buffer))
  ;; How do we "disable" this?
  (when sdml-ispell-mode
    (tree-sitter-ispell-run-buffer)))

;;;###autoload
(defun sdml-ispell-setup ()
  "Setup the mode, adding configuration to `tree-sitter-ispell'."
  (interactive)
  (add-to-list 'tree-sitter-ispell-grammar-text-mapping
               `(sdml-mode . ,tree-sitter-ispell-sdml-text-mapping)))

(provide 'sdml-ispell)

;;; sdml-ispell.el ends here
