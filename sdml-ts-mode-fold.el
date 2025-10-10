;;; sdml-ts-mode-fold.el --- Internal folding support -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; License: see sdml-ts-mode.el

;;; Commentary:

;; Internal module to organize all folding related functionality.

;;; Code:

(require 'treesit)
(require 'treesit-fold)
(require 'treesit-fold-indicators)

;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(defcustom sdml-ts-mode-enable-folding
  t
  "Enable `treesit-fold' support in all SDML buffers."
  :tag "Enable folding"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :group 'sdml)

;; --------------------------------------------------------------------------
;; Tree-Sitter ❱ Code Folding
;; --------------------------------------------------------------------------

(defun sdml-ts-mode--fold-binary (node offset)
  "Fold a binary literal rooted at NODE.
It is safe to ignore OFFSET."
  (treesit-fold-range-markers node offset "#[" "]"))

(defun sdml-ts-mode--fold-sequence (node offset)
  "Fold a sequence of things rooted at NODE.
It is safe to ignore OFFSET."
  (treesit-fold-range-markers node offset "[" "]"))

(defun sdml-ts-mode--fold-restriction (node offset)
  "Fold a restriction or constraint rooted at NODE.
It is safe to ignore OFFSET."
  (treesit-fold-range-markers node offset "{" "}"))

(defun sdml-ts-mode--fold-parameters (node offset)
  "Fold a parameter list rooted at NODE.
It is safe to ignore OFFSET."
  (treesit-fold-range-markers node offset "(" ")"))

(defun sdml-ts-mode--fold-formal-constraint-body (node offset)
  "Fold a formmal constraint body rooted at NODE.
It is safe to ignore OFFSET."
  (treesit-fold-range-markers node offset "is" "end"))

(defun sdml-ts-mode--fold-line-comment (node offset)
  "Fold a line comment body rooted at NODE.
It is safe to ignore OFFSET."
  (treesit-fold-range-line-comment node offset ";"))

(when sdml-ts-mode--debug-mode
  (makunbound 'sdml-ts-mode--fold-rule-set))

(defvar sdml-ts-mode-fold--rule-set
  '((line_comment . sdml-ts-mode--fold-line-comment)
    (module_body . treesit-fold-range-seq)
    (import_statement . sdml-ts-mode--fold-sequence)
    (constraint_environment . treesit-fold-range-seq)
    (constraint_environment . treesit-fold-range-seq)
    (function_body . treesit-fold-range-seq)
    (function_signature . sdml-ts-mode--fold-parameters)
    (metric_function_signature . sdml-ts-mode--fold-parameters)
    (formal_constraint . sdml-ts-mode--fold-formal-constraint-body)
    (atomic_sentence . sdml-ts-mode--fold-parameters)
    (functional_term . sdml-ts-mode--fold-parameters)
    (sequence_of_predicate_values . sdml-ts-mode--fold-sequence)
    (sequence_builder . sdml-ts-mode--fold-restriction)
    (binary . sdml-ts-mode--fold-binary)
    (sequence_of_values . sdml-ts-mode--fold-sequence)
    (value_constructor . sdml-ts-mode--fold-parameters)
    (mapping_value . sdml-ts-mode--fold-parameters)
    (mapping_type . sdml-ts-mode--fold-parameters)
    (from_definition_with . sdml-ts-mode--fold-sequence)
    (from_definition_without . sdml-ts-mode--fold-sequence)
    (annotation_only_body . treesit-fold-range-seq)
    (datatype_type_restrictions . treesit-fold-range-seq)
    (dimension_body . treesit-fold-range-seq)
    (entity_body . treesit-fold-range-seq)
    (source_entity . sdml-ts-mode--fold-sequence)
    (enum_body . treesit-fold-range-seq)
    (event_body . treesit-fold-range-seq)
    (metric_group_body . treesit-fold-range-seq)
    (rdf_def . sdml-ts-mode--fold-sequence)
    (structure_body . treesit-fold-range-seq)
    (type_class_def . sdml-ts-mode--fold-parameters)
    (type_class_body . treesit-fold-range-seq)
    (union_body . treesit-fold-range-seq)))

;; --------------------------------------------------------------------------
;; Tree-Sitter ❱ Code Folding ❱ Setup function
;; --------------------------------------------------------------------------

;;;###autoload
(defun sdml-ts-mode-fold-setup ()
  "Setup `treesit-fold-mode' and add mode hooks."
  (message "Setting up tree-sitter/fold for SDML")
  (when sdml-ts-mode--debug-mode
    (setq treesit-fold-range-alist
          (assq-delete-all 'sdml-ts-mode treesit-fold-range-alist)))

  ;; install rules into treesit-fold
  (add-to-list
   'treesit-fold-range-alist
   `(sdml-ts-mode . ,sdml-ts-mode-fold--rule-set))

  ;; add mode hooks by default
  (when sdml-ts-mode-enable-folding
    (add-hook 'sdml-ts-mode-hook 'treesit-fold-mode)
    (add-hook 'sdml-ts-mode-hook 'treesit-fold-indicators-mode)
    (add-hook 'sdml-ts-mode-hook 'treesit-fold-line-comment-mode)))

(provide 'sdml-ts-mode-fold)

;;; sdml-ts-mode-fold.el ends here
