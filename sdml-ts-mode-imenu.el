;;; sdml-ts-mode-imenu.el --- Internal imenu support -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; License: see sdml-ts-mode.el

;;; Commentary:

;; Internal module to organize imenu functionality.

;;; Code:

(require 'treesit)

;; --------------------------------------------------------------------------
;; Tree-Sitter ❱ iMenu ❱ Rules
;; --------------------------------------------------------------------------

(when sdml-ts-mode--debug-mode
  ;; maybe: (imenu-flush-cache)
  (makunbound 'sdml-ts-mode-imenu--rules))

(defvar sdml-ts-mode-imenu--rules
  '(("Module" "\\`module_def``'" nil nil)
    ("Datatype" "\\`datatype_def``'" nil nil)
    ("Dimension" "\\`dimension_def``'" nil nil)
    ("Entity" "\\`entity_def``'" nil nil)
    ("Enum" "\\`enum_def``'" nil nil)
    ("Event" "\\`event_def``'" nil nil)
    ("Property" "\\`property_def``'" nil nil)
    ("RDF" "\\`rdf_def``'" nil nil)
    ("Structure" "\\`structure_def``'" nil nil)
    ("Type Class" "\\`type_class_def``'" nil nil)
    ("Union" "\\`union_def``'" nil nil)
    ("Function" "\\`function_signature``'" nil nil)
    ("Constraint" "\\`constraint``'" nil nil)
    ))

;; --------------------------------------------------------------------------
;; Tree-Sitter ❱ iMenu ❱ Setup function
;; --------------------------------------------------------------------------

;;;###autoload
(defun sdml-ts-mode-imenu-setup ()
  "Setup treesit imenu integration."
  (message "Setting up tree-sitter/imenu for SDML")
  (when sdml-ts-mode-imenu--rules

    (setq imenu-case-fold-search nil)

    (setq-local
     treesit-simple-imenu-settings
     sdml-ts-mode-imenu--rules)))

(provide 'sdml-ts-mode-imenu)

;;; sdml-ts-mode-imenu.el ends here
