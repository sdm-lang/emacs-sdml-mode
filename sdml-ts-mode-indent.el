;;; sdml-ts-mode-indent.el --- Internal indent support -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; License: see sdml-ts-mode.el

;;; Commentary:

;; Internal module to organize all indentation related functionality.

;;; Code:

(require 'treesit)

;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(defcustom sdml-ts-mode-indent-offset
  2
  "Number of spaces for each indentation step in `sdml-ts-mode'."
  :tag "Indentation offset"
  :type 'integer
  :safe 'natnump
  :group 'sdml)

;; --------------------------------------------------------------------------
;; Tree-Sitter ❱ Indentation ❱ Rules
;; --------------------------------------------------------------------------

(defconst sdml-ts-mode-indent--definition-nodes
  (rx (or "datatype_def"
          "dimension_def"
          "entity_def"
          "enum_def"
          "event_def"
          "property_def"
          "rdf_def"
          "structure_def"
          "type_class_def"
          "union_def")))

(defconst sdml-ts-mode-indent--definition-body-nodes
  (rx (or "annotation_only_body"
                 "dimension_body"
                 "entity_body"
                 "enum_body"
                 "event_body"
                 "structure_body"
                 "type_class_body"
                 "union_body")))

(when sdml-ts-mode--debug-mode
  (message "Cleaning up sdml-ts-mode--indent-rules")
  (makunbound 'sdml-ts-mode-indent--rules))

(defvar sdml-ts-mode-indent--rules
  (let ((offset sdml-ts-mode-indent-offset)
        (no-offset 0))
    `(;; These are simply line-up rules, for the most part they're simple
      ;; except for weird Emacs regex escaping.


      ;; ((node-is "\\[") parent-bol ,no-offset)
      ;; ((node-is "]") parent-bol ,no-offset)
      ;; ((node-is "\\\\(") parent-bol ,no-offset)
      ;; ((node-is ")") parent-bol ,no-offset)
      ;; ((node-is "{") parent-bol ,no-offset)
      ;; ((and (node-is "}") (parent-is "datatype_def_restriction")) grand-parent ,no-offset)
      ;; ((node-is "}") parent-bol ,no-offset)

      ;; --------------------------------------------------------------------
      ;; These are relatively obvious and universal, except for the
      ;; top-level rule for the "is" opening a module.


      ;; ((and (node-is "is") (parent-is "module_body")) parent-bol ,offset)
      ;; ;;((and (node-is "is") (not (parent-is "module_body"))) grand-parent ,offset)
      ;; ((node-is "end") parent-bol ,no-offset)
      ;; ((node-is "line_comment") parent-bol ,offset)

      ;; --------------------------------------------------------------------
      ;; Rules for all annotation types; formal constraint sentences are
      ;; described separately below.


      ;; ((node-is "annotation") parent-bol ,offset)
      ;; ((parent-is "annotation_property") parent-bol ,offset)
      ;; ((node-is ,(rx (or "formal_constraint" "informal_constraint"))) parent-bol ,offset)
      ;; ((parent-is "informal_constraint") parent-bol ,no-offset)

      ;; --------------------------------------------------------------------
      ;; Modules and bodies.
      ((match "module" "module_def") no-indent 0)
      ((parent-is "module_version") parent-bol ,offset)
      ((parent-is "module") parent-bol ,offset)
      ((node-is "module") column-0 0)

      ((parent-is "datatype_def") parent-bol ,offset)
      ((parent-is "annotation_only_body") parent-bol ,offset)

      ;; ((match ,(rx (or "identifier" "iri" "quoted_string")) "module") parent-bol ,offset)
      ;; ((match ,(rx (or "import_statement")) "module_body") parent-bol ,offset)
      ;; ((match ,(rx (or "member_import" "module_import")) "import_statement") parent-bol ,offset)

      ;; --------------------------------------------------------------------
      ;; Definitions and their top-level properties.
      ;;((node-is ,sdml-ts-mode-indent--definition-nodes) parent-bol ,3)


      ;; ((node-is "definition") parent-bol, no-offset)
      ;; ((node-is "structure_def") parent-bol ,offset)
      ;; ((node-is "structure_body") parent-bol ,no-offset)
      ;; ((node-is "member") parent-bol ,no-offset)
      ;; ((node-is "member_def") parent ,offset)


      ;; ((match "identifier"
      ;;         ,sdml-ts-mode-indent--definition-nodes
      ;;         "name")
      ;;  parent-bol ,3)
      ;; ((node-is "from_definition_clause") parent ,offset)
      ;; ((match ,(rx (or "<-"
      ;;                  "←"
      ;;                  "opaque"
      ;;                  "identifier_reference"
      ;;                  "builtin_simple_type"))
      ;;         "data_type_def")
      ;;  parent-bol ,offset)
      ;; ((parent-is "datatype_def_restriction") parent-bol ,offset)

      ;; --------------------------------------------------------------------
      ;; Definition bodies
      ;;((node-is ,sdml-ts-mode-indent--definition-body-nodes) parent-bol ,5)


      ;; ((match ,(rx (or "source_entity"
      ;;                  "entity_identity"
      ;;                  "dimension_parent"))
      ;;         "dimension_body")
      ;;  parent-bol ,offset)
      ;; ((match "identity" "entity_body") parent-bol ,offset)
      ;; ((match "source_entity" "event_body") grand-parent ,offset)
      ;;
      ;; ((match ,(rx (or "identifier_reference"
      ;;                  "identifier"))
      ;;         "source_entity")
      ;;  grand-parent ,offset)

      ;; --------------------------------------------------------------------
      ;; Definition Members.


      ;; ((match  "type_variant" "union_body") parent-bol ,offset)
      ;; ((match  "value_variant" "enum_body") parent-bol ,offset)
      ;; ((match ,(rx (or "as" "identifier")) "type_variant") parent-bol ,offset)

      ;; --------------------------------------------------------------------
      ;; Formal Constraint Sentences

      ;; --------------------------------------------------------------------
      ;; Values


      ;; ((match "simple_value" "value_constructor") parent-bol ,offset)
      )))

;; --------------------------------------------------------------------------
;; Tree-Sitter ❱ Indentation ❱ Setup function
;; --------------------------------------------------------------------------

;;;###autoload
(defun sdml-ts-mode-indent-setup ()
  "Setup `treesit'-based indentation."

  (when sdml-ts-mode--debug-mode
      (message "%s" sdml-ts-mode-indent--rules)
      (setq-local treesit--indent-verbose t))

  ;; Turn off the old form
  (setq-local indent-tabs-mode nil)

  ;; Turn on tree-sitter form
  (setq-local
   treesit-simple-indent-rules
   `((sdml . ,sdml-ts-mode-indent--rules))))

(provide 'sdml-ts-mode-indent)

;;; sdml-ts-mode-indent.el ends here
