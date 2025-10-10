;;; sdml-ts-mode-abbrev.el --- Internal abbrev support -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; License: see sdml-ts-mode.el

;;; Commentary:

;; Internal module to organize all abbreviation related functionality.

;;; Code:

(require 'skeleton)

;; --------------------------------------------------------------------------
;; Abbrev ❱ Skeletons
;; --------------------------------------------------------------------------

(define-skeleton sdml-ts-mode-abbrev-new-module
  "New module."
  "Module name: "
  > "module " str | "new_module"
  > "  <https://example.org/vocabulary/" str | "new_module" "> is" \n
  > "" \n
  > "  import [ dc skos rdfs xsd ]" \n
  > "" \n
  > "  @skos:prefLabel = \"" str | "new_module" "\"@" locale-language \n
  > "  @dc:version = xsd:integer(1)" \n
  > "" \n
  > "  " _ \n
  > "" \n
  > "end" \n)

(define-skeleton sdml-ts-mode-abbrev-new-datatype
  "New datatype."
  "Datatype name: "
  > "datatype " str | "NewDatatype" " ← opaque " _ \n)

(define-skeleton sdml-ts-mode-abbrev-new-entity
  "New entity."
  "Entity name: "
  > "entity " str | "NewEntity" " is" \n
  > "  identity id → unknown" \n
  > "  " _ \n
  > "end" \n)

(define-skeleton sdml-ts-mode-abbrev-new-structure
  "New structure."
  "Structure name: "
  > "structure " str | "NewStructure" " is" \n
  > "  name → Type" \n
  > "  " _ \n
  > "end" \n)

(define-skeleton sdml-ts-mode-abbrev-new-property
  "New property."
  "Property name: "
  > "property " str | "NewProperty" "→ Type" \n)

(define-skeleton sdml-ts-mode-abbrev-new-event
  "New event."
  "Event name: "
  > "event " str | "NewEvent" " source " _ " is" \n
  > "  name → Type" \n
  > "" \n
  > "end" \n)

(define-skeleton sdml-ts-mode-abbrev-new-enum
  "New enum."
  "Enum name: "
  > "enum " str | "NewEnum" " of" \n
  > "  ValueOne" \n
  > "  " _ \n
  > "end" \n)

(define-skeleton sdml-ts-mode-abbrev-new-union
  "New discriminated union."
  "Union name: "
  > "union " str | "NewUnion" " of" \n
  > "  TypeOne as One" \n
  > "  " _ \n
  > "end" \n)

(define-skeleton sdml-ts-mode-abbrev-new-constraint
  "New informal constraint."
  "Constraint name: "
  > "assert " str | "invariant" " = \"" _ "\"@" locale-language \n)

(define-skeleton sdml-ts-mode-abbrev-new-formal-constraint
  "New formal constraint."
  "Constraint name: "
  > "assert " str | "invariant" " is" \n
  > "  ∀ self, " _ \n
  > "end" \n)

;; --------------------------------------------------------------------------

(define-skeleton sdml-ts-mode-abbrev-new-constraint-def
  "New formal constraint definition."
  "Definition name: "
  > "def " str | "defn" "() ≔ " _ \n)

(define-skeleton sdml-ts-mode-abbrev-new-constraint-forall
  "Universal quantified sentence."
  "Variable name: "
  > "∀ " str | "self" ", " _ \n)

(define-skeleton sdml-ts-mode-abbrev-new-constraint-exists
  "Existential quantified sentence."
  "Variable name: "
  > "∃ " str | "self" ", " _ \n)

;; --------------------------------------------------------------------------

(define-skeleton sdml-ts-mode-abbrev-new-ann-altlabel
  "SKOS alternate label." nil
  > "@skos:altLabel = \"" _ "\"@" locale-language \n)

(define-skeleton sdml-ts-mode-abbrev-new-ann-definition
  "SKOS definition." nil
  > "@skos:definition = \"" _ "\"@" locale-language \n)

(define-skeleton sdml-ts-mode-abbrev-new-ann-editorial
  "SKOS editorial note." nil
  > "@skos:editorialNote = \"" _ "\"@" locale-language \n)

(define-skeleton sdml-ts-mode-abbrev-new-ann-preflabel
  "SKOS preferred label." nil
  > "@skos:prefLabel = \"" _ "\"@" locale-language \n)

(define-skeleton sdml-ts-mode-abbrev-new-ann-comment
  "RDFS comment." nil
  > "@rdfs:comment = \"" _ "\"@" locale-language \n)

;; --------------------------------------------------------------------------
;; Abbrev ❱ Table
;; --------------------------------------------------------------------------

(when sdml-ts-mode--debug-mode
  (makunbound 'sdml-ts-mode-abbrev-table))

;; Note you still need a replacement string "" when using
;;      skeletons or the original text isn't removed.
(define-abbrev-table 'sdml-ts-mode-abbrev-table
  '(;; Declaration - Types
    ("mod" "" sdml-ts-mode-abbrev-new-module)
    ("dt" "" sdml-ts-mode-abbrev-new-datatype)
    ("enu" "" sdml-ts-mode-abbrev-new-enum)
    ("ev" "" sdml-ts-mode-abbrev-new-event)
    ("pr" "" sdml-ts-mode-abbrev-new-property)
    ("st" "" sdml-ts-mode-abbrev-new-structure)
    ("un" "" sdml-ts-mode-abbrev-new-union)
    ;; Declaration - Members
    ;; Annotation - Constraints
    ("ci" "" sdml-ts-mode-abbrev-new-constraint)
    ("cf" "" sdml-ts-mode-abbrev-new-formal-constraint)
    ("lall" "" sdml-ts-mode-abbrev-new-constraint-forall)
    ("lany" "" sdml-ts-mode-abbrev-new-constraint-exists)
    ;; Annotation - Properties
    ("pal" "" sdml-ts-mode-abbrev-new-ann-altlabel)
    ("ppl" "" sdml-ts-mode-abbrev-new-ann-preflabel)
    ("pdf" "" sdml-ts-mode-abbrev-new-ann-definition)
    ("ped" "" sdml-ts-mode-abbrev-new-ann-editorial)
    ("pco" "" sdml-ts-mode-abbrev-new-ann-comment)
    ;; data types
    ("dt?" "-> unknown")
    ("dtb" "boolean")
    ("dtd" "decimal")
    ("dtf" "double")
    ("dth" "binary")
    ("dti" "integer")
    ("dts" "string")
    ("dtu" "unsigned")))

(provide 'sdml-ts-mode-abbrev)

;;; sdml-ts-mode-abbrev.el ends here
