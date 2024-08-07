;;; sdml-mode-abbrev.el --- Abbreviation Support -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>

;;; License:

;; Copyright (c) 2023, 2024 Simon Johnston
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

;; Internal module.

;;; Code:

;; --------------------------------------------------------------------------
;; Abbreviations Defined
;; --------------------------------------------------------------------------

(define-skeleton sdml-mode-abbrev--new-module
  "New module."
  "Module name: "
  > "module " str | "new_module"
  > " base <https://example.org/vocabulary/" str | "new_module" "> is" \n
  > "" \n
  > "  import [ dc skos rdfs xsd ]" \n
  > "" \n
  > "  @skos:prefLabel = \"" str | "new_module" "\"@" locale-language \n
  > "  @dc:version = xsd:integer(1)" \n
  > "" \n
  > "  " _ \n
  > "" \n
  > "end" \n)

(define-skeleton sdml-mode-abbrev--new-datatype
  "New datatype."
  "Datatype name: "
  > "datatype " str | "NewDatatype" " ← opaque " _ \n)

(define-skeleton sdml-mode-abbrev--new-entity
  "New entity."
  "Entity name: "
  > "entity " str | "NewEntity" " is" \n
  > "  identity id → unknown" \n
  > "  " _ \n
  > "end" \n)

(define-skeleton sdml-mode-abbrev--new-structure
  "New structure."
  "Structure name: "
  > "structure " str | "NewStructure" " is" \n
  > "  name → Type" \n
  > "  " _ \n
  > "end" \n)

(define-skeleton sdml-mode-abbrev--new-property
  "New property."
  "Property name: "
  > "property " str | "NewProperty" " is" \n
  > "  role_name → Type" \n
  > "  " _ \n
  > "end" \n)

(define-skeleton sdml-mode-abbrev--new-event
  "New event."
  "Event name: "
  > "event " str | "NewEvent" " source " _ " is" \n
  > "  name → Type" \n
  > "" \n
  > "end" \n)

(define-skeleton sdml-mode-abbrev--new-enum
  "New enum."
  "Enum name: "
  > "enum " str | "NewEnum" " of" \n
  > "  ValueOne" \n
  > "  " _ \n
  > "end" \n)

(define-skeleton sdml-mode-abbrev--new-union
  "New discriminated union."
  "Union name: "
  > "union " str | "NewUnion" " of" \n
  > "  TypeOne as One" \n
  > "  " _ \n
  > "end" \n)

(define-skeleton sdml-mode-abbrev--new-constraint
  "New informal constraint."
  "Constraint name: "
  > "assert " str | "invariant" " = \"" _ "\"@" locale-language \n)

(define-skeleton sdml-mode-abbrev--new-formal-constraint
  "New formal constraint."
  "Constraint name: "
  > "assert " str | "invariant" " is" \n
  > "  ∀ self, " _ \n
  > "end" \n)

;; --------------------------------------------------------------------------

(define-skeleton sdml-mode-abbrev--new-constraint-def
  "New formal constraint definition."
  "Definition name: "
  > "def " str | "defn" "() ≔ " _ \n)

(define-skeleton sdml-mode-abbrev--new-constraint-forall
  "Universal quantified sentence."
  "Variable name: "
  > "∀ " str | "self" ", " _ \n)

(define-skeleton sdml-mode-abbrev--new-constraint-exists
  "Existential quantified sentence."
  "Variable name: "
  > "∃ " str | "self" ", " _ \n)

;; --------------------------------------------------------------------------

(define-skeleton sdml-mode-abbrev--new-ann-altlabel
  "SKOS alternate label." nil
  > "@skos:altLabel = \"" _ "\"@" locale-language \n)

(define-skeleton sdml-mode-abbrev--new-ann-definition
  "SKOS definition." nil
  > "@skos:definition = \"" _ "\"@" locale-language \n)

(define-skeleton sdml-mode-abbrev--new-ann-editorial
  "SKOS editorial note." nil
  > "@skos:editorialNote = \"" _ "\"@" locale-language \n)

(define-skeleton sdml-mode-abbrev--new-ann-preflabel
  "SKOS preferred label." nil
  > "@skos:prefLabel = \"" _ "\"@" locale-language \n)

(define-skeleton sdml-mode-abbrev--new-ann-comment
  "RDFS comment." nil
  > "@rdfs:comment = \"" _ "\"@" locale-language \n)


;; --------------------------------------------------------------------------
;; Final Abbrev Table
;; --------------------------------------------------------------------------

;; Note you still need a replacement string "" when using
;;      skeletons or the original text isn't removed.
(define-abbrev-table 'sdml-mode-abbrev-table
  '(;; Declaration - Types
    ("mo" "" sdml-mode-abbrev--new-module)
    ("dt" "" sdml-mode-abbrev--new-datatype)
    ("en" "" sdml-mode-abbrev--new-enum)
    ("ev" "" sdml-mode-abbrev--new-event)
    ("pr" "" sdml-mode-abbrev--new-property)
    ("st" "" sdml-mode-abbrev--new-structure)
    ("un" "" sdml-mode-abbrev--new-union)
    ;; Declaration - Members
    ;; Annotation - Constraints
    ("ci" "" sdml-mode-abbrev--new-constraint)
    ("cf" "" sdml-mode-abbrev--new-formal-constraint)
    ("all" "" sdml-mode-abbrev--new-constraint-forall)
    ("any" "" sdml-mode-abbrev--new-constraint-exists)
    ;; Annotation - Properties
    ("pal" "" sdml-mode-abbrev--new-ann-altlabel)
    ("ppl" "" sdml-mode-abbrev--new-ann-preflabel)
    ("pdf" "" sdml-mode-abbrev--new-ann-definition)
    ("ped" "" sdml-mode-abbrev--new-ann-editorial)
    ("pco" "" sdml-mode-abbrev--new-ann-comment)
    ;; data types
    ("uk" "-> unknown")
    ("db" "boolean")
    ("dd" "decimal")
    ("df" "double")
    ("dh" "binary")
    ("di" "integer")
    ("ds" "string")
    ("du" "unsigned")))


(provide 'sdml-mode-abbrev)

;;; sdml-mode-abbrev.el ends here
