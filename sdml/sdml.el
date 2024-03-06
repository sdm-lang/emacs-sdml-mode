;;; sdml.el --- SDML Model -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.2")  (dash "2.9.1"))
;; URL: https://github.com/johnstonskj/emacs-sdml-mode
;; Keywords: languages tools

;;; License:

;; Copyright (c) 2023, 2024 Simon Johnston

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

;; Complete description goes here.

;;; Code:

(require 'cl-lib)
(require 'cl-extra)
(require 'dash)

;; --------------------------------------------------------------------------
;; Generics
;; --------------------------------------------------------------------------

(cl-defgeneric has-annotations-p (value)
  "Return t if VALUE has any annotations, else nil.")

(cl-defgeneric is-namespace-p (value)
  "Return t if VALUE is a namespace containing elements, else nil.")

(cl-defgeneric has-namespace-members-p (value)
  "Return t if VALUE has any contained namespace elements, else nil.")

(cl-defgeneric has-body-p (value)
  "Return t if VALUE has a body, else nil.")

(cl-defgeneric print-to-stream (value &optional stream indenter)
  "Print VALUE to the optional STREAM and INDENTER.")

(cl-defstruct (indenter) (depth 0) (spaces 2))

(cl-defun indent (ind)
  "Return a copy of indenter IND with depth+1."
  (make-indenter :depth (1+ (indenter-depth ind))
                 :spaces (indenter-spaces ind)))

(cl-defun outdent (ind)
  "Return a copy of indenter IND with depth-1."
  (make-indenter :depth (1- (indenter-depth ind))
                 :spaces (indenter-spaces ind)))

(cl-defun indentation (ind)
  "Return a string of spaces for the current indenter IND depth."
  (make-string (* (indenter-depth ind) (indenter-spaces ind)) ?\s))

;; --------------------------------------------------------------------------
;; Types  Predicates
;; --------------------------------------------------------------------------

(cl-defmacro cl-null-typep (object type)
  "Return t if OBJECT is either nil or is of TYPE."
  `(or (null ,object)
       (cl-typep ,object ,type)))

(cl-defmacro cl-seq-typep (seq seq-type element-type)
  "Return t if SEQ is of SEQ-TYPE and elements all are of ELEMENT-TYPE."
  `(and (cl-typep ,seq ,seq-type)
        (cl-every (lambda (elt) (cl-typep elt ,element-type)) ,seq)))

(cl-defmacro cl-list-typep (lst element-type)
  "Return t if LST is a list and elements all are of ELEMENT-TYPE."
  `(cl-seq-typep ,lst 'list ,element-type))

(cl-defmacro cl-vector-typep (lst element-type)
  "Return t if LST is a list and elements all are of ELEMENT-TYPE."
  `(cl-seq-typep ,lst 'vector ,element-type))

;; --------------------------------------------------------------------------
;; Types  Identifiers
;; --------------------------------------------------------------------------

(cl-deftype sdml-identifier ()
  (list 'satisfies
        (lambda (value)
          (let ((value-str (cond
                            ((cl-typep value 'symbol) (symbol-name value))
                            ((cl-typep value 'string) value))))
            (when value-str
              (numberp (string-match "^[a-zA-Z]+$" value-str)))))))

(cl-defun sdml-indentifier->string (value)
  "Return the string representation of the identifier in VALUE.
If VALUE is not an identifier, return nil."
  (if (cl-typep value 'sdml-identifier)
      (format "%s" value)
    nil))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-deftype sdml-qualified-identifier ()
  (list 'satisfies
        (lambda (value)
          (let ((ids (split-string value ":")))
    (if (= (length ids) 2)
        (cl-every (lambda (elt) (cl-typep elt 'sdml-identifier)) ids))))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-deftype sdml-identifier-reference ()
  '(or sdml-identifier sdml-qualified-identifier))

;; --------------------------------------------------------------------------
;; Types  Module
;; --------------------------------------------------------------------------

(cl-defstruct (sdml-module)
  ;; module definition
  name base-uri version-info version-uri
  ;; module body
  imports annotations definitions)

(cl-deftype sdml-module ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-module-p object)
               (cl-typep (sdml-module-name object) 'sdml-identifier)
               (cl-null-typep (sdml-module-base-uri object) 'sdml-iri)
               (cl-null-typep (sdml-module-version-info object) 'string)
               (cl-null-typep (sdml-module-version-uri object) 'sdml-iri)
               (cl-typep (sdml-module-imports object) 'sdml-import-statement-list)
               (cl-typep (sdml-module-annotations object) 'sdml-annotation-list)
               (cl-typep (sdml-module-definitions object) 'sdml-definition-list)))))

(cl-defun is-versioned-p (value)
  "Return t if VALUE has a version info strong or version uri, else nil."
  (or (not (null (sdml-module-version-info value)))
      (not (null (sdml-module-version-uri value)))))

(cl-defun has-imports-p (value)
  "Return t if VALUE has any import statements, else nil."
  (not (null (sdml-module-imports value))))

(cl-defmethod has-annotations-p ((value sdml-module))
  "Return t if VALUE has any annotations, else nil."
  (not (null (sdml-module-annotations value))))

(cl-defmethod is-namespace-p ((_value sdml-module))
  "Return t if VALUE is a namespace containing elements, else nil."
  t)

(cl-defmethod has-namespace-members-p ((value sdml-module))
  "Return t if VALUE has any contained namespace elements, else nil."
  (not (null (sdml-module-definitions value))))

(cl-defmethod has-body-p ((value sdml-module))
  "Return t if VALUE has a body, else nil."
  (or (has-imports-p value)
      (has-annotations-p value)
      (has-namespace-members-p value)))

(cl-defmethod print-to-stream ((value sdml-module) &optional stream _indenter)
  "Print the `sdml-module' VALUE to the optional stream STREAM with INDENTER."
  (let ((indenter (make-indenter)))
    (princ (format "module %s " (sdml-module-name value)) stream)
    (when (sdml-module-base-uri value)
      (princ (format "%s " (sdml-module-base-uri value)) stream))
    (when (is-versioned-p value)
      (princ "version ")
      (when (sdml-module-version-info value)
        (princ (format "%s " (sdml-module-version-info value)) stream))
      (when (sdml-module-version-uri value)
        (princ (format "%s " (sdml-module-version-uri value)) stream)))
    (princ "is\n" stream)
    (when (has-body-p value)
      (when (has-imports-p value)
        (terpri stream)
        (-each (sdml-module-imports value)
          (lambda (stmt) (print-import-statement-to-stream stmt stream (indent indenter)))))
      (when (has-annotations-p value)
        (terpri stream)
        (-each (sdml-module-annotations value)
          (lambda (annotation)
            (print-to-stream annotation stream (indent indenter))
            (terpri stream))))
      (when (has-namespace-members-p value)
        (terpri stream)
        (-each (sdml-module-definitions value)
          (lambda (definition)
            (print-to-stream definition stream (indent indenter))
            (terpri stream)))))
    (princ "end\n" stream)))

;; --------------------------------------------------------------------------
;; Types  Module  Imports
;; --------------------------------------------------------------------------

(cl-deftype sdml-import-statement ()
  (or
   'sdml-identifier-reference
   (and 'list
        (list 'satisfies
              (lambda (lst)
                (cl-every (lambda (elt) (cl-typep elt 'sdml-identifier-reference))
                          lst))))))

(cl-deftype sdml-import-statement-list ()
  (and 'list
       (list 'satisfies
             (lambda (lst)
               (cl-every (lambda (elt) (cl-typep elt 'sdml-import-statement))
                         lst)))))

(cl-defun print-import-statement-to-stream (value &optional stream indenter)
  "Print the import statement VALUE to the optional stream STREAM with INDENTER."
  (cond ((cl-typep value 'list)
         (princ
          (format "%simport [ %s ]\n"
                  (indentation indenter)
                  (string-join (-sort #'string< value) " "))
          stream))
        ((cl-typep value 'sdml-identifier-reference)
         (princ
          (format "%simport %s\n" (indentation indenter) value)
          stream))))

;; --------------------------------------------------------------------------
;; Types  Module  Definitions
;; --------------------------------------------------------------------------

(cl-deftype sdml-definition ()
  '(or sdml-datatype
       sdml-entity
       sdml-enum
       sdml-event
       sdml-property
       sdml-structure
       sdml-union))

(cl-deftype sdml-definition-list ()
  (and 'list
       (list 'satisfies
             (lambda (lst)
               (cl-every (lambda (elt) (cl-typep elt 'sdml-definition))
                         lst)))))

;; --------------------------------------------------------------------------
;; Types  Module  Definitions  Datatype
;; --------------------------------------------------------------------------

(cl-defstruct (sdml-datatype)
  name base-type annotations)

(cl-deftype sdml-datatype ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-datatype-p object)
               (cl-typep (sdml-datatype-name object) 'sdml-identifier)
               (cl-typep (sdml-datatype-base-type object) 'sdml-identifier-reference)
               (cl-typep (sdml-datatype-annotations object) 'sdml-annotation-list)))))

(cl-defmethod has-annotations-p ((value sdml-datatype))
  "Return t if VALUE has any annotations, else nil."
  (not (null (sdml-datatype-annotations value))))

(cl-defmethod is-namespace-p ((_value sdml-datatype))
  "Return t if VALUE is a namespace containing elements, else nil."
  nil)

(cl-defmethod has-body-p ((value sdml-datatype))
  "Return t if VALUE has a body, else nil."
  (has-annotations-p value))

(cl-defmethod print-to-stream ((value sdml-datatype) &optional stream indenter)
  "Print the `sdml-datatype' VALUE to STREAM with INDENTER."
  (princ
   (format "%sdatatype %s <- %s"
           (indentation indenter)
           (sdml-datatype-name value)
           (sdml-datatype-base-type value))
   stream)
  (when (has-body-p value)
    (princ " is\n")
    (-each (sdml-datatype-annotations value)
      (lambda (annotation)
        (print-to-stream annotation stream (indent indenter))
        (terpri stream)))
    (princ (format "%send\n" (indentation indenter)))))

;; --------------------------------------------------------------------------
;; Types  Module  Definitions  Entity
;; --------------------------------------------------------------------------

(cl-defstruct (sdml-entity)
  name identity-member annotations members)

(cl-deftype sdml-entity ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-entity-p object)
               (cl-typep (sdml-entity-name object) 'sdml-identifier)
               (cl-typep (sdml-entity-identity-member object) 'sdml-member)
               (cl-typep (sdml-entity-annotations object) 'sdml-annotation-list)
               (cl-typep (sdml-entity-members object) 'sdml-member-list)))))

(cl-defmethod has-annotations-p ((value sdml-entity))
  "Return t if VALUE has any annotations, else nil."
  (not (null (sdml-entity-annotations value))))

(cl-defmethod is-namespace-p ((_value sdml-entity))
  "Return t if VALUE is a namespace containing elements, else nil."
  t)

(cl-defmethod has-namespace-members-p ((value sdml-entity))
  "Return t if VALUE has any contained namespace elements, else nil."
  (or (not (null (sdml-entity-identity-member value)))
      (not (null (sdml-entity-members value)))))

(cl-defmethod has-body ((value sdml-entity))
  "Return t if VALUE has a body, else nil."
  (or (has-annotations-p value)
      (has-namespace-members-p value)))

(cl-defmethod print-to-stream ((value sdml-entity) &optional stream indenter)
  "Print the `sdml-entity' VALUE to STREAM with INDENTER."
  (princ (format "%sentity %s" (indentation indenter) (sdml-entity-name value)) stream)
  (when (has-body-p value)
    (princ " is\n")
    (when (has-annotations-p value)
      (terpri stream)
      (-each (sdml-entity-annotations value)
        (lambda (annotation)
          (print-to-stream annotation stream (indent indenter))
          (terpri stream))))
    (princ "  identity ")
    (print-to-stream (sdml-entity-identity-member value) stream)
    (when (has-namespace-members-p value)
      (terpri stream)
      (-each (sdml-entity-members value)
      (lambda (member)
        (print-to-stream member stream (indent indenter)))))
    (princ "%send\n" (indentation indenter))))

;; --------------------------------------------------------------------------
;; Types  Module  Definitions  Enum
;; --------------------------------------------------------------------------

(cl-defstruct (sdml-enum)
  name annotations variants)

(cl-deftype sdml-enum ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-enum-p object)
               (cl-typep (sdml-enum-name object) 'sdml-identifier)
               (cl-null-typep (sdml-enum-annotations object) 'sdml-annotation-list)
               (cl-null-typep (sdml-enum-variants object) 'sdml-value-variant-list)))))

(cl-defmethod has-annotations-p ((value sdml-enum))
  "Return t if VALUE has any annotations, else nil."
  (not (null (sdml-enum-annotations value))))

(cl-defmethod is-namespace-p ((_value sdml-enum))
  "Return t if VALUE is a namespace containing elements, else nil."
  t)

(cl-defmethod has-namespace-members-p ((value sdml-enum))
  "Return t if VALUE has any contained namespace elements, else nil."
  (not (null (sdml-enum-variants value))))

(cl-defmethod has-body-p ((value sdml-enum))
  "Return t if VALUE has a body, else nil."
  (or (has-annotations-p value)
      (has-namespace-members-p value)))

(cl-defmethod print-to-stream ((value sdml-enum) &optional stream indenter)
  "Print the `sdml-enum' VALUE to STREAM with INDENTER."
  (princ
   (format "%senum %s"
           (indentation indenter)
           (sdml-enum-name value))
   stream)
  (when (has-body-p value)
    (princ " is\n")
    (when (has-annotations-p value)
      (terpri stream)
      (-each (sdml-enum-annotations value)
        (lambda (annotation)
          (print-to-stream annotation stream (indent indenter))
          (terpri stream))))
    (when (has-namespace-members-p value)
      (terpri stream)
      (-each (sdml-enum-variants value)
      (lambda (variant)
        (print-to-stream variant stream (indent indenter)))))
    (princ "%send\n" (indentation indenter))))

;; --------------------------------------------------------------------------
;; Types  Module  Definitions  Event
;; --------------------------------------------------------------------------

(cl-defstruct (sdml-event)
 name source annotations members)

(cl-deftype sdml-event ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-event-p object)
               (cl-typep (sdml-event-name object) 'sdml-identifier)
               (cl-typep (sdml-event-source object) 'sdml-identifier-reference)
               (cl-typep (sdml-event-annotations object) 'sdml-annotation-list)
               (cl-typep (sdml-event-members object) 'sdml-member-list)))))

(cl-defmethod has-annotations-p ((value sdml-event))
  "Return t if VALUE has any annotations, else nil."
  (not (null (sdml-event-annotations value))))

(cl-defmethod is-namespace-p ((_value sdml-event))
  "Return t if VALUE is a namespace containing elements, else nil."
  t)

(cl-defmethod has-namespace-members-p ((value sdml-event))
  "Return t if VALUE has any contained namespace elements, else nil."
  (not (null (sdml-event-members value))))

(cl-defmethod has-body-p ((value sdml-event))
  "Return t if VALUE has a body, else nil."
  (or (has-annotations-p value)
      (has-namespace-members-p value)))

(cl-defmethod print-to-stream ((value sdml-event) &optional stream indenter)
  "Print the `sdml-event' VALUE to STREAM with INDENTER."
  (princ
   (format "%sevent %s source %s"
           (indentation indenter)
           (sdml-event-name value)
           (sdml-event-source value))
   stream)
  (when (has-body-p value)
    (princ " is\n")
    (when (has-annotations-p value)
      (terpri stream)
      (-each (sdml-event-annotations value)
        (lambda (annotation)
          (print-to-stream annotation stream (indent indenter))
          (terpri stream))))
    (when (has-namespace-members-p value)
      (terpri stream)
      (-each (sdml-event-members value)
      (lambda (member)
        (print-to-stream member stream (indent indenter)))))
    (princ (format "%send\n" (indentation indenter)))))

;; --------------------------------------------------------------------------
;; Types  Module  Definitions  Property
;; --------------------------------------------------------------------------

(cl-defstruct (sdml-property)
  member)

(cl-deftype sdml-property ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-property-p object)
               (cl-typep (sdml-property-member object) 'sdml-member)))))

(cl-defmethod has-annotations-p ((value sdml-property))
  "Return t if VALUE has any annotations, else nil."
  (has-annotations-p (sdml-property-member value)))

(cl-defmethod is-namespace-p ((_value sdml-property))
  "Return t if VALUE is a namespace containing elements, else nil."
  nil)

(cl-defmethod has-body-p ((value sdml-property))
  "Return t if VALUE has a body, else nil."
  (has-annotations-p value))

(cl-defmethod print-to-stream ((value sdml-property) &optional stream indenter)
  "Print the `sdml-property' VALUE to STREAM with INDENTER."
  (princ (format "%sproperty " (indentation indenter)) stream)
  (print-to-stream (sdml-property-member value) stream indenter)
  (terpri stream))

;; --------------------------------------------------------------------------
;; Types  Module  Definitions  Structure
;; --------------------------------------------------------------------------

(cl-defstruct (sdml-structure)
  name annotations members)

(cl-deftype sdml-structure ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-structure-p object)
               (cl-typep (sdml-structure-name object) 'sdml-identifier)
               (cl-typep (sdml-structure-annotations object) 'sdml-annotation-list)
               (cl-typep (sdml-structure-members object) 'sdml-member-list)))))

(cl-defmethod has-annotations-p ((value sdml-structure))
  "Return t if VALUE has any annotations, else nil."
  (not (null (sdml-structure-annotations value))))

(cl-defmethod is-namespace-p ((_value sdml-structure))
  "Return t if VALUE is a namespace containing elements, else nil."
  t)

(cl-defmethod has-namespace-members-p ((value sdml-structure))
  "Return t if VALUE has any contained namespace elements, else nil."
  (not (null (sdml-structure-members value))))

(cl-defmethod has-body-p ((value sdml-structure))
  "Return t if VALUE has a body, else nil."
  (or (has-annotations-p value)
      (has-namespace-members-p value)))

(cl-defmethod print-to-stream ((value sdml-structure) &optional stream indenter)
  "Print the `sdml-structure' VALUE to STREAM with INDENTER."
  (princ
   (format "%sstructure %s"
           (indentation indenter)
           (sdml-structure-name value))
   stream)
  (when (has-body-p value)
    (princ " is\n")
    (when (has-annotations-p value)
      (terpri stream)
      (-each (sdml-structure-annotations value)
        (lambda (annotation)
          (print-to-stream annotation stream (indent indenter))
          (terpri stream))))
    (when (has-namespace-members-p value)
      (terpri stream)
      (-each (sdml-structure-members value)
      (lambda (member)
        (print-to-stream member stream (indent indenter)))))
    (princ (format "%send\n" (indentation indenter)))))

;; --------------------------------------------------------------------------
;; Types  Module  Definitions  Union
;; --------------------------------------------------------------------------

(cl-defstruct (sdml-union)
  name annotations variants)

(cl-deftype sdml-union ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-union-p object)
               (cl-typep (sdml-union-name object) 'sdml-identifier)
               (cl-null-typep (sdml-union-annotations object) 'sdml-annotation-list)
               (cl-null-typep (sdml-union-variants object) 'sdml-type-variant-list)))))

(cl-defmethod has-annotations-p ((value sdml-union))
  "Return t if VALUE has any annotations, else nil."
  (not (null (sdml-union-annotations value))))

(cl-defmethod is-namespace-p ((_value sdml-union))
  "Return t if VALUE is a namespace containing elements, else nil."
  t)

(cl-defmethod has-namespace-members-p ((value sdml-union))
  "Return t if VALUE has any contained namespace elements, else nil."
  (not (null (sdml-union-variants value))))

(cl-defmethod has-body-p ((value sdml-union))
  "Return t if VALUE has a body, else nil."
  (or (has-annotations-p value)
      (has-namespace-members-p value)))

(cl-defmethod print-to-stream ((value sdml-union) &optional stream indenter)
  "Print the `sdml-union' VALUE to STREAM with INDENTER."
  (princ
   (format "%sunion %s"
           (indentation indenter)
           (sdml-union-name value))
   stream)
  (when (has-body-p value)
    (princ " is\n")
    (when (has-annotations-p value)
      (terpri stream)
      (-each (sdml-union-annotations value)
        (lambda (annotation)
          (print-to-stream annotation stream (indent indenter)))))
    (when (has-namespace-members-p value)
      (terpri stream)
      (-each (sdml-union-variants value)
      (lambda (variant)
        (print-to-stream variant stream (indent indenter)))))
    (princ (format "%send\n" (indentation indenter)))))

;; --------------------------------------------------------------------------
;; Types  Structure Members
;; --------------------------------------------------------------------------

(cl-defstruct (sdml-member)
  name cardinality type annotations)

(cl-deftype sdml-member ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-member-p object)
               (cl-typep (sdml-member-name object) 'sdml-identifier)
               (cl-null-typep (sdml-member-cardinality object) 'sdml-cardinality)
               (cl-typep (sdml-member-type object) 'sdml-identifier-reference)
               (cl-typep (sdml-member-annotations object) 'sdml-annotation-list)))))

(cl-deftype sdml-member-list ()
  (and 'list
       (list 'satisfies
             (lambda (lst)
               (cl-every (lambda (elt) (cl-typep elt 'sdml-member))
                         lst)))))

(cl-defmethod has-annotations-p ((value sdml-member))
  "Return t if VALUE has any annotations, else nil."
  (not (null (sdml-member-annotations value))))

(cl-defmethod is-namespace-p ((_value sdml-member))
  "Return t if VALUE is a namespace containing elements, else nil."
  nil)

(cl-defmethod has-body-p ((value sdml-member))
  "Return t if VALUE has a body, else nil."
  (has-annotations-p value))

(cl-defmethod print-to-stream ((value sdml-member) &optional stream indenter)
  "Print the `sdml-member' VALUE to STREAM with INDENTER."
  (princ (format "%s%s -> " (indentation indenter) (sdml-member-name value)) stream)
  (print-to-stream (sdml-member-cardinality value) stream indenter)
  (princ (format " %s" (sdml-member-type value)) stream)
  (when (has-body-p value)
    (princ " is\n")
    (when (has-annotations-p value)
      (terpri stream)
      (-each (sdml-member-annotations value)
        (lambda (annotation)
          (print-to-stream annotation stream (indent indenter))
          (terpri stream))))
    (princ (format "%send\n" (indentation indenter)))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-defstruct (sdml-cardinality)
  (min-occurs 1) (max-occurs 1) ordering uniqueness)

(cl-deftype sdml-cardinality ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-cardinality-p object)
               (cl-typep (sdml-cardinality-min-occurs object) 'sdml-unsigned)
               (cl-null-typep (sdml-cardinality-max-occurs object) 'sdml-unsigned)
               (cl-null-typep (sdml-cardinality-ordering object) 'sdml-sequence-ordering)
               (cl-null-typep (sdml-cardinality-uniqueness object) 'sdml-sequence-uniqueness)))))

(cl-deftype sdml-sequence-ordering ()
  '(member ordered unordered))

(cl-deftype sdml-sequence-uniqueness ()
  '(member unique nonunique))

(cl-defun is-cardinality-default (value)
  "Return t if VALUE is a cardinality of `{1}'."
  (and (eql (sdml-cardinality-min-occurs value) 1)
       (eql (sdml-cardinality-max-occurs value) 1)
       (null (sdml-cardinality-ordering value))
       (null (sdml-cardinality-uniqueness value))))

(cl-defmethod print-to-stream ((value sdml-cardinality) &optional stream _indenter)
  "Print the `sdml-cardinality' VALUE to STREAM with INDENTER."
  (unless (is-cardinality-default value)
    (let ((ordering  (sdml-cardinality-ordering value))
          (uniqueness (sdml-cardinality-uniqueness value))
          (min-occurs (sdml-cardinality-min-occurs value))
          (max-occurs (sdml-cardinality-max-occurs value)))
      (princ "{" stream)
    (when ordering
      (princ (format "%s " ordering)))
    (when uniqueness
      (princ (format "%s " uniqueness)))
    (princ (format "%s" min-occurs))
    (when (not (eql min-occurs max-occurs))
      (princ ".." stream)
      (if (not (null max-occurs))
          (princ (format "%s" max-occurs))))
    (princ "}" stream))))

;; --------------------------------------------------------------------------
;; Types  Variants  Value Variant
;; --------------------------------------------------------------------------

(cl-defstruct (sdml-value-variant)
  name annotations)

(cl-deftype sdml-value-variant ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-value-variant-p object)
               (cl-typep (sdml-value-variant-name object) 'sdml-identifier)
               (cl-typep (sdml-value-variant-annotations object) 'sdml-annotation-list)))))

(cl-deftype sdml-value-variant-list ()
  (and 'list
       (list 'satisfies
             (lambda (lst)
               (cl-every (lambda (elt) (cl-typep elt 'sdml-value-variant))
                         lst)))))

(cl-defmethod has-annotations-p ((value sdml-value-variant))
  "Return t if VALUE has any annotations, else nil."
  (not (null (sdml-value-variant-annotations value))))

(cl-defmethod is-namespace-p ((_value sdml-value-variant))
  "Return t if VALUE is a namespace containing elements, else nil."
  nil)

(cl-defmethod has-body-p ((value sdml-value-variant))
  "Return t if VALUE has a body, else nil."
  (has-annotations-p value))

(cl-defmethod print-to-stream ((value sdml-value-variant) &optional stream indenter)
  "Print the `sdml-value-variant' VALUE to STREAM with INDENTER."
  (princ (format "%s%s" (indentation indenter) (sdml-value-variant-name value)) stream)
  (when (has-body-p value)
    (princ " is\n")
    (when (has-annotations-p value)
      (terpri stream)
      (-each (sdml-value-variant-annotations value)
        (lambda (annotation)
          (print-to-stream annotation stream (indent indenter))
          (terpri stream))))
    (princ (format "%send\n" (indentation indenter)))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-defstruct (sdml-type-variant)
  name rename-as annotations)

(cl-deftype sdml-type-variant ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-type-variant-p object)
               (cl-typep (sdml-type-variant-name object) 'sdml-identifier-reference)
               (cl-null-typep (sdml-type-variant-rename-as object) 'sdml-identifier)
               (cl-typep (sdml-type-variant-annotations object) 'sdml-annotation-list)))))

(cl-deftype sdml-type-variant-list ()
  (and 'list
       (list 'satisfies
             (lambda (lst)
               (cl-every (lambda (elt) (cl-typep elt 'sdml-type-variant))
                         lst)))))

(cl-defmethod has-annotations-p ((value sdml-type-variant))
  "Return t if VALUE has any annotations, else nil."
  (not (null (sdml-type-variant-annotations value))))

(cl-defmethod is-namespace-p ((_value sdml-type-variant))
  "Return t if VALUE is a namespace containing elements, else nil."
  nil)

(cl-defmethod has-body-p ((value sdml-type-variant))
  "Return t if VALUE has a body, else nil."
  (has-annotations-p value))

(cl-defmethod print-to-stream ((value sdml-type-variant) &optional stream indenter)
  "Print the `sdml-type-variant' VALUE to STREAM with INDENTER."
  (princ (format "%s%s" (indentation indenter) (sdml-type-variant-name value)) stream)
  (when (sdml-type-variant-rename-as value)
    (princ (format " as %s" (sdml-type-variant-rename-as value)) stream))
  (when (has-body-p value)
    (princ " is\n")
    (when (has-annotations-p value)
      (terpri stream)
      (-each (sdml-type-variant-annotations value)
        (lambda (annotation)
          (print-to-stream annotation stream (indent indenter))
          (terpri stream))))
    (princ (format "%send\n" (indentation indenter)))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-deftype sdml-variant ()
  (or 'sdml-value-variant 'sdml-type-variant))

;; --------------------------------------------------------------------------
;; Types  Values
;; --------------------------------------------------------------------------

(cl-deftype sdml-simple-value ()
  (or 'sdml-explicit-boolean 'number 'string 'sdml-iri 'sdml-binary))

(cl-deftype sdml-explicit-boolean ()
  '(member true false))

(cl-deftype sdml-unsigned ()
  (list 'integer 0))

(cl-deftype sdml-byte ()
  (list 'integer 0 128))

(cl-deftype sdml-iri ()
  (and 'string (list 'satisfies (lambda (str) (string-match "^<[^<>]+>$" str)))))

(cl-deftype sdml-binaryp ()
  (and 'vector
       (list 'satisfies (lambda (vec)
                          (cl-every (lambda (elt) (cl-typep elt 'sdml-byte))
                                    vec)))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-deftype sdml-value ()
  (or 'sdml-simple-value
      'sdml-identifier-reference
      'sdml-value-constructor
      'sdml-mapping-value
      'sdml-value-lis))

(cl-deftype sdml-value-list ()
  (and 'list
       (list 'satisfies
             (lambda (lst)
               (cl-every (lambda (elt) (cl-typep elt 'sdml-value))
                         lst)))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-defstruct (sdml-value-constructor)
  name value)

(cl-deftype sdml-value-constructor ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-value-constructor-p object)
               (cl-typep (sdml-value-constructor-name object) 'sdml-identifier-reference)
               (cl-typep (sdml-value-constructor-value object) 'sdml-simple-value)))))

(cl-defmethod print-to-stream ((value sdml-value-constructor) &optional stream)
  "Print the `value-constructor' VALUE to STREAM."
  (princ (sdml-value-constructor-name value) stream)
  (princ "(" stream)
  (princ (sdml-value-constructor-value value) stream)
  (princ ")" stream))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-defstruct (sdml-mapping-value)
  key value)

(cl-deftype sdml-mapping-value ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-mapping-value-p object)
               (cl-typep (sdml-mapping-value-key object) 'sdml-simple-value)
               (cl-typep (sdml-mapping-value-value object) 'sdml-value)))))

(cl-defmethod print-to-stream ((value sdml-mapping-value) &optional stream _indenter)
  "Print the `mapping-value' VALUE to STREAM."
  (princ (format "%s -> %s"
                 (sdml-mapping-value-key value)
                 (sdml-mapping-value-value value))
         stream))

;; --------------------------------------------------------------------------
;; Types  Annotations  Properties
;; --------------------------------------------------------------------------

(cl-defstruct (sdml-annotation-property)
  name value)

(cl-deftype sdml-annotation-property ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-annotation-property-p object)
               (cl-typep (sdml-annotation-property-name object) 'sdml-identifier-reference)
               (cl-typep (sdml-annotation-property-value object) 'sdml-value)))))

(cl-defmethod is-namespace-p ((_value sdml-annotation-property))
  "Return t if VALUE is a namespace containing elements, else nil."
  nil)

(cl-defmethod print-to-stream ((value sdml-annotation-property) &optional stream indenter)
  "Print the `annotation-property' VALUE to the optional STREAM with INDENTER."
  (princ (format "%s@%s = %S"
                 (indentation indenter)
                 (sdml-annotation-property-name value)
                 (sdml-annotation-property-value value))
         stream))

;; --------------------------------------------------------------------------
;; Types  Annotations  Properties
;; --------------------------------------------------------------------------

(cl-defstruct (sdml-annotation-constraint)
  name value)

(cl-deftype sdml-annotation-constraint ()
  (list 'satisfies
        (lambda (object)
          (and (sdml-annotation-constraint-p object)
               (cl-typep (sdml-annotation-constraint-name object) 'sdml-identifier)
               (cl-typep (sdml-annotation-constraint-value object) 'sdml-value)))))

(cl-defmethod is-namespace ((_value annotation-constraint))
  "Return t if VALUE is a namespace containing elements, else nil."
  nil)

(cl-defmethod print-to-stream ((_value annotation-constraint) &optional _stream)
  "Print the `annotation-constraint' VALUE to STREAM."
  nil)

;; --------------------------------------------------------------------------

(cl-deftype sdml-annotation ()
  (or 'sdml-annotation-property 'sdml-annotation-constraint))

(cl-deftype sdml-annotation-list ()
  (and 'list
       (list 'satisfies
             (lambda (lst)
               (cl-every (lambda (elt) (cl-typep elt 'sdml-annotation))
                         lst)))))

(provide 'sdml)

;;; sdml.el ends here
