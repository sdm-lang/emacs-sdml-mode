;;; sdml-model.el --- SDML Model -*- lexical-binding: t; -*-

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
(require 'dash)

(define-error 'invalid-argument "Invalid argument")

(define-error 'invalid-argument-type "Invalid argument type" 'invalid-argument)

;; --------------------------------------------------------------------------
;; Predicates
;; --------------------------------------------------------------------------

(cl-defun optional (predicate value)
  "Return t if the VALUE is nil or matches the PREDICATE, else nil."
  (if (null value)
      t
    (apply predicate (list value))))

(cl-defun required (predicate value)
  "Return t if the VALUE is not nil and matches the PREDICATE, else nil."
  (if (null value)
      nil
    (apply predicate (list value))))

(cl-defun qualified-identifier-p (value)
  "Return t if VALUE is a qualified identifier, else nil."
  (let ((ids (split-string value ":")))
    (if (= (length ids) 2)
        (-all-p #'identifier-p ids))))

(cl-defun identifier-p (value)
  "Return t if VALUE is a valid identifier, else nil."
  (cond
   ((symbolp value)
    (identifier-p (symbol-name value)))
   ((stringp value)
    (numberp (string-match "^[a-zA-Z]+$" value)))
   (t nil)))

(cl-defun indentifier-string (value)
  "Return the string representation of the identifier in VALUE.

If VALUE is not an identifier, return nil."
  (if (identifier-p value)
      (cond
       ((stringp value) value)
       ((symbolp value) (symbol-name value))
       (t (format "%S" value)))
    nil))

(cl-defun identifier-reference-p (value)
  "Return t if VALUE is either an identifier or qualified identifier, else nil."
  (or (identifier-p value) (qualified-identifier-p value)))

(cl-defun iri-p (value)
  "Return t if VALUE is a valid URL, else nil."
  (and (string value) (string-match "^<[^<>]+>$" value)))

(cl-defun non-empty-list-p (value)
  "Return t if VALUE is a non-empty list, else nil."
  (required #'listp value))

(cl-defun non-empty-list-of-p (value predicate)
  "Return t if VALUE is a non-empty list of values matching PREDICATE, else nil."
  (and (non-empty-list-p value)
       (list-of-p value predicate)))

(cl-defun list-of-p (value predicate)
  "Return t if VALUE is a list of values matching PREDICATE, else nil."
  (and (listp value) (-all-p predicate value)))

(cl-defun assert-type (value type-predicate name &optional type-name)
  "If VALUE is not of TYPE-PREDICATE then signal an error.

NAME is used as the identifier of the value being tested.
If the predicate's name is not helpful in describing the
type of VALUE in error messages you can override it with
TYPE-NAME."
  (unless (apply type-predicate (list value))
    (signal 'invalid-argument-type (list name value (or type-name type-predicate)))))

(cl-defun assert-type-if-present (value type-predicate name &optional type-name)
  "If VALUE is not of TYPE-PREDICATE then signal an error.

NAME is used as the identifier of the value being tested.
If the predicate's name is not helpful in describing the
type of VALUE in error messages you can override it with
TYPE-NAME."
  (unless (or (null value) (apply type-predicate (list value)))
    (signal 'invalid-argument-type (list name value (or type-name type-predicate)))))

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

(cl-defgeneric print-to-stream (value &optional stream)
  "Print VALUE to the optional STREAM.")

;; --------------------------------------------------------------------------
;; Module
;; --------------------------------------------------------------------------

(cl-defstruct
    (module (:constructor --create-module))
  (name) (base-uri) (version-info) (version-uri) (body)
  (imports) (annotations) (definitions))

(cl-defun make-module (name &key base-uri version-info version-uri imports annotations definitions)
  "Make a new `module' structure with NAME and optional content.

- BASE-URI :: The URI for this module, definition and member names are
  appended to this.
- VERSION-INFO :: .
- VERSION-URI :: .
- IMPORTS :: .
- ANNOTATIONS :: .
- DEFINITIONS :: ."
  (assert-type name #'identifier-p 'name)
  (assert-type-if-present base-uri #'iri-p 'base-uri)
  (assert-type-if-present version-info #'stringp 'version-info)
  (assert-type-if-present version-uri #'iri-p 'version-uri)
  (assert-type-if-present imports
                          (lambda (lst)
                            (list-of-p lst #'identifier-reference-p))
                          'imports
                          'list-of-identifier-reference-p)
  (assert-type-if-present annotations
                          (lambda (lst)
                            (list-of-p lst #'annotation-p))
                          'annotations
                          'list-of-annotation-p)
  (assert-type-if-present definitions
                          (lambda (lst)
                            (list-of-p lst #'module-definition-p))
                          'definitions
                          'list-of-definition-p)
  (--create-module :name (indentifier-string name)
                   :base-uri base-uri
                   :imports imports
                   :annotations annotations
                   :definitions definitions))

(cl-defun is-versioned-p (value)
  "Return t if VALUE has a version info strong or version uri, else nil."
  (or (not (null (module-version-info value)))
      (not (null (module-version-uri value)))))

(cl-defun has-imports-p (value)
  "Return t if VALUE has any import statements, else nil."
  (not (null (module-imports value))))

(cl-defmethod has-annotations-p ((value module))
  "Return t if VALUE has any annotations, else nil."
  (not (null (module-annotations value))))

(cl-defmethod is-namespace-p ((_value module))
  "Return t if VALUE is a namespace containing elements, else nil."
  t)

(cl-defmethod has-namespace-members-p ((value module))
  "Return t if VALUE has any contained namespace elements, else nil."
  (not (null (module-definitions value))))

(cl-defmethod has-body-p ((value module))
  "Return t if VALUE has a body, else nil."
  (or (has-imports-p value)
      (has-annotations-p value)
      (has-namespace-members-p value)))

(cl-defmethod print-to-stream ((value module) &optional stream)
  "Print the `module' VALUE to the optional stream STREAM."
  (princ "module " stream)
  (princ (concat (module-name value) " ") stream)
  (when (module-base-uri value)
    (princ (concat (module-base-uri value) " ") stream))
  (when (is-versioned-p value)
    (princ "version ")
    (when (module-version-info value)
      (princ (concat (module-version-info value) " ") stream))
    (when (module-version-uri value)
      (princ (concat (module-version-uri value) " ") stream)))
  (princ "is" stream)
  (terpri stream)
  (when (has-body-p value)
    (when (has-imports-p value)
      (terpri stream)
      (princ "  import [" stream)
      (terpri stream)
      (-each (-sort #'string< (module-imports value))
        (lambda (import)
          (princ "    " stream)
          (princ import stream)
          (terpri stream)))
      (princ "  ]" stream)
      (terpri stream))
    (when (has-annotations-p value)
      (terpri stream)
      (-each (module-annotations value)
        (lambda (annotation)
          (princ "  " stream)
          (print-to-stream annotation stream)
          (terpri stream))))
    (when (has-namespace-members-p value)
      (terpri stream)
      (-each (module-definitions value)
        (lambda (definition)
          (print-to-stream definition stream)
          (terpri stream)))))
  (princ "end" stream)
  (terpri stream))

;; --------------------------------------------------------------------------
;; Module Definitions
;; --------------------------------------------------------------------------

(cl-defun module-definition-p (value)
  "Return t if VALUE is a top-level module definition, else nil."
  (or (datatype-p value)
      (entity-p value)
      (enum-p value)
      (event-p value)
      (property-p value)
      (structure-p value)
      (union-p value)))

;; --------------------------------------------------------------------------

(cl-defstruct
    (datatype (:constructor --create-datatype))
  name base-type annotations)

(cl-defun make-datatype (name base-type &key annotations)
  "Return a new `datatype' structure with NAME and BASE-TYPE.

Additionally a list of ANNOTATIONS may be provided."
  (assert-type name #'identifier-p name)
  (assert-type base-type #'identifier-reference-p 'base-type)
  (assert-type-if-present annotations
                          (lambda (lst)
                            (list-of-p lst #'annotation-p))
                          'annotations
                          'list-of-annotation-p)
  (--create-datatype :name name :base-type base-type :annotations annotations))

(cl-defmethod has-annotations-p ((value datatype))
  "Return t if VALUE has any annotations, else nil."
  (not (null (datatype-annotations value))))

(cl-defmethod is-namespace-p ((_value datatype))
  "Return t if VALUE is a namespace containing elements, else nil."
  nil)

(cl-defmethod has-body-p ((value datatype))
  "Return t if VALUE has a body, else nil."
  (has-annotations-p value))

(cl-defmethod print-to-stream ((value datatype) &optional stream)
  "Print the `datatype' VALUE to STREAM."
  (princ "  datatype " stream)
  (princ (datatype-name value) stream)
  (princ " <- " stream)
  (princ (datatype-base-type value) stream)
  (when (has-body-p value)
    (princ " is")
    (terpri stream)
    (-each (datatype-annotations value)
      (lambda (annotation)
        (princ "    " stream)
        (print-to-stream annotation stream)
        (terpri stream)))
    (princ "  end"))
    (terpri stream))

;; --------------------------------------------------------------------------

(cl-defstruct
    (entity (:constructor --create-entity))
  name identity annotations members)

(cl-defmethod has-annotations-p ((value entity))
  "Return t if VALUE has any annotations, else nil."
  (not (null (datatype-annotations value))))

(cl-defmethod is-namespace-p ((_value entity))
  "Return t if VALUE is a namespace containing elements, else nil."
  t)

(cl-defmethod has-namespace-members-p ((value entity))
  "Return t if VALUE has any contained namespace elements, else nil."
  (or (not (null (entity-identity value)))
      (not (null (entity-members value)))))

(cl-defmethod has-body ((value entity))
  "Return t if VALUE has a body, else nil."
  (or (has-annotations-p value)
      (has-namespace-members-p value)))

;; --------------------------------------------------------------------------

(cl-defstruct
    (enum (:constructor --create-enum))
  name annotations variants)

(cl-defun make-enum (name &key annotations variants)
  "Return a new `enum' structure with NAME.

Additionally a list of ANNOTATIONS and VARIANTS may be provided."
  (assert-type name #'identifier-p 'name)
  (assert-type-if-present annotations
                          (lambda (lst)
                            (list-of-p lst #'annotation-p))
                          'annotations
                          'list-of-annotation-p)
  ;; TODO: allow simply identifiers also
  (assert-type-if-present variants
                          (lambda (lst)
                            (list-of-p lst #'variant-p))
                          'variants
                          'list-of-variant-p)
  (--create-enum :name name :annotations annotations :variants variants))

(cl-defmethod has-annotations-p ((value enum))
  "Return t if VALUE has any annotations, else nil."
  (not (null (enum-annotations value))))

(cl-defmethod is-namespace-p ((_value enum))
  "Return t if VALUE is a namespace containing elements, else nil."
  t)

(cl-defmethod has-namespace-members-p ((value enum))
  "Return t if VALUE has any contained namespace elements, else nil."
  (not (null (enum-variants value))))

(cl-defmethod has-body-p ((value enum))
  "Return t if VALUE has a body, else nil."
  (or (has-annotations-p value)
      (has-namespace-members-p value)))

;; --------------------------------------------------------------------------

(cl-defstruct
    (event (:constructor --create-event))
 name source annotations members)

(cl-defmethod has-annotations-p ((value event))
  "Return t if VALUE has any annotations, else nil."
  (not (null (event-annotations value))))

(cl-defmethod is-namespace-p ((_value event))
  "Return t if VALUE is a namespace containing elements, else nil."
  t)

(cl-defmethod has-namespace-members-p ((value event))
  "Return t if VALUE has any contained namespace elements, else nil."
  (not (null (event-members value))))

(cl-defmethod has-body-p ((value event))
  "Return t if VALUE has a body, else nil."
  (or (has-annotations-p value)
      (has-namespace-members-p value)))

;; --------------------------------------------------------------------------

(cl-defstruct
    (property (:constructor --create-property))
  member)

;; --------------------------------------------------------------------------

(cl-defstruct
    (structure (:constructor --create-structure))
  name annotations members)

(cl-defmethod has-annotations-p ((value structure))
  "Return t if VALUE has any annotations, else nil."
  (not (null (structure-annotations value))))

(cl-defmethod is-namespace-p ((_value structure))
  "Return t if VALUE is a namespace containing elements, else nil."
  t)

(cl-defmethod has-namespace-members-p ((value structure))
  "Return t if VALUE has any contained namespace elements, else nil."
  (not (null (structure-members value))))

(cl-defmethod has-body-p ((value structure))
  "Return t if VALUE has a body, else nil."
  (or (has-annotations-p value)
      (has-namespace-members-p value)))

;; --------------------------------------------------------------------------

(cl-defstruct
    (union (:constructor --create-union))
  name annotations variants)

(cl-defmethod has-annotations-p ((value union))
  "Return t if VALUE has any annotations, else nil."
  (not (null (union-annotations value))))

(cl-defmethod is-namespace-p ((_value union))
  "Return t if VALUE is a namespace containing elements, else nil."
  t)

(cl-defmethod has-namespace-members-p ((value union))
  "Return t if VALUE has any contained namespace elements, else nil."
  (not (null (union-variants value))))

(cl-defmethod has-body-p ((value union))
  "Return t if VALUE has a body, else nil."
  (or (has-annotations-p value)
      (has-namespace-members-p value)))


;; --------------------------------------------------------------------------
;; Members
;; --------------------------------------------------------------------------

(cl-defstruct
    (member (:constructor --create-member))
  name cardinality type annotations)

(cl-defmethod has-annotations-p ((value member))
  "Return t if VALUE has any annotations, else nil."
  (not (null (type-variant-annotations value))))

(cl-defmethod is-namespace-p ((_value member))
  "Return t if VALUE is a namespace containing elements, else nil."
  nil)

(cl-defmethod has-body-p ((value member))
  "Return t if VALUE has a body, else nil."
  (has-annotations-p value))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-defstruct
    (cardinality (:constructor --create-cardinality))
  ordering uniqueness min-occurs max-occurs)

(cl-defun ordering-p (value)
  "Return t if this VALUE is a valid ordering constraint name, else nil."
  (member value '(ordered unordered)))

(cl-defun uniqueness-p (value)
  "Return t if this VALUE is a valid uniqueness constraint name, else nil."
  (member value '(unique nonunique)))

(cl-defmethod is-namespace-p ((_value cardinality))
  "Return t if VALUE is a namespace containing elements, else nil."
  nil)

;; --------------------------------------------------------------------------
;; Variants
;; --------------------------------------------------------------------------

(cl-defun variant-p (value)
  "Return t if VALUE is a variant, else nil."
  (or (value-variant-p value)
      (type-variant-p value)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-defstruct
    (value-variant (:constructor --create-value-variant))
  name annotations)

(cl-defun make-value-variant (name &key annotations)
  "Return a new `value-variant' structure with NAME.

Additionally a list of ANNOTATIONS may be provided."
  (assert-type name #'identifier-p 'name)
  (assert-type-if-present annotations
                          (lambda (lst)
                            (list-of-p lst #'annotation-p))
                          'annotations
                          'list-of-annotation-p)
  (--create-value-variant :name name :annotations annotations))

(cl-defmethod has-annotations-p ((value value-variant))
  "Return t if VALUE has any annotations, else nil."
  (not (null (value-variant-annotations value))))

(cl-defmethod is-namespace-p ((_value value-variant))
  "Return t if VALUE is a namespace containing elements, else nil."
  nil)

(cl-defmethod has-body-p ((value value-variant))
  "Return t if VALUE has a body, else nil."
  (has-annotations-p value))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-defstruct
    (type-variant (:constructor --create-type-variant))
  name rename-as annotations)

(cl-defun make-type-variant (name &key rename-as annotations)
  "Return a new `type-variant' structure with NAME.

Additionally a provided RENAME-AS identifier and list of ANNOTATIONS may
be provided."
  (assert-type name #'identifier-p 'name)
  (assert-type rename-as #'identifier-reference-p 'rename-as)
  (assert-type-if-present annotations
                          (lambda (lst)
                            (list-of-p lst #'annotation-p))
                          'annotations
                          'list-of-annotation-p)
  (--create-type-variant :name name :annotations annotations))

(cl-defmethod has-annotations-p ((value type-variant))
  "Return t if VALUE has any annotations, else nil."
  (not (null (type-variant-annotations value))))

(cl-defmethod is-namespace-p ((_value type-variant))
  "Return t if VALUE is a namespace containing elements, else nil."
  nil)

(cl-defmethod has-body-p ((value type-variant))
  "Return t if VALUE has a body, else nil."
  (has-annotations-p value))

;; --------------------------------------------------------------------------
;; Values
;; --------------------------------------------------------------------------

(cl-defun value-p (value)
  "Return t if VALUE is an annotation, else nil."
  (or (simple-value-p value)
      (value-constructor-p value)
      (identifier-reference-p value)
      (mapping-value-p value)
      (list-of-p value #'value-p)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-defstruct
    (value-constructor (:constructor --create-value-constructor))
  name value)

(cl-defun make-value-constructor (name value)
  "Return a new `value-constructor' structure with NAME and VALUE."
  (assert-type name #'identifier-reference-p name)
  (assert-type value #'simple-value-p value)
  (--create-value-constructor :name name :value value))

(cl-defmethod print-to-stream ((value value-constructor) &optional stream)
  "Print the `value-constructor' VALUE to STREAM."
  (princ (value-constructor-name value) stream)
  (princ "(" stream)
  (princ (value-constructor-value value) stream)
  (princ ")" stream))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-defstruct
    (mapping-value (:constructor --create-mapping-value))
  key value)

(cl-defun make-mapping-value (key value)
  "Return a new `mapping-value' structure with KEY mapping to VALUE."
  (assert-type key #'simple-value-p key)
  (assert-type value #'value-p value)
  (--create-mapping-value :key key :value value))

(cl-defmethod print-to-stream ((value mapping-value) &optional stream)
  "Print the `mapping-value' VALUE to STREAM."
  (princ (mapping-value-key value) stream)
  (princ " -> " stream)
  (princ (mapping-value-value value) stream))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-defun simple-value-p (value)
  "Return t if VALUE is a `simple-value', else nil."
  (or (explicit-boolean-p value)
      (numberp value)
      (stringp value)
      (iri-p value)
      (binaryp value)))

(cl-defun explicit-boolean-p (value)
  "Return t if VALUE is a `explicit-boolean', else nil.

An explicit Boolean is either the symbol `true' or the symbol `false'."
  (member value '(true false)))

(cl-defun bytep (value)
  "Return t if VALUE is a `byte', else nil."
  (and (unsignedp value)
       (<= value 255)))

(cl-defun binaryp (value)
  "Return t if VALUE is a `binary' (vector of bytes), else nil."
  (and (vectorp value)
       (list-of-p value #'bytep)))

(cl-defun unsignedp (value)
  "Return t if VALUE is a `unsigned' integer, else nil."
  (and (integerp value)
       (>= value 0)))

;; --------------------------------------------------------------------------
;; Annotations
;; --------------------------------------------------------------------------

(cl-defun annotation-p (value)
  "Return t if VALUE is an annotation, else nil."
  (or (annotation-property-p value)
      (annotation-constraint-p value)))

;; --------------------------------------------------------------------------

(cl-defstruct
    (annotation-property (:constructor --create-annotation-property))
  name value)

(cl-defun make-annotation-property (name value)
  "Return a new `annotation-property' structure with NAME and VALUE."
  (assert-type name #'identifier-reference-p name)
  (assert-type value #'value-p value)
  (--create-annotation-property :name name :value value))

(cl-defmethod is-namespace-p ((_value annotation-property))
  "Return t if VALUE is a namespace containing elements, else nil."
  nil)

(cl-defmethod print-to-stream ((value annotation-property) &optional stream)
  "Print the `annotation-property' VALUE to the optional stream STREAM."
  (unless (annotation-property-p value)
    (signal 'invalid-argument-type (list 'value value)))
  (princ "@" stream)
  (princ (annotation-property-name value) stream)
  (princ " = " stream)
  (prin1 (annotation-property-value value) stream))

;; --------------------------------------------------------------------------

(cl-defstruct
    (annotation-constraint
     (:constructor --create-annotation-constraint))
  name value)

(cl-defmethod is-namespace ((_value annotation-constraint))
  "Return t if VALUE is a namespace containing elements, else nil."
  nil)

(cl-defmethod print-to-stream ((_value annotation-constraint) &optional _stream)
  "Print the `annotation-constraint' VALUE to STREAM."
  nil)


(provide 'sdml-model)

;;; sdml-model.el ends here
