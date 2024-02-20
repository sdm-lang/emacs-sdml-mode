;;; sdml.el --- SDML Model -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Keywords: 
;; Version: 0.0.1

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

;;; Code:

(require 'cl-lib)
(require 'dash)

(define-error 'invalid-argument "Invalid argument")

(define-error 'invalid-argument-type "Invalid argument type" 'invalid-argument)

;; --------------------------------------------------------------------------
;; Predicates
;; --------------------------------------------------------------------------

(defun qualified-identifier-p (value)
  "Returns t if VALUE is a qualified identifier, else nil."
  (let ((ids (string-split value ":")))
    (if (= (length ids) 2)
        (-all-p #'identifier-p ids))))

(defun identifier-p (value)
  "Returns t if VALUE is a valid identifier, else nil."
  (cond
   ((symbolp value)
    (identifier-p (symbol-name value)))
   ((stringp value)
    (string-match "^[a-zA-Z]+$" value))
   (t nil)))

(defun identifier-reference-p (value)
  "Returns t if VALUE is either an identifier or qualified identifier, else nil."
  (or (identifier-p value) (qualified-identifier-p value)))

(defun iri-p (value)
  (and (string value) (string-match "^<[^<>]+>$" value)))

(defun non-empty-list-p (value)
  (and (listp value) (not (null value))))

(defun non-empty-list-of-p (value predicate)
  (and (non-empty-list-p value)
       (list-of-p value predicate)))

(defun list-of-p (value predicate)
  (and (listp value) (-all-p predicate value)))

;; --------------------------------------------------------------------------
;; Module
;; --------------------------------------------------------------------------

(cl-defstruct
    (module
     (:constructor --create-module))
  name base-uri version-info version-uri body)

(cl-defun module (name body &key base-uri version-info version-uri)
  "Make a new `module' structure with NAME, BODY and an optional BASE-URI iri."
  (unless (identifier-p name)
    (signal 'invalid-argument-type (list 'name name 'identifier)))
  (unless (or (not base-uri) (iri-p base-uri))
    (signal 'invalid-argument-type (list 'base-uri base-uri 'iri)))
  (unless (or (not version-info) (stringp version-info))
    (signal 'invalid-argument-type (list 'version-info version-info 'string)))
  (unless (or (not version-uri) (iri-p version-uri))
    (signal 'invalid-argument-type (list 'version-uri version-uri 'iri)))
  (unless (module/body-p body)
    (signal 'invalid-argument-type (list 'body body 'module/body)))
  (--create-module :name name :base-uri base-uri :body body))

(cl-defun print-module (value &optional stream depth)
  "Print the module VALUE to the optional stream STREAM.
   Currently DEPTH is ignored."
  (princ "module " stream)
  (princ (module-name value) stream)
  (when (module-base-uri value)
    (princ (module-base-uri value)) stream)
  (princ " is" stream)
  (terpri stream)
  (print-module/body (module-body value) stream depth)
  (princ "end" stream)
  (terpri stream))

;; --------------------------------------------------------------------------

(cl-defstruct
    (module/body
     (:constructor --create-module/body))
  imports annotations definitions)

(cl-defun make-module/body (&key imports annotations definitions)
  "Make a new module body with lists of IMPORTS, ANNOTATIONS, and DEFINITIONS."
  (unless (list-of-p imports #'identifier-reference-p)
    (signal 'invalid-argument-type (list 'imports imports 'identifier-reference)))
  (unless (list-of-p annotations #'annotation-p)
    (signal 'invalid-argument-type (list 'annotations annotations 'annotation)))
  (unless (list-of-p definitions #'definition-p)
    (signal 'invalid-argument-type (list 'definitions definitions 'definition)))
  (--create-module/body
   :imports imports
   :annotations annotations
   :definitions definitions))

(cl-defun make-module-with-body (name &key base imports annotations definitions)
  "Make a new `module' and body together. Only the module NAME is required, all
   other properties are optional."
  (make-module name
   (make-module/body
    :imports imports
    :annotations annotations
    :definitions definitions)
   :base base))

(defun print-module/body (value &optional stream depth)
  "Print the module body VALUE to the optional stream STREAM.
   Currently DEPTH is ignored."
  (when (module/body-imports value)
    (terpri stream)
    (princ "  import [" stream)
    (terpri stream)
    (-each (-sort #'string< (module/body-imports value))
      (lambda (import)
        (princ "    " stream)
        (princ import stream)
        (terpri stream)))
    (princ "  ]" stream)
    (terpri stream))
  (when (module/body-annotations value)
    (terpri stream)
    (-each (module/body-annotations value)
      (lambda (annotation)
        (princ "  " stream)
        (print-annotation annotation stream depth)
        (terpri stream))))
  (when (module/body-definitions value)
    (terpri stream)
    (-each (module/body-definitions value)
      (lambda (definition)
        (print-definition definition stream depth)
        (terpri stream))))
  )

;; --------------------------------------------------------------------------
;; Annotations
;; --------------------------------------------------------------------------

(defun annotation-p (value)
  (or (annotation-property-p value)
      (annotation-constraint-p value)))

(defun print-annotation (value &optional stream depth)
  "Print the annotation (`property' or `constraint') VALUE to the optional
   stream STREAM. Currently DEPTH is ignored."
  (cond
   ((annotation-property-p value)
    (print-annotation-property value stream depth))
   ((annotation-constraint-p value)
    (print-annotation-constraint value stream depth))))

;; --------------------------------------------------------------------------

(cl-defstruct
    (annotation-property
     (:constructor --create-annotation-property))
  name value)

(cl-defun make-annotation-property (name value)
  (unless (identifier-reference-p  name)
    (signal 'invalid-argument-type (list 'name name 'identifier-reference)))
  (--create-annotation-property :name name :value value))

(defun print-annotation-property (value &optional stream depth)
  "Print the `annotation-property' VALUE to the optional stream STREAM.
   Currently DEPTH is ignored."
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

;; --------------------------------------------------------------------------
;; Definitions
;; --------------------------------------------------------------------------

(defun definition-p (value)
  (or (datatype-p value)
      (entity-p value)
      (enum-p value)
      (event-p value)
      (property-p value)
      (structure-p value)
      (union-p value)))

(defun print-definition (value &optional stream depth)
  (cond
   ((datatype-p value) (print-datatype value stream depth))
   ((entity-p value) (print-entity value stream depth))
   ((enum-p value) (print-enum value stream depth))
   ((event-p value) (print-event value stream depth))
   ((property-p value) (print-property value stream depth))
   ((structure-p value) (print-structure value stream depth))
   ((union-p value) (print-union value stream depth))))

;; --------------------------------------------------------------------------

(cl-defstruct
    (datatype
     (:constructor --create-datatype))
  name base annotations)

(cl-defun make-datatype (name base &key annotations)
  (unless (identifier-p  name)
    (signal 'invalid-argument-type (list 'name name 'identifier)))
  (unless (identifier-reference-p  base)
    (signal 'invalid-argument-type (list 'base base 'identifier-reference)))
  (unless (list-of-p annotations #'annotation-p)
    (signal 'invalid-argument-type (list 'annotations annotations 'annotation)))
  (--create-datatype :name name :base base :annotations annotations))

(defun datatype-has-body-p (value)
  (datatype-annotations value))

(defun print-datatype (value &optional stream depth)
  (princ "  datatype " stream)
  (princ (datatype-name value) stream)
  (princ " <- " stream)
  (princ (datatype-base value) stream)
  (when (datatype-has-body-p value)
    (princ " is")
    (terpri stream)
    (-each (datatype-annotations value)
      (lambda (annotation)
        (princ "    " stream)
        (print-annotation annotation stream depth)
        (terpri stream)))
    (princ "  end"))
    (terpri stream))

;; --------------------------------------------------------------------------

(cl-defstruct
    (entity
     (:constructor --create-entity))
  name identity annotations members groups)

(defun entity-has-body-p (value)
  (or (entity-identity value)
      (entity-annotations value)
      (entity-members value)
      (entity-groups value)))

;; --------------------------------------------------------------------------

(cl-defstruct
    (enum
     (:constructor --create-enum))
  name annotations variants)

(cl-defun make-enum (name &key annotations variants)
  (unless (identifier-p  name)
    (signal 'invalid-argument-type (list 'name name 'identifier)))
  (unless (list-of-p annotations #'annotation-p)
    (signal 'invalid-argument-type (list 'annotations annotations 'annotation)))
  ;; TODO: allow simply identifiers also
  (unless (list-of-p variants #'value-variant-p)
    (signal 'invalid-argument-type (list 'variants variants 'value-variant)))
  (--create-enum :name name :annotations annotations :variants variants))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-defstruct
    (value-variant
     (:constructor --create-type-variant))
  name annotations)

(defun value-variant-has-body-p (value)
  (value-variant-annotations value))

(cl-defun make-value-variant (name &key annotations)
  (unless (identifier-p  name)
    (signal 'invalid-argument-type (list 'name name 'identifier)))
  (unless (list-of-p annotations #'annotation-p)
    (signal 'invalid-argument-type (list 'annotations annotations 'annotation)))
 (--create-value-variant :name name :annotations annotations))

;; --------------------------------------------------------------------------

(cl-defstruct
    (event
     (:constructor create-event))
 name source annotations members groups)

;; --------------------------------------------------------------------------

(cl-defstruct
    (property
     (:constructor create-property))
  name annotations members groups)

;; --------------------------------------------------------------------------

(cl-defstruct
    (structure
     (:constructor create-structure))
  name annotations members groups)

;; --------------------------------------------------------------------------

(cl-defstruct
    (union
     (:constructor create-union))
  name annotations variants)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(cl-defstruct
    (type-variant
     (:constructor create-type-variant))
  name rename-as annotations)


(provide 'sdml)

;;; sdml.el ends here

(let ((ex-module (module 'example
                           (make-module/body
                            :imports
                            '(sdml dc xsd skos)
                            :annotations
                            (list
                             (make-annotation-property "skos:prefLabel" "Example")
                             (make-annotation-property "skos:version" 2))
                            :definitions
                            (list (make-datatype 'name 'string))))))
  (message "%s"
           (with-output-to-string (print-module ex-module))))
