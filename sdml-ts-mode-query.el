;;; sdml-ts-mode-query.el --- Internal query support -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; License: see sdml-ts-mode.el

;;; Commentary:

;; Internal module to organize all interesting query.
;;

;;; Code:

(defconst sdml-ts-mode-query--any-comment
  '(line_comment))

(defconst sdml-ts-mode-query--global-keywords
  '[ "as"
     "assert"
     "class"
     "datatype"
     "dimension"
     "end"
     "entity"
     "enum"
     "event"
     "from"
     "group"
     "import"
     "is"
     "metric"
     "module"
     "of"
     "property"
     "rdf"
     "structure"
     "union"
     (unknown_type) ])

(defconst sdml-ts-mode-query--xsd-datatypes
  '("anyURI"
    "base64Binary"
    "boolean"
    "byte"
    "date"
    "dateTime"
    "dateTimeStamp"
    "dayTimeDuration"
    "decimal"
    "double"
    "duration"
    "float"
    "gDay"
    "gMonth"
    "gMonthDay"
    "gYear"
    "gYearMonth"
    "hexBinary"
    "int"
    "integer"
    "iri"
    "language"
    "long"
    "negativeInteger"
    "nonNegativeInteger"
    "nonPositiveInteger"
    "normalizedString"
    "positiveInteger"
    "short"
    "string"
    "time"
    "token"
    "unsigned"
    "unsignedByte"
    "unsignedInt"
    "unsignedLong"
    "unsignedShort"
    "yearMonthDuration"))

(defconst sdml-ts-mode-query--owl-datatypes
  ("rational" "real"))

(defconst sdml-ts-mode-query--owl-classes
  ("Nothing" "Thing"))

(defconst sdml-ts-mode-query--sdml-datatypes
  ("binary" "iri"))

(defconst sdml-ts-mode-query--builtin-types
  '(builtin_types))

(provide 'sdml-ts-mode-query)

;;; sdml-ts-mode-query.el ends here
