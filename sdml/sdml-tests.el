; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'cl-lib)
(require 'sdml)

(ert-deftest test-valid--identifier ()
  "Tests the predicate `identifier-p' with valid values."
  (let ((examples '(sdml "sdml")))
    (dolist (example examples)
             (should (equal (cl-typep example 'sdml-identifier) t)))))

(ert-deftest test-invalid--identifier ()
  "Tests the predicate `identifier-p' with invalid values."
  (let ((examples '("sd ml" "sd-ml")))
    (dolist (example examples)
             (should (equal (cl-typep example 'sdml-identifier) nil)))))

(let ((ex-module (make-sdml-module
                  :name 'example
                  :imports '(sdml dc xsd skos)
                  :annotations
                  (list
                   (make-sdml-annotation-property :name "skos:prefLabel" :value "Example")
                   (make-sdml-annotation-property :name "skos:version" :value 2))
                  :definitions
                  (list
                   (make-sdml-datatype
                    :name 'name
                    :base-type 'string
                    :annotations
                    (list
                     (make-sdml-annotation-property :name "skos:prefLabel" :value "Another")))))))
  (message "%s"
           (with-output-to-string (print-to-stream ex-module))))

