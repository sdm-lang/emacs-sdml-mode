; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'sdml-model)

(ert-deftest test-valid--identifier-p ()
  "Tests the predicate `identifier-p' with valid values."
  (let ((examples '(sdml "sdml")))
    (dolist (example examples)
             (should (equal (identifier-p example) t)))))

(ert-deftest test-invalid--identifier-p ()
  "Tests the predicate `identifier-p' with invalid values."
  (let ((examples '("sd ml" "sd-ml")))
    (dolist (example examples)
             (should (equal (identifier-p example) nil)))))

(let ((ex-module (make-module 'example
                  :imports '(sdml dc xsd skos)
                  :annotations
                  (list
                   (make-annotation-property "skos:prefLabel" "Example")
                   (make-annotation-property "skos:version" 2))
                  :definitions
                  (list (make-datatype 'name 'string)))))
  (message "%s"
           (with-output-to-string (print-to-stream ex-module))))


