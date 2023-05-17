;;; flycheck-sdml.el --- Use Flycheck to run sdml-lint -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Keywords: lint
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.2") (flycheck "32") (tree-sitter "0.18.0") (tsc "0.18.0") (dash "2.9.1"))

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

;; This package provides a back-end for Flycheck that uses tree-sitter queries to
;; select model elements with issues.

;; Install
;;
;; `(use-package flycheck-sdml
;;    :ensure t
;;    :config (flycheck-sdml-setup))'
;;

;; Usage
;;
;; To enable, simply ensure Flycheck mode is enabled for your buffer.
;;
;; Rather than manually, you can enable this by setting `flycheck-mode' for
;; all SDML files.
;;
;; `(use-package flycheck-sdml
;;    :ensure t
;;    :hook (sdml-mode . flycheck-mode)
;;    :config (flycheck-sdml-setup))'

;; Customization
;;
;; Lint rules are defined in the custom variable `sdml-lint-rules' with the form:
;;
;; `(rule-id "Issue message" level "tree-sitter query")'
;;


;;; Code:

(require 'flycheck)
(require 'tree-sitter)
(require 'tsc)
(require 'dash)

;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(makunbound 'sdml-lint-rules)
(unintern 'sdml-lint-rules)

(defcustom sdml-lint-rules
  `((module-name-case
     "Module names may not start with upper-case"
     warning
     "((module name: (identifier) @name) (#match? @name \"^[A-Z]\"))")
    ;; ----------------------------------------------------------------------
    (type-name-case
     "Type names may not start with lower-case"
     warning
     "([(entity_def name: (identifier) @name) (structure_def name: (identifier) @name) (event_def name: (identifier) @name) (enum_def name: (identifier) @name)] (#match? @name \"^[a-z]\"))")
    ;; ----------------------------------------------------------------------
    (annotation-string-no-language
     "Annotation strings should always include a language identifier"
     warning
     ,(concat "(annotation value: (value (string !language) @string))"
              "(annotation value: (value (list_of_values (string !language) @string)))"))
    ;; ----------------------------------------------------------------------
    (annotation-string-empty
     "Did you mean to use an empty annotation string value?"
     warning
     ,(concat "((annotation value: (value (string) @value)) (#eq? @value \"\\\"\\\"\"))"
              "((annotation value: (value (list_of_values (string) @value))) (#eq? @value \"\\\"\\\"\"))"))
    ;; ----------------------------------------------------------------------
    (entity-no-identity
     "Entities should include an identifying member"
     warning
     "(entity_def !identity) @entity")
    ;; ----------------------------------------------------------------------
    (types-missing-bodies
     "Incomplete type definition"
     info
     "[(entity_def !body) (structure_def !body) (event_def !body) (enum_def !body)] @type")
    ;; ----------------------------------------------------------------------
    (member-by-value-target-cardinality
     "By-value member cardinality is the default and may be removed"
     info
     "((member_by_value targetCardinality: (cardinality_expression) @card) (#eq? @card \"{1..}\"))")
    ;; ----------------------------------------------------------------------
    (member-by-ref-source-cardinality
     "By-reference member source cardinality is the default and may be removed"
     info
     "((member_by_reference sourceCardinality: (cardinality_expression) @card) (#eq? @card \"{0..}\"))")
    ;; ----------------------------------------------------------------------
    (member-by-ref-target-cardinality
     "By-reference member target cardinality is the default and may be removed"
     info
     "((member_by_reference targetCardinality: (cardinality_expression) @card) (#eq? @card \"{0..}\"))"))
  "SDML lint rules for Flycheck, these use tree-sitter queries to select issues."
  :tag "Lint rules for Flycheck."
  :type '(repeat (list (symbol :tag "Identifier")
                       (string :tag "Message")
                       (choice :tag "Level"
                               (const :tag "Error" error)
                               (const :tag "Warning" warning)
                               (const :tag "Informational" info)
                               (const :tag "Ignore" nil))
                       (string :tag "Match Query")))
  :group 'sdml)

;; string with no language for current locale:
;; (format "^@%s\(-%s\)?]" locale-language locale-country)
;; ((annotation value: (value (string (language_tag) @value))) (#match? @value "^@en"))

;; --------------------------------------------------------------------------
;; Actual checker
;; --------------------------------------------------------------------------

(defun flycheck-sdml--make-error (rule-id err-message lint-level capture)
  "Turn CAPTURE into a `flycheck-error'.

The resulting error uses metadata from the rule definitions, specifically
RULE-ID, ERR-MESSAGE, and LINT-LEVEL."
  (let* ((node (cdr capture))
         (captured (tsc-node-text node))
         (start (tsc-node-start-position node))
         (end-pos (tsc-node-end-position node)))
    (flycheck-error-new-at-pos
     start
     lint-level
     (format "%s: %s" err-message captured)
     :end-pos end-pos
     :checker 'sdml
     :id rule-id)))

(defun flycheck-sdml--run-lint-rule (rule)
  "Execute RULE returning a list of any matching issues."
  (let ((rule-name (nth 0 rule))
        (err-message (nth 1 rule))
        (lint-level (nth 2 rule))
        (patterns (nth 3 rule)))
    (message "rule: %s" patterns)
    (tsc--without-restriction
      (when-let*
          ((query
            (condition-case err
                (tsc-make-query tree-sitter-language patterns)
              ((tsc-query-invalid-node-type
                tsc-query-invalid-field
                tsc-query-invalid-capture)
               (message "%s: %s" (get (car err) 'error-message) (cadr err))
               nil)
              (tsc-query-invalid
               (message "%s" (get (car err) 'error-message))
               nil)))
           (root-node (tsc-root-node tree-sitter-tree))
           (captures (tsc-query-captures query root-node #'tsc--buffer-substring-no-properties)))
        (if (= (length captures) 0)
            (progn
              (message "No captures found in query %s [%s]" patterns rule-name)
              '())
          (mapcar (lambda (cap) (flycheck-sdml--make-error rule-name err-message lint-level cap)) captures))))))

;; --------------------------------------------------------------------------
;; Actual checker
;; --------------------------------------------------------------------------

(defun flycheck-sdml--start (checker callback)
  "Flycheck start function for sdml.

CHECKER is this checker, and CALLBACK is the flycheck dispatch function."
  (let* ((results (-flatten-n
                   1
                   (mapcar 'flycheck-sdml--run-lint-rule
                           (-filter (lambda (rule)
                                      (and (not (equal (nth 1 rule) 'nil))
                                           (not (string= (nth 2 rule) ""))))
                                    sdml-lint-rules)))))
    (funcall callback 'finished results)))


(flycheck-define-generic-checker 'sdml
  "Report errors by the built-in linter.

You can edit the rules checked by the linter, by customizing the variable
`sdml-lint-rules'. Each rule consists of:
- an identifier (symbol)
- a lint/error level (symbol)
- a tree-sitter query"
  :start #'flycheck-sdml--start
  :modes '(sdml-mode))
;;  :predicate (lambda () sdml-mode))

;; --------------------------------------------------------------------------
;; Setup function
;; --------------------------------------------------------------------------

;;;###autoload
(defun flycheck-sdml-setup ()
  "Setup flycheck-sdml.

Add `sdml' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'sdml))

(provide 'flycheck-sdml)

;;; flycheck-sdml.el ends here
