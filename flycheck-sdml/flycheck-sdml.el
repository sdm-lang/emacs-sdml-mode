;;; flycheck-sdml.el --- Use Flycheck to run sdml-lint -*- lexical-binding: t; -*-

;; Copyright (c) 2023, 2024 Simon Johnston

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Version: 0.1.3
;; Package-Requires: ((emacs "28.2") (flycheck "32") (tree-sitter "0.18.0") (tsc "0.18.0") (dash "2.9.1") (sdml-mode "0.1.6"))
;; URL: https://github.com/johnstonskj/emacs-sdml-mode
;; Keywords: languages tools

;;; License:

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
;;    :config (add-hook 'flycheck-mode-hook #'flycheck-sdml-setup))'

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
(require 'sdml-mode)

;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(defcustom flycheck-sdml-lint-rules
  `((module-name-case
     "Module names may not start with upper-case"
     warning
     "((module name: (identifier) @name) (#match? @name \"^[[:upper:]]\"))")
    ;; ----------------------------------------------------------------------
    (type-name-case
     "Type names may not start with lower-case"
     warning
     "([(entity_def name: (identifier) @name) (structure_def name: (identifier) @name) (event_def name: (identifier) @name) (enum_def name: (identifier) @name) (union_def name: (identifier) @name)] (#match? @name \"^[[:lower:]]\"))")
    ;; ----------------------------------------------------------------------
    (annotation-string-no-language
     "Annotation strings should always include a language identifier"
     warning
     ,(concat "(annotation_property value: (value (simple_value (string !language) @string)))"
              "(annotation_property value: (value (sequence_of_values (simple_value (string !language) @string))))"))
    ;; ----------------------------------------------------------------------
    (annotation-string-empty
     "Did you mean to use an empty annotation string value?"
     warning
     ,(concat "((annotation_property value: (value (simple_value (string) @value))) (#eq? @value \"\\\"\\\"\"))"
              "((annotation_property value: (value (sequence_of_values (simple_value (string) @value)))) (#eq? @value \"\\\"\\\"\"))"))
    ;; ----------------------------------------------------------------------
    (types-missing-bodies
     "Incomplete type definition, no body specified"
     info
     "[(entity_def !body) (structure_def !body) (event_def !body) (enum_def !body) (union_def !body)] @type")
    ;; ----------------------------------------------------------------------
    (members-using-unknown
     "Incomplete member definition, using unknown type"
     info
     "(type_reference (unknown_type) @type)")
    ;; ----------------------------------------------------------------------
    (member-unrestricted-types
     "It's bad form to use unrestricted simple types"
     info
     "(type_reference (builtin_simple_type) @type)")
    ;; ----------------------------------------------------------------------
    (member-cardinality
     "By-value member cardinality is the default and may be removed"
     info
     "((member cardinality: (cardinality_expression) @card) (#eq? @card \"{1..}\"))"))
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
               (message "%s | %s" (get (car err) 'error-message) patterns)
               nil)))
           (root-node (tsc-root-node tree-sitter-tree))
           (captures (tsc-query-captures query root-node #'tsc--buffer-substring-no-properties)))
        (if (= (length captures) 0)
            '()
          (mapcar (lambda (cap) (flycheck-sdml--make-error rule-name err-message lint-level cap)) captures))))))

;; --------------------------------------------------------------------------
;; Actual checker
;; --------------------------------------------------------------------------

(defun flycheck-sdml--start (checker callback)
  "Flycheck start function for sdml.

CHECKER is this checker, and CALLBACK is the flycheck dispatch function."
  (message "Running flycheck checker %s" checker)
  (let* ((results (-flatten-n
                   1
                   (mapcar #'flycheck-sdml--run-lint-rule
                           (-filter (lambda (rule)
                                      (and (not (equal (nth 1 rule) 'nil))
                                           (not (string= (nth 2 rule) ""))))
                                    flycheck-sdml-lint-rules)))))
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
  "Setup SDML in Flycheck.

Add `sdml' to `flycheck-checkers'."
  (interactive)
  ;; We should avoid raising any error in this function, as in combination
  ;; with `global-flycheck-mode' it will render Emacs unusable.
  (with-demoted-errors "Error in flycheck-sdml-setup: %S"
    (add-to-list 'flycheck-checkers 'sdml)))

(provide 'flycheck-sdml)

;;; flycheck-sdml.el ends here
