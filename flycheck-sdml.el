;;; flycheck-sdml.el --- Use Flycheck to run sdml-lint -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Keywords: lint
;; Version: 0.1.0

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

(require 'flycheck)
(require 'tree-sitter)
(require 'tsc) ;; 0.18.0
(require 'dash)

;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(defcustom sdml-lint-rules
  '((module-name-case
     warning
     "((module name: (identifier) @name) (#match? @name \"^[A-Z]\"))")
    (type-name-case
     warning
     "")
    (data-type-name-case
     warning
     "")
    (annotation-string-no-language
     warning
     (concat "(annotation value: (value (string !language) @string))"
             "(annotation value: (value (list_of_values (string !language) @string)))"))
    (entity-no-identity
     warning
     "(entity_def !identity) @entity")
    (types-missing-bodies
     info
     "[(entity_def !body) (structure_def !body) (event_def !body) (enum_def !body)] @entity")
    )
  "Lint rules for SDML source."
  :tag "Lint rules"
  :type '(repeat (list (symbol :tag "Rule Name")
                       (string :tag "Message")
                       (choice (const :tag "Error" error)
                               (const :tag "Warning" warning)
                               (const :tag "Informational" info)
                               (const :tag "Ignore" nil)
                               :tag "Lint Level")
                       (string :tag "Match Query")))
  :group 'sdml)

(setq sdml-lint-rules
      `((module-name-case
         "Module names may not start with upper-case"
         warning
         "((module name: (identifier) @name) (#match? @name \"^[A-Z]\"))")
        (type-name-case
         "Type names may not start with lower-case"
         warning
         "([(entity_def name: (identifier) @name) (structure_def name: (identifier) @name) (event_def name: (identifier) @name) (enum_def name: (identifier) @name)] (#match? @name "^[a-z]"))")
        (annotation-string-no-language
         "Annotation strings should always include a language identifier"
         warning
         ,(concat "(annotation value: (value (string !language) @string))"
                  "(annotation value: (value (list_of_values (string !language) @string)))"))
        (entity-no-identity
         "Entities should include an identifying member"
         warning
         "(entity_def !identity) @entity")
        (types-missing-bodies
         "Incomplete type definition"
         info
         "[(entity_def !body) (structure_def !body) (event_def !body) (enum_def !body)] @type")
        ))

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
               (tree-sitter--echo "%s: %s" (get (car err) 'error-message) (cadr err))
               nil)
              (tsc-query-invalid
               (tree-sitter--echo "%s" (get (car err) 'error-message))
               nil)))
           (root-node (tsc-root-node tree-sitter-tree))
           (captures (tsc-query-captures query root-node #'tsc--buffer-substring-no-properties)))
        (if (= (length captures) 0)
            (tree-sitter--echo "No captures found")
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
    (message "finished: %s" (pp results))
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
