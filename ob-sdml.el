;;; ob-sdml.el --- Org-Babel for SDML -*- lexical-binding: t; -*-

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

(require 'org)

(require 'ob)


(defgroup ob-sdml nil
  "Org Babel support."
  :tag ""
  :prefix "ob-sdml-"
  :group 'org-babel)

(defcustom ob-sdml-cmd "sdml"
  "Name of the command to use for processing SDML source.
May be either a command in the path, like sdml
or an absolute path name, like /usr/local/bin/sdml
parameters may be used, like sdml -v"
  :tag "Org Babel SDML Command"
  :type 'string
  :group 'org-babel)


(defvar org-babel-default-header-args:sdml
  '((:results . "file")
    (:exports . "results"))
  "Default arguments to use when evaluating a SDML source block.")

(defun org-babel-expand-body:sdml (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params)))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
	     (value (cdr pair)))
	 (setq body
	       (replace-regexp-in-string
		(concat "$" (regexp-quote name))
		(if (stringp value) value (format "%S" value))
		body
		t
		t))))
     vars)
    body))

(defun org-babel-execute:sdml (body params)
  "Execute a block of SDML code with org-babel.
The code to process is in BODY, the block parameters are in PARAMS.
This function is called by `org-babel-execute-src-block'."
  (message "About to...")
  (let* ((out-file (cdr (or (assoc :file params)
			                (error "You need to specify a :file parameter"))))
         (cmd (or (cdr (assoc :cmd params)) ob-sdml-cmd))
	     (cmdline (cdr (assoc :cmdline params)))
         (output-format (if (and (string-prefix-p "draw" cmdline)
                                 (not (string-search "--output-format" cmdline)))
                            (format " --output-format %s" (file-name-extension out-file))
                          ""))
	     (coding-system-for-read 'utf-8) ; use utf-8 with sub-processes
	     (coding-system-for-write 'utf-8)
	     (in-file (org-babel-temp-file (format "%s-" ob-sdml-cmd))))
    (with-temp-file in-file
      (insert (org-babel-expand-body:sdml body params)))
    (message "%s" (concat cmd
         	 " " cmdline
             output-format
	         " --output-file " (org-babel-process-file-name out-file)
             " " (org-babel-process-file-name in-file)))
    (org-babel-eval
     (concat cmd
         	 " " cmdline
             output-format
	         " --output-file " (org-babel-process-file-name out-file)
             " " (org-babel-process-file-name in-file)) "")
    ;; signal that output has already been written to file
    nil))

(defun org-babel-prep-session:sdml (_session _params)
  "Return an error because sdml does not support sessions."
  (error "SDML does not support sessions"))

(defun ob-sdml-setup ()
  "Set of mapping."
  (add-to-list 'org-babel-load-languages '(sdml . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))

(defun ob-sdml-unload ()
  "Remove all."
  (dolist (sym '(org-babel-default-header-args:sdml-draw
                 org-babel-expand-body:sdml-draw
                 org-babel-execute:sdml-draw
                 org-babel-prep-session:sdml-draw))
    (makunbound sym)
    (unintern sym nil))
  (unload-feature 'ob-sdml))

(provide 'ob-sdml)

;;; ob-sdml.el ends here
