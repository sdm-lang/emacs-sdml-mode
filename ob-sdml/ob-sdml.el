;;; ob-sdml.el --- Org-Babel for SDML -*- lexical-binding: t; -*-

;; Copyright (c) 2023, 2024 Simon Johnston

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; Version: 0.1.3
;; Package-Requires: ((emacs "28.2") (org "9.5.5") (sdml-mode "0.1.6"))
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

;; Provides Babel integration for SDML source blocks.  This relies on the
;; command-line tool `sdml' (https://github.com/johnstonskj/rust-sdml) to
;; run checkers, diagram generators, and conversion tools.
;;

;; Usage
;;
;; The following header arguments are specifically used on source blocks:
;;
;; `:cmd' -- optional, the name or path to the command-line tool.  Default is "sdml".
;; `:cmdline' -- required, denotes the action to perform.
;; `:file' -- required, the file to output results to.
;;
;; Examples
;;
;; #+BEGIN_SRC sdml :cmdline draw --diagram concepts :file diagram.svg
;;
;;
;; #+BEGIN_SRC sdml :cmdline draw --diagram concepts --output-format source :file diagram.dot
;;

;;; Code:

(require 'org)
(require 'ob)
(require 'sdml-mode)

(defcustom ob-sdml-cmd "sdml"
  "Name of the command to use for processing SDML source.
May be either a command in the path, like sdml
or an absolute path name, like /usr/local/bin/sdml
parameters may be used, like sdml -v."
  :tag "Org Babel SDML command"
  :type 'file
  :group 'org-babel)

(defcustom ob-sdml-log-filter 'errors
  "The level of logging the `ob-sdml-cmd' command-line tool should emit."
  :tag "Org Babel SDML log filter"
  :type '(choice (const :tag "No Logging" none)
                 (const :tag "Errors" errors)
                 (const :tag "Warnings" warnings)
                 (const :tag "Informational" information)
                 (const :tag "Debugging" debugging)
                 (const :tag "Tracing" tracing))
  :group 'org-babel)

(defcustom ob-sdml-no-color nil
  "Disable color output from the `ob-sdml-cmd' command."
  :tag "Org Babel SDML color switch"
  :type `(choice (const :tag "Default" ,nil)
                 (const :tag "No Color" t))
  :group 'org-babel)


(defvar org-babel-default-header-args:sdml
  '((:results . "file")
    (:exports . "results"))
  "Default arguments to use when evaluating a SDML source block.")

;; Following required naming convention
(defun ob-sdml-expand-body (body params)
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
  (let* ((out-file (cdr (or (assoc :file params)
			                (error "You need to specify a :file parameter"))))
         (cmd (or (cdr (assoc :cmd params)) sdml-cli-name))
	     (cmdline (cdr (assoc :cmdline params)))
         (output-format (if (and (string-prefix-p "draw" cmdline)
                                 (not (string-search "--output-format" cmdline)))
                            (format " --output-format %s" (file-name-extension out-file))
                          ""))
	     (coding-system-for-read 'utf-8) ; use utf-8 with sub-processes
	     (coding-system-for-write 'utf-8)
	     (in-file (org-babel-temp-file (format "%s-" sdml-cli-name))))
    (let ((expanded-source (ob-sdml-expand-body body params)))
      (message expanded-source)
      (with-temp-file in-file
        (insert expanded-source)))
    (let ((full-cmd-string (concat cmd
         	                       " " cmdline
                                   output-format
	                               " --output " (org-babel-process-file-name out-file)
                                   " --input " (org-babel-process-file-name in-file))))
      (message full-cmd-string)
      (org-babel-eval full-cmd-string ""))
    ;; signal that output has already been written to file
    nil))

(defun org-babel-prep-session:sdml (_session _params)
  "Return an error because sdml does not support sessions."
  (error "SDML does not support sessions"))

(defun ob-sdml-setup ()
  "Set up language mapping for Babel."
  (add-to-list 'org-babel-load-languages '(sdml . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))

(provide 'ob-sdml)

;;; ob-sdml.el ends here
