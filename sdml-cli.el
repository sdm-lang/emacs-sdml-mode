;;; sdml-cli.el --- CLI integration -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>

;;; License:

;; Copyright (c) 2024 Simon Johnston

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

;; Internal module.

;;; Code:

(require 'ansi-color) ;; built-in

;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(defcustom sdml-cli-name "sdml"
  "The name of the command line tool to use for SDML actions."
  :tag "CLI tool name"
  :type 'file
  :group 'sdml)

(defcustom sdml-cli-log-filter 'none
  "The level of log information to emit from the command-line tool."
  :tag "Logging filter level"
  :type '(choice (const :tag "None" none)
                 (const :tag "Errors" errors)
                 (const :tag "Warnings" warnings)
                 (const :tag "Information" information)
                 (const :tag "Debugging" debugging)
                 (const :tag "Tracing" tracing))
  :group 'sdml)

(defcustom sdml-cli-load-path nil
  "A list of directories to be used as the `SDML_PATH' variable value."
  :tag "CLI load path"
  :type '(repeat directory)
  :group 'sdml)

;; --------------------------------------------------------------------------
;; Section heading
;; --------------------------------------------------------------------------

(defconst sdml-cli-default-output-buffer-name "*SDML Command-Line Output*")

(defconst sdml-cli-default-error-buffer-name "*SDML Command-Line Errors*")

(defun sdml-cli--make-arg (plist-args key &optional arg-name ignore-me)
  "Make a CLI argument with KEY from PLIST-ARGS."
  (when (not ignore-me)
    (let ((value (plist-get plist-args key))
          (arg-name (if (not (null arg-name))
                        arg-name
                      (substring (symbol-name key) 1))))
      (if (null value) "" (format "--%s %s" arg-name value)))))

(defun sdml-cli-make-command (command &rest plist-args)
  "Make an sdml command-line COMMAND with additional PLIST-ARGS."
  (interactive)
  (when (eq major-mode 'sdml-mode)
    (let* ((cli-name (or sdml-cli-name "sdml"))
           (cmd-name (executable-find cli-name))
           (file-name (buffer-file-name (current-buffer))))
      (cond
       ((null cmd-name)
        (message "couldn't find the sdml cli: %s" cli-name))
       ((null file-name)
        (message "buffer doesn't have a file name"))
       (t
        (string-join
         (list cmd-name
               (sdml-cli--make-arg plist-args :log-filter)
               command
               (sdml-cli--make-arg plist-args :validation-level "level" (not (string= command "validate")))
               (sdml-cli--make-arg plist-args :depth nil (not (string= command "deps")))
               (sdml-cli--make-arg plist-args :output-format)
               (sdml-cli--make-arg plist-args :output-file)
               (or (sdml-cli--make-arg plist-args :input-file)
                   (let ((arg-value (plist-get plist-args :input-module)))
                     (if (null arg-value) "" arg-value))))
         " "))))))


(defun sdml-cli-run-command (command &optional output-buffer-name error-buffer-name)
  "Run COMMAND with output to OUTPUT-BUFFER-NAME and ERROR-BUFFER-NAME.

If not specified `output-buffer-name' is set to
`sdml-cli-default-output-buffer-name' and `error-buffer-name' is set
to `sdml-cli-default-error-buffer-name'."
  (let ((output-buffer-name (or output-buffer-name sdml-cli-default-output-buffer-name))
        (current-load-path (or (getenv "SDML_PATH") "")))
    (with-environment-variables (("SDML_PATH" (concat current-load-path
                                                      (string-join sdml-cli-load-path ":"))))
      (with-output-to-temp-buffer output-buffer-name
        (shell-command command
                       output-buffer-name
                       (or error-buffer-name sdml-cli-default-error-buffer-name))
        (pop-to-buffer output-buffer-name)
        ;; colorize output
        (ansi-color-apply-on-region (point-min) (point-max))
        ;; make read-only
        (special-mode)))))


(provide 'sdml-cli)

;;; sdml-cli.el ends here
