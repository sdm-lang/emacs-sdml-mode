;;; sdml-mode-cli.el --- CLI integration -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>

;;; License:

;; Copyright (c) 2023, 2024 Simon Johnston
;;
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

;; Internal module.

;;; Code:

(require 'ansi-color) ;; built-in

;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(defcustom sdml-mode-cli-name "sdml"
  "The name of the command line tool to use for SDML actions."
  :tag "CLI tool name"
  :type 'file
  :group 'sdml)

(defcustom sdml-mode-cli-log-filter 'none
  "The level of log information to emit from the command line tool."
  :tag "Logging filter level"
  :type '(choice (const :tag "None" none)
                 (const :tag "Errors" errors)
                 (const :tag "Warnings" warnings)
                 (const :tag "Information" information)
                 (const :tag "Debugging" debugging)
                 (const :tag "Tracing" tracing))
  :group 'sdml)

(defcustom sdml-mode-cli-load-path nil
  "A list of directories to be used as the `SDML_PATH' variable value."
  :tag "CLI load path"
  :type '(repeat directory)
  :group 'sdml)

;; --------------------------------------------------------------------------
;; Section heading
;; --------------------------------------------------------------------------

(defconst sdml-mode-cli-default-output-buffer-name "*SDML Command-Line Output*")

(defconst sdml-mode-cli-default-error-buffer-name "*SDML Command-Line Errors*")

(defun sdml-mode-cli-make-arg (plist-args key &optional arg-name ignore-me)
  "Make a CLI argument with KEY from PLIST-ARGS."
  (when (not ignore-me)
    (let ((value (plist-get plist-args key))
          (arg-name (if (not (null arg-name))
                        arg-name
                      (substring (symbol-name key) 1))))
      (if (null value) "" (format "--%s %s" arg-name value)))))

(defun sdml-mode-cli-make-command (command &rest plist-args)
  "Make an sdml COMMAND with additional PLIST-ARGS."
  (interactive)
  (when (derived-mode-p 'sdml-mode)
    (let* ((cli-name (or sdml-mode-cli-name "sdml"))
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
               (sdml-mode-cli-make-arg plist-args :log-filter)
               command
               (sdml-mode-cli-make-arg plist-args :validation-level "level" (not (string= command "validate")))
               (sdml-mode-cli-make-arg plist-args :depth nil (not (string= command "deps")))
               (sdml-mode-cli-make-arg plist-args :output-format)
               (sdml-mode-cli-make-arg plist-args :output-file)
               (or (sdml-mode-cli-make-arg plist-args :input-file)
                   (let ((arg-value (plist-get plist-args :input-module)))
                     (if (null arg-value) "" arg-value))))
         " "))))))


(defun sdml-mode-cli-run-command (command &optional output-buffer-name error-buffer-name)
  "Run COMMAND with output to OUTPUT-BUFFER-NAME and ERROR-BUFFER-NAME.

If not specified `output-buffer-name' is set to
`sdml-cli-default-output-buffer-name' and `error-buffer-name' is set
to `sdml-cli-default-error-buffer-name'."
  (let ((output-buffer-name (or output-buffer-name sdml-mode-cli-default-output-buffer-name))
        (current-load-path (or (getenv "SDML_PATH") "")))
    (with-environment-variables (("SDML_PATH" (concat current-load-path
                                                      (string-join sdml-mode-cli-load-path ":"))))
      (with-output-to-temp-buffer output-buffer-name
        (shell-command command
                       output-buffer-name
                       (or error-buffer-name sdml-mode-cli-default-error-buffer-name))
        (pop-to-buffer output-buffer-name)
        ;; colorize output
        (ansi-color-apply-on-region (point-min) (point-max))
        ;; make read-only
        (special-mode)))))


(provide 'sdml-mode-cli)

;;; sdml-mode-cli.el ends here
