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
(require 'seq)

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

(defcustom sdml-mode-cli-no-color nil
  "Suppress color output from the command line tool."
  :tag "Suppress color"
  :type 'boolean
  :group 'sdml)

;; --------------------------------------------------------------------------
;; Section heading
;; --------------------------------------------------------------------------

(defconst sdml-mode-cli-default-output-buffer-name "*SDML Command-Line Output*")

(defconst sdml-mode-cli-default-error-buffer-name "*SDML Command-Line Errors*")

(defun sdml-mode-cli-make-arg (name value)
  "Make an argument string from NAME and VALUE."
  (format "--%s %s" name value))

(defun sdml-mode-cli-make-command (command &rest args)
  "Make an sdml COMMAND with additional ARGS."
  (when (or t (derived-mode-p 'sdml-mode))
    (let* ((cli-name (or sdml-mode-cli-name "sdml"))
           (cmd-name (executable-find cli-name))
           (pre-args (list
                      cmd-name
                      (sdml-mode-cli-make-arg 'log-filter sdml-mode-cli-log-filter)
                      (if sdml-mode-cli-no-color "--no-color" nil)
                      command))
           (args (mapcar (lambda (arg) (cond
                                   ((eq arg 'current-buffer)
                                    (sdml-mode-cli-make-arg 'input (buffer-file-name)))
                                   (t arg)))
                         args)))
      (cond
       ((null cmd-name)
        (message "couldn't find the sdml cli: %s" cli-name))
       (t
        (string-join (append pre-args args) " "))))))

(defun sdml-mode-cli--make-refresh-cmd (cmd env out err)
  "Return a lambda to refresh the output buffer from a command.
CMD is the command-line to run, ENV is the environment variables
to add, OUT is the output buffer and ERR the error buffer."
  (lambda ()
    (interactive)
    (setq buffer-read-only nil)
    (delete-region  (point-min) (point-max))
    (with-environment-variables (("SDML_PATH" env))
      (shell-command cmd out err)
      ;; colorize output
      (ansi-color-apply-on-region (point-min) (point-max))
      (setq buffer-read-only t))))

(defun sdml-mode-cli-run-command (command &optional output-buffer-name error-buffer-name refresh-fn)
  "Run COMMAND with output to OUTPUT-BUFFER-NAME and ERROR-BUFFER-NAME.

If not specified OUTPUT-BUFFER-NAME is set to
`sdml-cli-default-output-buffer-name' and ERROR-BUFFER-NAME is set
to `sdml-cli-default-error-buffer-name'.

The boolean REFRESH-FN indicates that a refresh function should
be added to the buffer with a key binding to \"g\"."
  (let ((is-special (null output-buffer-name))
        (output-buffer-name (or output-buffer-name sdml-mode-cli-default-output-buffer-name))
        (load-path (concat (or (getenv "SDML_PATH") "")
                           (string-join sdml-mode-cli-load-path ":"))))
    (with-environment-variables (("SDML_PATH" load-path))
      (shell-command command
                     output-buffer-name
                     (or error-buffer-name sdml-mode-cli-default-error-buffer-name))
      (when is-special
        (pop-to-buffer output-buffer-name)
        ;; colorize output
        (ansi-color-apply-on-region (point-min) (point-max))
        ;; make read-only
        (special-mode)
        (when refresh-fn
        ;; install refresh command
        (use-local-map (copy-keymap special-mode-map))
        (local-set-key "g" (sdml-mode-cli--make-refresh-cmd command
                                                            load-path
                                                            output-buffer-name
                                                            error-buffer-name)))))))

(provide 'sdml-mode-cli)

;;; sdml-mode-cli.el ends here
