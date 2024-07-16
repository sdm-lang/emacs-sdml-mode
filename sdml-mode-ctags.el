;;; sdml-mode-ctags.el --- Universal Ctags Support -*- lexical-binding: t; -*-

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

;; To enable SDML tagging you will need to install the Universal Ctags
;; configuration file from:
;;
;;     https://github.com/sdm-lang/sdml-ctags
;;
;; Internal module.

;;; Code:

;; --------------------------------------------------------------------------
;; Requirements, both of these are optional
;; --------------------------------------------------------------------------

(defvar company-ctags-modes)
(declare-function company-ctags-find-table "company-ctags")

(declare-function projectile-acquire-root "projectile")


;; --------------------------------------------------------------------------
;; Customization
;; --------------------------------------------------------------------------

(defcustom sdml-mode-ctags-command "/opt/homebrew/bin/ctags"
  "The command path/name for Universal Ctags."
  :tag "U-Ctags command path"
  :type 'file
  :group 'sdml)


(defcustom sdml-mode-ctags-output-file-name "tags"
  "The name of the generated tag file."
  :tag "ctags output file name"
  :type 'file
  :group 'sdml)


;; --------------------------------------------------------------------------
;; Universal Ctag generation
;; --------------------------------------------------------------------------

(defun sdml-mode-ctags-tag-file-path (&optional file-path)
  "Return a path to a tag file for the current buffer.
If FILE-PATH is provided the tag file is found relative to that
path instead."
  (let* ((current-buffer-dir (file-name-directory
                              (or file-path (buffer-file-name))))
         (fallback (concat current-buffer-dir
                           sdml-mode-ctags-output-file-name)))
  (cond ((featurep 'company-ctags)
         (let ((found (company-ctags-find-table)))
           (if found (car found) fallback)))
        ((featurep 'projectile)
         (concat (projectile-acquire-root current-buffer-dir)
                 sdml-mode-ctags-output-file-name))
        (t fallback))))

(defun sdml-mode-ctags-generate ()
  "Generate a TAGS file for the current SDML project."
  (interactive)
  (let ((tag-file-path (sdml-mode-ctags-tag-file-path)))
    (shell-command (format "%s -R -e -o %s" sdml-mode-ctags-command tag-file-path))))

;; --------------------------------------------------------------------------
;; Ctags Minor Mode
;; --------------------------------------------------------------------------

;;;###autoload
(define-minor-mode
  sdml-mode-ctags-mode
  "Minor mode to provide tagging of SDML source."

  :group 'sdml

  :tag "Enable SDML tagging minor mode"

  :lighter nil

  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s T") 'sdml-mode-ctags-generate)
    (add-to-list 'minor-mode-map-alist (cons 'sdml-mode-ctags-mode map)))

  (when (featurep 'company-ctags)
    (add-to-list 'company-ctags-modes 'sdml-mode)))

(provide 'sdml-mode-ctags)

;;; sdml-mode-ctags.el ends here
