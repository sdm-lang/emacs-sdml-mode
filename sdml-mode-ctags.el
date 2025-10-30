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

;; Copyright 2023-2025 Simon Johnston <johnstonskj@gmail.com>
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Minor mode to provide tagging of SDML (sdml-mode) source.
;;
;; To enable SDML tagging you will need to install the Universal Ctags
;; configuration file from:
;;
;;     https://github.com/sdm-lang/sdml-ctags

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
  "The command path/name for universal Ctags."
  :tag "Universal Ctags command path"
  :type 'file
  :group 'sdml)


(defcustom sdml-mode-ctags-output-file-name "tags"
  "The name of the generated tag file."
  :tag "Ctags output file name"
  :type 'file
  :group 'sdml)


;; --------------------------------------------------------------------------
;; Universal Ctag generation
;; --------------------------------------------------------------------------

(defun sdml-mode-ctags-tag-file-path (&optional file-path)
  "Return a path to a tag file for the current buffer.

If FILE-PATH is not provided the file name of the current buffer is used
instead.

If the `company-ctags' package is loaded, use the function
`company-ctags-find-table' to find a tag file location.

If the `projectile' package is loaded, use the variable
`projectile-project-root' to determine the directory in which to
put the tag file named `sdml-mode-ctags-output-file-name'.

If neither of these are present the directory containing FILE-NAME
will be used as the location for the tag file named
 `sdml-mode-ctags-output-file-name'."
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
  "Generate a TAGS file for the current SDML project.

This command executes the Universal Ctags executable specified in
`sdml-mode-ctags-command' to create a tag file determined by the
function `sdml-mode-ctags-tag-file-path'."
  (interactive)
  (let ((tag-file-path (sdml-mode-ctags-tag-file-path)))
    (shell-command (format "%s -R -e -o %s" sdml-mode-ctags-command tag-file-path))))


;; --------------------------------------------------------------------------
;; Key Bindings
;; --------------------------------------------------------------------------

(defvar sdml-mode-ctags-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s g") 'sdml-mode-ctags-generate)
    map)
  "Key map for SDML ctags minor mode.")

;; --------------------------------------------------------------------------
;; Ctags Minor Mode
;; --------------------------------------------------------------------------

;;;###autoload
(define-minor-mode
  sdml-mode-ctags-mode
  "Minor mode to provide tagging of SDML source.

Key bindings:
  {sdml-mode-ctags-mode-map}"

  :group 'sdml

  :tag "Enable SDML tagging minor mode"

  :lighter nil

  (add-to-list 'minor-mode-map-alist
               (cons 'sdml-mode-ctags-mode
                     sdml-mode-ctags-mode-map))

  (when (featurep 'company-ctags)
    (add-to-list 'company-ctags-modes 'sdml-mode-ctags-mode)))

(provide 'sdml-mode-ctags)

;;; sdml-mode-ctags.el ends here
