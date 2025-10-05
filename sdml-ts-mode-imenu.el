;;; sdml-ts-mode-imenu.el --- Internal imenu support -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>
;; License: see sdml-ts-mode.el

;;; Commentary:

;; Internal module to organize imenu functionality.

;;; Code:

(require 'treesit)

;; --------------------------------------------------------------------------
;; Tree-Sitter ❱ iMenu ❱ Rules
;; --------------------------------------------------------------------------

(when sdml-ts-mode--debug-mode
  (makunbound 'sdml-ts-mode-imenu--rules)
  (imenu-flush-cache))

(defvar sdml-ts-mode-imenu--rules
  '())

;; --------------------------------------------------------------------------
;; Tree-Sitter ❱ iMenu ❱ Setup function
;; --------------------------------------------------------------------------

;;;###autoload
(defun sdml-ts-mode-imenu-setup ()
  "Setup treesit imenu integration."
  (when sdml-ts-mode-imenu--rules
    (setq-local
     treesit-simple-imenu-settings
     sdml-ts-mode-imenu--rules)))

(provide 'sdml-ts-mode-imenu)


;;; sdml-ts-mode-imenu.el ends here
