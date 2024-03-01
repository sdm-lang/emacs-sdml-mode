;;; sdml-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck_sdml-cli" "flycheck_sdml-cli.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flycheck_sdml-cli.el

(register-definition-prefixes "flycheck_sdml-cli" '("sdml-cli-"))

;;;***

;;;### (autoloads nil "sdml-cli" "sdml-cli.el" (0 0 0 0))
;;; Generated autoloads from sdml-cli.el

(register-definition-prefixes "sdml-cli" '("sdml-cli-"))

;;;***

;;;### (autoloads nil "sdml-mode" "sdml-mode.el" (0 0 0 0))
;;; Generated autoloads from sdml-mode.el

(autoload 'sdml-mode "sdml-mode" "\
Major mode for editing SDML files.

  Key bindings:
  \\{sdml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.sdml?\\'" . sdml-mode))

(register-definition-prefixes "sdml-mode" '("sdml-mode-" "tree-sitter-indent-sdml-scopes"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sdml-mode-autoloads.el ends here
