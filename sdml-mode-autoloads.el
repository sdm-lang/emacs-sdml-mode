;;; sdml-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck_sdml-mode-cli" "flycheck_sdml-mode-cli.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flycheck_sdml-mode-cli.el

(register-definition-prefixes "flycheck_sdml-mode-cli" '("sdml-cli-"))

;;;***

;;;### (autoloads nil "sdml-mode" "sdml-mode.el" (0 0 0 0))
;;; Generated autoloads from sdml-mode.el

(autoload 'sdml-mode "sdml-mode" "\
Major mode for editing SDML files.

  Key bindings:
  \\{sdml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.sdml?\\'" . sdml-mode))

(register-definition-prefixes "sdml-mode" '("sdml-mode-"))

;;;***

;;;### (autoloads nil "sdml-mode-abbrev" "sdml-mode-abbrev.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from sdml-mode-abbrev.el

(register-definition-prefixes "sdml-mode-abbrev" '("sdml-mode-abbrev-"))

;;;***

;;;### (autoloads nil "sdml-mode-cli" "sdml-mode-cli.el" (0 0 0 0))
;;; Generated autoloads from sdml-mode-cli.el

(register-definition-prefixes "sdml-mode-cli" '("sdml-cli-"))

;;;***

;;;### (autoloads nil "sdml-mode-hl" "sdml-mode-hl.el" (0 0 0 0))
;;; Generated autoloads from sdml-mode-hl.el

(register-definition-prefixes "sdml-mode-hl" '("sdml-mode-"))

;;;***

;;;### (autoloads nil "sdml-mode-indent" "sdml-mode-indent.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from sdml-mode-indent.el

(register-definition-prefixes "sdml-mode-indent" '("sdml-mode-indent-"))

;;;***


;;; Generated autoloads from flycheck_sdml-mode-cli.el

(register-definition-prefixes "flycheck_sdml-mode-cli" '("sdml-mode-cli-"))


;;; Generated autoloads from flycheck_sdml-mode-indent.el

(autoload 'sdml-mode-indent-mode "flycheck_sdml-mode-indent" "\
Minor mode to provide indentation when editing SDML source.

This is a minor mode.  If called interactively, toggle the
`Sdml-Mode-Indent mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `sdml-mode-indent-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "flycheck_sdml-mode-indent" '("sdml-mode-indent-"))


;;; Generated autoloads from flycheck_sdml-mode.el

(autoload 'sdml-mode "flycheck_sdml-mode" "\
A major mode for editing SDML (Simple Domain Modeling Language) files.

This major mode will, by default, enable the following minor modes:

- `abbrev-mode'
- `prettify-symbols-mode' (see `sdml-mode-prettify-symbols-alist')
- `tree-sitter-mode'
- `sdml-mode-ctags-mode'

  Key bindings:
  \\{sdml-mode-map}

(fn)" t)
(add-to-list 'auto-mode-alist '("\\.sdml?\\'" . sdml-mode))
(register-definition-prefixes "flycheck_sdml-mode" '("sdml-mode-"))


;;; Generated autoloads from sdml-mode-cli.el

(register-definition-prefixes "sdml-mode-cli" '("sdml-mode-cli-"))


;;; Generated autoloads from sdml-mode.el

(autoload 'sdml-mode "sdml-mode" "\
A major mode for editing SDML (Simple Domain Modeling Language) files.

This major mode will, by default, enable the following minor modes:

- `abbrev-mode'
- `prettify-symbols-mode' (see `sdml-mode-prettify-symbols-alist')
- `tree-sitter-mode'
- `sdml-mode-hl-mode'
- `sdml-mode-indent-mode'
- `sdml-mode-ctags-mode'

  Key bindings:
  \\{sdml-mode-map}

(fn)" t)
(add-to-list 'auto-mode-alist '("\\.sdml?\\'" . sdml-mode))
(register-definition-prefixes "sdml-mode" '("sdml-mode-"))


;;; Generated autoloads from sdml-mode-indent.el

(autoload 'sdml-mode-indent-mode "sdml-mode-indent" "\
Minor mode to provide indentation when editing SDML source.

This is a minor mode.  If called interactively, toggle the
`Sdml-Mode-Indent mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `sdml-mode-indent-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "sdml-mode-indent" '("sdml-mode-indent-"))


;;; Generated autoloads from flycheck_sdml-mode-hl.el

(autoload 'sdml-mode-hl-mode "flycheck_sdml-mode-hl" "\
Minor mode to provide highlighting of SDML source.

This is a minor mode.  If called interactively, toggle the
`Sdml-Mode-Hl mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `sdml-mode-hl-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "flycheck_sdml-mode-hl" '("sdml-mode-"))


;;; Generated autoloads from sdml-mode-hl.el

(autoload 'sdml-mode-hl-mode "sdml-mode-hl" "\
Minor mode to provide highlighting of SDML source.

This is a minor mode.  If called interactively, toggle the
`Sdml-Mode-Hl mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `sdml-mode-hl-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "sdml-mode-hl" '("sdml-mode-"))


;;; Generated autoloads from flycheck_sdml-mode-abbrev.el

(register-definition-prefixes "flycheck_sdml-mode-abbrev" '("sdml-mode-abbrev-"))


;;; Generated autoloads from sdml-mode-abbrev.el

(register-definition-prefixes "sdml-mode-abbrev" '("sdml-mode-abbrev-"))


;;; Generated autoloads from flycheck_sdml-mode-ctags.el

(autoload 'sdml-mode-ctags-mode "flycheck_sdml-mode-ctags" "\
Minor mode to provide tagging of SDML source.

Key bindings:
  {sdml-mode-ctags-mode-map}

This is a minor mode.  If called interactively, toggle the
`Sdml-Mode-Ctags mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `sdml-mode-ctags-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "flycheck_sdml-mode-ctags" '("sdml-mode-ctags-"))


;;; Generated autoloads from sdml-mode-ctags.el

(autoload 'sdml-mode-ctags-mode "sdml-mode-ctags" "\
Minor mode to provide tagging of SDML source.

Key bindings:
  {sdml-mode-ctags-mode-map}

This is a minor mode.  If called interactively, toggle the
`Sdml-Mode-Ctags mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `sdml-mode-ctags-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "sdml-mode-ctags" '("sdml-mode-ctags-"))


;;; Generated autoloads from flycheck_sdml-mode-hydra.el

(register-definition-prefixes "flycheck_sdml-mode-hydra" '("sdml-hydra-"))


;;; Generated autoloads from sdml-mode-hydra.el

(register-definition-prefixes "sdml-mode-hydra" '("sdml-hydra-"))



;;; Generated autoloads from flycheck_sdml.el

(register-definition-prefixes "flycheck_sdml" '("sdml--font-lock-settings"))


;;; Generated autoloads from sdml.el

(register-definition-prefixes "sdml" '("sdml--"))



;;; Generated autoloads from flycheck_sdml-ts-mode.el

(autoload 'sdml-ts-mode "flycheck_sdml-ts-mode" "\
A major mode for editing SDML (Simple Domain Modeling Language) files.

Key bindings:

\\{sdml-ts-mode-map}

This major mode will, by default, enable the following minor modes:

- `abbrev-mode' (see `sdml-ts-mode-abbrev-table')
- `prettify-symbols-mode' (see `sdml-ts-mode-prettify-symbols-alist')
- `treesit-mode'
- `treesit-fold-mode'
- `treesit-fold-indicators-mode'
- `treesit-fold-line-comment-mode'

(fn)" t)
(when (treesit-available-p) (unless (assoc 'sdml treesit-language-source-alist) (add-to-list 'treesit-language-source-alist '(sdml "https://github.com/sdm-lang/tree-sitter-sdml"))) (add-to-list 'auto-mode-alist '("\\.sdml?\\'" . sdml-ts-mode)))
(register-definition-prefixes "flycheck_sdml-ts-mode" '("sdml-ts-mode-"))


;;; Generated autoloads from sdml-ts-mode.el

(autoload 'sdml-ts-mode "sdml-ts-mode" "\
A major mode for editing SDML (Simple Domain Modeling Language) files.

Key bindings:

\\{sdml-ts-mode-map}

This major mode will, by default, enable the following minor modes:

- `abbrev-mode' (see `sdml-ts-mode-abbrev-table')
- `prettify-symbols-mode' (see `sdml-ts-mode-prettify-symbols-alist')
- `treesit-mode'
- `treesit-fold-mode'
- `treesit-fold-indicators-mode'
- `treesit-fold-line-comment-mode'

(fn)" t)
(when (treesit-available-p) (unless (assoc 'sdml treesit-language-source-alist) (add-to-list 'treesit-language-source-alist '(sdml "https://github.com/sdm-lang/tree-sitter-sdml"))) (add-to-list 'auto-mode-alist '("\\.sdml?\\'" . sdml-ts-mode)))
(register-definition-prefixes "sdml-ts-mode" '("sdml-ts-mode-"))


;;; Generated autoloads from sdml-ts-mode-fold.el

(autoload 'sdml-ts-mode-fold-setup "sdml-ts-mode-fold" "\
Setup `treesit-fold-mode' and add mode hooks.")
(register-definition-prefixes "sdml-ts-mode-fold" '("sdml-ts-mode-"))


;;; Generated autoloads from sdml-ts-mode-indent.el

(autoload 'sdml-ts-mode-indent-setup "sdml-ts-mode-indent" "\
Setup `treesit'-based indentation.")
(register-definition-prefixes "sdml-ts-mode-indent" '("sdml-ts-mode-indent-"))


;;; Generated autoloads from sdml-ts-mode-font-lock.el

(autoload 'sdml-ts-mode-font-lock-setup "sdml-ts-mode-font-lock" "\
Setup `treesit'-based font-lock highlighting.")
(register-definition-prefixes "sdml-ts-mode-font-lock" '("sdml-ts-mode-font-lock--"))


;;; Generated autoloads from sdml-ts-mode-abbrev.el

(register-definition-prefixes "sdml-ts-mode-abbrev" '("sdml-ts-mode-abbrev-"))


;;; Generated autoloads from sdml-ts-mode-imenu.el

(autoload 'sdml-ts-mode-imenu-setup "sdml-ts-mode-imenu" "\
Setup treesit imenu integration.")
(register-definition-prefixes "sdml-ts-mode-imenu" '("sdml-ts-mode-imenu--rules"))



;;; Generated autoloads from sdml-ts-mode-query.el

(register-definition-prefixes "sdml-ts-mode-query" '("sdml-ts-mode-query--"))

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sdml-mode-autoloads.el ends here
