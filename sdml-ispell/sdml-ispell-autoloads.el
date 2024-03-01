;;; sdml-ispell-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck_sdml-ispell" "flycheck_sdml-ispell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flycheck_sdml-ispell.el

(autoload 'sdml-ispell-mode "flycheck_sdml-ispell" "\
Minor mode to allow ispell checking in SDML text content.

This is a minor mode.  If called interactively, toggle the
`Sdml-Ispell mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `sdml-ispell-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Key bindings:
\\{sdml-ispell-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'sdml-ispell-setup "flycheck_sdml-ispell" "\
Setup the mode, adding configuration to `tree-sitter-ispell'." t nil)

(register-definition-prefixes "flycheck_sdml-ispell" '("sdml-ispell-"))

;;;***

;;;### (autoloads nil "sdml-ispell" "sdml-ispell.el" (0 0 0 0))
;;; Generated autoloads from sdml-ispell.el

(autoload 'sdml-ispell-mode "sdml-ispell" "\
Minor mode to allow ispell checking in SDML text content.

This is a minor mode.  If called interactively, toggle the
`Sdml-Ispell mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `sdml-ispell-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Key bindings:
\\{sdml-ispell-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'sdml-ispell-setup "sdml-ispell" "\
Setup the mode, adding configuration to `tree-sitter-ispell'." t nil)

(register-definition-prefixes "sdml-ispell" '("sdml-ispell-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sdml-ispell-autoloads.el ends here
