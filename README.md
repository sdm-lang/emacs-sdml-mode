# Emacs SDML Mode

This package provides an Emacs tree-sitter based major mode for SDML - the
[https://github.com/johnstonskj/tree-sitter-sdml](Simple Domain Modeling Language).


# Installing

``` elisp
(use-package sdml-mode :ensure t)
```

# Usage

Once installed the major mode should be used for any file ending in `.sdm`
or `.sdml` with highlighting and indentation support.


# Dependencies

This package depends upon the following:

- `tree-sitter` :: the core parser support.
- `tree-sitter-hl` :: font-lock highlighting using `tree-sitter` queries.
- `tree-sitter-indent` :: indentation support  using `tree-sitter` queries.
- `tree-sitter-ispell` :: spell chqecking for text content using `tree-sitter` queries.
