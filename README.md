# Emacs SDML Mode

This package provides an Emacs tree-sitter based major mode for SDML - the
[https://github.com/johnstonskj/tree-sitter-sdml](Simple Domain Modeling Language).


# Installing

Currently the package is not published and so installation has to be done manually. First clone the Git repository to a
local path.

```bash
> git clone https://github.com/johnstonskj/emacs-sdml-mode.git
```

The following uses `use-package` but any equivalent package manager should work. The function `sdml-mode-setup` ensures
the installation of the SDML parser with the core `tree-sitter` package.

```elisp
(use-package sdml-mode
  :load-path "/path/to/repo"
  :config (sdml-mode-setup))
```

Additionally you will need to manually install the parser at this time. Start by cloning the repository.

```bash
> git clone https://github.com/johnstonskj/tree-sitter-sdml.git
```

Build just the parser dynamic library, but importantly you need to select a specific ABI version.

```bash
> TS_GENERATE_ABI=13 make build_parser
```

Finally copy the dynamic library to a location in the path specified by the Emacs variable `tree-sitter-load-path`.

```base
> cp build/libtree-sitter-sdml.dylib ~/.tree-sitter/bin/sdml.dylib
```

# Usage

Once installed the major mode should be used for any file ending in `.sdm` or `.sdml` with highlighting and indentation
support.

## Highlighting

Syntax highlighting is provided by the `tree-sitter-hl-mode` minor mode based on the configuration in the constant
`sdml-mode-tree-sitter-hl-patterns`.

## Indentation

Line indentation is provided by the `tree-sitter-indent-mode` minor mode based on the configuration in the constant
`sdml-mode-folding-definitions`.

## Code Folding

Block Folding is provided by the `ts-fold-mode` minor mode based on the configuration in the constant
`tree-sitter-indent-sdml-scopes`. Note that folding of groups of line comments is also supported.

* `C-c C-s -` -- fold item
* `C-c C-s +` -- unfold item
* `C-c C-s C--` -- fold all items in buffer
* `C-c C-s C-+` -- unfold all items in buffer
* `C-c C-s /` -- unfold item and all children
* `C-c C-s .` -- toggle fold/unfold state

As well as the mechanics of folding, the `ts-fold` package also has indicator support for windowed clients and this is
enabled by default with `window-system` is non-nil.

## Debugging

* `C-c C-s d` -- open the tree-sitter debug view
* `C-c C-s q -- open the tree-sitter query builder
