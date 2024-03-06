# SDML Mode for Emacs

![SDML Logo Text](https://raw.githubusercontent.com/sdm-lang/.github/main/profile/horizontal-text.svg)

This package provides an Emacs tree-sitter based major mode for SDML - the
[Simple Domain Modeling Language](https://github.com/johnstonskj/tree-sitter-sdml).


## Installing

Currently the package is not published and so installation has to be done manually.

### Install manually

First clone the Git repository to a local path.

```bash
    git clone https://github.com/johnstonskj/emacs-sdml-mode.git
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
    git clone https://github.com/johnstonskj/tree-sitter-sdml.git
```

Build just the parser dynamic library, but importantly you need to select a specific ABI version.

```bash
    TS_GENERATE_ABI=13 make build_parser
```

Finally copy the dynamic library to a location in the path specified by the Emacs variable `tree-sitter-load-path`.

```bash
    cp build/libtree-sitter-sdml.dylib ~/.tree-sitter/bin/sdml.dylib
```

## Usage

Once installed the major mode should be used for any file ending in `.sdm` or `.sdml` with highlighting and indentation
support.

### Highlighting

Syntax highlighting is provided by the `tree-sitter-hl-mode` minor mode based on the configuration in the constant
`sdml-mode-tree-sitter-hl-patterns`.

The `sdml-mode` also adds to the `prettify-symbols-alist` list, the set of symbols is in the custom variable
`sdml-prettify-symbols-alist`.

![Syntax Highlighting](./images/emacs-editing.png)

### Indentation

Line indentation is provided by the `tree-sitter-indent-mode` minor mode based on the configuration in the constant
`sdml-mode-folding-definitions`.

Default indentation is two spaces, although this can be overridden by the custom variable `sdml-mode-indent-offset`.

### Code Folding

Block Folding is provided by the `ts-fold-mode` minor mode based on the configuration in the constant
`sdml-mode-tree-sitter-indent-scopes`. Note that folding of groups of line comments is also supported.

* `C-c C-s -` -- fold item
* `C-c C-s +` -- unfold item
* `C-c C-s C--` -- fold all items in buffer
* `C-c C-s C-+` -- unfold all items in buffer
* `C-c C-s /` -- unfold item and all children
* `C-c C-s .` -- toggle fold/unfold state

As well as the mechanics of folding, the `ts-fold` package also has a fringe indicator support for windowed clients and this is
enabled by default with `window-system` is non-`nil`.

To switch to left/right fringe (default is left-fringe):

    (setq ts-fold-indicators-fringe 'right-fringe)

To lower/higher the fringe overlay's priority (default is 30):

    (setq ts-fold-indicators-priority 30)

### Abbreviations and Skeletons

This package creates a new `abbrev-table`, named `sdml-mode-abbrev-table`, which provides a number of useful skeletons for
the following. `abbrev-mode` is enabled by `sdml-mode` and when typing one of the abbreviations below type space to expand.

Typing `d t SPC` will prompt for a name and expand into the SDML declaration `datatype MyName ← opaque _` where the
underscore character represents the new cursor position.

**Declarations**: `mo`=module, `dt`=datatype, `en`=enum, `ev`=event, `pr`=property, `st`=structure, `un`=union

**Annotation Properties**: `pal`=skos:altLabel, `pdf`=skos:definition, `ped`=skos:editorialNote, `ppl`=skos:prefLabel, `pco`=rdfs:comment

**Constraints**: `ci`=informal, `cf`=formal, `all`=universal, `any`=existential

**Datatypes**: `db`=boolean, `dd`=decimal, `df`=double, `dh`=binary, `di`=integer, `sd`=string, `du`=unsigned

Note that for annotation properties with language string values the skeletons will add the value of the Emacs
variable `locale-language` as the language tag.

### Debugging

* `C-c C-s d` -- open the tree-sitter debug view
* `C-c C-s q` -- open the tree-sitter query builder

# Add-Ons

## Ispell

The additional package `sdml-ispell` provides *selective* spell checking by only checking selected nodes in the tree.

```elisp
(use-package sdml-ispell
  :after sdml-mode
  :load-path "/path/to/repo"
  :config (sdml-ispell-setup))
```

* `C-c C-s s` -- spell check the item at point
* `C-c C-s C-s` -- spell check all items in the buffer

By default only strings and comments are checked, although this can be overridden by the custom variable
`tree-sitter-ispell-sdml-text-mapping`.

### Flycheck

The additional package `flycheck-sdml` provides on-the-fly linting for SDML buffers.

```elisp
(use-package flycheck-sdml
  :after (flycheck sdml-mode)
  :load-path "/path/to/repo"
  :config (flycheck-sdml-setup))
```

To enable, simply ensure Flycheck mode is enabled for your buffer. Rather than per-buffer, you can enable this by
setting `flycheck-mode` for all SDML files with a hook.

```elisp
(use-package flycheck-sdml
  :after (flycheck sdml-mode)
  :load-path "/path/to/repo"
  :hook (sdml-mode . flycheck-mode)
  :config (flycheck-sdml-setup))
```

The entire set of lint rules are stored in the custom variable `sdml-lint-rules`.

### Org-Babel

Org-Babel support provides the ability to call the SDML [command-line tool](https://github.com/johnstonskj/rust-sdml) to
produce diagrams and more.

![Syntax Highlighting](./images/emacs-org-mode.png)

For example, the following source block calls the CLI to draw a concept diagram for the
enclosed module.

```
#+NAME: lst:rentals-example
#+CAPTION: Rentals Concepts
#+BEGIN_SRC sdml :cmdline draw --diagram concepts :file ./rentals-concepts.svg :exports both
module rentals is

  entity Vehicle

  entity Location

  entity Customer

  entity Booking

end
#+END_SRC
```

The results block then references the resulting image.

```
#+NAME: fig:rentals-example-concepts
#+CAPTION: Rentals Concepts
#+RESULTS: lst:rentals-example
[[file:./rentals-concepts.svg]]
```

But, what if we want to produce more than one diagram from the same source? By using the built-in
/[noweb](https://orgmode.org/manual/Noweb-Reference-Syntax.html)/ syntax we can create a new source block, but reference
the original content. This source block has different command-line parameters and has it's own results block as well.

```
#+NAME: fig:rentals-example-erd
#+BEGIN_SRC sdml :cmdline draw --diagram concepts :file ./rentals-erd.svg :exports results :noweb yes
<<lst:rentals-example>>
#+END_SRC
```

## Contributing

The packages in this repository should pass the standard package checks, including:

* `byte-compile-file`
* `package-lint`
* `checkdoc`

The following known *errors* are reported by package-lint due to the naming convention imposed by dependencies.

``` text
sdml-ispell.el with package-lint 20230525.1346:
__:0: error: "tree-sitter-ispell-sdml-text-mapping" doesn't start with package's prefix "sdml-ispell".

sdml-mode.el with byte-compile using Emacs 28.1:
__:0: error: "tree-sitter-hl-face:type.scope" doesn't start with package's prefix "sdml".
__:0: error: `tree-sitter-hl-face:type.scope' contains a non-standard separator `:', use hyphens instead (see Elisp Coding Conventions).
__:0: error: "tree-sitter-indent-sdml-scopes" doesn't start with package's prefix "sdml".

```


## License

This package is released under the Apache License, Version 2.0. See LICENSE file for details.

## Changes

The `0.1.x` series are all pre-release and do not appear in ELPA/MELPA.
