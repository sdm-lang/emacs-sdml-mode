;;; sdml-mode-hydra.el --- Hydra for SDML -*- lexical-binding: t; -*-

;; Author: Simon Johnston <johnstonskj@gmail.com>

;;; License:

;; Copyright (c) 2025 Simon Johnston
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

;; Definition of a Hydra interface for SDML actions.

;;; Code:

(require 'sdml-mode)
(require 'sdml-mode-ctags)
(require 'pretty-hydra)

(cond
 ((featurep 'pretty-hydra)
  (pretty-hydra-define sdml-mode-hydra
    (:color blue :quit-key "q" :title "SDML")
    ("Edit"
     (("t"     sdml-mode-current-buffer-dependency-tree  "Open dependency tree")
      ("T"     sdml-mode-current-buffer-dependency-graph "Open dependency graph")
      ("v"     sdml-mode-validate-current-buffer         "Validate current buffer")
      ("V"     sdml-mode-validate-file                   "Validate file")
      ("g"     sdml-mode-ctags-generate                  "Generate ctags")
      ("f"     flycheck-list-errors                      "Open Flycheck errors")
      ("n"     display-line-numbers-mode                 "Toggle line numbers" :toggle t))
     "Document"
     (("d d"   sdml-mode-document-module                 "Document module")
      ("d b"   sdml-mode-document-project                "Document project")
      ("d c"   sdml-mode-draw-concept                    "Draw concept diagram")
      ("d e"   sdml-mode-draw-entities                   "Draw E/R diagram")
      ("d u"   sdml-mode-draw-uml                        "Draw UML diagram"))
     "Generate"
     (("c r"   sdml-mode-generate-rdf                    "Generate RDF representation")
      ("c s"   sdml-mode-generate-scheme                 "Generate scheme representation")
      ("c j"   sdml-mode-generate-json                   "Generate JSON representation")
      ("c t"   sdml-mode-generate-with-tera              "Generate using Tera templates"))
     "Debug"
     (("h d"   tree-sitter-debug-mode                    "Toggle ts debug tree mode" :toggle t)
      ("h q"   tree-sitter-query-builder                 "Open ts query builder"))))
  (define-key sdml-mode-map (kbd "C-c C-s h") 'sdml-mode-hydra/body)
  (define-key sdml-mode-map (kbd "f9 s") 'sdml-mode-hydra/body))

 ;;    ((featurep 'hydra) (message "plain")
 ;;     (defhydra sdml-mode-hydra (:color pink :hint nil)
 ;;       "
 ;; ^Edit^                       ^Document^           ^Tree-Sitter^
 ;; ^^^^^^^^-----------------------------------------------------------------
 ;; _t_: dependency tree         _u_: unmark          _d_: debug tree
 ;; _T_: dependency graph        _U_: unmark up       _q_: query builder
 ;; _v_: validate buffer
 ;; _V_: validate file
 ;; "
 ;;       ("t" sdml-mode-current-buffer-dependency-tree)
 ;;       ("T" sdml-mode-current-buffer-dependency-graph)
 ;;       ("v" sdml-mode-validate-current-buffer)
 ;;       ("V" sdml-mode-validate-file)
 ;;       ("u" nil)
 ;;       ("U" Buffer-menu-save)
 ;;       ("d" tree-sitter-debug-mode)
 ;;       ("q" tree-sitter-query-builder)))
 (t
  (message "Install 'hydra or 'pretty-hydra")))

(provide 'sdml-mode-hydra)

;;; sdml-mode-hydra.el ends here
