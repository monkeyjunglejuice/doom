;;; init.sl -*- lexical-binding: t; -*-

;;  ____________________________________________________________________________
;;; COMPILATION SETTINGS

;; Compile ahead of time to avoid lagging caused by sudden compilation
;; (setq! package-native-compile t)
;; (setq! native-comp-jit-compilation nil)

;; Always use these additional arguments to sync or upgrade Doom Emacs:
;; doom sync --gc --aot
;; doom upgrade --aot

;;  ____________________________________________________________________________
;;; DOOM MODULES

(doom! :input
       ;; layout

       :completion
       (corfu +orderless +icons +dabbrev)
       (vertico +icons)

       :ui
       doom
       hl-todo
       (modeline +light)
       ophints
       (popup +custom)
       (vc-gutter +pretty)
       (window-select +switch-window)

       :editor
       (evil +everywhere)
       file-templates
       (format +onsave +lsp)
       lispy
       snippets

       :emacs
       (dired +icons)
       electric
       eww
       (ibuffer +icons)
       undo
       vc

       :term
       eshell
       vterm

       :checkers
       (syntax +flymake)
       spell
       ;; grammar

       :tools
       ;; ansible
       ;; biblio
       ;; collab
       ;; debugger
       ;; direnv
       ;; docker
       ;; editorconfig
       eval
       (lookup +dictionary)
       llm
       (lsp +eglot)
       magit
       make
       ;; pass
       pdf
       tree-sitter
       ;; upload

       :os
       (:if (featurep :system 'macos) macos)
       (tty +osc)

       :lang
       ;; (cc +lsp +tree-sitter)
       common-lisp
       data
       (elixir +lsp +tree-sitter)
       (elixir-contrib +iex +mix)
       emacs-lisp
       (erlang +lsp +tree-sitter)
       ;; gleam
       ;; graphviz
       ;; (haskell +lsp +tree-sitter)
       (json +lsp +tree-sitter)
       ;; (javascript +lsp +tree-sitter)
       ;; (julia +lsp +tree-sitter +snail)
       ;; (lua +fennel +lsp +tree-sitter)
       markdown
       ;; (ocaml +tree-sitter)
       (org +pandoc)
       ;; (python +lsp +tree-sitter)
       ;; (rest +jq)
       ;; (rust +lsp +tree-sitter)
       (scheme +guile)
       (sh +lsp +tree-sitter)
       ;; (swift +lsp +tree-sitter)
       (web +lsp +tree-sitter)
       (yaml +lsp +tree-sitter)
       ;; (zig +lsp +tree-sitter)

       :email
       ;; (mu4e +org +gmail)

       :app
       ;; calendar
       everywhere
       ;; irc
       ;; (rss +org)

       :config
       (default +bindings +smartparens +gnupg))

;;  ____________________________________________________________________________
;;; COMMENTARY

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).
