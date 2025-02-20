;;; init.el -*- lexical-binding: t; -*-

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
       ;; deft
       doom
       hl-todo
       ;; indent-guides
       ;; ligatures
       modeline
       ophints
       (popup +defaults)
       (vc-gutter +pretty)
       ;; vi-tilde-fringe
       window-select

       :editor
       (evil +everywhere)
       file-templates
       ;; fold
       (format +onsave +lsp)
       lispy
       ;; rotate-text
       snippets
       ;; word-wrap

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
       ;; (spell +flyspell)
       ;; grammar

       :tools
       ;; ansible
       ;; biblio
       ;; collab
       debugger
       ;; direnv
       ;; docker
       ;; editorconfig
       eval
       (lookup +dictionary)
       (lsp +eglot)
       (magit +forge)
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
       graphviz
       ;; (haskell +lsp +tree-sitter)
       (json +lsp +tree-sitter)
       ;; (javascript +lsp +tree-sitter)
       (lfe +lsp)
       (lua +fennel +lsp +tree-sitter)
       markdown
       ;; nim
       (ocaml +tree-sitter)
       (org +pandoc)
       ;; python
       ;; (racket +lsp)
       ;; rest
       ;; (rust +lsp +tree-sitter)
       ;; (scheme +guile +gambit)
       (sh +fish +lsp +tree-sitter)
       ;; solidity
       ;; swift
       (web +lsp +tree-sitter)
       ;; (yaml +lsp +tree-sitter)
       ;; zig

       :email
       ;; (mu4e +org +gmail)

       :app
       ;; calendar
       ;; emms
       everywhere
       ;; irc
       ;; (rss +org)

       :config
       (default +bindings +smartparens +gnupg))

;;  ____________________________________________________________________________
;;; DECLARATIONS

(defvar my-theme-light nil "The default light theme.")
(defvar my-theme-dark nil "The default dark theme.")
(defvar my-frame-opacity 100 "The default frame opacity.")

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
