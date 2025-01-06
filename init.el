;;; init.el -*- lexical-binding: t; -*-

;;  ____________________________________________________________________________
;;; COMPILATION SETTINGS

(setq! package-native-compile t)
;; (setq! native-comp-jit-compilation nil)

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
       ;; doom-quit
       ;; (emoji +unicode)
       hl-todo
       indent-guides
       ;; ligatures
       modeline
       ophints
       (popup +defaults)
       ;; tabs
       treemacs
       (vc-gutter +pretty)
       vi-tilde-fringe
       ;; (window-select +switch-window)
       workspaces
       ;; zen

       :editor
       (evil +everywhere)
       file-templates
       ;; fold
       (format +onsave +lsp)
       lispy
       ;; objed
       ;; rotate-text
       snippets
       word-wrap

       :emacs
       (dired +icons)
       electric
       ;; eww
       (ibuffer +icons)
       undo
       vc

       :term
       eshell
       ;; shell
       ;; term
       vterm

       :checkers
       (syntax +flymake +icons)
       ;; (spell +flyspell)
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
       emacs-lisp
       (erlang +lsp +tree-sitter)
       ;; (haskell +lsp +tree-sitter)
       (json +lsp +tree-sitter)
       ;; (javascript +lsp +tree-sitter)
       (lua +fennel +lsp +tree-sitter)
       (markdown)
       ;; nim
       (ocaml +lsp +tree-sitter)
       (org +pandoc +passwords)
       graphviz
       ;; python
       (racket +lsp +xp)
       ;; rest
       ;; (rust +lsp +tree-sitter)
       (scheme +guile)
       (sh +fish +lsp +tree-sitter)
       ;; solidity
       ;; swift
       (web +lsp +tree-sitter)
       (yaml +lsp +tree-sitter)
       ;; zig

       :email
       ;; (mu4e +org +gmail)

       :app
       ;; calendar
       ;; emms
       ;; everywhere
       ;; irc
       ;; (rss +org)

       :config
       (default +bindings +gnupg +smartparens))

;;  ____________________________________________________________________________
;;; DEFINITIONS

(defvar doom-theme-light 'doom-bluloco-light
  "Specifies the default light Doom Emacs theme.")
(defvar doom-theme-dark 'doom-bluloco-dark
  "Specifies the default dark Doom Emacs theme.")

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
