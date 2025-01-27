;;; lang/elixir/config.el -*- lexical-binding: t; -*-

(load! "lang/elixir/config" doom-modules-dir)

(after! eglot
  (pushnew! eglot-server-programs
            '((elixir-mode) . ("elixir-ls")))
  (add-hook! 'elixir-mode-hook
             #'eglot-ensure)
  (setq-hook! 'elixir-mode-hook
    eglot-connect-timeout 60))

(when (modulep! +iex) (load! "+iex"))

(when (modulep! +mix) (load! "+mix"))
