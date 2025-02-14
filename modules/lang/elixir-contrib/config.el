;;; lang/elixir-contrib/config.el -*- lexical-binding: t; -*-

(when (modulep! :lang elixir)
  (when (modulep! :tools lsp +eglot)
    (after! eglot
    (pushnew! eglot-server-programs '((elixir-mode) . ("elixir-ls")))
    (add-hook! 'elixir-mode-hook #'eglot-ensure)
    (setq-hook! 'elixir-mode-hook eglot-connect-timeout 60)))
  (when (modulep! +iex) (load! "+iex"))
  (when (modulep! +mix) (load! "+mix")))
