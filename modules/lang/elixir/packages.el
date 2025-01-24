;;; lang/elixir/packages.el -*- no-byte-compile: t; -*-

(load! "lang/elixir/packages" doom-modules-dir)

(when (modulep! +iex)
  (package! inf-elixir
    :recipe (:host github :repo "J3RN/inf-elixir" :branch "master")))

(when (modulep! +mix)
  (package! mix))
