;;; lang/elixir-contrib/packages.el -*- no-byte-compile: t; -*-

(when (modulep! +iex)
  (package! inf-elixir
    :recipe (:host github :repo "J3RN/inf-elixir" :branch "master")))

(when (modulep! +mix)
  (package! mix))
