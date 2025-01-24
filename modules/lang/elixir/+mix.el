;;; lang/elixir/+mix.el -*- lexical-binding: t; -*-

(use-package! mix
  :defer t
  :config
  (defun mix-enable-maybe-h ()
    (when (locate-dominating-file default-directory "mix.exs")
      (map! :localleader
            (:map elixir-mode-map
                  (:prefix ("m" . "mix")
                   :n "c" #'mix-compile
                   :n "l" #'mix-last-command
                   :n "m" #'mix-execute-task
                   :n "t" #'mix-test)))
      (mix-minor-mode 1)))
  :hook
  (elixir-mode . mix-enable-maybe-h))
