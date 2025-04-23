;;; lang/elixir-contrib/+iex.el -*- lexical-binding: t; -*-

(use-package! inf-elixir
  :autoload
  inf-elixir--find-project-root

  :init

  (set-repl-handler! 'elixir-mode #'inf-elixir-run)
  (set-eval-handler! 'elixir-mode #'inf-elixir-send-region)

  (defun inf-elixir-run ()
    "Starts IEx in the project context when a Mix project is detected.
The commands are `inf-elixir-project-command' (defaults to \"iex -S mix\") and
`inf-elixir-base-command' (defaults to \"iex\")."
    (interactive)
    ;; Return the project root directory if a mix.exs file is found
    (if (inf-elixir--find-project-root)
        (progn (message "inf-elixir: IEx running in project context...")
               (inf-elixir-project))
      (progn (message "inf-elixir: IEx running standalone...")
             (inf-elixir))))

  (defalias 'run-elixir #'inf-elixir-run "Alias for `inf-elixir-run'")

  :config

  (set-popup-rule! "^\\*Inf-Elixir.*\\*" :size 0.35 :quit nil :ttl nil)
  (setq! inf-elixir-switch-to-repl-on-send t)

  (defun inf-elixir-switch-to-repl ()
    "Switch to IEx buffer."
    (interactive)
    (when-let* ((buf (inf-elixir--determine-repl-buf)))
      (pop-to-buffer buf)))

  (defun inf-elixir-recompile ()
    "Send `IEx.Helpers.recompile/1' to recompile the current Mix project.
This function simply recompiles Elixir modules, without reloading configuration
or restarting applications. This means any long running process may crash on
recompilation, as changed modules will be temporarily removed and recompiled,
without going through the proper code change callback."
    (interactive)
    (inf-elixir--send (format "recompile()"))
    (if inf-elixir-switch-to-repl-on-send
        (goto-char (point-max))))

  (defun inf-elixir-observer ()
    "Start the Erlang Observer in IEx."
    (interactive)
    (inf-elixir--send (format ":observer.start()")))

  (defun inf-elixir-self ()
    "Return the PID of the IEx process."
    (interactive)
    (inf-elixir--send (format "self()")))

  (defun inf-elixir-flush ()
    "Flush the IEx mailbox."
    (interactive)
    (inf-elixir--send (format "flush()")))

  (map! :localleader
        (:map elixir-mode-map
         :n "'" #'inf-elixir-run
         :n "SPC" #'inf-elixir-switch-to-repl
         :n "r" #'inf-elixir-reload-module
         :n "c" #'inf-elixir-recompile
         :n "o" #'inf-elixir-observer
         :n "s" #'inf-elixir-self
         :n "f" #'inf-elixir-flush
         (:prefix ("e" . "eval")
          :n "b" #'inf-elixir-send-buffer
          :n "l" #'inf-elixir-send-line
          :v "r" #'inf-elixir-send-region))
        (:map inf-elixir-mode-map
         :n "r" #'inf-elixir-reload-module
         :n "c" #'inf-elixir-recompile
         :n "o" #'inf-elixir-observer
         :n "s" #'inf-elixir-self
         :n "f" #'inf-elixir-flush
         ))

  :hook

  (elixir-mode . inf-elixir-minor-mode)
  (inf-elixir-mode . rainbow-delimiters-mode))
