;;DOOMDIR/config.el -*- lexical-binding: t; -*-

;;  ____________________________________________________________________________
;;; FRAME PLACEMENT

;; Initial frame
(pushnew! initial-frame-alist
          '(width . 80)
          '(height . 54)
          '(left . 940)
          '(top . 0))

;; Default frame
(pushnew! default-frame-alist
          '(width . 102)
          '(height . 54)
          '(left . 0))

;;   ____________________________________________________________________________
;;; FONTS

(setq! doom-font
       (font-spec :family "FiraCode Nerd Font" :size 15 :weight 'normal)
       doom-serif-font
       (font-spec :family "FiraCode Nerd Font" :size 15 :weight 'light)
       doom-variable-pitch-font
       (font-spec :family "Fira Sans" :size 18))

;;  ____________________________________________________________________________
;;; THEMES

;; Add personal themes directory
(load! "~/.emacs.themes/init-themes.el")

;; Set light/dark theme
(setq! doom-theme-light 'doom-one-light)
(setq! doom-theme-dark 'doom-city-lights)

;; Switch between dark/light theme based on the system appearance
(defun my-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (setq! doom-theme
         (pcase appearance
           ('light (load-theme doom-theme-light t)
                   doom-theme-light)
           ('dark (load-theme doom-theme-dark t)
                  doom-theme-dark))))

(add-hook! 'ns-system-appearance-change-functions #'my-apply-theme)

;;  ____________________________________________________________________________
;;; KEYBINDINGS

(setq! doom-leader-key "SPC"
       doom-leader-alt-key "M-SPC"
       doom-localleader-key ","
       doom-localleader-alt-key "M-SPC ,")

;; doom-leader-map
(map! :leader
      :desc "M-x"                   ":"   nil
      :desc "Switch buffer"         "<"   nil
      :desc "Org capture"           "X"   nil
      :desc "Switch to last buffer" "`"   nil
      :desc "M-x"                   "m"   #'execute-extended-command
      :desc "Switch buffer"         ","   #'switch-to-buffer
      :desc "Lisp"                  "l"   #'sly
      :desc "Eshell"                "e"   #'+eshell/here
      :desc "IEx"                   "r"   #'inf-elixir)

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; MACOS

;; Make the <Command> key on MacOS act as <Ctrl> key: "C- ..."
(setq! mac-command-modifier 'control)

;; make the <Option> key on MacOS act as <Meta> key for "M- ..."
(setq! mac-option-modifier 'meta)

;;  ____________________________________________________________________________
;;; MISC UI

(setq! which-key-idle-delay 0.2)

(after! hl-line
  (setq! global-hl-line-modes
         '(special-mode org-agenda-mode dired-mode)))

;;  ____________________________________________________________________________
;;; MINIBUFFER
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Minibuffer>

;; Modal editing in the minibuffer, too?
(setq! evil-collection-setup-minibuffer t)

;; Show the depth of recursive minibuffers?
(minibuffer-depth-indicate-mode 1)

;;  ____________________________________________________________________________
;;; BUFFERS

;; Kill both buffer and window
(map! :leader
      :desc "Kill buffer and window" :n "b D" #'kill-buffer-and-window)

;; TODO: Toggle between `+popup/raise' and `+popup/buffer'

(after! ibuffer
  (add-hook! 'ibuffer-mode-hook #'ibuffer-auto-mode)
  (setq! ibuffer-marked-face 'dired-marked))

;;  ____________________________________________________________________________
;;; ELDOC
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Lisp-Doc>
;; <https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc>

(setq! eldoc-minor-mode-string nil
       eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
       eldoc-echo-area-display-truncation-message nil
       eldoc-echo-area-prefer-doc-buffer nil
       eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)

;;  ____________________________________________________________________________
;;; RECENT FILES

(after! recentf
  ;; Exclude certain files
  (add-to-list 'recentf-exclude
               (expand-file-name
                (concat doom-emacs-dir ".local/etc/workspaces/autosave"))))

;;  ____________________________________________________________________________
;;; BACKUP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Backup>

;; Make backup before saving files
(setq! make-backup-files t)

;; Specify file name/path patterns and directories ("REGEXP" . "DIRECTORY")
(setq! backup-directory-alist
       `(("." . ,(concat (getenv "HOME") "/Documents/backup/emacs/"))))

;;  ____________________________________________________________________________
;;; DIRED
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Dired>

(after! dired
  (setq! dired-kill-when-opening-new-dired-buffer t)
  (add-hook! 'dired-mode-hook
             #'dired-hide-details-mode
             #'dired-omit-mode
             (setq dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^\\."))
  (map! :localleader :mode dired-mode
        :n "d" #'dired-hide-details-mode))

;;  ____________________________________________________________________________
;;; COMINT

(after! comint
  (setq! comint-input-ignoredups t
         comint-scroll-to-bottom-on-input 'this))

;;  ____________________________________________________________________________
;;; SHELLS

(defvar my-shell (executable-find "fish"))

;; Use a Posix shell under the hood to avoid certain problems
(setq! shell-file-name (executable-find (or "dash" "bash" "zsh" "sh"))
       explicit-shell-file-name my-shell)

;; Set the default shell for Vterm
(after! vterm
  (setq! vterm-shell my-shell))

;; Eshell
(after! eshell
  ;; Browseable Eshell buffer even during output
  (setq! eshell-scroll-to-bottom-on-output nil)
  (set-eshell-alias!
   "q" "exit"
   "l" "ls"
   "ll" "ls -lh"
   "lla" "ls -lhA"
   "lt" "eza -T"
   "llt" "eza -lT"
   "llat" "eza -lAT"
   "mkdir" "mkdir -p -v $*"
   "r" "trash $*"
   ;; Emacs commands
   "e" "find-file $1"
   "f" "find-file $1"
   "fo" "find-file-other-window $1"
   "d" "dired $*"
   "do" "dired-other-window $*"
   "g" "magit-status"
   ;; Tar archives
   "targ" "tar cfvz $*"
   "targx" "tar xfvz $*"
   "tarb" "tar cfvj $*"
   "tarbx" "tar xfvj $*"
   ;; Lisp
   "lisp" "rlwrap ros -Q run $*"
   "lisp-swank" "rlwrap ros -Q run --eval \"(ql:quickload :swank)\" --eval \"(swank:create-server :dont-close t)\""
   ;; macOS
   "app-unblock" "sudo xattr -d com.apple.quarantine $*"
   "app-clear" "sudo xattr -crv $*"
   "app-sign" "sudo codesign --force --deep --sign - $*"
   ;; Homebrew
   "brewup" "brew update && brew upgrade"
   "brewu" "brew update"
   ;; Apt-get
   "pacu" "sudo apt-get update"
   "pacup" "sudo apt-get update && sudo apt-get upgrade"
   "pacupd" "sudo apt-get dist-upgrade"
   "pacs" "apt-cache search $*"
   "pacinfo" "apt-cache show $*"
   "paci" "sudo apt-get install --no-install-recommends $*"
   "pacli" "apt list --installed"
   "paclig" "apt list --installed | grep $*"
   "pacmark" "sudo apt-mark $*"
   "pacr" "sudo apt-get remove --purge $*"
   "pacar" "sudo apt-get autoremove --purge $*"
   ;; Guix
   "guixup" "guix pull && guix package -u"
   ;; Nix
   "nixup" "nix-channel --update nixpkgs && nix-env -u '*'"
   ;; Too small /tmp directory
   "resizetmp" "sudo mount -o remount,size=8G,noatime /tmp"))

;;  ____________________________________________________________________________
;;; PDF-TOOLS

(after! pdf-tools
  ;; Compile without asking
  (pdf-tools-install :no-query))

;;  ____________________________________________________________________________
;;; ORG

(after! org
  (setq! org-directory "~/Documents/org/"))

;;  ____________________________________________________________________________
;;; EDITING / PROGRAMMING

(global-tree-sitter-mode 1)

;; Structural editing
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>
;; <https://github.com/emacs-evil/evil-cleverparens>
;; (after! (:and smartparens evil-cleverparens)
;;   (setq! sp-hybrid-kill-excessive-whitespace t))

;; TODO: Add keybindings for search and replace
;; The 'query-' variant asks with each string. Confirm with "SPC",
;; or omit the current selection via "n"

;; Delete the whole indentation instead spaces one-by-one via <backspace>?
;; (Possibly shadowed by 3rd-party packages like 'smartparens-mode'
(setq! backward-delete-char-untabify-method 'all)

;; Indentation guides
(after! indent-bars
  (setq! indent-bars-no-descend-lists nil
         ;; indent-bars-highlight-current-depth '(:face fringe)
         indent-bars-display-on-blank-lines t))

;; Line numbers
(setq! display-line-numbers-type 'relative)

(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  #'display-line-numbers-mode)

(add-hook! '(prog-mode-hook conf-mode-hook)
           #'rainbow-delimiters-mode)

;;  ____________________________________________________________________________
;;; LISP

(defun my-lisp-modes ()
  "Generates a non-exhaustive list of loaded Lisp-related modes.
This list is derived from the smartparens package"
  (seq-filter #'fboundp '(clojure-mode
                          clojurec-mode
                          clojurescript-mode
                          clojurex-mode
                          clojure-ts-mode
                          clojurescript-ts-mode
                          clojurec-ts-mode
                          common-lisp-mode
                          emacs-lisp-mode
                          fennel-mode
                          gerbil-mode
                          lfe-mode  ; addition
                          lisp-mode
                          lisp-data-mode  ; addition
                          racket-mode
                          scheme-mode
                          stumpwm-mode
                          )))

(defun my-lisp-repl-modes ()
  "Generates a non-exhaustive list of loaded Lisp-related REPLs.
This list is derived from the smartparens package."
  (seq-filter #'fboundp '(cider-repl-mode
                          eshell-mode
                          fennel-repl-mode
                          geiser-repl-mode
                          inf-clojure-mode
                          inferior-emacs-lisp-mode
                          inferior-lfe-mode  ; addition
                          inferior-lisp-mode
                          inferior-scheme-mode
                          lisp-interaction-mode
                          monroe-mode
                          racket-repl-mode
                          scheme-interaction-mode
                          slime-repl-mode
                          sly-mrepl-mode
                          )))

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; ELISP

(add-hook! 'emacs-lisp-mode-hook
  (indent-bars-mode -1))

(after! lispy
  ;; Make sure that the comment with the result is placed after the evaluated
  ;; expression, not inside it
  (advice-add 'lispy-eval-and-comment
              :around #'evil-collection-elisp-mode-last-sexp)
  (map! :localleader
        :map lisp-interaction-mode-map
        :map emacs-lisp-mode-map
        :prefix "e"
        :n "c" #'lispy-eval-and-comment))

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; COMMON LISP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Executing-Lisp>
;; <http://joaotavora.github.io/sly/>
;; <https://github.com/joaotavora/sly>

;; Default Lisp implementation
(setq! inferior-lisp-program "ros -Q run")

(add-to-list '+lisp-quicklisp-paths "~/.roswell/lisp/quicklisp" 'append)

(add-hook! '(lisp-mode-hook lisp-data-mode-hook)
  (indent-bars-mode -1))

(after! sly
  ;; Set Sly Lisp implementations
  (setq! sly-lisp-implementations
         '((roswell ("ros" "-Q" "run"))
           (sbcl ("ros" "-L" "sbcl" "-Q" "run") :coding-system utf-8-unix)
           (ccl ("ros" "-L" "ccl-bin" "-Q" "run"))))
  (setq! sly-default-lisp 'roswell
         sly-command-switch-to-existing-lisp 'always
         sly-complete-symbol-function #'sly-flex-completions)
  (add-hook! 'sly-mrepl-mode-hook
             #'rainbow-delimiters-mode)
  ;; Override some of Doom's default Common Lisp keybindings
  (map! (:map sly-db-mode-map
         :n "gr" #'sly-db-restart-frame)
        (:map sly-inspector-mode-map
         :n "gb" #'sly-inspector-pop
         :n "gr" #'sly-inspector-reinspect
         :n "gR" #'sly-inspector-fetch-all
         :n "K"  #'sly-inspector-describe-inspectee)
        (:map sly-xref-mode-map
         :n "gr" #'sly-recompile-xref
         :n "gR" #'sly-recompile-all-xrefs)
        (:map lisp-mode-map
         :n "gb" #'sly-pop-find-definition-stack)
        (:localleader
         :map lisp-mode-map
         :desc "Sly"                       "'" #'sly
         :desc "Sly (ask)"                 ";" (cmd!! #'sly '-)
         :desc "Expand macro"              "m" #'macrostep-expand
         :desc "Find file in Quicklisp" "f" #'+lisp/find-file-in-quicklisp
         :desc "Quickload System"       "q" #'sly-quickload
         (:prefix "c"  ; ("c" . "compile")
          :desc "Compile toplevel form" "c" #'sly-compile-defun
          :desc "Compile file"          "C" nil ; #'sly-compile-file
          :desc "Compile file"          "f" #'sly-compile-file
          :desc "Compile/load file"     "F" nil ; #'sly-compile-and-load-file
          :desc "Load file"             "l" #'sly-load-file
          :desc "Compile/load file"     "L" #'sly-compile-and-load-file
          :desc "Remove notes"          "n" #'sly-remove-notes
          :desc "Compile region"        "r" #'sly-compile-region)
         (:prefix "e"  ; ("e" . "evaluate")
          :desc "Evaluate buffer"        "b" #'sly-eval-buffer
          :desc "Evaluate defun"         "d" #'sly-overlay-eval-defun
          :desc "Evaluate last"          "e" #'sly-eval-last-expression
          :desc "Evaluate/print last"    "E" #'sly-eval-print-last-expression
          :desc "Evaluate defun (async)" "f" #'sly-eval-defun
          :desc "Undefine function"      "F" nil ; #'sly-undefine-function
          :desc "Evaluate region"        "r" #'sly-eval-region)
         (:prefix ("u" . "undefine")
          :desc "Undefine function"      "f" #'sly-undefine-function
          :desc "Unintern symbol"        "s" #'sly-unintern-symbol)
         (:prefix "g"  ; ("g" . "goto")
          :desc "Go back"              "b" #'sly-pop-find-definition-stack
          :desc "Go to"                "d" #'sly-edit-definition
          :desc "Go to (other window)" "D" #'sly-edit-definition-other-window
          :desc "Next note"            "n" #'sly-next-note
          :desc "Previous note"        "N" #'sly-previous-note
          :desc "Next sticker"         "s" #'sly-stickers-next-sticker
          :desc "Previous sticker"     "S" #'sly-stickers-prev-sticker)
         (:prefix "h"  ; ("h" . "help")
          :desc "Who calls"               "<" #'sly-who-calls
          :desc "Calls who"               ">" #'sly-calls-who
          :desc "Lookup format directive" "~" #'hyperspec-lookup-format
          :desc "Lookup reader macro"     "#" #'hyperspec-lookup-reader-macro
          :desc "Apropos"                 "a" #'sly-apropos
          :desc "Who binds"               "b" #'sly-who-binds
          :desc "Disassemble symbol"      "d" #'sly-disassemble-symbol
          :desc "Describe symbol"         "h" #'sly-describe-symbol
          :desc "HyperSpec lookup"        "H" #'sly-hyperspec-lookup
          :desc "Who macro-expands"       "m" #'sly-who-macroexpands
          :desc "Apropos package"         "p" #'sly-apropos-package
          :desc "Who references"          "r" #'sly-who-references
          :desc "Who specializes"         "s" #'sly-who-specializes
          :desc "Who sets"                "S" #'sly-who-sets)
         (:prefix "r"  ; ("r" . "repl")
          :desc "Clear REPL"         "c" #'sly-mrepl-clear-repl
          :desc "Load System"        "l" #'sly-asdf-load-system
          :desc "Quit connection"    "q" #'sly-quit-lisp
          :desc "Restart connection" "r" #'sly-restart-inferior-lisp
          :desc "Reload Project"     "R" #'+lisp/reload-project
          :desc "Sync REPL"          "s" #'sly-mrepl-sync)
         (:prefix "s"  ; ("s" . "stickers")
          :desc "Toggle breaking stickers" "b" #'sly-stickers-toggle-break-on-stickers
          :desc "Clear defun stickers"     "c" #'sly-stickers-clear-defun-stickers
          :desc "Clear buffer stickers"    "C" #'sly-stickers-clear-buffer-stickers
          :desc "Fetch stickers"           "f" #'sly-stickers-fetch
          :desc "Replay stickers"          "r" #'sly-stickers-replay
          :desc "Add/remove sticker"       "s" #'sly-stickers-dwim)
         (:prefix "t"  ; ("t" . "test")
          :desc "Test system" "s" #'sly-asdf-test-system)
         (:prefix "T"  ; ("T" . "trace")
          :desc "Toggle"         "t" #'sly-toggle-trace-fdefinition
          :desc "Toggle (fancy)" "T" #'sly-toggle-fancy-trace
          :desc "Untrace all"    "u" #'sly-untrace-all))))

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; LFE
;; <https://lfe.io/>

(use-package! lfe-mode
  :defer t
  :config
  (set-repl-handler! 'lfe-mode #'inferior-lfe)
  (set-eval-handler! 'lfe-mode #'lfe-eval-region)
  ;; (set-formatter! 'lisp-indent #'apheleia-indent-lisp-buffer :modes '(lfe-mode))
  (add-hook! 'lfe-mode-hook
             #'eglot-ensure))

(use-package! inferior-lfe
  :defer t
  :when (modulep! :tools eval)
  :config
  (set-popup-rule! "^\\*inferior-lfe.*\\*" :size 0.3 :quit nil :ttl nil)
  (add-hook! 'inferior-lfe-mode-hook
             #'rainbow-delimiters-mode)
  ;; (setq! inferior-lfe-program "lfe")
  ;; (setq! inferior-lfe-program-options '("-nobanner"))
  ;; (setq! inferior-lfe-check-if-rebar-project t)
  ;; (setq! inferior-lfe-check-if-mix-project t)
  ;; (setq! inferior-lfe-indent-on-Cj t)
  )

(use-package! lfe-indent
  :defer t)

(use-package! lfe-start
  :defer t)

(after! eglot
  (add-to-list 'eglot-server-programs
               '(lfe-mode . ("~/code/lfe-ls/_build/prod/bin/lfe-ls"
                             "--transport" "tcp" "--port" :autoport))))

;;  ____________________________________________________________________________
;;; ELIXIR

(add-hook! 'elixir-mode-hook
           #'inf-elixir-minor-mode
           #'mix-minor-mode
           #'eglot-ensure)

;;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; REPL

(use-package! inf-elixir
  :when (modulep! :tools eval)
  :defer t
  :config
  (set-repl-handler! 'elixir-mode #'inf-elixir)
  (set-eval-handler! 'elixir-mode #'inf-elixir-send-region)
  (set-popup-rule! "^\\*Inf-Elixir.*\\*" :size 0.3 :quit nil :ttl nil)
  (defun inf-elixir-recompile ()
    "Send `IEx.Helpers.recompile/1' to recompile the current Mix project.
Note this function simply recompiles Elixir modules, without reloading
configuration or restarting applications."
    (interactive)
    (inf-elixir--send (format "recompile()"))
    (if inf-elixir-switch-to-repl-on-send
        (goto-char (point-max))))
  (defun inf-elixir-observer ()
    "Start the Erlang Observer in IEx."
    (interactive)
    (inf-elixir--send (format ":observer.start()")))
  (setq! inf-elixir-switch-to-repl-on-send nil)
  (add-hook! 'inf-elixir-mode-hook
             #'rainbow-delimiters-mode)
  ;; TODO: Refactor this mess into one expression
  (map! :localleader :mode elixir-mode :prefix ("e" . "eval")
        :nv "l" #'inf-elixir-send-line)
  (map! :localleader :mode elixir-mode :prefix ("e" . "eval")
        :nv "r" #'inf-elixir-send-region)
  (map! :localleader :mode elixir-mode :prefix ("e" . "eval")
        :nv "b" #'inf-elixir-send-buffer)
  (map! :localleader :mode elixir-mode
        :n "R" #'inf-elixir-recompile)
  (map! :localleader :mode elixir-mode
        :n "r" #'inf-elixir-reload-module)
  (map! :localleader :mode elixir-mode
        :n "o" #'inf-elixir-observer)
  (map! :localleader :mode elixir-mode
        :n "s" #'inf-elixir-project))

;; <https://github.com/elixir-lsp/elixir-ls>
;; <https://gist.github.com/Nezteb/dc63f1d5ad9d88907dd103da2ca000b1>
(after! eglot
  ;; A longer timeout seems required for the first run in a new project
  (add-to-list 'eglot-server-programs
               '(elixir-mode . ("elixir-ls"))))

;;  ____________________________________________________________________________
;;; LOAD EXTERNAL ELISP

;; Private user information
(load! "~/.emacs.lisp/my-private.el")

;; Personal Elisp
(load! "~/.emacs.lisp/my-tools.el")

;;; MJJ blog
(load! "~/Documents/monkeyjunglejuice/static/mjj-publish.el")

;;  ____________________________________________________________________________
;;; COMMENTARY

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Iosevka" :size 16)
;;       doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-bluloco-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/Documents/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
