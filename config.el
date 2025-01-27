;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;  ____________________________________________________________________________
;;; FRAMES

;; Initial frame placement
(pushnew! initial-frame-alist
          '(width . 90)
          '(height . 54)
          '(left . 940)
          '(top . 0))

;; Default frame placement
(pushnew! default-frame-alist
          '(width . 90)
          '(height . 54)
          '(left . 940)
          '(top . 0))

;; Bring frame to the front
(select-frame-set-input-focus (selected-frame))

;;  ____________________________________________________________________________
;;; FONTS

(setq! doom-font
       (font-spec :family "Iosevka Dee" :size 16 :weight 'normal)
       doom-serif-font
       (font-spec :family "Iosevka Dee Slab" :size 16 :weight 'medium)
       doom-variable-pitch-font
       (font-spec :family "Crimson Pro" :size 18 :weight 'normal))

;;  ____________________________________________________________________________
;;; THEMES

;; Theme settings
(after! modus-themes
  (setq! modus-themes-bold-constructs t))

(after! ef-themes
  (setq! ef-themes-mixed-fonts t))

;;; - Set light/dark theme:
(setq! doom-theme-light 'ef-reverie)
(setq! doom-theme-dark 'ef-symbiosis)

;; Switch between dark/light theme based on the system appearance
;; <https://github.com/d12frosted/homebrew-emacs-plus?tab=readme-ov-file#system-appearance-change>
(defun my-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (setq! doom-theme
         (pcase appearance
           ('light (load-theme doom-theme-light t)
                   (doom/set-frame-opacity 100)
                   doom-theme-light)
           ('dark (load-theme doom-theme-dark t)
                  (doom/set-frame-opacity 80)
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
      :desc nil               ":" nil  ; M-x
      :desc nil               "<" nil  ; Switch buffer
      :desc nil               "X" nil  ; Org capture
      :desc nil               "`" nil  ; Switch to last buffer
      :desc nil               "~" nil  ; Toggle last popup
      :desc "Toggle popups"   "`" #'+popup/toggle
      :desc "Command"         "m" #'execute-extended-command
      :desc "Complex command" "M" #'consult-complex-command
      :desc "Switch buffer"   "," #'switch-to-buffer
      :desc "Previous window" "j" #'evil-window-mru
      :desc "Lisp"            "l" #'sly
      :desc "Eshell"          "e" #'+eshell/here
      :desc "IEx"             "r" #'inf-elixir-run)

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - MacOS

;; Make the <Command> key on MacOS act as <Ctrl> key: "C- ..."
(setq! mac-command-modifier 'control)

;; Make the <Option> key on MacOS act as <Meta> key for "M- ..."
;; (setq! mac-option-modifier 'meta)

;;  ____________________________________________________________________________
;;; WINDOW MANAGEMENT
;; <https://github.com/dimitri/switch-window>

(use-package! switch-window
  :config
  (setq! switch-window-background t)
  (setq! switch-window-multiple-frames nil)
  (setq! switch-window-threshold 1)
  (setq! switch-window-mvborder-increment 1)
  ;; Vim-like keybindings for window resizing
  (setq! switch-window-extra-map
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "k")   #'switch-window-mvborder-up)
           (define-key map (kbd "j")   #'switch-window-mvborder-down)
           (define-key map (kbd "h")   #'switch-window-mvborder-left)
           (define-key map (kbd "l")   #'switch-window-mvborder-right)
           (define-key map (kbd "=")   #'balance-windows)
           (define-key map (kbd "SPC") #'switch-window-resume-auto-resize-window)
           map))
  (setq! switch-window-minibuffer-shortcut 109)  ; "m"
  (setq! switch-window-qwerty-shortcuts
         '("a" "s" "d" "f" "g"
           "q" "w" "e" "r" "t" "y"
           "u" "i" "o" "p"
           "z" "x" "c" "v"
           "b" "n"))
  (set-face-attribute 'switch-window-background nil
                      :foreground 'unspecified
                      :inherit 'shadow)
  (set-face-attribute 'switch-window-label nil
                      :inherit 'show-paren-match-expression
                      :height 1.0)
  :bind
  ;; Bind `switch-window' commands to regular Emacs keybindings
  ;; TODO Set up Evil equivalents and replace some default Doom bindings
  ("C-x o"   . switch-window)
  ("C-x 1"   . switch-window-then-maximize)
  ("C-x 2"   . switch-window-then-split-below)
  ("C-x 3"   . switch-window-then-split-right)
  ("C-x 0"   . switch-window-then-delete)
  ("C-x 4 0" . switch-window-then-kill-buffer)
  ("C-x 4 d" . switch-window-then-dired)
  ("C-x 4 f" . switch-window-then-find-file)
  ("C-x 4 b" . switch-window-then-display-buffer)
  ("C-x 4 s" . switch-window-then-swap-buffer))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Popup windows

;; Make `other-window' work on popup-windows too
(setq! +popup-default-parameters
       (remove '(no-other-window . t) +popup-default-parameters))

;; Adjust defaults
;; (plist-put +popup-defaults :height 0.38)

;;  ____________________________________________________________________________
;;; MINIBUFFER
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Minibuffer>

;; Show the depth of recursive minibuffers?
(minibuffer-depth-indicate-mode 1)

;; Delete duplicates from the command history
(setq! history-delete-duplicates t)

;; Modal editing in the minibuffer, too?
(setq! evil-collection-setup-minibuffer t)

;;  ____________________________________________________________________________
;;; MISC UI

(menu-bar-mode -1)

(after! vertico
  (setq! vertico-cycle nil))

(after! which-key
  (setq! which-key-idle-delay 0.4))

(after! hl-line
  (setq! hl-line-sticky-flag nil)
  (setq! global-hl-line-modes '(text-mode
                                special-mode
                                org-agenda-mode
                                dired-mode)))

;; Don't hide mode-line
(after! hide-mode-line
  (advice-add 'hide-mode-line-mode :around
              (lambda (orig &optional args) nil)))

;;  ____________________________________________________________________________
;;; BUFFER MANAGEMENT

;; Kill both buffer and window
(map! :leader
      :desc "Kill buffer and window" :n "b D" #'kill-buffer-and-window)

(after! ibuffer
  (add-hook! 'ibuffer-mode-hook #'ibuffer-auto-mode))

;; Kill all buffers at once
(defun my-kill-all-buffers ()
  "Really kill all buffers at once."
  (interactive)
  (save-some-buffers)
  (let ((kill-buffer-query-functions '()))
    (mapc #'kill-buffer (buffer-list))))

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
;;; SHELLS

(defvar my-shell (executable-find "fish"))
(setq! explicit-shell-file-name my-shell)

;; Use a Posix shell under the hood to avoid problems wherever Emacs (or Emacs
;; packages) spawn child processes via shell commands and rely on their output
(setq! shell-file-name (or (executable-find "dash")
                           (executable-find "bash")
                           (executable-find "sh")))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Vterm

(after! vterm
  (setq! vterm-shell my-shell))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Eshell

(after! eshell
  (setq! eshell-scroll-to-bottom-on-output nil)
  (setq eshell-list-files-after-cd t)
  (setq! eshell-term-name "xterm-256color")
  (set-eshell-alias!
   "q"           "exit"
   "l"           "ls $*"
   "la"          "ls -A $*"
   "ll"          "ls -lh $*"
   "lla"         "ls -lhA $*"
   "lt"          "eza -T --icons $*"
   "lat"         "eza -AT --icons $*"
   "llt"         "eza -lT --icons $*"
   "llat"        "eza -lAT --icons $*"
   "up"          "eshell-up $1"
   "cdp"         "cd-to-project"
   "mkdir"       "mkdir -p $*"
   "tr"          "trash $*"
   ;; Emacs commands
   "f"           "find-file $1"
   "fo"          "find-file-other-window $1"
   "d"           "dired $1"
   "do"          "dired-other-window $1"
   "g"           "magit-status"
   "doomS"       "doom sync --gc --aot"
   "doomU"       "doom upgrade --aot"
   ;; Git
   "git"         "git --no-pager $*"
   ;; Tar archives
   "targ"        "tar cfvz $*"
   "targx"       "tar xfvz $*"
   "tarb"        "tar cfvj $*"
   "tarbx"       "tar xfvj $*"
   ;; Lisp
   "lisp"        "rlwrap ros -Q run $*"
   "lisp-swank"  "rlwrap ros -Q run --eval \"(ql:quickload :swank)\" --eval \"(swank:create-server :dont-close t)\""
   ;; macOS
   "app-unblock" "sudo xattr -d com.apple.quarantine $*"
   "app-clear"   "sudo xattr -crv $*"
   "app-sign"    "sudo codesign --force --deep --sign - $*"
   ;; Homebrew
   "brewup"      "brew update && brew upgrade"
   "brewu"       "brew update"
   ;; Apt-get
   "pacu"        "sudo apt-get update"
   "pacup"       "sudo apt-get update && sudo apt-get upgrade"
   "pacupd"      "sudo apt-get dist-upgrade"
   "pacs"        "apt-cache search $*"
   "pacinfo"     "apt-cache show $*"
   "paci"        "sudo apt-get install --no-install-recommends $*"
   "pacli"       "apt list --installed"
   "paclig"      "apt list --installed | grep $*"
   "pacmark"     "sudo apt-mark $*"
   "pacr"        "sudo apt-get remove --purge $*"
   "pacar"       "sudo apt-get autoremove --purge $*"
   ;; Guix
   "guixup"      "guix pull && guix package -u"
   ;; Nix
   "nixup"       "nix-channel --update nixpkgs && nix-env -u '*'"
   ;; Too small /tmp directory
   "resizetmp"   "sudo mount -o remount,size=8G,noatime /tmp"))

;;  ____________________________________________________________________________
;;; COMINT

(after! comint
  (setq! comint-input-ignoredups t
         comint-scroll-to-bottom-on-input 'this))

;;  ____________________________________________________________________________
;;; DIRED
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Dired>

(after! dired
  ;; Listing columns; Switch arguments with "C-u s" e.g. hide backups with -B
  (setq! dired-listing-switches "-lhFA -v --group-directories-first")
  ;; (setq! dired-kill-when-opening-new-dired-buffer t)
  (add-hook! 'dired-mode-hook
             #'dired-hide-details-mode
             #'dired-omit-mode
             (setq! dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^\\."))
  (map! :localleader :mode dired-mode
        :n "d" #'dired-hide-details-mode)
  ;; BUG <https://github.com/doomemacs/doomemacs/issues/8170>
  (defun reset-cursor-after-wdired-exit ()
    "Restore the evil-normal-state-cursor to 'box' after exiting Editable Dired."
    (kill-local-variable 'evil-normal-state-cursor)
    (kill-local-variable 'cursor-type)
    (setq evil-normal-state-cursor 'box)
    (setq cursor-type 'box))
  (advice-add 'wdired-finish-edit :after #'reset-cursor-after-wdired-exit) ;; ZZ
  (advice-add 'wdired-abort-changes :after #'reset-cursor-after-wdired-exit) ;; ZQ
  (advice-add 'evil-force-normal-state :after #'reset-cursor-after-wdired-exit)) ;; ESC

;;  ____________________________________________________________________________
;;; CALENDAR
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Calendar_002fDiary>

(setq! calendar-date-style 'iso
       calendar-week-start-day 1
       calendar-weekend-days '(6 0))

;;  ____________________________________________________________________________
;;; UTILITIES

;; Show and manage OS processes
(setq! proced-auto-update-interval 1
       proced-auto-update-flag t
       proced-descend t)

;;  ____________________________________________________________________________
;;; WEB BROWSERS
;; <https://www.gnu.org/software/emacs/manual/html_mono/eww.html#Top>

(setq! url-privacy-level '(email lastloc cookies))
(url-setup-privacy-info)

(defun my-user-agent (browser-name)
  ;; TODO: make user-agent selection interactive
  (cond
   ((equal browser-name 'safari-macos)
    (setq! url-user-agent
           "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/603.3.8 (KHTML, like Gecko) Version/11.0.1 Safari/603.3.8"))
   ((equal browser-name 'safari-iphone)
    (setq! url-user-agent
           "Mozilla/5.0 (iPhone; CPU iPhone OS 18_2 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.2 Mobile/15E148 Safari/604.1"))
   ((equal browser-name 'w3m)
    (setq! url-user-agent
           "w3m/0.5.3+git2020050"))
   (t
    (setq! url-user-agent
           'default))))

;; Set the user agent for the internal web browser
(my-user-agent 'safari-iphone)

;; Default system browser
(setq! browse-url-browser-function #'eww-browse-url)

;; Secondary web browser
(setq! browse-url-secondary-browser-function #'browse-url-default-browser)

;; Keybindings
(map! :leader
      :desc "Browse URL"           "o w"   #'browse-url
      :desc "Browse URL external"  "o W"   #'browse-url-default-macosx-browser
      :desc "Browse URL in Webkit" "o C-w" #'xwidget-webkit-browse-url)

;;  ____________________________________________________________________________
;;; PDF-TOOLS

(after! pdf-tools
  ;; Compile without asking
  (pdf-tools-install :no-query))

;;  ____________________________________________________________________________
;;; PROJECTS / WORKSPACES

;; Permanently display workspaces in minibuffer
;; (after! persp-mode
;;   (defun display-workspaces-in-minibuffer ()
;;     (with-current-buffer " *Minibuf-0*"
;;       (erase-buffer)
;;       (insert (+workspace--tabline))))
;;   (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
;;   (+workspace/display))

;;  ____________________________________________________________________________
;;; ORG

(after! org
  (setq! org-directory "~/Documents/org/")
  (setq! org-hide-leading-stars nil))

;; <https://github.com/alphapapa/org-sticky-header>
(use-package! org-sticky-header
  :after org
  :config
  (add-hook! 'org-mode-hook #'org-sticky-header-mode))

;;  ____________________________________________________________________________
;;; EDITING / PROGRAMMING

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Swap keys

;; <https://github.com/wbolster/evil-swap-keys>
(use-package! evil-swap-keys
  :config
  (global-evil-swap-keys-mode 1)
  ;; Shift number keys in all Evil states for any buffer type
  (setq-hook! 'evil-local-mode-hook
    evil-swap-keys-text-input-states
    '(emacs insert replace visual normal motion operator operator-pending))
  (add-hook! 'evil-local-mode-hook
             #'evil-swap-keys-swap-number-row))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Tree-Sitter text objects

(map! (:map +tree-sitter-outer-text-objects-map
            ;; Use 'm' like "module" instead
            "m" (evil-textobj-tree-sitter-get-textobj "class.outer"))
      (:map +tree-sitter-inner-text-objects-map
            ;; Use 'm' like "module" instead
            "m" (evil-textobj-tree-sitter-get-textobj "class.inner")))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Kill-ring, yanking, copy & paste
;; <https://github.com/NicholasBHubbard/clean-kill-ring.el>

(use-package! clean-kill-ring
  :config
  (clean-kill-ring-mode 1))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Parenthesis

;; Color-code nested parens
;; <https://github.com/Fanael/rainbow-delimiters>
(add-hook! '(prog-mode-hook conf-mode-hook)
           #'rainbow-delimiters-mode)

;; Structural editing: Lispyville
;; <https://oremacs.com/lispy>
;; <https://github.com/abo-abo/lispy>
(after! (lispy lispyville)
  (map! :map lispy-mode-map-lispy
        ;; Unbind individual bracket keys
        "[" nil
        "]" nil
        ;; Re-bind commands bound to bracket keys by default
        "M-[" #'lispyville-previous-opening
        "M-]" #'lispyville-next-opening))

;; Structural editing: Smartparens
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>
(after! smartparens
  (setq! smartparens-global-strict-mode t)
  (setq! show-paren-mode nil
         show-smartparens-global-mode t)
  ;; Custom keybinding set, a blend of standard Emacs sexp keybindings
  ;; and Paredit keybindings
  ;; (map! :map smartparens-mode-map
  ;;       ;; Navigation
  ;;       "C-M-f"           #'sp-forward-sexp
  ;;       "C-M-b"           #'sp-backward-sexp
  ;;       "C-M-u"           #'sp-backward-up-sexp
  ;;       "C-M-d"           #'sp-down-sexp
  ;;       "C-M-p"           #'sp-backward-down-sexp
  ;;       "C-M-n"           #'sp-up-sexp
  ;;       "C-M-a"           #'sp-beginning-of-sexp
  ;;       "C-M-e"           #'sp-end-of-sexp
  ;;       ;; Depth-changing commands
  ;;       "C-M-g"           #'sp-unwrap-sexp
  ;;       "C-M-s"           #'sp-splice-sexp
  ;;       ;; Forward slurp/barf
  ;;       "C-)"             #'sp-forward-slurp-sexp
  ;;       "C-}"             #'sp-forward-barf-sexp
  ;;       ;; Backward slurp/barf
  ;;       "C-("             #'sp-backward-slurp-sexp
  ;;       "C-{"             #'sp-backward-barf-sexp
  ;;       ;; Misc
  ;;       "C-M-k"           #'sp-kill-sexp
  ;;       "C-M-<backspace>" #'sp-backward-kill-sexp
  ;;       "C-M-SPC"         #'sp-mark-sexp
  ;;       "C-M-w"           #'sp-copy-sexp
  ;;       "C-M-t"           #'sp-transpose-sexp
  ;;       "M-("             #'sp-wrap-round)
  )

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Line numbers

(setq! display-line-numbers-type 'relative)

(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  #'display-line-numbers-mode)

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Indentation

;; Delete the whole indentation instead spaces one-by-one via <backspace>?
;; (Possibly shadowed by 3rd-party packages like 'smartparens-mode'
(setq! backward-delete-char-untabify-method 'all)

;; <https://github.com/Malabarba/aggressive-indent-mode>
(use-package! aggressive-indent
  :defer t
  :config
  (global-aggressive-indent-mode 1))

;; Indentation guides
(after! indent-bars
  (setq! indent-bars-no-descend-lists t
         indent-bars-highlight-current-depth '(:face fringe)
         indent-bars-display-on-blank-lines t)
  (remove-hook! 'text-mode-hook
    #'indent-bars-mode))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Searching

;; TODO: Add keybindings for search and replace
;; The 'query-' variant asks with each string. Confirm with "SPC",
;; or omit the current selection via "n"

;;  ____________________________________________________________________________
;;; LISP

(defun my-lisp-src-modes ()
  "Generates a non-exhaustive list of loaded Lisp-related modes.
Entries are derived from the smartparens package"
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
Entries are derived from the smartparens package."
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

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Elisp

(add-hook! 'emacs-lisp-mode-hook
  (indent-bars-mode -1))

(after! (lispy lispyville)
  ;; Making sure that the comment with the result is placed after the evaluated
  ;; expression, not inside it
  (advice-add 'lispy-eval-and-comment
              :around #'evil-collection-elisp-mode-last-sexp)
  (map! :localleader
        :map (emacs-lisp-mode-map lisp-interaction-mode-map)
        :prefix "e"
        :n "c" #'lispy-eval-and-comment))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Common Lisp
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
  (add-hook! 'sly-net-process-close-hooks
             ;; Switch to normal state when connection is closed
             (when (string-match-p "^\\*sly-mrepl.*\\*" (buffer-name))
               (evil-normal-state)))
  ;; TODO: Change some of Doom's default Common Lisp keybindings
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
         :desc "Find file in Quicklisp"    "f" #'+lisp/find-file-in-quicklisp
         :desc "Quickload System"          "q" #'sly-quickload
         (:prefix "c"  ; ("c" . "compile")
          :desc "Compile toplevel form"    "c" #'sly-compile-defun
          :desc "Compile file"             "C" nil ; #'sly-compile-file
          :desc "Compile file"             "f" #'sly-compile-file
          :desc "Compile/load file"        "F" nil ; #'sly-compile-and-load-file
          :desc "Load file"                "l" #'sly-load-file
          :desc "Compile/load file"        "L" #'sly-compile-and-load-file
          :desc "Remove notes"             "n" #'sly-remove-notes
          :desc "Compile region"           "r" #'sly-compile-region)
         (:prefix "e"  ; ("e" . "evaluate")
          :desc "Evaluate buffer"          "b" #'sly-eval-buffer
          :desc "Evaluate defun"           "d" #'sly-overlay-eval-defun
          :desc "Evaluate last"            "e" #'sly-eval-last-expression
          :desc "Evaluate/print last"      "E" #'sly-eval-print-last-expression
          :desc "Evaluate defun (async)"   "f" #'sly-eval-defun
          :desc "Undefine function"        "F" nil ; #'sly-undefine-function
          :desc "Evaluate region"          "r" #'sly-eval-region)
         (:prefix ("u" . "undefine")
          :desc "Undefine function"        "f" #'sly-undefine-function
          :desc "Unintern symbol"          "s" #'sly-unintern-symbol)
         (:prefix "g"  ; ("g" . "goto")
          :desc "Go back"                  "b" #'sly-pop-find-definition-stack
          :desc "Go to"                    "d" #'sly-edit-definition
          :desc "Go to (other window)"     "D" #'sly-edit-definition-other-window
          :desc "Next note"                "n" #'sly-next-note
          :desc "Previous note"            "N" #'sly-previous-note
          :desc "Next sticker"             "s" #'sly-stickers-next-sticker
          :desc "Previous sticker"         "S" #'sly-stickers-prev-sticker)
         (:prefix "h"  ; ("h" . "help")
          :desc "Who calls"                "<" #'sly-who-calls
          :desc "Calls who"                ">" #'sly-calls-who
          :desc "Lookup format directive"  "~" #'hyperspec-lookup-format
          :desc "Lookup reader macro"      "#" #'hyperspec-lookup-reader-macro
          :desc "Apropos"                  "a" #'sly-apropos
          :desc "Who binds"                "b" #'sly-who-binds
          :desc "Disassemble symbol"       "d" #'sly-disassemble-symbol
          :desc "Describe symbol"          "h" #'sly-describe-symbol
          :desc "HyperSpec lookup"         "H" #'sly-hyperspec-lookup
          :desc "Who macro-expands"        "m" #'sly-who-macroexpands
          :desc "Apropos package"          "p" #'sly-apropos-package
          :desc "Who references"           "r" #'sly-who-references
          :desc "Who specializes"          "s" #'sly-who-specializes
          :desc "Who sets"                 "S" #'sly-who-sets)
         (:prefix "r"  ; ("r" . "repl")
          :desc "Clear REPL"               "c" #'sly-mrepl-clear-repl
          :desc "Load System"              "l" #'sly-asdf-load-system
          :desc "Quit connection"          "q" #'sly-quit-lisp
          :desc "Restart connection"       "r" #'sly-restart-inferior-lisp
          :desc "Reload Project"           "R" #'+lisp/reload-project
          :desc "Sync REPL"                "s" #'sly-mrepl-sync)
         (:prefix "s"  ; ("s" . "stickers")
          :desc "Toggle breaking stickers" "b" #'sly-stickers-toggle-break-on-stickers
          :desc "Clear defun stickers"     "c" #'sly-stickers-clear-defun-stickers
          :desc "Clear buffer stickers"    "C" #'sly-stickers-clear-buffer-stickers
          :desc "Fetch stickers"           "f" #'sly-stickers-fetch
          :desc "Replay stickers"          "r" #'sly-stickers-replay
          :desc "Add/remove sticker"       "s" #'sly-stickers-dwim)
         (:prefix "t"  ; ("t" . "test")
          :desc "Test system"              "s" #'sly-asdf-test-system)
         (:prefix "T"  ; ("T" . "trace")
          :desc "Toggle"                   "t" #'sly-toggle-trace-fdefinition
          :desc "Toggle (fancy)"           "T" #'sly-toggle-fancy-trace
          :desc "Untrace all"              "u" #'sly-untrace-all))))

;; The hyperspec must be installed first. Adapt the path below:
(after! hyperspec
  (setq! common-lisp-hyperspec-root
         (concat "file://"
                 (expand-file-name "~/common-lisp/.hyperspec/HyperSpec/")))
  (setq! common-lisp-hyperspec-symbol-table
         (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
  (setq! common-lisp-hyperspec-issuex-table
         (concat common-lisp-hyperspec-root "Data/Map_IssX.txt")))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - LFE
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
  :when (modulep! :tools eval)
  :defer t
  :config
  (set-popup-rule! "^\\*inferior-lfe.*\\*" :size 0.3 :quit nil :ttl nil)
  (add-hook! 'inferior-lfe-mode-hook #'rainbow-delimiters-mode)
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
               ;; FIXME: the LFE language server is broken
               ;; https://github.com/mdbergmann/lfe-ls/issues/10#issue-2714103164
               '(lfe-mode . ("~/code/lfe-ls/_build/prod/bin/lfe-ls"
                             "--transport" "tcp" "--port" :autoport))))

;;  ____________________________________________________________________________
;;; OCAML

(use-package! utop
  :when (and (modulep! :lang ocaml) (modulep! :tools eval))
  :config
  (set-repl-handler! 'tuareg-mode #'utop)
  (set-eval-handler! 'tuareg-mode #'utop-eval-region)
  (set-popup-rule! "^\\*utop\\*" :size 0.3 :quit nil :ttl nil)
  (setq! utop-command "opam exec -- dune utop . -- -emacs")
  (add-hook! 'utop-mode-hook
             ;; HACK: Thats how to get completions from Merlin in Utop
             ;; <https://github.com/ocaml-community/utop/issues/455>
             (progn (merlin-mode +1) (merlin-mode -1))))

;;  ____________________________________________________________________________
;;; LOAD EXTERNAL ELISP

;; Private user information
(use-package! my-private)
;; Personal Elisp
(use-package! my-tools)
;; MJJ blog machinery
(use-package! mjj-publish)

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
