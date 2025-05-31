;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;  ____________________________________________________________________________
;;; DEBUG / BENCHMARK

(use-package! benchmark-init
  :when init-file-debug
  :config
  ;; Disable collection of benchmark data after init is done
  (add-hook! 'doom-first-input-hook #'benchmark-init/deactivate))

;;  ____________________________________________________________________________
;;; FRAMES

;; Initial frame placement
(pushnew! initial-frame-alist
          '(fullscreen . maximized))

;; (pushnew! initial-frame-alist
;;           '(width . 80)
;;           '(height . 47)
;;           '(left . 940)
;;           '(top . 0))

;; Default placement for frames that have been created after the initial frame
(pushnew! default-frame-alist
          '(width . 80)
          '(height . 47)
          '(left . 940)
          '(top . 0))

;; Bring frame to the front
(select-frame-set-input-focus (selected-frame))

;;  ____________________________________________________________________________
;;; FONTS

;; Menlo derivative
;; (setq! doom-font
;;        (font-spec :family "MesloLGMDZ Nerd Font"
;;                   :size 15
;;                   :weight 'normal)
;;        doom-serif-font
;;        (font-spec :family "MesloLGMDZ Nerd Font"
;;                   :size 15
;;                   :weight 'normal)
;;        doom-variable-pitch-font
;;        (font-spec :family "Gentium Plus"
;;                   :size 17
;;                   :weight 'normal))

;; Custom Iosevka build
(setq! doom-font
       (font-spec :family "Iosevka Dee"
                  :size 16
                  :weight 'normal
                  :width 'semi-expanded)
       doom-serif-font
       (font-spec :family "Iosevka Dee"
                  :size 16
                  :weight 'normal
                  :width 'semi-expanded)
       doom-variable-pitch-font
       (font-spec :family "Gentium Plus"
                  :size 17
                  :weight 'normal))

;;  ____________________________________________________________________________
;;; THEMES

;; My themes
;; <https://github.com/monkeyjunglejuice/matrix-emacs-theme>
;; <https://github.com/monkeyjunglejuice/beach-emacs-theme>
(use-package! my-themes)

;; Declarations
(defvar my-theme-light nil "The default light theme.")
(defvar my-theme-dark nil "The default dark theme.")
(defvar my-frame-opacity 100 "The default frame opacity.")

;; Do not extend `region' background past the end of the line
(custom-set-faces
 '(region ((t :extend nil))))

;;; - Set light/dark theme:
(setq my-theme-light 'doom-one-light)
(setq my-theme-dark 'doom-tokyo-night)

;; Switch between dark/light theme based on the system appearance
;; <https://github.com/d12frosted/homebrew-emacs-plus?tab=readme-ov-file#system-appearance-change>
(defun my-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (setq doom-theme
        (pcase appearance
          ('light (load-theme my-theme-light t)
                  ;; Global variable will be used as an argument to launch
                  ;; emacsclient frames
                  (doom/set-frame-opacity (setq my-frame-opacity 100))
                  ;; Eventually return theme name to set `doom-theme'
                  my-theme-light)
          ('dark (load-theme my-theme-dark t)
                 ;; Global variable will be used as an argument to launch
                 ;; emacsclient frames
                 (doom/set-frame-opacity (setq my-frame-opacity 90))
                 ;; Eventually return theme name to set `doom-theme'
                 my-theme-dark))))

(add-hook! 'ns-system-appearance-change-functions #'my-apply-theme)

;; BUG Fix for https://github.com/doomemacs/doomemacs/issues/6720
(after! diff-hl
  (add-hook! 'doom-load-theme-hook
    (defun +vc-gutter-make-diff-hl-faces-transparent-h ()
      (mapc (doom-rpartial #'set-face-background (face-background 'default))
            '(diff-hl-insert
              diff-hl-delete
              diff-hl-change)))))

;;  ____________________________________________________________________________
;;; SHELL

(defvar my-shell (executable-find "fish"))
(setq! explicit-shell-file-name my-shell)

;; Use a Posix shell under the hood to avoid problems wherever Emacs (or Emacs
;; packages) spawn child processes via shell commands and rely on their output
(setq! shell-file-name (or (executable-find "bash")
                           (executable-find "zsh")
                           (executable-find "bash")
                           (executable-find "sh")))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Eshell

(use-package! eshell
  :config
  (setq! eshell-scroll-to-bottom-on-output nil)
  (setq eshell-list-files-after-cd t)
  (setq! eshell-term-name "xterm-256color"))

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
 "tm"          "trash $*"
 ;; Emacs commands
 "f"           "find-file $1"
 "fo"          "find-file-other-window $1"
 "d"           "dired $1"
 "do"          "dired-other-window $1"
 "g"           "magit-status"
 "doomS"       "doom sync --aot"
 "doomU"       "doom upgrade --aot"
 "doomR"       "doom/restart"
 ;; Adblocker
 "hblock-off"  "hblock -S none -D none"
 ;; Git
 "git"         "git --no-pager $*"
 ;; Tar archives
 "targ"        "tar cfvz $*"
 "targx"       "tar xfvz $*"
 "tarb"        "tar cfvj $*"
 "tarbx"       "tar xfvj $*"
 ;; Common Lisp
 "lisp"        "rlwrap ros -Q run $*"
 "lisp-swank"  "rlwrap ros -Q run --eval \"(ql:quickload :swank)\" --eval \"(swank:create-server :dont-close t)\""
 ;; Erlang
 "reb"         "rebar3 $*"
 ;; macOS
 "app-unblock" "sudo xattr -d com.apple.quarantine $*"
 "app-clear"   "sudo xattr -crv $*"
 "app-sign"    "sudo codesign --force --deep --sign - $*"
 ;; Homebrew
 "brewU"       "brew update && brew upgrade"
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
 "guixU"      "guix pull && guix package -u"
 ;; Nix
 "nixU"       "nix-channel --update nixpkgs && nix-env -u '*'"
 ;; Too small tmp directory
 "resizetmp"   "sudo mount -o remount,size=8G,noatime /tmp")

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Vterm

(after! vterm
  (setq! vterm-shell my-shell))

;;  ____________________________________________________________________________
;;; HELPERS

(defun my-launch-shell (command name)
  "Launch programs via shell COMMAND. The NAME can be an arbitrary string.
The sub-process can be managed via `list-processes'"
  (start-process-shell-command
   name
   (concat "*Process " name "*")  ; with named buffer or `nil' without buffer
   command)
  (message (concat "Launching shell command " name "...done")))

;;  ____________________________________________________________________________
;;; KEYBINDINGS

(setq! doom-leader-key ","
       doom-leader-alt-key "M-,"
       doom-localleader-key "SPC"
       doom-localleader-alt-key "M-SPC")

(map! :leader
      ;; doom-leader-map
      :desc nil                      ":"     nil ; M-x
      :desc nil                      "<"     nil ; Switch buffer
      :desc nil                      "X"     nil ; Org capture
      :desc nil                      "`"     nil ; Switch to last buffer
      :desc nil                      "~"     nil ; Toggle last popup
      :desc "Project buffers"        ","     #'projectile-switch-to-buffer
      :desc "Project files"          "."     #'projectile-find-file
      :desc "Directories"            "d"     #'consult-dir
      :desc "Eshell"                 "e"     #'+eshell/toggle
      :desc "Command"                "m"     #'execute-extended-command
      :desc "Complex command"        "M"     #'consult-complex-command
      :desc "IEx"                    "r"     #'inf-elixir-run
      :desc "Horizontal split"       "S"     #'+evil/window-split-and-follow
      :desc "Vertical split"         "V"     #'+evil/window-vsplit-and-follow
      ;; doom-leader-buffer-map
      :desc "Kill buffer and window" "b D"   #'kill-buffer-and-window
      ;; doom-leader-file-map
      :desc nil                      "f l"   nil ; Locate file
      ;; doom-leader-insert-map
      :desc nil                      "i y"   nil ; From clipboard
      :desc "Form kill-ring"         "i p"   #'+default/yank-pop
      ;; doom-leader-open-map
      :desc "Browse URL"             "o W"   #'browse-url
      :desc "Browse URL external"    "o w"   #'browse-url-default-macosx-browser
      :desc "Browse URL in Webkit"   "o C-w" #'xwidget-webkit-browse-url
      ;; doom-leader-toggle-map
      :desc "Toggle popups"          "t p"   #'+popup/toggle
      ;; evil-window-map
      :desc nil                      "w `"   #'+popup/raise
      :desc nil                      "w ~"   #'+popup/buffer)

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - MacOS

;; Make the <Command> key on MacOS act as <Ctrl> key: "C- ..."
(setq! mac-command-modifier 'control)

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Which-key

(after! which-key
  (setq! which-key-idle-delay 0.5
         which-key-idle-secondary-delay 0.1))

;;  ____________________________________________________________________________
;;; EVIL MODE

(after! evil-vars
  (setq! evil-want-minibuffer t
         evil-move-cursor-back nil))

(after! evil-collection
  (setq! evil-collection-setup-minibuffer t))

;;; - Evil states cheatsheet
;; :n  normal
;; :v  visual
;; :i  insert
;; :e  emacs
;; :o  operator
;; :m  motion
;; :r  replace
;; :g  global  (binds the key without evil current-global-map)

;;  ____________________________________________________________________________
;;; OS INTEGRATION

;; Use the MacOS trash instead of freedesktop.org ~/.local/share/Trash
;; <https://github.com/emacsorphanage/osx-trash>
(use-package! osx-trash
  :when (eq system-type 'darwin)
  :config
  (osx-trash-setup))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Utilities

;; Show and manage OS processes
(setq! proced-auto-update-interval 1
       proced-auto-update-flag t
       proced-enable-color-flag t
       proced-descend t)

;;  ____________________________________________________________________________
;;; WINDOW MANAGEMENT

(setq! switch-to-buffer-obey-display-actions t)

;; <https://github.com/dimitri/switch-window>
(after! switch-window
  (defun switch-window--then-other-window (prompt function)
    "PROMPT a question and let use select or create a window to run FUNCTION."
    (let ((f (switch-window--get-preferred-function function)))
      (switch-window--then
       prompt
       (lambda ()
         (select-window
          (if (one-window-p)
              ;; TODO Write advice instead, default was `split-window-right`
              (split-window-sensibly)
            (next-window)))
         (call-interactively f))
       (lambda () (call-interactively f))
       nil
       2)))
  (custom-set-faces '(switch-window-label
                      ((t :height 1.0 :weight bold))))
  (custom-set-faces '(switch-window-background
                      ((t :inherit 'whitespace-space :background unspecified))))
  (setq! switch-window-multiple-frames t
         switch-window-threshold 1
         switch-window-mvborder-increment 1
         switch-window-background t)
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
  (setq! switch-window-minibuffer-shortcut 109) ; "m"
  (setq! switch-window-qwerty-shortcuts
         '("s" "d" "f"
           "w" "e" "r"
           "z" "x" "c"
           "u" "i" "o"))
  ;; Bind `switch-window' commands to default Doom Emacs keybindings
  (map! :leader
        "w m"    #'switch-window-then-maximize
        "w s"    #'switch-window-then-split-below
        "w v"    #'switch-window-then-split-right
        "w d"    #'switch-window-then-delete
        "w D"    #'switch-window-then-kill-buffer
        "w , d"  #'switch-window-then-dired
        "w , f"  #'switch-window-then-find-file
        "w , b"  #'switch-window-then-display-buffer
        "w , s"  #'switch-window-then-swap-buffer))

(after! ace-window
  (setq! ace-window-display-mode t
         aw-display-mode-overlay nil
         aw-dispatch-when-more-than 1
         aw-scope 'global)
  (setq! aw-dispatch-alist
         '((?  aw-flip-window)
           (?m aw-swap-window "Swap Windows")
           (?M aw-move-window "Move Window")
           (?b aw-switch-buffer-in-window "Select Buffer")
           (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
           (?C aw-copy-window "Copy Window")
           (?D aw-delete-window "Delete Window")
           (?O delete-other-windows "Delete Other Windows")
           (?F aw-split-window-fair "Split Fair Window")
           (?S aw-split-window-vert "Split Vert Window")
           (?V aw-split-window-horz "Split Horz Window")
           (?X aw-execute-command-other-window "Execute Command Other Window")
           (?T aw-transpose-frame "Transpose Frame")
           (?? aw-show-dispatch-help)))
  (setq! aw-keys '(?s ?d ?f
                   ?w ?e ?r
                   ?z ?x ?c
                   ?u ?i ?o))
  (set-face-attribute 'aw-background-face nil
                      :inherit 'shadow))

(use-package! edwina
  :config
  (setq! switch-to-buffer-obey-display-actions nil)
  (setq! display-buffer-base-action '(display-buffer-below-selected))
  (setq! edwina-mfact 0.5)
  (edwina-mode 1)
  (map! :leader
        "j RET"   #'edwina-zoom
        "j r"     #'edwina-arrange
        "j ]"     #'edwina-inc-mfact
        "j ["     #'edwina-dec-mfact
        "j h"     #'edwina-inc-nmaster
        "j l"     #'edwina-dec-nmaster
        "j s"     #'edwina-clone-window
        "j d"     #'edwina-delete-window
        "j j"     #'edwina-select-next-window
        "j k"     #'edwina-select-previous-window
        "j C-j"   #'edwina-swap-next-window
        "j C-k"   #'edwina-swap-previous-window
        "j J"     #'evil-window-decrease-height
        "j K"     #'evil-window-increase-height
        "j f"     #'evil-window-mru)
  (after! winum
    (map! :leader
          "j 1" #'winum-select-window-1
          "j 2" #'winum-select-window-2
          "j 3" #'winum-select-window-3
          "j 4" #'winum-select-window-4
          "j 5" #'winum-select-window-5
          "j 6" #'winum-select-window-6
          "j 7" #'winum-select-window-7
          "j 8" #'winum-select-window-8
          "j 9" #'winum-select-window-9
          "j 0" #'winum-select-window-0-or-10)))
;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Popup windows

(defun popup-size () (floor (frame-height) 3))

(when (modulep! :ui popup)
  ;; Adjust defaults
  (setq! +popup-defaults
         (list :side   'bottom
               :height 0.35
               :width  40
               :quit   t
               :select #'ignore
               :ttl    5))
  ;; Make `other-window' work on popup-windows
  (setq! +popup-default-parameters
         (remove '(no-other-window . t) +popup-default-parameters))
  ;; Make Eglot help windows higher
  (after! eglot
    (set-popup-rule!
      "^\\*eglot-help" :size #'+popup-shrink-to-fit :quit t :select t)))

(set-popup-rules!
  (when (modulep! :ui popup +all)
    '(("^\\*"  :slot 1 :vslot -1 :select t)
      ("^ \\*" :slot 1 :vslot -1 :size 0.35)))
  (when (modulep! :ui popup +defaults)
    '(("^\\*Completions" :ignore t)
      ("^\\*Local variables\\*$"
       :vslot -1 :slot 1 :size +popup-shrink-to-fit)
      ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
       :vslot -2 :size +popup-shrink-to-fit :autosave t :quit t :ttl nil)
      ("^\\*\\(?:doom \\|Pp E\\)"  ; transient buffers (no interaction required)
       :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0)
      ("^\\*doom:"  ; editing buffers (interaction required)
       :vslot -4 :size 0.35 :autosave t :select t :modeline t :quit nil :ttl t)
      ("^\\*doom:\\(?:v?term\\|e?shell\\)-popup"  ; editing buffers (interaction required)
       :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)
      ("^\\*\\(?:Wo\\)?Man "
       :vslot -6 :size +popup-shrink-to-fit :select t :quit t :ttl 0)
      ("^\\*Calc"
       :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0)
      ("^\\*Customize"
       :slot 2 :side bottom :size 0.5 :select t :quit nil)
      ("^ \\*undo-tree\\*"
       :slot 2 :side left :size 20 :select t :quit t)
      ;; `help-mode', `helpful-mode'
      ("^\\*\\([Hh]elp\\|Apropos\\)"
       :slot 2 :vslot -8 :size +popup-shrink-to-fit :select t)
      ("^\\*eww\\*"  ; `eww' (and used by dash docsets)
       :vslot -11 :size +popup-shrink-to-fit :select t)
      ("^\\*xwidget"
       :vslot -11 :size +popup-shrink-to-fit :select nil)
      ("^\\*info\\*$"  ; `Info-mode'
       :slot 2 :vslot 2 :size +popup-shrink-to-fit :select t)))
  '(("^\\*Warnings\\|*Disabled" :vslot 99 :size +popup-shrink-to-fit)
    ("^\\*Backtrace" :vslot 99 :size +popup-shrink-to-fit :quit nil)
    ("^\\*CPU-Profiler-Report " :side bottom :vslot 100 :slot 1 :height +popup-shrink-to-fit :width 0.5 :quit nil)
    ("^\\*Memory-Profiler-Report " :side bottom :vslot 100 :slot 2 :height +popup-shrink-to-fit :width 0.5 :quit nil)
    ("^\\*Process List\\*" :side bottom :vslot 101 :size +popup-shrink-to-fit :select t :quit t)
    ("^\\*\\(?:Proced\\|timer-list\\|Abbrevs\\|Output\\|Occur\\|unsent mail.*?\\|message\\)\\*" :ignore t)))

;;  ____________________________________________________________________________
;;; MINIBUFFER
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Minibuffer>

;; Show the depth of recursive minibuffers?
(minibuffer-depth-indicate-mode 1)

;; Delete duplicates from the command history?
(setq! history-delete-duplicates t)

;;  ____________________________________________________________________________
;;; MISC

(after! vertico
  (setq! vertico-cycle nil
         vertico-count 12))

;; Not everything is line-oriented, e.g. Lisp code
(after! hl-line
  (setq! hl-line-sticky-flag nil)
  (setq! global-hl-line-modes '(special-mode
                                org-agenda-mode
                                dired-mode)))

;; Don't ever hide the mode-line
(after! hide-mode-line
  (advice-add #'hide-mode-line-mode :around
              (lambda (orig &optional args) nil)))

;; The light-weight mode-line is too high, fix that
(when (modulep! :ui modeline +light)
  (setq! +modeline-height 24))

;;  ____________________________________________________________________________
;;; BUFFER MANAGEMENT

(setq! initial-major-mode #'lisp-interaction-mode)

(after! ibuffer
  (add-hook! 'ibuffer-mode-hook #'ibuffer-auto-mode))

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
(setq! make-backup-files t
       vc-make-backup-files t)

;; Specify file name/path patterns and directories ("REGEXP" . "DIRECTORY")
(setq! backup-directory-alist
       `(("." . ,(concat (getenv "HOME") "/Documents/backup/emacs/"))))

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
  (setq! dired-kill-when-opening-new-dired-buffer t)
  (add-hook! 'dired-mode-hook
             #'dired-omit-mode
             (setq! dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^\\."))
  (map! :localleader :mode dired-mode
        :n "d" #'dired-hide-details-mode)
  ;; BUG Fix for <https://github.com/doomemacs/doomemacs/issues/8170>
  (defun reset-cursor-after-wdired-exit ()
    "Restore the evil-normal-state-cursor to \='box' after exiting Editable Dired."
    (kill-local-variable 'evil-normal-state-cursor)
    (kill-local-variable 'cursor-type)
    (setq evil-normal-state-cursor 'box)
    (setq cursor-type 'box))
  (advice-add 'wdired-finish-edit :after #'reset-cursor-after-wdired-exit) ;; ZZ
  (advice-add 'wdired-abort-changes :after #'reset-cursor-after-wdired-exit) ;; ZQ
  (advice-add 'evil-force-normal-state :after #'reset-cursor-after-wdired-exit)) ;; ESC

(after! dirvish
  ;; Can't be set in `dired-mode-hook' because that gets overridden by `dirvish'
  (setq! dirvish-hide-details t))

;;  ____________________________________________________________________________
;;; CALENDAR
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Calendar_002fDiary>

(setq! calendar-date-style 'iso
       calendar-week-start-day 1
       calendar-weekend-days '(6 0))

;;  ____________________________________________________________________________
;;; WEB BROWSERS
;; <https://www.gnu.org/software/emacs/manual/html_mono/eww.html#Top>

(setq! url-privacy-level '(email lastloc cookies))
(url-setup-privacy-info)

(defun my-user-agent (browser-name)
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

;; Set the user agent for EWW, the internal web browser
(my-user-agent 'safari-iphone)

;; Default system browser
(setq! browse-url-browser-function #'eww-browse-url)

;; Secondary web browser
(setq! browse-url-secondary-browser-function #'browse-url-default-browser)

;;  ____________________________________________________________________________
;;; PDF-TOOLS

(after! pdf-tools
  ;; Compile without asking
  (pdf-tools-install :no-query))

;;  ____________________________________________________________________________
;;; AI TOOLS

(defun my-ollama-models (prefix)
  "List all locally installed Ollama models and add PREFIX to each element.
PREFIX can be either \"nil\", or \"ollama_chat/\" or \"ollama/\" to produce
Aider-compatible model names."
  (let* ((output (shell-command-to-string "ollama list"))
         (lines (split-string output "\n" t))
         models)
    (dolist (line (cdr lines))
      (when (string-match "^\\([^[:space:]]+\\)" line)
        (push (concat prefix (match-string 1 line)) models)))
    (nreverse models)))

(defvar my-num-ctx (* 128 1024) "Default context length for Qwen2.5-7b and up.")

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Aider

(use-package! aider
  :config
  (setq! aider-popular-models (my-ollama-models "ollama_chat/"))
  (aider-doom-enable))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Ellama
;; <https://github.com/s-kostyaev/ellama>

(use-package! ellama
  :init
  (setq! ellama-language "English")
  (setq! ellama-sessions-directory "~/Documents/org/ellama-sessions")
  (require 'llm-ollama)
  (setq! ellama-provider
         (make-llm-ollama
          :chat-model "huihui_ai/qwen2.5-abliterate:7b-instruct-q4_K_M"
          :default-chat-temperature 0.6
          :default-chat-non-standard-params `(("num_ctx" . ,my-num-ctx))
          :embedding-model "nomic-embed-text"
          ))
  (setq! ellama-coding-provider
         (make-llm-ollama
          :chat-model "qwen2.5-coder:7b-instruct-q5_K_M"
          :default-chat-temperature 0.6
          :default-chat-non-standard-params `(("num_ctx" . ,my-num-ctx))
          :embedding-model "nomic-embed-text"
          ))
  (setq! ellama-translation-provider
         (make-llm-ollama
          :chat-model "huihui_ai/qwen2.5-abliterate:7b-instruct-q4_K_M"
          :default-chat-temperature 0.6
          :default-chat-non-standard-params `(("num_ctx" . ,my-num-ctx))
          :embedding-model "nomic-embed-text"
          ))
  (setq! ellama-summarization-provider
         (make-llm-ollama
          :chat-model "huihui_ai/qwen2.5-abliterate:7b-instruct-q4_K_M"
          :default-chat-temperature 0.6
          :default-chat-non-standard-params '(("num_ctx" . ,my-num-ctx))
          :embedding-model "nomic-embed-text"
          ))
  (setq! ellama-extraction-provider
         (make-llm-ollama
          :chat-model "qwen2.5-coder:7b-instruct-q5_K_M"
          :default-chat-temperature 0.6
          :default-chat-non-standard-params `(("num_ctx" . ,my-num-ctx))
          :embedding-model "nomic-embed-text"
          ))
  (setq! ellama-naming-provider
         (make-llm-ollama
          :chat-model "huihui_ai/qwen2.5-abliterate:7b-instruct-q4_K_M"
          :default-chat-temperature 0.6
          :default-chat-non-standard-params '(("stop" . ("\n")))
          :embedding-model "nomic-embed-text"
          ))
  :config
  (setq! ellama-chat-display-action-function #'display-buffer-pop-up-window)
  (setq! ellama-instant-display-action-function #'display-buffer-pop-up-window))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Gptel

(use-package! gptel
  :config
  (setq! gptel-default-mode 'org-mode)
  (setq! gptel-directives '((default . "")))
  (setq!
   gptel-model 'qwen2.5-coder:7b-instruct-q5_K_M
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models (my-ollama-models nil))))

;;  ____________________________________________________________________________
;;; PROJECT MANAGEMENT

(after! projectile
  (setq! projectile-project-search-path '("~/code/" "~/common-lisp")))

;;  ____________________________________________________________________________
;;; EGLOT

(after! eglot
  (setq! eglot-code-action-indications '()))

;;  ____________________________________________________________________________
;;; EDITING / PROGRAMMING

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Tree-Sitter text objects

(after! evil
  (map! (:map +tree-sitter-outer-text-objects-map
              ;; Use 'm' like module" instead
              "m" (evil-textobj-tree-sitter-get-textobj "class.outer"))
        (:map +tree-sitter-inner-text-objects-map
              ;; Use 'm' like "module" instead
              "m" (evil-textobj-tree-sitter-get-textobj "class.inner"))))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Kill-ring, yanking, copy & paste

;; <https://github.com/NicholasBHubbard/clean-kill-ring.el>
(use-package! clean-kill-ring
  :config
  (clean-kill-ring-mode 1))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Parenthesis

;; Highlight enclosing parens, or whole expressions?
(setq! show-paren-style 'parenthesis)

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
        "M-[" #'lispyville-next-opening
        "M-{" #'lispyville-previous-opening
        "M-]" #'lispyville-next-closing
        "M-}" #'lispyville-previous-closing))

(after! lispyville
  ;; Prefer Lispyville instead of Smartparens
  (add-hook! '(prog-mode-hook conf-mode-hook) #'lispyville-mode)
  ;; Turn off Smartparens when Lispyville is enabled
  (add-hook! 'lispyville-mode-hook (smartparens-mode -1)))

;; Structural editing: Smartparens
;; <https://github.com/Fuco1/smartparens>
;; <https://smartparens.readthedocs.io/en/latest/>
;; (after! smartparens
;;   (setq! show-smartparens-global-mode t)
;;   ;; Custom keybinding set, a blend of standard Emacs sexp keybindings
;;   ;; and Paredit keybindings
;;   ;; TODO: Enable these keybindings in Evil insert mode and Emacs mode only
;;   (map! :map smartparens-mode-map
;;         ;; Navigation
;;         "C-M-f"           #'sp-forward-sexp
;;         "C-M-b"           #'sp-backward-sexp
;;         "C-M-u"           #'sp-backward-up-sexp
;;         "C-M-d"           #'sp-down-sexp
;;         "C-M-p"           #'sp-backward-down-sexp
;;         "C-M-n"           #'sp-up-sexp
;;         "C-M-a"           #'sp-beginning-of-sexp
;;         "C-M-e"           #'sp-end-of-sexp
;;         ;; Depth-changing commands
;;         "C-M-g"           #'sp-unwrap-sexp
;;         "C-M-s"           #'sp-splice-sexp
;;         ;; Forward slurp/barf
;;         "C-)"             #'sp-forward-slurp-sexp
;;         "C-}"             #'sp-forward-barf-sexp
;;         ;; Backward slurp/barf
;;         "C-("             #'sp-backward-slurp-sexp
;;         "C-{"             #'sp-backward-barf-sexp
;;         ;; Misc
;;         "C-M-k"           #'sp-kill-sexp
;;         "C-M-<backspace>" #'sp-backward-kill-sexp
;;         "C-M-SPC"         #'sp-mark-sexp
;;         "C-M-w"           #'sp-copy-sexp
;;         "C-M-t"           #'sp-transpose-sexp
;;         "M-("             #'sp-wrap-round))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Line numbers

(setq! display-line-numbers-type 'relative)

;; Turn off line numbers
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  #'display-line-numbers-mode)

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Indentation

;; Delete the whole indentation instead spaces one-by-one via <backspace>?
;; (Possibly shadowed by 3rd-party packages like 'smartparens-mode'
(setq! backward-delete-char-untabify-method 'hungry)

;; Keep code always intented
;; <https://github.com/Malabarba/aggressive-indent-mode>
(use-package! aggressive-indent
  :hook
  ((prog-mode conf-mode) . aggressive-indent-mode))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Searching

;; TODO: Add keybindings for search and replace

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Comments

(after! newcomment
  (setq! comment-empty-lines t))

;; Don't continue comment when opening a new line
(setq! +evil-want-o/O-to-continue-comments nil)

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Completion

(after! corfu
  (setq! corfu-popupinfo-delay '(0.5 . 0.3)
         corfu-popupinfo-hide nil
         corfu-preview-current nil))

;;  ____________________________________________________________________________
;;; ORG MODE

(after! org
  (setq! org-directory "~/Documents/org/")
  (setq! org-ellipsis " ▼ ")
  (setq! org-adapt-indentation 'headline-data
         org-indent-mode-turns-on-hiding-stars nil
         org-startup-indented nil
         org-hide-leading-stars nil)
  ;; Don't add leading indentation to code blocks, remove them during export
  (setq! org-edit-src-content-indentation 0
         org-src-preserve-indentation nil))

;; <https://github.com/alphapapa/org-sticky-header>
(use-package! org-sticky-header
  :after org
  :config
  (add-hook! 'org-mode-hook #'org-sticky-header-mode))

;;  ____________________________________________________________________________
;;; LISP

(defun my-lisp-src-modes ()
  "Generates a non-exhaustive list of loaded Lisp-related modes.
Entries are derived from the smartparens package."
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
                          lfe-mode ; addition
                          lisp-mode
                          lisp-data-mode ; addition
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
                          inferior-lfe-mode ; addition
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

(after! evil-collection
  (evil-collection-elisp-mode-setup))

(after! (lispy lispyville)
  ;; Making sure that the comment with the result is placed after the evaluated
  ;; expression, not inside it
  (advice-add #'lispy-eval-and-comment
              :around #'evil-collection-elisp-mode-last-sexp)
  (map! :localleader
        :map (emacs-lisp-mode-map lisp-interaction-mode-map)
        :prefix "e"
        :n "c" #'lispy-eval-and-comment))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - Common Lisp
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Executing-Lisp>
;; <http://joaotavora.github.io/sly/> and <https://github.com/joaotavora/sly>


(after! sly
  ;; Default Lisp implementation
  (setq! inferior-lisp-program "sbcl")
  ;; Add Quicklisp shortcut
  (add-to-list 'sly-mrepl-shortcut-alist '("quickload" . sly-quickload))
  ;; Set Sly Lisp implementations
  (setq! sly-lisp-implementations
         '((sbcl ("sbcl") :coding-system utf-8-unix)
           (ccl ("ccl"))))
  (setq! sly-default-lisp 'sbcl
         sly-command-switch-to-existing-lisp 'always
         sly-complete-symbol-function #'sly-flex-completions)
  (add-hook! 'sly-mrepl-mode-hook
             #'rainbow-delimiters-mode)
  (add-hook! 'sly-net-process-close-hooks
             ;; Switch to normal state when connection is closed
             (when (string-match-p "^\\*sly-mrepl.*\\*" (buffer-name))
               (evil-normal-state)))
  (set-popup-rules!
    '(("^\\*sly-mrepl"       :vslot 2 :size 0.35 :quit nil :ttl nil)
      ("^\\*sly-compilation" :vslot 3 :ttl nil)
      ("^\\*sly-traces"      :vslot 4 :ttl nil)
      ("^\\*sly-description" :vslot 5 :size #'+popup-shrink-to-fit :ttl 0)
      ;; Do not display debugger or inspector buffers in a popup window. These
      ;; buffers are meant to be displayed with sufficient vertical space.
      ("^\\*sly-\\(?:db\\|inspector\\)" :ignore t)))
  ;; Change some of Doom's default Common Lisp keybindings
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
;;; - SCHEME
;; <https://www.nongnu.org/geiser>

(after! geiser
  (set-popup-rules!
    '(("^\\*[gG]eiser \\(dbg\\|xref\\|messages\\)\\*$" :slot 1 :vslot -1)
      ("^\\*Geiser documentation\\*$" :slot 2 :vslot 2 :select t :size #'+popup-shrink-to-fit)
      ("^\\*Geiser .+ REPL" :size 0.35 :quit nil :ttl nil))))

;;    . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;;; - RACKET
;; <https://www.racket-mode.com>

(after! racket-mode
  (set-popup-rule! "^\\*Racket REPL" :size 0.35 :quit nil :ttl nil)
  (set-popup-rule! "^\\*Racket Describe" :size #'+popup-shrink-to-fit :quit t :select t)
  (set-eval-handler! 'racket-mode #'racket-send-region)
  (setq! racket-xp-eldoc-level 'minimal)
  ;; Make sure that the last character of the sexp is not cut off
  (advice-add #'racket-send-last-sexp
              :around #'evil-collection-elisp-mode-last-sexp)
  (advice-add #'racket-expand-last-sexp
              :around #'evil-collection-elisp-mode-last-sexp)
  ;; Change some of Doom's default Racket keybindings
  (map! (:localleader
         :map racket-mode-map
         "a" #'racket-align
         "A" #'racket-unalign
         "f" #'racket-fold-all-tests
         "F" #'racket-unfold-all-tests
         "h" nil                        ; rebind to prefix "d", key "d"
         "i" #'racket-unicode-input-method-enable
         "l" #'racket-logger
         "o" #'racket-profile
         "p" #'racket-cycle-paren-shapes
         "t" #'racket-test
         "u" #'racket-backward-up-list
         "y" #'racket-insert-lambda
         "s" nil                        ; rebind to prefix "r", key "r"
         "R" nil                        ; rebind to prefix "r", key "s"
         (:prefix ("r" . "run")
                  "r" #'racket-run
                  "s" #'racket-run-and-switch-to-repl
                  "m" #'racket-run-module-at-point)
         (:prefix ("m" . "macros")
                  "d" #'racket-expand-definition
                  "e" #'racket-expand-last-sexp
                  "r" #'racket-expand-region
                  "a" #'racket-expand-again)
         (:prefix ("g" . "goto")
                  "b" #'racket-unvisit
                  "d" #'xref-find-definitions ; replace obsolete command
                  "m" #'racket-visit-module
                  "r" #'racket-open-require-path)
         (:prefix ("e" . "eval")
                  "d" #'racket-send-definition
                  "e" #'racket-send-last-sexp
                  "r" #'racket-send-region)
         (:prefix ("h" . "help")
                  "d" #'racket-xp-describe
                  "s" #'racket-describe-search
                  "[" #'racket-describe-back
                  "]" #'racket-describe-forward)
         :map racket-repl-mode-map
         "l" #'racket-logger
         "h" nil                        ; rebind to "d"
         "d" #'racket-repl-documentation
         "y" #'racket-insert-lambda
         "u" #'racket-backward-up-list
         (:prefix ("m" . "macros")
                  "d" #'racket-expand-definition
                  "e" #'racket-expand-last-sexp
                  "f" #'racket-expand-file
                  "r" #'racket-expand-region)
         (:prefix ("g" . "goto")
                  "b" #'racket-unvisit
                  "m" #'racket-visit-module
                  "d" #'racket-repl-visit-definition))))

;;  ____________________________________________________________________________
;;; ERLANG

(after! erlang
  ;; FIXME repl-handler and eval-handler don't work with Doom out-of-the box
  (set-repl-handler! 'erlang-mode #'erlang-repl)
  (set-eval-handler! 'erlang-mode #'erlang-compile)
  (set-popup-rule! ".*\\*erlang\\*" :size 0.35 :quit nil :ttl nil)
  (defun erlang-compile-debug ()
    "Compile for debug with debug_info and export_all."
    (interactive)
    (inferior-erlang-compile t))
  (map! :localleader
        (:map erlang-mode-map
         :n "'"   #'erlang-shell
         :n "SPC" #'erlang-shell-display
         :n "c"   #'erlang-compile
         :n "C"   #'erlang-compile-debug
         (:prefix ("g" . "goto")
          :n "f"  #'erlang-beginning-of-function
          :n "F"  #'erlang-end-of-function
          :n "c"  #'erlang-beginning-of-clause
          :n "C"  #'erlang-end-of-clause)
         (:prefix ("m" . "mark")
          :n "f"  #'erlang-mark-function
          :n "c"  #'erlang-mark-clause))))

;;  ____________________________________________________________________________
;;; GLEAM
;; <https://github.com/gleam-lang/gleam-mode>

(use-package! gleam-ts-mode
  :when (modulep! :lang gleam)
  :defer t
  :mode (rx ".gleam" eos)
  :config
  (when (modulep! :tools lsp +eglot)
    (after! eglot
      (pushnew! eglot-server-programs
                '((gleam-ts-mode) . ("gleam" "lsp")))
      (add-hook! 'gleam-ts-mode-hook #'eglot-ensure))))

;;  ____________________________________________________________________________
;;; OCAML

(use-package! utop
  :when (and (modulep! :lang ocaml) (modulep! :tools eval))
  :config
  (set-repl-handler! 'tuareg-mode #'utop)
  (set-eval-handler! 'tuareg-mode #'utop-eval-region)
  (set-popup-rule! "^\\*utop\\*" :size 0.35 :quit nil :ttl nil)
  (when (featurep 'aggressive-indent)
    (cl-pushnew #'tuareg-interactive-mode aggressive-indent-excluded-modes))
  ;; (setq! utop-command "opam exec -- utop -emacs")
  (setq! utop-command "opam exec -- dune utop . -- -emacs")
  (when (modulep! :lang ocaml -lsp)
    (add-hook! 'tuareg-interactive-mode-hook
               ;; HACK: Thats how to get completions from Merlin
               ;; <https://github.com/ocaml-community/utop/issues/455#issuecomment-2061093803>
               (progn (merlin-mode +1) (merlin-mode -1)))
    (add-hook! 'utop-mode-hook
               ;; HACK: Thats how to get completions from Merlin
               ;; <https://github.com/ocaml-community/utop/issues/455#issuecomment-2061093803>
               (progn (merlin-mode +1) (merlin-mode -1)))))

(when (modulep! :lang ocaml)
  (defun opam-install-basic-packages ()
    "Install standard package selection for editor support and development."
    (interactive)
    (message (concat "Installing basic packages for switch: "
                     (opam-switch--get-current-switch)))
    (start-process
     "opam-install-basic-packages"        ; Emacs process name
     "*opam-install*"                     ; Emacs output buffer
     "opam" "install"                     ; shell command
     "--yes"                              ; answer "yes" to all questions
     ;; Package selection
     "dune"
     "dune-release"
     "merlin"
     "tuareg"
     "ocaml-lsp-server"
     "ocamlformat"
     "utop"
     "odoc"
     "omod"
     "domainslib"
     )))

;;  ____________________________________________________________________________
;;; LOAD EXTERNAL ELISP

;; Packages are natively compiled, run `doom sync --gc --aot' after changes

;; Private user information
(use-package! my-private)

;; Personal Elisp
(use-package! my-tools)

;; MJJ blog machinery
;; <https://github.com/monkeyjunglejuice/monkeyjunglejuice.github.io/tree/master/static>
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
