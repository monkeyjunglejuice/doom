;;; $DOOMDIR/packages.el -*- no-byte-compile: t; -*-

(package! eshell-did-you-mean :disable t)
(package! evil-snipe :disable t)
(package! iedit :disable t)
(package! ivy :disable t)
(package! swiper :disable t)
(package! flycheck-popup-tip :disable t)
;; (package! ace-window :disable t)

(package! evil-swap-keys)
(package! aggressive-indent)
(package! clean-kill-ring)
(package! org-sticky-header)
(package! show-font)
(package! srfi)
(package! scheme-complete)

(package! modus-themes
  :recipe (:host github :repo "protesilaos/modus-themes"
           :build t)
  :pin "895e10936adac93aa8187c9cc91092dbca898677")  ; v4.6.0

(package! my-elisp
  :recipe (:local-repo "~/.emacs.lisp"
           :build t))

(package! mjj-publish
  :recipe (:local-repo "~/Documents/monkeyjunglejuice"
           :files ("static/*.el")
           :build t))

;;  ____________________________________________________________________________
;;; COMMENTARY

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")

;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)
