;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror: (package! some-package)
;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;; (package! another-package :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package :recipe (:host github :repo "username/repo" :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property: (package! builtin-package :disable t)

;; General
(package! evil-tutor)
(package! windresize)
(package! drag-stuff)
(package! imenu-list)
(package! dimmer)
(package! reverse-im)
(package! visual-replace)

;; Programming
(package! devdocs)
(package! colorful-mode)

;; Org
(package! org-superstar)
(package! org-fancy-priorities)

;; Disabled
(package! woman :disable t)
