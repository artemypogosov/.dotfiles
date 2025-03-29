;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 'battery)
(require 'lsp)
(require 'lsp-haskell)

;; Place your private configuration here.
;; Remember, you do not need to run 'doom sync' after modifying this file.

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name    "Artem Pogosov"
      user-mail-address "artemypogosov@gmail.com")

;;;;;;;;;;;;;
;;; FONTS ;;;
;;;;;;;;;;;;;

;; ‘doom-font’ – standard monospace font that is used for most things in Emacs.
;; ‘doom-variable-pitch-font’ – variable font which is useful in some Emacs plugins.
;; ‘doom-big-font’ – used in doom-big-font-mode; useful for presentations.
;; ‘font-lock-comment-face’ – for comments.
;; ‘font-lock-keyword-face’ – for keywords with special significance like ‘setq’ in elisp.

(setq doom-font (font-spec :family "Source Code Pro" :size  15)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
      doom-big-font (font-spec :family "Source Code Pro" :size 20))

;;;;;;;;;;;;;;
;;; THEMES ;;;
;;;;;;;;;;;;;;

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
 '(font-lock-comment-face :slant italic)
 '(font-lock-keyword-face :slant italic))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Org/")
(setq org-hide-emphasis-markers t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; PROJECTILE
(setq projectile-project-search-path '("~/Projects"))

;; OTHER
(setq undo-limit 80000000               ; Raise undo-limit to 80Mb
      evil-want-fine-undo t             ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t               ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"      ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil)        ; I can trust my computers ... can't I?

(display-time-mode 0)


;; (global-subword-mode 1)

;; Hooks so haskell and literate haskell major modes trigger LSP setup
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(defun org-todo-region ()
  (interactive)
  (let ((scope (if mark-active 'region 'tree))
        (state (org-fast-todo-selection))
        (org-enforce-todo-dependencies nil))
    (org-map-entries (lambda () (org-todo state)) nil scope)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MY FUNCTIONS AND BINDINGS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples
;;(map! :leader "c z" #'kill-buffer-and-window)
;;(map! :nv "s" (cmd! (evil-ex "%s//g")))
(setq evil-ex-substitute-case 'sensitive)

(defun substitute-and-restore (mode)
 (interactive)
  (save-excursion
    (let ((original-pos (point))
          (expression (cond
                        ((string= mode "with-confirmation") "%s##gc")
                        ((string= mode "no-confirmation") "%s##g")
                        (t "default"))))
      ;; Perform the substitution
      (minibuffer-with-setup-hook
          (lambda () (backward-char (if (string= mode "with-confirmation") 3 2)))
        (evil-ex expression))
      (goto-char original-pos))))


(defun substitute-and-restore-dispatch ()
  "Dispatch `substitute-and-restore` with the correct mode based on the key pressed."
  (interactive)
  ;; Get the pressed key
  (let* ((key (key-description (this-command-keys)))
         (mode (cond
                ((string= key "f") "with-confirmation")
                ((string= key "F") "no-confirmation")
                (t "default"))))
    (substitute-and-restore mode)))


(evil-define-key 'normal 'global (kbd "f") 'substitute-and-restore-dispatch)
(evil-define-key 'normal 'global (kbd "F") 'substitute-and-restore-dispatch)


(setq evil-snipe-scope 'buffer)


(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.05)))))

;; -------------------------------------------------------------------
;;
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `Load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `Add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
