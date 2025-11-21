;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here.
;; Remember, you do not need to run 'doom sync' after modifying this file.

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Artem Pogosov"
      user-mail-address "artemypogosov@gmail.com")

;; ‘doom-font’ – standard monospace font that used for most things in Emacs.
;; ‘doom-variable-pitch-font’ – variable font which is useful in some Emacs plugins.
;; ‘doom-big-font’ – used in doom-big-font-mode; useful for presentations.
;; ‘font-lock-comment-face’ – for comments.
;; ‘font-lock-keyword-face’ – for keywords with special significance like ‘setq’ in elisp.

 (setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 17)
       doom-variable-pitch-font (font-spec :family "Ubuntu" :size 17)
       doom-big-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 20))

;; Face style to use with comments and keywords
(custom-set-faces!
 '(font-lock-comment-face :slant italic)
 '(font-lock-keyword-face :slant italic))

;; Use Noto Color Emoji for emoji characters
;; Also used to draw icons in modeline
(set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
(setq doom-theme 'doom-gruvbox)

(after! doom-themes
  ;; Disable bold font in every mode (in ORG->'Headings style' section bold is added)
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic t))

;; 'undo-limit' 80000000 - raise undo-limit to 80Mb
;; 'evil-want-fine-undo' - by default while in insert all changes are one big blob. Be more granular
;; 'auto-save-default'   - nobody likes to loose work, I certainly don't
;; 'truncate-string-ellipsis'  - unicode ellipsis are nicer than "...", and also save space
;; 'password-cache-expiry'     - I can trust my computers... can't I?
;; 'delete-by-moving-to-trash' - remove files to trash
;; 'display-line-numbers-type' - style of line numbers. For relative line numbers, set this to `relative'.
;; 'evil-ex-substitute-case'   - case sensitive match in evil-ex-substitute command
;; 'global-auto-revert-non-file-buffers' - enables Auto Reverting for all types of buffers
;; 'evil-snipe-scope' - scope of evil-snipe search
;; 'imenu-list-focus-after-activation' - focus imenu-list side window after it was toggled
(setq
 undo-limit 80000000
 evil-want-fine-undo t
 auto-save-default t
 truncate-string-ellipsis "…"
 password-cache-expiry nil
 delete-by-moving-to-trash t
 trash-directory "~/.local/share/Trash/files"
 projectile-project-search-path '("~/Projects")
 display-line-numbers-type 'relative
 evil-ex-substitute-case 'sensitive
 evil-snipe-scope 'buffer
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil
 +zen-text-scale 1
 imenu-list-auto-resize t
 imenu-list-focus-after-activation t)

;; 'setq' vs 'setq-default'
;; Use 'setq' for non-buffer-local variables
;; (variables that are truly global)
;; Use 'setq-default' for buffer-local variables
;; Buffer local variables have a default value.
;; So all buffers inherit the default value.

;; 'fill-column' - display vertical limit line
(setq-default fill-column 120)

(defun my/set-fill-column-if-unset ()
  (unless (local-variable-p 'fill-column)
    (setq-default fill-column 120)))

(add-hook! prog-mode
  (my/set-fill-column-if-unset)
  (display-fill-column-indicator-mode))

;; 'global-auto-revert-mode' - auto sync buffers when they are changed by other process
(global-auto-revert-mode t)
(global-display-fill-column-indicator-mode 1)

(defun my/generate-dashboard ()
  (let* ((art '(" ⠀⠀⠀⠀⠀⠀⠀⢠⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⡄⠀⠀⠀⠀⠀⠀⠀ "
                " ⠀⠀⠀⠀⠀⠀⠀⢸⣿⣷⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⣾⣿⡇⠀⠀⠀⠀⠀⠀⠀ "
                " ⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣦⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣴⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀ "
                " ⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣿⣷⡀⢰⠒⠒⠢⣤⠔⠒⠒⡆⢀⣼⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀ "
                " ⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣿⣿⣿⡉⠁⠀⠀⠀⠀⠀⠈⢉⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀ "
                " ⠀⠀⠀⠀⠀⠀⣠⢼⣿⣿⣿⣿⡿⠿⠓⠀⠀⠀⠀⠀⠀⠀⠚⠻⠿⣿⣿⣿⣿⡧⣄⠀⠀⠀⠀⠀⠀ "
                " ⠀⠀⠀⠀⢠⠎⠁⠈⣿⠟⠉⠁⠀⢀⣀⣤⣶⣶⣶⣶⣶⣤⣀⡀⠀⠈⠉⠻⢿⠁⠈⠱⣄⠀⠀⠀⠀ "
                " ⠀⠀⠀⣰⠃⠀⠀⠀⠀⠀⠀⣠⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣄⠀⠀⠀⠀⠀⠀⠈⢆⠀⠀⠀ "
                " ⠀⠀⢠⠇⠀⠀⠀⠀⠀⠠⠾⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠿⠷⠄⠀⠀⠀⠀⠀⠸⡄⠀⠀ "
                " ⠀⠀⠈⡽⠃⠀⠀⠀⣴⣶⣶⣶⣤⡈⠻⣿⣿⣿⣿⣿⣿⣿⠟⢁⣤⣶⣶⣶⣦⠀⠀⠀⠘⢫⡁⠀⠀ "
                " ⠀⢀⡞⠀⠀⠀⠀⣸⣿⣿⠿⠿⢿⣿⣦⠙⣿⣿⣿⣿⣿⠋⣴⣿⡿⠿⠿⣿⣿⣧⠀⠀⠀⠀⢳⡀⠀ "
                " ⠀⡞⠀⠀⠀⠀⢰⣿⠋⢀⣠⣄⡀⠙⢿⣧⠘⣿⣿⣿⠃⣼⡿⠋⢀⣠⣄⡈⠙⣿⡇⠀⠀⠀⠀⢱⠀ "
                " ⣸⠀⠀⠀⠀⠀⣼⡇⢰⣿⣿⣿⣿⡆⠈⣿⣆⢻⣿⡟⢰⣿⠁⢰⣿⣿⣿⣿⡆⢸⣧⠀⠀⠀⠀⠀⣇ "
                " ⡏⠀⠀⠀⠀⠀⣿⡇⢸⣿⣿⣿⣿⡿⠀⣿⣿⡈⠿⢁⣿⣿⠀⢿⣿⣿⣿⣿⡇⢸⣿⠀⠀⠀⠀⠀⢸ "
                " ⡇⠀⡄⠀⠀⠀⣿⣷⡀⠙⠿⠿⠟⢁⣼⣿⣿⣿⣶⣿⣿⣿⣧⡈⠻⠿⠿⠋⢀⣾⣿⠀⠀⠀⢠⡀⢸ "
                " ⠷⠚⡇⠀⠀⠀⢹⣿⣿⣶⣤⣤⣶⣿⣿⠿⠛⠉⠉⠉⠛⠿⣿⣿⣶⣤⣤⣶⣿⣿⡏⠀⠀⠀⢸⠑⠾ "
                " ⠀⠀⡇⠀⠀⠀⠈⡉⠛⠻⠿⠿⠛⠋⣡⣴⣿⣿⣿⣿⣿⣦⣌⡙⠛⠿⠿⠟⠛⢉⠁⠀⠀⠀⢸⠀⠀ "
                " ⠀⠀⢇⠀⠀⠀⠀⢻⣿⣶⣶⣶⣾⣿⣿⣿⠋⣠⣤⣄⠙⣿⣿⣿⣷⣶⣶⣶⣿⡟⠀⠀⠀⠀⢸⠀⠀ "
                " ⠀⠀⢸⡀⠀⠀⠀⠀⠹⣿⣿⣿⣿⣿⣿⡇⣼⣿⣿⣿⣧⠘⣿⣿⣿⣿⣿⣿⠏⠀⠀⠀⠀⢀⡇⠀⠀ "
                " ⠀⠀⠀⢣⠀⠀⠀⠀⠀⠙⢿⣿⣿⣿⣿⣇⢹⣿⣿⣿⡟⢰⣿⣿⣿⣿⡿⠋⠀⠀⠀⠀⠀⡜⠀⠀⠀ "
                " ⠀⠀⠀⠈⢣⡀⣧⡀⠀⠀⠀⠙⠿⣿⣿⣿⣦⣉⠉⣉⣴⣿⣿⣿⠿⠋⠀⠀⠀⢀⣴⠀⡜⠁⠀⠀⠀ "
                " ⠀⠀⠀⠀⠀⠙⠉⠘⢢⡀⠀⠀⠀⠀⠉⠛⠛⠛⠛⠛⠛⠛⠉⠀⠀⠀⠀⢀⡴⠋⠈⠋⠀⠀⠀⠀⠀ "
                " ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⠦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡴⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀ "
                " ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠓⢤⡀⠀⠀⠀⠀⠀⠀⠀⢀⡤⠞⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ "
                " ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠑⠒⠤⠤⠤⠒⠊⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ "
                "                                       "
                "⠀     To see with eyes unclouded by hate.⠀⠀   "))
         (longest-line (apply #'max (mapcar #'length art))))
    (put-text-property
     (point)
     (dolist (line art (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'my/generate-dashboard)

(defun my/session-file-exists ()
  "Check if a session file exists based on workspace or desktop settings."
  (cond
   ((modulep! :ui workspaces)
    (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
   ((require 'desktop nil t)
    (file-exists-p (desktop-full-file-name)))))

(setq +doom-dashboard-menu-sections
      '(("Recent files" :action recentf-open-files)
        ("Open project" :action projectile-switch-project)
        ("Last session" :action doom/quickload-session :when (my/session-file-exists))
        ("Bookmarks"    :action bookmarks-jump)
        ("Org-agenda"   :action org-agenda :when (fboundp 'org-agenda))))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(after! doom-modeline
  (setq doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-highlight-modified-buffer-name t
        doom-modeline-position-column-format '("")
        mode-line-position-line-format '("")
        doom-modeline-buffer-encoding nil
        doom-modeline-project-name nil
        doom-modeline-persp-name nil
        doom-modeline-persp-icon nil
        doom-modeline-modal nil
        doom-modeline-indent-info t
        doom-modeline-display-misc-in-all-mode-lines nil)

  (display-time-mode -1)
  (column-number-mode -1)
  (line-number-mode -1)
  ;; Disable size indication in all buffers
  (add-hook! after-change-major-mode (size-indication-mode -1)))

;; Enable file's follow mode in dirvish and treemacs side bars
(after! treemacs
  (treemacs-follow-mode 1))

(defun my/open-home-dired ()
  (interactive)
  (dired "~"))

(after! dired
  (require 'dired-x)

  ;; Hide dotfiles by default & update dired buffer after some changes
  (add-hook! dired-mode (dired-omit-mode) (auto-revert-mode))

  ;; Hide dotfiles when omit mode is ON
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

  ;; Override Evil so Backspace toggles omit mode
  (map! :map dired-mode-map
        :n "DEL" #'dired-omit-mode
        :n "<backspace>" #'dired-omit-mode)

  (map! :map dirvish-mode-map
        :n "DEL" #'dired-omit-mode
        :n "<backspace>" #'dired-omit-mode))

;; 'dirvish' - extends 'dired'
(after! dirvish
  (dirvish-side-follow-mode 1)
  (setq dirvish-hide-details t
        dired-mouse-drag-files t
        dirvish-mode-line-format '(:left (sort file-time symlink) :right (yank index))
        ;; Use 'b' + letter
        dirvish-quick-access-entries
        '(("h" "~/" "Home")
          ("t"  "~/.local/share/Trash/" "Trash")
          ("o" "~/Org" "Org")
          ("d" "~/Downloads" "Downloads")
          ("pi" "~/Pictures" "Pictures")
          ("pr" "~/Projects" "Projects"))))

;; 'TODO'      - needs to be done
;; 'NEXT'      - next one to be considered
;; 'STARTED'   - in progress
;; 'WAIT'      - blocked by something, have to wait
;; 'HOLD'      - hold (wait) on purpose
;; 'DONE'      - ready
;; 'CANCELLED' - no longer needed

(defconst my/org-root-dir "~/Org")
(defconst my/org-personal-dir (directory-files-recursively (concat my/org-root-dir "/agenda/personal") "\\.org$"))
(defconst my/org-work-dir (directory-files-recursively (concat my/org-root-dir "/agenda/work") "\\.org$"))

(after! org
  (setq org-directory my/org-root-dir
        org-startup-folded 'content
        ;; Location of .orgids
        org-id-locations-file (concat my/org-root-dir "/.orgids")
        org-agenda-files  (append my/org-personal-dir my/org-work-dir (list "~/Org/inbox.org"))
        org-fancy-priorities-list '("" "" "")
        org-superstar-headline-bullets-list '( "●" "○" "⟁"  "⟐" "✿")
        org-tag-alist '(;; Affiliation
                        ("personal" . ?P) ("work" . ?W)
                        ;; Projects...
                        ;; Activities
                        ("shopping" . ?S) ("gym" . ?G) ("birthday" . ?B)
                        ;; Other
                        ("wishlist" . ?L)  ("repeated" . ?R))
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "STARTED(s!)" "WAIT(w)" "HOLD(h)" "|" "DONE(d!)" "CANCELLED(c)"))
        org-todo-keyword-faces '(("TODO"      :foreground "#afb224" :underline t)
                                 ("NEXT"      :foreground "#fabd2f" :underline t)
                                 ("STARTED"   :foreground "#b16286" :underline t)
                                 ("HOLD"      :foreground "#458588" :underline t)
                                 ("WAIT"      :foreground "#fe8019" :underline t)
                                 ("DONE"      :foreground "#665c54" :underline t)
                                 ("CANCELLED" :foreground "#cc241d" :underline t))
        org-hide-emphasis-markers t))

(custom-set-faces!
  '(org-level-1 :foreground "#83a598" :inherit outline-1 :height 1.2    :weight bold)
  '(org-level-2 :foreground "#e7ab36" :inherit outline-2 :height 1.1    :weight bold)
  '(org-level-3 :foreground "#9e7edf" :inherit outline-3 :height 1.05   :weight bold)
  '(org-level-4 :foreground "#5e8b4d" :inherit outline-4 :height 1.025  :weight bold)
  '(org-level-5 :foreground "#d44c3b" :inherit outline-5 :height 1.0125 :weight bold)
  '(org-link    :foreground "#64a2f4"))

(setq org-deadline-warning-days 7
      org-agenda-custom-commands
      '(("p" "Personal"
         ((agenda "" ((org-agenda-files my/org-personal-dir)))
          (tags-todo "personal" ((org-agenda-overriding-header "Personal Tasks:")))
          (tags-todo "-{.*}" ((org-agenda-overriding-header "Untagged Tasks:")
                              (org-agenda-files my/org-personal-dir)))))
        ("w" "Work"
         ((agenda "" ((org-agenda-files my/org-work-dir)))
          (tags-todo "work" ((org-agenda-overriding-header "Work tasks:")))
          (tags-todo "-{.*}" ((org-agenda-overriding-header "Untagged Tasks:")
                              (org-agenda-files my/org-work-dir)))))
        ("i" "Inbox"
         ((agenda "" ((org-agenda-files '("inbox.org"))))
          (todo "" ((org-agenda-files '("inbox.org"))
                    (org-agenda-overriding-header "Inbox notes:"))))) ))

(after! org
  (setq org-roam-directory my/org-root-dir
        org-roam-capture-templates
        '(("d" "Default" plain
           "%?"
           :if-new (file+head "notes/${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)

          ("L" "Linux" plain
           "%?"
           :if-new (file+head "computer_science/linux/${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)

          ("l" "Linux Cheatsheets" plain
           "%?"
           :if-new (file+head "computer_science/linux/cheatsheets/${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)

          ("p" "Programming" plain
           "%?"
           :if-new (file+head "computer_science/programming/${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)

          ("n" "Network" plain
           "%?"
           :if-new (file+head "computer_science/network/${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)

          ("e" "English" plain
           "%?"
           :if-new (file+head "english/${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t))))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(after! org
  (add-hook! org-mode
    (global-display-fill-column-indicator-mode -1)
    (org-superstar-mode)
    (org-fancy-priorities-mode)
    (add-hook! after-save :local (org-babel-tangle))))

(after! org
  (use-package! calfw
    :init
    (setq cfw:render-line-breaker #'cfw:render-line-breaker-wordwrap
          cfw:display-calendar-holidays nil
          calendar-week-start-day 1)))

;; always open the preview window at the right
(setq markdown-split-window-direction 'right)

(after! web-mode
  ;; Enable auto-closing tags in web-mode (like html files)
  (require 'sgml-mode)
  ;; Identation
  (add-hook! web-mode
    (sgml-electric-tag-pair-mode)
    ;; Only set defaults if no .editorconfig is active for this buffer
    (let ((has-editorconfig (and (boundp 'editorconfig-properties-hash)
                                 editorconfig-properties-hash)))
      (unless has-editorconfig
        (setq-local web-mode-markup-indent-offset 2
                    web-mode-css-indent-offset    2
                    web-mode-code-indent-offset   2)))))

;; EMMET (html, css)
(defun +web/indent-or-yas-or-emmet-expand ()
  "Do-what-I-mean on TAB.
   Invokes `indent-for-tab-command' if at or before text bol,
  `yas-expand' if on a snippet, or `emmet-expand-line'."
  (interactive)
  (call-interactively
   (cond
    ((or (<= (current-column) (current-indentation))
         (not (eolp))
         (not (or (memq (char-after) (list ?\n ?\s ?\t))
                  (eobp))))
     #'indent-for-tab-command)
    ((and (modulep! :editor snippets)
          (require 'yasnippet nil t)
          (yas--templates-for-key-at-point))
     #'yas-expand)
    (t #'emmet-expand-line))))

;; 'indent-bars-mode' - shows vertical bars to visually indicate indentation levels
(add-hook! after-change-major-mode
  (when (derived-mode-p 'yaml-mode)
    (indent-bars-mode t)))

(after! lsp-mode
  ;; Remove symbol and all usages higlighting
  (setq lsp-enable-symbol-highlighting nil
        lsp-code-action-no-header t
        lsp-code-action-show-menu t)

  ;; Also explicitly remove the highlight hooks
  (remove-hook 'lsp-mode-hook #'lsp-enable-symbol-highlighting))

(setq-hook! 'python-mode-hook +format-with 'black)

;; Disable code formatter in yaml
(after! yaml-mode
  (add-hook! yaml-mode (apheleia-mode -1)))

(after! spell-fu
  (setq spell-fu-idle-delay 0.5) ; default is 0.25
  (setq-default spell-fu-word-regexp "\\b\\([A-Za-z]+\\(['’][A-Za-z]+\\)?\\)\\b")

  (remove-hook 'prog-mode-hook #'spell-fu-mode)

  ;; Enable only in text-like modes
  (add-hook! org-mode (spell-fu-mode))
  (add-hook! markdown-mode (spell-fu-mode))
  (add-hook! text-mode (spell-fu-mode)))

;; 'rainbow-mode' - default mode for highlighting colors in Doom Emacs
(remove-hook 'prog-mode-hook #'rainbow-mode)
(remove-hook 'css-mode-hook #'rainbow-mode)
(remove-hook 'emacs-lisp-mode-hook #'rainbow-mode)

;; 'colorful-mode' - inline preview of hex code colors
(use-package! colorful-mode
  :custom
  (colorful-use-prefix t)
  (colorful-prefix-string "•")
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode +1))

(add-hook! org-mode
  (setq prettify-symbols-alist '(("#+begin_src"   . "»")
                                 ("#+end_src"     . "«")
                                 ("#+begin_quote" . "❝")
                                 ("#+end_quote"   . "❞")))
  (prettify-symbols-mode 1))

(use-package! idle-underline-mode
  :hook (prog-mode . idle-underline-mode)
  :init
  (setq idle-underline-idle-time 0.2))

(after! idle-underline-mode
  (set-face-attribute 'idle-underline nil
                      :underline t
                      :background 'unspecified
                      :inherit nil))

(visual-replace-global-mode 1)

(setq visual-replace-keep-initial-position t
      visual-replace-default-to-full-scope t)

(after! visual-replace
  (add-hook! visual-replace-minibuffer-mode (visual-replace-toggle-case-fold)))

(use-package! drag-stuff
  ;; Use :defer 't in order to lazy load the package
  :defer t
  :init
  ;; Enable in certain modes (optional)
  (add-hook! (prog-mode text-mode) (drag-stuff-mode))
  :config
  ;; Keybindings in evil-visual-state (most useful here)
  (define-key evil-visual-state-map (kbd "M-j") #'drag-stuff-down)
  (define-key evil-visual-state-map (kbd "M-k") #'drag-stuff-up)

  ;; Optional: enable for normal mode line dragging
  (define-key evil-normal-state-map (kbd "M-j") #'drag-stuff-down)
  (define-key evil-normal-state-map (kbd "M-k") #'drag-stuff-up))

;; Dim inactive windows
(dimmer-configure-org)
(dimmer-configure-magit)
(dimmer-configure-which-key)
(dimmer-configure-company-box)
(dimmer-mode t)

;; Allows you to use keybindings with non English layouts
(use-package! reverse-im
  :custom
  ;; Replace with your input method, for example "ukrainian-computer"
  (reverse-im-input-methods '("ukrainian-computer" "russian-computer"))
  :config
  (reverse-im-mode t))

(after! company
  ;; Core behavior settings
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.2
        company-show-quick-access t
        company-tooltip-limit 20
        company-tooltip-align-annotations t)

  ;; Prioritize company-files
  (setq company-backends (cons 'company-files (delete 'company-files company-backends))
        company-files-exclusions nil
        company-files-chop-trailing-slash t)

  ;; Helper: check if something looks like a file path
  (defun my/looks-like-path-p (input)
    "Return t if INPUT looks like a file path."
    (or (string-match-p "^/" input)              ;; Absolute
        (string-match-p "^~/" input)             ;; Home dir
        (string-match-p "^\\.\\{1,2\\}/" input)   ;; ./ ../
        (string-match-p "^[a-zA-Z0-9._-]+/" input))) ;; relative like foo/bar

  ;; Custom backend that triggers file path completion
  (defun my/company-path-trigger (command &optional arg &rest ignored)
    "Company backend to trigger file path completion."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-files))
      (prefix
       (let ((grabbed (or (company-grab-symbol) "")))
         (when (my/looks-like-path-p grabbed)
           (company-files 'prefix))))
      (t (apply 'company-files command arg ignored))))

  ;; Enable for all major modes, but avoid duplicates
  (defun my/enable-path-completion ()
    "Add file path completion trigger if not already present."
    (setq-local company-backends
                (cl-remove-duplicates
                 (cons 'my/company-path-trigger company-backends)
                 :test #'equal)))

  (add-hook! after-change-major-mode (my/enable-path-completion)))

;; "ID of the last appt notification so it can be updated instead of duplicated."
(defvar my/appt-notification-id nil)

(after! appt
  (setq appt-message-warning-time 10
        appt-display-interval 1)

  (require 'notifications)

  (setq appt-disp-window-function
        (lambda (remaining _new-time msg)
          (setq my/appt-notification-id
                (notifications-notify
                 :title (format "In %s minutes" remaining)
                 :body msg
                 :urgency 'normal
                 :replaces-id my/appt-notification-id))))

  (advice-add 'appt-check :before #'org-agenda-to-appt)
  (appt-activate t))

(defun my/visual-replace-with-query ()
  "Call visual-replace with query mode for this invocation only."
  (interactive)
  (let (hook) ;; declare hook first
    (add-hook! minibuffer-setup
      (visual-replace-toggle-query)
      (remove-hook 'minibuffer-setup-hook hook))
    (call-interactively #'visual-replace)))

(map! :leader
      (:prefix ("r" . "replace")
       :desc "Replace" "r" #'visual-replace
       :desc "Replace at point" "p" #'visual-replace-thing-at-point
       :desc "Replace selected" "s" #'visual-replace-selected
       :desc "Replace in line" "l" (lambda ()
                                     (interactive)
                                     (let ((pos (point)))  ;; save current position
                                       (goto-char (line-beginning-position))
                                       (push-mark (line-end-position) t t)
                                       (call-interactively #'visual-replace)
                                       (goto-char pos)))
       :desc "Replace with confirm" "c" #'my/visual-replace-with-query))

(define-key visual-replace-mode-map (kbd "+") visual-replace-secondary-mode-map)

;; Vim text-objects alternative for the lazy
(after! expand-region
  (map! :nv "M-e" #'er/expand-region)
  (map! :leader
        (:prefix ("e" . "expand-region")
         :desc "Mark JS function" "f" #'er/mark-js-function ;; vaf
         :desc "Mark JS inner return" "r" #'er/mark-js-inner-return ;; vi(
         :desc "Mark JS outer return" "R" #'er/mark-js-outer-return
         :desc "Mark JS if" "i" #'er/mark-js-if ;; vaj / vij
         :desc "Mark JS call" "c" #'er/mark-js-call ;; vaF
         :desc "Mark JS comment" "C" #'er/mark-comment ;; vac
         :desc "Mark inside pairs" "B" #'er/mark-inside-pairs ;; viB
         :desc "Mark paragraph" "p" #'er/mark-paragraph ;; vip
         :desc "Mark url" "u" #'er/mark-url ;; vau
         :desc "Mark inside quotes" "q" #'er/mark-inside-quotes ;; vi" / viq
         :desc "Mark outside quotes" "Q" #'er/mark-outside-quotes ;; va" / vaq
         :desc "Mark html attribute" "a" #'er/mark-html-attribute
         :desc "Mark JS object property" "o" #'er/mark-js-object-property ;; vaj / via / vaa / vaA
         :desc "Mark org code block" "e" #'er/mark-org-code-block))) ;; vae / vie

(after! org
  (map! :leader
        :prefix ("n" . "notes")
        (:prefix ("r" . "roam")
         :desc "Open UI graph" "o" #'org-roam-ui-open)))

(map! :leader
      :prefix ("g" . "git")
      :desc "Open file in remote repo" "O" #'+vc/browse-at-remote)

(map! :leader
      (:prefix ("s" . "search")
       :desc "Search TODO" "." #'hl-todo-occur
       :desc "Search TODO from dir" "," #'hl-todo-rgrep))

(map! :leader
      (:prefix ("o" . "open")
       :desc "Bookmark manager" "m" #'list-bookmarks))

(map! :leader
     (:prefix ("d" . "devdocs")
      :desc "Install" "+" #'devdocs-install
      :desc "Delete" "-" #'devdocs-delete
      :desc "Lookup" "l" #'devdocs-lookup
      :desc "Select docs" "L" #'devdocs-peruse
      :desc "Search on the site" "s" #'devdocs-search
      :desc "Update all docs" "u" #'devdocs-update-all))

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle treemacs" "t" #'+treemacs/toggle
       :desc "Toggle imenu sidebar" "m" #'imenu-list-smart-toggle))

;; Windows manipulation
(map! :leader
      :prefix "w"
      "M" #'maximize-window
      "C" #'delete-other-windows
      "z" #'windresize)

;; Calendar
(map! :leader
      (:prefix ("o" . "open")
       :desc "Open calendar" "c" #'=calendar))

;; Markdown
(after! markdown-mode
  (map! :localleader
        :mode markdown-mode
        :desc "Markdown live preview" "l" #'markdown-live-preview-mode))

;; Complete file path
(map! :i "M-p" #'company-files)

;; Manage workspaces
(map! :leader
      :prefix "TAB"
      :desc "Delete workspace" "k" #'+workspace/kill
      :desc "Delete saved workspace" "K" #'+workspace/delete)

;; Quit Emacs
(map! :leader
      :prefix "q"
      :desc "Quit Emacs and ask to save" "Q" #'evil-quit-all)

;; Help
(map! :leader
      :prefix "h"
      :desc "Find text in documentation" "a" #'apropos-documentation
      :desc "Man page" "w" #'+default/man-or-woman)

;; Save buffer by pressing C-s
(after! evil
  (define-key evil-insert-state-map (kbd "C-s")
              (lambda ()
                (interactive)
                (save-buffer)
                (evil-normal-state)))
  (define-key evil-normal-state-map (kbd "C-s") #'save-buffer))

;; Comment lines
(defun my/comment-line-and-next ()
  "Comment the current line and move to the next."
  (interactive)
  (evilnc-comment-or-uncomment-lines 1)
  (forward-line 1))

(after! evil
  (define-key evil-normal-state-map (kbd "C-/") #'my/comment-line-and-next)
  (define-key evil-insert-state-map (kbd "C-/") #'my/comment-line-and-next))

;; SPC
(map! :leader "'" nil "~" nil "*" nil ";" nil "a" nil "X" nil ":" nil "x" nil "u" nil)

;; Window
(map! :leader :prefix "w"
      "C-<up>"  nil "C-<down>"  nil "C-<left>"  nil "C-<right>" nil "<up>"  nil
      "<down>"  nil "<left>"    nil "<right>"   nil "C-="       nil "C-_"   nil
      "d"       nil "g"         nil "o"         nil ":"         nil)

;; Toggle
(map! :leader :prefix "t" "d" nil)

;; Org-mode
(map! :after org :map org-mode-map :localleader "*" nil "@" nil "a" nil "c" nil "g" nil "n" nil "s" nil "r" nil "P" nil)

;; Buffer
(map! :leader :prefix "b" "d" nil "n" nil "p" nil "l" nil "z" nil "m" nil "M" nil "B" nil "Z" nil "S" nil "C" nil)

;; Workspace
(let ((chars "0123456789")
      (special-chars "hjklrsw"))
  (dotimes (i (length chars))
    (let ((key (format "%c" (aref chars i))))
      (map! :leader :prefix "TAB" key nil))))

(map! :leader :prefix "TAB" "`" nil "d" nil "D" nil)

;; Help
(map! :leader :prefix "h"
      "RET"    nil "C-\\"   nil "."      nil "4"      nil "<help>" nil "i"      nil
      "A"      nil "C"      nil "<f1>"   nil "E"      nil "F"      nil "g"      nil
      "K"      nil "I"      nil "l"      nil "L"      nil "M"      nil "O"      nil
      "o"      nil "n"      nil "p"      nil "P"      nil "q"      nil "u"      nil
      "W"      nil "V"      nil "R"      nil "T"      nil "s"      nil "S"      nil)

(map! :leader :prefix ("h b" . "bindings") "f" nil "k" nil "t" nil "m" nil)

(map! :leader :prefix ("h d" . "bindings")
      "b" nil "c" nil "d" nil "l" nil "L" nil "n" nil
      "p" nil "t" nil "u" nil "x" nil "N" nil "s" nil "S" nil)

;; Projectile
(map! :leader :prefix "p" "&" nil "f" nil "g" nil "k" nil "o" nil "e" nil)

;; GIT
(map! :leader :prefix ("g" . "git") "'" nil "o" nil "c" nil "D" nil "C" nil "l" nil "f" nil)

;; Insert
(map! :leader :prefix "i" "p" nil "y" nil)

;; File
(map! :leader :prefix "f" "c" nil "d" nil "e" nil "l" nil "p" nil "E" nil)

(dotimes (i 10) (define-key evil-window-map (number-to-string i) nil))

;; Remove all 'SPC w' and 'SPC h' C-<key> bindings
(let ((chars "abcdefghijklmnopqrstuvwxyz")
      (special-chars "hjklrsw"))
  (dotimes (i (length chars))
    (let ((key (format "C-%c" (aref chars i))))
      (map! :leader :prefix "w" key nil)
      (map! :leader :prefix "h" key nil)))
  (dotimes (i (length special-chars))
    (let ((key (format "C-S-%c" (aref special-chars i))))
      (map! :leader :prefix "w" key nil))))
