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

 (setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 16)
       doom-variable-pitch-font (font-spec :family "Ubuntu" :size 16)
       doom-big-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 20))

;; Face style to use with comments and keywords
(custom-set-faces!
 '(font-lock-comment-face :slant italic)
 '(font-lock-keyword-face :slant italic))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
(setq doom-theme 'doom-gruvbox)

(after! doom-themes
  (setq doom-themes-enable-bold t
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
;; 'company-idle-delay' - code completion delay; nil - off code completion; use C-SPC for manual completion
(setq
 undo-limit 80000000
 evil-want-fine-undo t
 auto-save-default t
 truncate-string-ellipsis "…"
 password-cache-expiry nil
 delete-by-moving-to-trash t
 trash-directory "~/.local/share/Trash/files"
 display-line-numbers-type t
 projectile-project-search-path '("~/Projects")
 evil-ex-substitute-case 'sensitive
 global-auto-revert-non-file-buffers t
 evil-snipe-scope 'buffer
 imenu-list-focus-after-activation t
 ;; indent-bars-display-on-blank-lines nil
 ;; company-idle-delay nil
 )

;; 'setq' vs 'setq-default'
;; 'setq' - use it to set a value with a global vars
;; 'setq-default' - use it to set a 'buffer-local' vars
;; To check if the var is buffer-local: S-K --> 'fill-column' is a buffer-local variable.

;; 'fill-column' - display vertical limit line
(setq-default fill-column 120)

;; 'global-auto-revert-mode' - auto sync buffers when they are changed by another program
;; 'indent-bars-mode' - use tabs instead of spaces
;; 'rainbow-mode' - inline preview of hex code colors
(global-auto-revert-mode t)
(global-display-fill-column-indicator-mode 1)
(indent-bars-mode -1)
;; (indent-tabs-mode t)

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
        ("Last session" :when (my/session-file-exists) :action doom/quickload-session)
        ("Bookmarks" :action bookmarks-jump)
        ("Org-agenda" :when (fboundp 'org-agenda) :action org-agenda)))

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
  (add-hook 'after-change-major-mode-hook (lambda () (size-indication-mode -1))))

;; 'TODO'      - needs to be done
;; 'STARTED'   - in progress
;; 'WAIT'      - blocked by something, have to wait
;; 'HOLD'      - hold (wait) on purpose
;; 'DONE'      - ready
;; 'CANCELLED' - no longer needed

(defconst my/org-root-dir "~/Org"
  "Root directory for all my Org-related files.")

(after! org
  (setq org-directory my/org-root-dir
        ;; Location of .orgids
        org-id-locations-file (concat my/org-root-dir "/.orgids")
        org-agenda-files '("agenda/personal" "agenda/work" "quick.org")
        org-fancy-priorities-list '("" "" "")
        org-superstar-headline-bullets-list '( "●" "○" "⟁"  "⟐" "✿")
        org-tag-alist '(;; Affiliation
                        ("personal" . ?P) ("work" . ?W)
                        ;; Projects...
                        ;; Activities
                        ("shopping" . ?S) ("gym" . ?G) ("birthday" . ?B)
                        ;; Other
                        ("wishlist" . ?L)  ("repeated" . ?R))
        org-todo-keywords '((sequence "TODO(t)" "STARTED(s!)" "WAIT(w)" "HOLD(h)" "|" "DONE(d!)" "CANCELLED(c)"))
        org-todo-keyword-faces '(("TODO"      :foreground "#afb224" :underline t)
                                 ("DONE"      :foreground "#665c54" :underline t)
                                 ("STARTED"   :foreground "#b16286" :underline t)
                                 ("CANCELLED" :foreground "#cc241d" :underline t)
                                 ("HOLD"      :foreground "#458588" :underline t)
                                 ("WAIT"      :foreground "#fe8019" :underline t))
        org-hide-emphasis-markers t))

(custom-set-faces!
  '(org-level-1 :foreground "#83a598" :inherit outline-1 :height 1.2)
  '(org-level-2 :foreground "#e7ab36" :inherit outline-2 :height 1.1)
  '(org-level-3 :foreground "#d3869b" :inherit outline-3 :height 1.05)
  '(org-level-4 :foreground "#89b482" :inherit outline-4 :height 1.025)
  '(org-level-5 :foreground "#f28534" :inherit outline-5 :height 1.0125))

;; org-deadline-warning-days
(setq org-agenda-custom-commands
      '(("p" "Personal"
         ((agenda "" ((org-agenda-files '("agenda/personal"))))
          (tags-todo "personal" ((org-agenda-overriding-header "Personal Tasks:")))
          (tags-todo "-{.*}" ((org-agenda-overriding-header "Untagged Tasks:")))))

        ("w" "Work"
         ((agenda "" ((org-agenda-files '("agenda/work"))))
          (tags-todo "work" ((org-agenda-overriding-header "Work tasks:")))
          (tags-todo "-{.*}" ((org-agenda-overriding-header "Untagged Tasks:")))))

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

(after! org
  (add-hook 'org-mode-hook (lambda ()
                             (global-display-fill-column-indicator-mode -1)
                             (org-superstar-mode)
                             (org-fancy-priorities-mode)
                             (add-hook 'after-save-hook 'org-babel-tangle nil t))))

(add-hook 'org-mode-hook
          (lambda ()
            (setq prettify-symbols-alist '(("#+begin_src"   . "»")
                                           ("#+end_src"     . "«")
                                           ("#+begin_quote" . "❝")
                                           ("#+end_quote"   . "❞")))
            (prettify-symbols-mode 1)))

(use-package! rainbow-mode
  :defer t
  :hook ((css-mode scss-mode sass-mode html-mode web-mode emacs-lisp-mode org-mode) . rainbow-mode))

(use-package! drag-stuff
  ;; Use :defer 't in order to lazy load the package
  :defer t
  :init
  ;; enable in certain modes (optional)
  (add-hook 'prog-mode-hook #'drag-stuff-mode)
  (add-hook 'text-mode-hook #'drag-stuff-mode)
  :config
  ;; keybindings in evil-visual-state (most useful here)
  (define-key evil-visual-state-map (kbd "M-j") #'drag-stuff-down)
  (define-key evil-visual-state-map (kbd "M-k") #'drag-stuff-up)

  ;; optional: enable for normal mode line dragging
  (define-key evil-normal-state-map (kbd "M-j") #'drag-stuff-down)
  (define-key evil-normal-state-map (kbd "M-k") #'drag-stuff-up))

(after! spell-fu
  (setq spell-fu-idle-delay 1) ; default is 0.25
  (setq-default spell-fu-word-regexp "\\b\\([A-Za-z]+\\(['’][A-Za-z]+\\)?\\)\\b"))

(dimmer-configure-org)
(dimmer-configure-magit)
(dimmer-configure-which-key)
(dimmer-configure-company-box)
(dimmer-mode t)

;; (after! flycheck
;;   (flycheck-add-mode 'javascript-eslint 'js-mode)
;;   (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
;;   (flycheck-add-mode 'javascript-eslint 'web-mode))

;; (add-hook 'js-mode-hook #'flycheck-mode)
;; (add-hook 'rjsx-mode-hook #'flycheck-mode)
;; (add-hook 'web-mode-hook #'flycheck-mode)

;; (add-hook 'tide-mode-hook #'tide-hl-identifier-mode)
;; (setq tide-hl-identifier-mode t)

;; (custom-set-faces!
;; '(tide-hl-identifier-face :underline t :background nil))

;; 'dirvish' - extends 'dired'
(after! dirvish
  (setq dirvish-hide-details t
        dirvish-mode-line-format '(:left (sort file-time symlink) :right (yank index))
        dirvish-quick-access-entries
        '(("h" "~/" "Home")
          ("t"  "~/.local/share/Trash/" "Trashes")
          ("o" my/org-root-dir "Org")
          ("d" "~/Downloads" "Downloads")
          ("pi" "~/Pictures" "Pictures")
          ("pr" "~/Projects" "Projects"))))

(defun my/substitute (mode)
 (interactive)
  (save-excursion
    (let ((original-pos (point))
          (expression (cond
                        ((string= mode "global-file-ask") "%s##gc")
                        ((string= mode "global-file") "%s##g")
                        ((string= mode "global-line") "s##g")
                        (t "default"))))
      ;; Perform the substitution
      (minibuffer-with-setup-hook
          (lambda () (backward-char (if (string= mode "global-file-ask") 3 2)))
        (evil-ex expression))
      (goto-char original-pos))))

(map! :leader
      :prefix "r"
      :desc "Replace" "r" (lambda ()
                            (interactive)
                            (my/substitute "global-line"))
      :desc "Replace" "R" (lambda ()
                            (interactive)
                            (my/substitute "global-file"))
      :desc "Replace" "A" (lambda ()
                            (interactive)
                            (my/substitute "global-file-ask")))

(map! :leader
      (:prefix ("t" . "Toggle")
       :desc "Toggle treemacs" "t" #'+treemacs/toggle
       :desc "Toggle imenu sidebar" "s" #'imenu-list-smart-toggle))

(map! :leader
      :prefix "h"
      "l" #'devdocs-lookup)

(map! :leader
      :prefix "w"
      "M" #'maximize-window)

(map! :leader
      :prefix "p"
      "S" #'projectile-replace)

(map! :leader
      :prefix "b"
      :desc "Bookmark list" "m" #'bookmark-bmenu-list)

(map! :leader
      :prefix "TAB"
      :desc "Delete workspace" "k" #'+workspace/kill
      :desc "Delete saved workspace" "K" #'+workspace/delete)

(map! :leader
      :prefix "q"
      :desc "Quit Emacs and ask to save" "Q" #'evil-quit-all)

(map! :leader
      :prefix "h"
      :desc "Find text in documentation" "a" #'apropos-documentation
      :desc "Man page" "w" #'+default/man-or-woman)

(map! :leader
      :prefix ("g" . "git")
       :desc "Open file in remote repo" "O" #'+vc/browse-at-remote)

(after! evil
  (define-key evil-normal-state-map (kbd "C-s") #'save-buffer)
  (define-key evil-insert-state-map (kbd "C-s") #'save-buffer))

(defun my/comment-line-and-next ()
  "Comment the current line and move to the next."
  (interactive)
  (evilnc-comment-or-uncomment-lines 1)
  (forward-line 1))

(after! evil
  (define-key evil-normal-state-map (kbd "C-/") #'my/comment-line-and-next)
  (define-key evil-insert-state-map (kbd "C-/") #'my/comment-line-and-next))

;; SPC
(map! :leader
      "'" nil
      "~" nil
      "*" nil
      ";" nil
      "a" nil
      "X" nil)

;; Window
(map! :leader
      :prefix "w"
      "C-<up>"    nil
      "C-<down>"  nil
      "C-<left>"  nil
      "C-<right>" nil
      "<up>"      nil
      "<down>"    nil
      "<left>"    nil
      "<right>"   nil
      "C-="       nil
      "C-_"       nil
      "d"         nil
      "g"         nil
      "o"         nil
      ":"         nil)

;; Toggle
(map! :leader
      :prefix "t"
      "d" nil)

;; Org-mode
(map! :after org
      :map org-mode-map
      :localleader
      "*" nil
      "@" nil
      "a" nil
      "c" nil
      "g" nil
      "n" nil
      "s" nil
      "r" nil
      "P" nil)

;; Buffer
(map! :leader
      :prefix "b"
      "d" nil
      "n" nil
      "p" nil
      "l" nil
      "z" nil
      "M" nil
      "B" nil
      "Z" nil
      "S" nil
      "C" nil)

;; Workspace
(let ((chars "0123456789")
      (special-chars "hjklrsw"))
  (dotimes (i (length chars))
    (let ((key (format "%c" (aref chars i))))
      (map! :leader :prefix "TAB" key nil))))

(map! :leader
      :prefix "TAB"
      "`" nil
      "d" nil
      "D" nil)

;; Help
(map! :leader
      :prefix "h"
      "RET"    nil
      "C-\\"   nil
      "."      nil
      "4"      nil
      "<help>" nil
      "i"      nil
      "A"      nil
      "C"      nil
      "<f1>"   nil
      "E"      nil
      "F"      nil
      "g"      nil
      "K"      nil
      "I"      nil
      "l"      nil
      "L"      nil
      "M"      nil
      "O"      nil
      "o"      nil
      "n"      nil
      "p"      nil
      "P"      nil
      "q"      nil
      "u"      nil
      "W"      nil
      "V"      nil
      "R"      nil
      "T"      nil
      "s"      nil
      "S"      nil)

(map! :leader
      :prefix ("h b" . "bindings")
      "f" nil
      "k" nil
      "t" nil
      "m" nil)

(map! :leader
      :prefix ("h d" . "bindings")
      "b" nil
      "c" nil
      "d" nil
      "l" nil
      "L" nil
      "n" nil
      "p" nil
      "t" nil
      "u" nil
      "x" nil
      "N" nil
      "s" nil
      "S" nil)

;; Projectile
(map! :leader
      :prefix "p"
      "&" nil
      "f" nil
      "g" nil
      "k" nil
      "o" nil
      "e" nil)

;; GIT
(map! :leader
      :prefix ("g" . "git")
      "'" nil
      "o" nil
      "c" nil
      "D" nil
      "C" nil
      "l" nil
      "f" nil)

;; Insert
(map! :leader
      :prefix "i"
      "p" nil
      "y" nil)

;; File
(map! :leader
      :prefix "f"
      "c" nil
      "d" nil
      "e" nil
      "l" nil
      "p" nil
      "E" nil)

(dotimes (i 10)
  (define-key evil-window-map (number-to-string i) nil))

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

(fmakunbound 'woman)

(mapatoms (lambda (sym)
            (when (string-prefix-p "woman" (symbol-name sym))
              (fmakunbound sym))))
