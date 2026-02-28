;;; $DOOMDIR/config.el -*- lexical-binding: t; no-byte-compile: t -*-

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
  ;; Keywords
  '(font-lock-keyword-face :slant italic)
  ;; Standard comments
  '(font-lock-comment-face :foreground "#928374" :slant italic)
  '(font-lock-comment-delimiter-face :foreground "#928374" :slant italic)
  ;; Documentation blocks and JSDoc tags (@param, @returns, etc.)
  '(font-lock-doc-face :foreground "#928374" :slant italic)
  '(font-lock-doc-markup-face :foreground "#928374" :slant italic)
  '(tree-sitter-hl-face:comment :foreground "#928374" :slant italic)
  '(tree-sitter-hl-face:doc :foreground "#928374" :slant italic))

;; Use Noto Color Emoji for emoji characters
;; Also used to draw icons in modeline
(set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
(setq doom-theme 'doom-gruvbox)

(after! doom-themes
  (setq doom-themes-enable-italic t
        ;; Disable bold font in every mode. In ORG->'Headings style' bold is enabled
        doom-themes-enable-bold nil))

(setq undo-limit 80000000                          
      auto-save-default t                         
      truncate-string-ellipsis "…"               
      password-cache-expiry nil                 
      delete-by-moving-to-trash t                  
      trash-directory "~/.local/share/Trash/files" 
      global-auto-revert-non-file-buffers t
      projectile-project-search-path '("~/Projects") 
      display-line-numbers-type 'relative 
      evil-ex-substitute-case 'sensitive 
      evil-want-fine-undo t 
      evil-snipe-scope 'buffer 
      auto-revert-verbose nil 
      scroll-margin 5
      +zen-text-scale 1) 

;; Auto sync buffers when they are changed by other process
(global-auto-revert-mode t)

;; 'setq' vs 'setq-default'
;; Use 'setq' for non-buffer-local variables
;; (variables that are truly global)
;; Use 'setq-default' for buffer-local variables
;; Buffer local variables have a default value.
;; So all buffers inherit the default value.

;; Line-wrapping column width
(setq-default fill-column 120) 
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (display-fill-column-indicator-mode 1))))

(defvar my/dashboard-cache nil)

(defun my/dashboard-render-banner ()
  "Generate and cache the Doom dashboard banner only once."
  (let* ((art '(" ███████ ███    ███  █████   ██████ ███████ " 
                " ██      ████  ████ ██   ██ ██      ██      "
                " █████   ██ ████ ██ ███████ ██      ███████ "
                " ██      ██  ██  ██ ██   ██ ██           ██ "
                " ███████ ██      ██ ██   ██  ██████ ███████ "
                ""                                           
                ""                                           
                "     To see with eyes unclouded by hate."))
         (longest-line (apply #'max (mapcar #'length art))))
    (with-temp-buffer
      (dolist (line art)
        (insert
         (+doom-dashboard--center
          +doom-dashboard--width
          (concat line (make-string (max 0 (- longest-line (length line))) 32)))
         "\n"))
      (buffer-string))))

(defun my/generate-dashboard ()
  (insert
   (propertize
    (or my/dashboard-cache (setq my/dashboard-cache (my/dashboard-render-banner)))
    'face 'doom-dashboard-banner)))

;; Adapt on theme change
(add-hook 'doom-load-theme-hook (lambda () (setq my/dashboard-cache nil)))

(setq +doom-dashboard-ascii-banner-fn #'my/generate-dashboard)

(defun my/session-file-exists ()
  "Check if a session file exists based on workspace or desktop settings."
  (cond
   ((modulep! :ui workspaces)
    (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
   ((require 'desktop nil t)
    (file-exists-p (desktop-full-file-name)))))

(setq +doom-dashboard-menu-sections
      '(("Load session" :action doom/quickload-session :when (my/session-file-exists))
        ("Recent files" :action recentf-open-files)
        ("Open project" :action projectile-switch-project)
        ("Org-agenda"   :action org-agenda :when (fboundp 'org-agenda))))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(after! doom-modeline
  (remove-hook 'doom-first-buffer-hook #'doom-modeline-mode)
  (add-hook 'after-init-hook #'doom-modeline-mode))

(after! doom-modeline
  (setq doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-highlight-modified-buffer-name t
        doom-modeline-position-column-format '("")
        mode-line-position-line-format '("")
        doom-modeline-buffer-encoding nil
        doom-modeline-project-name nil
        doom-modeline-percent-position nil
        doom-modeline-persp-name nil
        doom-modeline-persp-icon nil
        doom-modeline-modal nil
        doom-modeline-indent-info t
        doom-modeline-display-misc-in-all-mode-lines nil)

  (display-time-mode -1)
  (column-number-mode -1)
  (line-number-mode -1)
  ;; Disable size indication
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode))

(after! treemacs
  (treemacs-follow-mode 1)
  (treemacs-indent-guide-mode 1)
  (treemacs-git-commit-diff-mode 1))

(defun my/open-home-dired ()
  (interactive)
  (dired "~/"))

;; 'dirvish' - extends 'dired'
(after! dirvish
  (setq dirvish-hide-details t
        dired-mouse-drag-files t
        dired-kill-when-opening-new-dired-buffer t
        dirvish-mode-line-format '(:left (sort file-time symlink) :right (yank index))
        ;; Use 'b' + letter
        dirvish-quick-access-entries
        '(("h" "~/" "Home")
          ("t"  "~/.local/share/Trash/" "Trash")
          ("o" "~/Org" "Org")
          ("d" "~/Downloads" "Downloads")
          ("pi" "~/Pictures" "Pictures")
          ("pr" "~/Projects" "Projects")))
  (dirvish-side-follow-mode 1))

(add-hook 'window-configuration-change-hook
  (defun my/dired-force-omit-off ()
    (when (derived-mode-p 'dired-mode)
      (dired-omit-mode -1))))

(after! dired-x
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..+$")))

(with-eval-after-load 'dirvish
  (setq dirvish-attributes
        (remove 'hidden dirvish-attributes)))

(map! :map dired-mode-map
      :n "M-h" #'dired-omit-mode)

;; Hide "." and ".." hard links
(after! dired
  (setq dired-listing-switches "-Ahlv --group-directories-first"))

(defconst my/org-root-dir (expand-file-name "~/Org"))

;; Cache: directory → list of org files
(defvar my/org-dir-cache (make-hash-table :test 'equal))

(defun my/org-files (subdir)
  "Return cached list of Org files inside SUBDIR under `my/org-root-dir`."
  (let* ((dir (expand-file-name subdir my/org-root-dir))
         (cached (gethash dir my/org-dir-cache)))
    (or cached
        (puthash dir (directory-files-recursively dir "\\.org$") my/org-dir-cache))))

(after! org 
  (setq org-directory my/org-root-dir
        org-startup-folded 'content
        ;; Location of .orgids
        org-id-locations-file (concat my/org-root-dir "/.orgids")
        org-agenda-files (append (my/org-files "agenda/personal")
                                 (my/org-files "agenda/work")
                                 (list (expand-file-name "inbox.org" my/org-root-dir)))
        calendar-week-start-day 1
        org-fancy-priorities-list '("" "" "")
        org-tag-alist '(("personal" . ?P) ("work" . ?W) ("repeated" . ?R))
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "STARTED(s!)" "WAIT(w@)" "HOLD(h@)" "|" "DONE(d!)" "CANCELLED(c@)"))
        org-todo-keyword-faces '(("TODO"      :foreground "#afb224" :underline t)  ;; needs to be done
                                 ("NEXT"      :foreground "#fabd2f" :underline t)  ;; next one to be considered
                                 ("STARTED"   :foreground "#b16286" :underline t)  ;; in progress
                                 ("HOLD"      :foreground "#458588" :underline t)  ;; blocked by something, have to wait
                                 ("WAIT"      :foreground "#fe8019" :underline t)  ;; hold (wait) on purpose
                                 ("DONE"      :foreground "#665c54" :underline t)  ;; ready
                                 ("CANCELLED" :foreground "#cc241d" :underline t)) ;; no longer needed
        org-hide-emphasis-markers t)
  
  ;; Conditional auto-tangling
  (defun my/org-auto-tangle ()
    (when (and buffer-file-name
               (string-match-p
                "\\(config\\.org\\|README\\.org\\)$"
                buffer-file-name))
      (org-babel-tangle)))

  (defun my/org-auto-tangle-setup ()
    (add-hook! 'after-save-hook
               :local #'my/org-auto-tangle))

  (add-hook! org-mode
             #'org-fancy-priorities-mode
             #'my/org-auto-tangle-setup
             (indent-bars-mode -1)))

(custom-set-faces!
  '(org-level-1 :foreground "#83a598" :inherit outline-1 :height 1.2    :weight bold :extend t)
  '(org-level-2 :foreground "#e7ab36" :inherit outline-2 :height 1.1    :weight bold :extend t)
  '(org-level-3 :foreground "#9e7edf" :inherit outline-3 :height 1.05   :weight bold :extend t)
  '(org-level-4 :foreground "#5e8b4d" :inherit outline-4 :height 1.025  :weight bold :extend t)
  '(org-level-5 :foreground "#d44c3b" :inherit outline-5 :height 1.0125 :weight bold :extend t)
  '(org-link    :foreground "#83a598")
  '(bold :weight bold :foreground "#ebdbb2"))

(setq org-deadline-warning-days 7
      org-agenda-skip-function-global
      '(lambda ()
         (let ((category (org-get-category)))
           (if (string= category "birthdays")
               (save-excursion (outline-next-heading) (point))
             nil)))
      org-agenda-custom-commands
      '(("p" "Personal"
         ((agenda "" ((org-agenda-files (my/org-files "agenda/personal"))))
          (tags-todo "personal"
                     ((org-agenda-overriding-header "Personal Tasks:")
                      (org-agenda-files (my/org-files "agenda/personal"))))
          (tags-todo "-{.*}"
                     ((org-agenda-overriding-header "Untagged Tasks:")
                      (org-agenda-files (my/org-files "agenda/personal"))))))
        
        ("w" "Work"
         ((agenda "" ((org-agenda-files (my/org-files "agenda/work"))))
          (tags-todo "work"
                     ((org-agenda-overriding-header "Work Tasks:")
                      (org-agenda-files (my/org-files "agenda/work"))))
          (tags-todo "-{.*}"
                     ((org-agenda-overriding-header "Untagged Tasks:")
                      (org-agenda-files (my/org-files "agenda/work"))))))
        
        ("i" "Inbox"
         ((agenda "" ((org-agenda-files
                       (list (expand-file-name "inbox.org" my/org-root-dir)))))
          (todo "" ((org-agenda-overriding-header "Inbox Notes:")
                    (org-agenda-files
                     (list (expand-file-name "inbox.org" my/org-root-dir)))))))))

(after! calfw
  (setq cfw:render-line-breaker #'cfw:render-line-breaker-wordwrap
        cfw:display-calendar-holidays nil
        calendar-week-start-day 1))

(after! org-roam
  (setq org-roam-directory (file-name-as-directory my/org-root-dir)
        org-roam-capture-templates
        '(("a" "Agenda Personal [Org/agenda/personal]" plain
           "%?"
           :if-new (file+head "agenda/personal/${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          
          ("A" "Agenda Work [Org/agenda/work]" plain
           "%?"
           :if-new (file+head "agenda/work/${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          
          ("d" "Default [Org/notes/]" plain
           "%?"
           :if-new (file+head "notes/${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          
          ("l" "Linux Tools [computer_science/linux/tools/]" plain
           "%?"
           :if-new (file+head "computer_science/linux/tools/${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          
          ("L" "Linux [computer_science/linux/" plain
           "%?"
           :if-new (file+head "computer_science/linux/${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)

          ("e" "English" plain
           "%?"
           :if-new (file+head "english/${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)))
  (org-roam-db-autosync-mode 1))

;; UI graph
(use-package! org-roam-ui
  :after org-roam
  :commands (org-roam-ui-mode org-roam-ui-open)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(setq markdown-split-window-direction 'right)

(use-package! minuet
  :defer t
  :init (map! :i "M-i" 'minuet-show-suggestion)
  :config
  (setq minuet-provider 'gemini)
  (plist-put minuet-gemini-options :model "gemini-2.0-flash")
  (plist-put minuet-gemini-options :api-key "GEMINI_API_KEY")

  (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)

  (map! :i  "M-y"   #'minuet-complete-with-minibuffer
        (:map minuet-active-mode-map
         :i "M-["   #'minuet-previous-suggestion
         :i "M-]"   #'minuet-next-suggestion
         :i "M-A"   #'minuet-accept-suggestion
         :i "M-a"   #'minuet-accept-suggestion-line
         :i "M-c"   #'minuet-dismiss-suggestion)))

(defun my/get-gemini-key ()
  (or (getenv "GEMINI_API_KEY")
      (user-error "Gemini API key not found in environment variables!")))

(defvar my/gptel-directives
  '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely")
    (buffer . "You are a large language model living in Emacs and a helpful assistant.
Strict Constraints:

1. No Markdown: Do not use triple backticks (```) or language identifiers.
2. Commented Explanations: any explanation MUST be wrapped in comment blocks.
3. Laconism: Respond concisely. ")
    (code . "You are a Senior Full-Stack Software Engineer. Your output is a raw code-streaming interface.
Strict Constraints:

1. Zero Prose: Strictly adhere to a zero-prose policy. No prefaces or summaries.
2. No Markdown: Do not use triple backticks (```) or language identifiers.
3. Raw Implementation: Output only the code. If context is provided, output only the modified lines or replacement block.
4. Professional Rigor: Follow SOLID principles and project naming conventions.
5. Commented Explanations: If any explanation is absolutely necessary, it MUST be wrapped in comment blocks.

Begin the raw code transmission immediately.")
    (writing . "You are a large language model and a writing assistant. Respond concisely.")))

(after! gptel
  (setq gptel-model 'gemini-2.5-flash
        gptel-backend (gptel-make-gemini "Gemini" :key #'my/get-gemini-key :stream t)
        gptel-default-mode 'org-mode
        gptel-directives my/gptel-directives
        gptel-include-reasoning nil
        gptel-quick-word-count 200
        gptel-quick-timeout 60
        gptel-track-media t
        gptel-highlight-methods '(face)
        gptel-display-buffer-action 
        '((display-buffer-reuse-window 
           display-buffer-in-side-window)
          (side . right)
          (window-width . 0.40))))

(after! gptel-quick
  (setq gptel-quick-word-count 400
        gptel-quick-timeout 60)

  (setq gptel-quick-posframe-parameters
        '(:internal-border-width 15
          :internal-border-color "#4c566a"
          :border-width 1
          :border-color "#51afef"
          :max-height 25
          :min-width 85))

  (setq gptel-quick-system-message
        (lambda (_)
          "Briefly explain the code. Logical completeness only. Zero prose. No filler."))

  (advice-add 'gptel-quick--callback-posframe :after
              (lambda (&rest _)
                (when-let ((buf (get-buffer " *gptel-quick*")))
                  (with-current-buffer buf
                    (markdown-mode)
                    (setq-local markdown-hide-markup t)
                    (display-line-numbers-mode -1)
                    (if (fboundp 'spell-fu-mode) (spell-fu-mode -1))
                    (font-lock-flush)
                    (font-lock-ensure)))))
  
  (advice-add 'gptel-quick :around
              (lambda (orig-fun &rest args)
                (let ((gptel-max-tokens 1200))
                  (apply orig-fun args)))))

(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil
        lsp-clients-lua-language-server-bin "/usr/bin/lua-language-server"
        lsp-clients-lua-language-server-main-location "/usr/lib/lua-language-server/main.lua"))

;; Disable format-on-save for some modes where formatters are unreliable
(setq +format-on-save-disabled-modes
      '(sql-mode tex-mode latex-mode LaTeX-mode org-msg-edit-mode emacs-lisp-mode))

;; Allow per-project overrides via .dir-locals.el
(setq-default +format-with nil)

;; Fix 'dockfmt' path
(add-to-list 'exec-path "~/.local/share/go/bin")
(setenv "PATH" (concat "~/.local/share/go/bin:" (getenv "PATH")))

(after! apheleia
  ;; Biome
  (set-formatter! 'biome
    '("biome" "format" "--stdin-file-path" filepath))

  ;; Prettier (Daemon and CLI)
  (set-formatter! 'prettierd '("prettierd" filepath))
  (set-formatter! 'prettier '("prettier" "--stdin-filepath" filepath))

  ;; JSON
  (set-formatter! 'jsonfmt
    '("prettier" "--stdin-filepath" filepath)
    :modes '(json-mode jsonc-mode))

  ;; YAML
  (set-formatter! 'yamlfix
    '("prettier" "--stdin-filepath" filepath)
    :modes '(yaml-mode yaml-ts-mode))

  ;; Lua (Stylua)
  (set-formatter! 'stylua
    '("stylua"
      ;; Let Stylua walk up the tree to find .stylua.toml
      "--search-parent-directories"
      ;; Tell Stylua what file is being formatted when using stdin
      "--stdin-filepath" filepath
      "-")
    :modes '(lua-mode lua-ts-mode))

  ;; Shell
  (set-formatter! 'shfmt
    '("shfmt" "-filename" filepath "-")
    :modes '(sh-mode bash-ts-mode zsh-ts-mode)))

(defun my/js-smart-formatter ()
  "Return a formatter symbol for the current JS/TS/JSON project.
Chooses biome/prettierd/prettier based on project config files."
  (let* ((root (or (and (fboundp 'projectile-project-root)
                        (projectile-project-root))
                   default-directory))
         (biome-configs '("biome.json"
                          "biome.jsonc"
                          "biome.config.js"
                          "biome.config.cjs"
                          "biome.config.mjs"
                          "biome.config.ts"))
         (prettier-configs '(".prettierrc"
                             ".prettierrc.json"
                             ".prettierrc.js"
                             ".prettierrc.cjs"
                             ".prettierrc.yaml"
                             ".prettierrc.yml"
                             "prettier.config.js"
                             "prettier.config.cjs")))
    (cond
     ;; Any explicit Prettier config → prefer Prettierd
     ((seq-some (lambda (f)
                  (file-exists-p (expand-file-name f root)))
                prettier-configs)
      'prettierd)

     ;; Any Biome config file
     ((seq-some (lambda (f)
                  (file-exists-p (expand-file-name f root)))
                biome-configs)
      'biome)

     ;; Biome config embedded in package.json
     ((let ((pkg (expand-file-name "package.json" root)))
        (and (file-exists-p pkg)
             (with-temp-buffer
               (insert-file-contents pkg)
               (string-match-p "\"biome\"" (buffer-string)))))
      'biome)
     
     ;; Fallback 1
     ((executable-find "biome")     'biome)
     ((executable-find "prettierd") 'prettierd)
     ((executable-find "prettier")  'prettier)

     ;; Fallback 2
     (t 'prettier))))

(defun my/js-assign-smart-formatter ()
  "Decide and set `+format-with` for this JS/TS/JSON buffer."
  (setq-local +format-with (my/js-smart-formatter)))

(dolist (hook '(js2-mode-hook
                js-mode-hook
                js-ts-mode-hook
                tsx-ts-mode-hook
                typescript-ts-mode-hook
                typescript-mode-hook
                web-mode-hook
                json-mode-hook
                jsonc-mode-hook))
  (add-hook hook #'my/js-assign-smart-formatter))

;; Shell
(setq-hook! 'sh-mode-hook +format-with 'shfmt)

;; Yaml
(setq-hook! '(yaml-mode-hook yaml-ts-mode-hook) +format-with 'yamlfix)

;; Lua
(setq-hook! 'lua-mode-hook    +format-with 'stylua)
(setq-hook! 'lua-ts-mode-hook +format-with 'stylua)

(add-hook! 'ediff-prepare-buffer-hook (flycheck-mode -1))

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
    ;; Pure Emmet expansion (added by me instead of 'emmet-expand-yas')
    (t #'emmet-expand-line))))

;; Show vertical bars to visually indicate indentation levels
(add-hook! yaml-mode #'indent-bars-mode)

;; Vim text-objects alternative for the lazy
(use-package! expand-region
  :defer t
  :init (map! :nv "M-e" #'er/expand-region)
  :config
  (map! :leader
        (:prefix ("e" . "expand-region")
         :desc "Mark JS function"          "f" #'er/mark-js-function
         :desc "Mark JS inner return"      "r" #'er/mark-js-inner-return
         :desc "Mark JS outer return"      "R" #'er/mark-js-outer-return
         :desc "Mark JS if"                "i" #'er/mark-js-if
         :desc "Mark JS call"              "c" #'er/mark-js-call
         :desc "Mark JS comment"           "C" #'er/mark-comment
         :desc "Mark inside pairs"         "b" #'er/mark-inside-pairs
         :desc "Mark outside pairs"        "B" #'er/mark-outside-pairs
         :desc "Mark inside quotes"        "q" #'er/mark-inside-quotes
         :desc "Mark outside quotes"       "Q" #'er/mark-outside-quotes
         :desc "Mark paragraph"            "p" #'er/mark-paragraph
         :desc "Mark url"                  "u" #'er/mark-url
         :desc "Mark html attribute"       "a" #'er/mark-html-attribute
         :desc "Mark JS object property"   "o" #'er/mark-js-object-property
         :desc "Mark org code block"       "e" #'er/mark-org-code-block)))

;; Disable rainbow-mode everywhere Doom enables it
(remove-hook 'prog-mode-hook #'rainbow-mode)
(remove-hook 'css-mode-hook #'rainbow-mode)

(after! web-mode
  ;; Disable CSS colorization in HTML (perf improvement)
  (setq web-mode-enable-css-colorization nil))

(after! css-mode
  ;; Disable built-in CSS color highlighting
  (setq css-fontify-colors nil))

(use-package! colorful-mode
  :custom
  (colorful-use-prefix t)
  (colorful-prefix-string "•")
  (colorful-only-strings 'only-prog)
  :config
  (add-hook! prog-mode #'colorful-mode))

(use-package! idle-underline-mode
  :hook (prog-mode . idle-underline-mode)
  :config
  (setq idle-underline-idle-time 0.2
        idle-underline-ignore-context '(string comment))
  (set-face-attribute 'idle-underline nil
                      :underline t
                      :inherit nil
                      :background 'unspecified))

(add-hook! prog-mode #'rainbow-delimiters-mode)

(after! spell-fu
  (setq-default spell-fu-idle-delay 1
                spell-fu-word-regexp "\\b\\([A-Za-z]+\\(['’][A-Za-z]+\\)?\\)\\b")

  ;; Disable spell-fu in all programming modes
  (add-hook! prog-mode-hook (spell-fu-mode -1))

  ;; Extra safety for Lua modes
  (add-hook! (lua-mode lua-ts-mode) (spell-fu-mode -1))

  ;; Enable spell-fu only in text-like modes
  (add-hook! text-mode-hook #'spell-fu-mode))

(add-hook! org-mode
  (setq-local prettify-symbols-alist
              '(("#+begin_src"   . "»")
                ("#+end_src"     . "«")
                ("#+begin_quote" . "❝")
                ("#+end_quote"   . "❞")
                
                ("#+BEGIN_SRC"   . "»")
                ("#+END_SRC"     . "«")
                ("#+BEGIN_QUOTE" . "❝")
                ("#+END_QUOTE"   . "❞")))
  (prettify-symbols-mode 1))

(defun my/org-pretty-list-bullets (limit)
  "Replace Org list bullets (+ and -) with prettier glyphs."
  (when (re-search-forward
         "^[ \t]*\\([+-]\\)\\s-+"
         limit t)
    (compose-region
     (match-beginning 1)
     (match-end 1)
     (pcase (match-string 1)
       ("+" "➤")
       ("-" "➤")))
    t))

(defun my/org-enable-pretty-bullets ()
  (font-lock-add-keywords
   nil
   '((my/org-pretty-list-bullets))
   'append)
  (font-lock-flush))

(add-hook! org-mode
  (my/org-enable-pretty-bullets))

(use-package! visual-replace
  :defer t
  :init
  (visual-replace-global-mode 1)
  (setq visual-replace-keep-initial-position t
        visual-replace-default-to-full-scope t)
  :config
  ;; Temporary query mode hook
  (defun my/visual-replace-enable-query ()
    (visual-replace-toggle-query)
    (remove-hook 'minibuffer-setup-hook #'my/visual-replace-enable-query))

  (defun my/visual-replace-with-query ()
    "Call visual-replace with query mode once."
    (interactive)
    (add-hook 'minibuffer-setup-hook #'my/visual-replace-enable-query)
    (call-interactively #'visual-replace))

  (map! :leader
        (:prefix ("r" . "replace")
         :desc "Replace"                "r" #'visual-replace
         :desc "Replace at point"       "p" #'visual-replace-thing-at-point
         :desc "Replace selected"       "s" #'visual-replace-selected
         :desc "Replace in line"        "l"
         (lambda ()
           (interactive)
           (let ((pos (point)))
             (goto-char (line-beginning-position))
             (push-mark (line-end-position) t t)
             (call-interactively #'visual-replace)
             (goto-char pos)))
         :desc "Replace with confirm"   "c" #'my/visual-replace-with-query))
  
  (define-key visual-replace-mode-map (kbd "+") visual-replace-secondary-mode-map))

(use-package! drag-stuff
  :init
  (add-hook! (prog-mode text-mode) #'drag-stuff-mode)
  :config
  (map! :v "M-j" #'drag-stuff-down
        :v "M-k" #'drag-stuff-up
        :n "M-j" #'drag-stuff-down
        :n "M-k" #'drag-stuff-up))

(use-package! dimmer
  :init (setq dimmer-fraction 0.2
              dimmer-adjustment-mode :foreground)
  :config
  (dimmer-configure-org)
  (dimmer-configure-magit)
  (dimmer-configure-which-key)
  (dimmer-configure-company-box)

  (dimmer-mode t))

(use-package! reverse-im
  :init
  (setq reverse-im-input-methods '("ukrainian-computer"))
  :config
  (reverse-im-mode 1))

(defvar my/appt-notification-id nil)

(defun my/appt-clean-message (msg)
  "Remove trailing Org tags from appt message while keeping time and title."
  (string-trim-right (replace-regexp-in-string "[ \t]*:[[:alnum:]_:@]+::?$" "" msg)))

(when (daemonp)
  (run-with-idle-timer
   30 nil
   (lambda ()
     (require 'appt)
     (require 'notifications)

     (setq appt-message-warning-time 10
           appt-display-interval 1)

     (setq appt-disp-window-function
           (lambda (remaining _new-time msg)
             (setq my/appt-notification-id
                     (notifications-notify
                      :title (format "In %s minutes" remaining)
                      :body (my/appt-clean-message msg)
                      :urgency 'normal
                      :replaces-id my/appt-notification-id))))
     (appt-activate t)
     (run-at-time "30 sec" 600
                  (lambda ()
                    (setq appt-time-msg-list nil)
                    (org-agenda-to-appt))))))

(defmacro my/unbind (&rest forms)
  "Bulk-unbind Doom leader keys using hash-map style plist input.
Each FORM should be a plist: (:prefix \"p\" :keys (\"a\" \"b\")).
If :keys is omitted, unbinds the prefix itself."
  `(progn
     ,@(cl-loop
        for form in forms
        collect
        (let ((prefix (plist-get form :prefix))
              (keys   (plist-get form :keys)))
          (unless prefix
            (error "my/unbind: :prefix is required in form: %S" form))

          (if keys
              ;; Case 1: Unbind specific keys under the prefix
              (let (spec)
                (dolist (k keys)
                  (setq spec (append spec (list k nil))))
                `(map! :leader :prefix ,prefix ,@spec))
            
            ;; Case 2: Unbind the prefix itself
            `(map! :leader ,prefix nil))))))

(my/unbind
 (:prefix "X")
 (:prefix "w"
  :keys ("C-<up>" "C-<down>" "C-<left>" "C-<right>" "<up>" "<down>"
         "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "<left>" "<right>"
         "C-=" "C-_" "d" "g" "o" ":" "+" "-" "<" ">" "_" "|" "m" "q"
         "C-b" "C-c" "C-f" "C-h" "C-j" "C-k" "C-l" "C-n" "C-o" "C-p"
         "C-q" "C-r" "C-s" "C-t" "C-u" "C-v" "C-w" "C-x" 
         "C-S-h" "C-S-j" "C-S-k" "C-S-l" "C-S-r" "C-S-S" "C-S-w"))
 (:prefix "TAB" :keys ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "d" "D" "`" "R"))
 (:prefix "b"   :keys ("d" "n" "p" "l" "z" "m" "M" "Z" "s" "C"))
 (:prefix "c"   :keys ("E"))
 (:prefix "f"   :keys ("e" "s"))
 (:prefix "h"
  :keys ("RET" "." "4" "?" "A" "b" "C" "e" "E" "F"
         "g" "I" "K" "l" "L" "M" "n" "o" "O" "p" "P" "V"
         "q" "i" "R" "s" "S" "u" "W" "x" "<help>" "<f1>" "C-\\"))
 (:prefix "i"   :keys ("e" "p" "r" "u"))
 (:prefix "n"   :keys ("a" "l" "n" "N" "*" "y" "Y"))
 (:prefix "o"   :keys ("-" "P"))
 (:prefix "p"   :keys (">" "f" "F" "o"))
 (:prefix "s"   :keys ("e" "I" "j" "k" "K" "L" "m" "o" "O" "r" "t" "T"))
 (:prefix "t"   :keys ("g" "c" "I" "v")))

(map! :leader
      (:prefix ("d" . "devdocs")
       :desc "Install" "+" #'devdocs-install
       :desc "Delete" "-" #'devdocs-delete
       :desc "Lookup" "l" #'devdocs-lookup
       :desc "Select docs" "L" #'devdocs-peruse
       :desc "Search on the site" "s" #'devdocs-search
       :desc "Update all docs" "u" #'devdocs-update-all))

(defun my/toggle-minuet ()
  "Toggle Minuet auto-suggestions globally."
  (interactive)
  (if (bound-and-true-p minuet-auto-suggestion-mode)
      (progn
        (minuet-auto-suggestion-mode -1)
        (message "Minuet AI: Off"))
    (progn
      ;; This will trigger the loading of the minuet package if it hasn't loaded yet
      (minuet-auto-suggestion-mode 1)
      (message "Minuet AI: On"))))

(map! :leader
      :desc "Toggle Minuet AI" "t m" #'my/toggle-minuet)

(map! :leader
      :desc "Toggle highlight mode" "o l h" #'gptel-highlight-mode
      :desc "Toggle gptel mode" "o l g" #'gptel-mode
      :desc "Remove all context" "o l R" #'gptel-context-remove-all)

(map! :leader
      :prefix ("g" . "git")
      :desc "Rebase autosquash" "ca" #'magit-rebase-autosquash
      :desc "Diff current file" "d" #'vc-ediff)

(after! git-timemachine
  (evil-define-key 'normal git-timemachine-mode-map
    (kbd "?") #'git-timemachine-help)

  (evil-define-key 'motion git-timemachine-mode-map
    (kbd "?") #'git-timemachine-help))

;; Tangle org-file
(after! evil-org
  (map! :localleader
        :map org-mode-map
        :desc "Tangle buffer" "s" #'org-babel-tangle
        :desc "Insert hline and move down" "+" #'org-table-hline-and-move))

;; Calendar
(map! :leader
      (:prefix ("o" . "open")
       :desc "Open calendar" "c" #'=calendar))

;; Open ROAM Graph
(map! :leader
      :prefix ("n" . "notes")
      (:prefix ("r" . "roam")
       :desc "Open UI Graph" "o" #'org-roam-ui-open))

;; Windows manipulation
(map! :leader
      :prefix "w"
      "M" #'maximize-window
      "C" #'delete-other-windows
      "z" #'windresize)

;; File
(map! :leader
      :prefix "f"
      :desc "Update recent files" "z" #'recentf-cleanup
      :desc "Find file at point" "a" #'find-file-at-point)

;; Bookmarks
(map! :leader
      (:prefix ("o" . "open")
       :desc "Bookmark manager" "b" #'list-bookmarks))

;; Markdown
(after! markdown-mode
  (map! :localleader
        :map markdown-mode-map
        :desc "Live preview" "l" #'markdown-live-preview-mode))

;; Search 'TODO' keywords
;; Also use ]t & [t to jump between pre/next 'TODO' keywords
(after! hl-todo
  (map! :leader
        (:prefix ("s" . "search")
         :desc "Search 'TODO'" "." #'hl-todo-occur
         :desc "Search 'TODO' from dir" "," #'hl-todo-rgrep)))

;; Toggle 
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle treemacs" "t" #'+treemacs/toggle
       :desc "Focus treemacs" "T" #'treemacs-select-window))

;; Manage workspaces
(map! :leader
      :prefix "TAB"
      :desc "Delete workspace" "k" #'+workspace/kill
      :desc "Delete saved workspace" "K" #'+workspace/delete)

;; LSP
(map! :leader
      :prefix "c"
      :desc "LSP Remove known project" "z" #'lsp-workspace-folders-remove)

;; Help
(map! :leader
      :prefix "h"
      :desc "Find text in documentation" "a" #'apropos-documentation
      :desc "Man page" "w" #'+default/man-or-woman)

;; Quit Emacs
(map! :leader
      :prefix "q"
      :desc "Quit Emacs and ask to save" "Q" #'evil-quit-all)

;; Complete file path
(map! :i "M-p" #'cape-file)

;; Save buffer with C-s
(after! evil
  (map! :i "C-s"
        (lambda ()
          (interactive)
          (save-buffer)
          (evil-normal-state)))
  (map! :n "C-s" #'save-buffer))

;; Comment line and move down
(defun my/comment-line-and-next ()
  (interactive)
  (evilnc-comment-or-uncomment-lines 1))

(after! evil
  (map! :n "C-/" #'my/comment-line-and-next
        :i "C-/" #'my/comment-line-and-next))
