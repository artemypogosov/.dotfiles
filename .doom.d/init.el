;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;layout               ; using Doom with non-qwerty keyboard layouts.

       :completion
       (company +childframe)  ; the ultimate code completion backend
       (vertico +icons)       ; the search engine of the future
       ;; helm                ; the *other* search engine for love and life
       ;; ido                 ; the other *other* search engine...

       :ui
       doom                ; what makes DOOM look the way it does
       doom-dashboard      ; a nifty splash screen for Emacs
       doom-quit           ; DOOM quit-message prompts when you quit Emacs
       (emoji +unicode)    ; 🙂
       hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides       ; highlighted indent columns
       modeline            ; snazzy, Atom-inspired modeline, plus API
       nav-flash           ; blink cursor line after big motions
       ophints             ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       treemacs            ; a project drawer, like neotree but cooler
       (vc-gutter +pretty) ; vcs diff in the fringe
       window-select       ; visually switch windows
       workspaces          ; tab emulation, persistence & separate workspaces
       zen                 ; distraction-free coding or writing
       ;; tabs             ; a tab bar for Emacs
        unicode          ; extended unicode support for various languages
       ;; (ligatures +extra)  ; ligatures and symbols to make your code pretty again
       ;; vi-tilde-fringe     ; fringe tildes to mark beyond EOB

       :editor
       (evil +everywhere)  ; come to the dark side, we have cookies
       file-templates      ; auto-snippets for empty files
       multiple-cursors    ; editing in many places at once

       ;; TODO bind --> js2-mode-toggle-element ;;
       fold                ; (nigh) universal code folding

       ;; TODO configure it. We can not only FORMAT on SAVE, but we can FORMAT using different commands
       ;;(format +onsave)  ; automated prettiness

       ;; NOTE Project is on pause. Should I try it? It can expand text objects
       ;;objed             ; text object editing for the innocent

       ;; NOTE Gives an ability to handle parentheses?
       ;;lispy             ; vim for lisp, for people who don't like vim

       ;;parinfer          ; turn lisp into python, sort of
       ;; TODO not sure if I need it
       rotate-text         ; cycle region at point between text candidates
       snippets            ; my elves. They type so I don't have to
       word-wrap           ; soft wrapping with language-aware indent

       :emacs
       (dired +dirvish +icons)    ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       (ibuffer +icons)  ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;; eshell            ; the elisp shell that works everywhere
       ;; shell             ; simple shell REPL for Emacs
       ;; term              ; basic terminal emulator for Emacs
       vterm               ; the best terminal emulation in Emacs

       :checkers
       ;; TODO configure flycheck and linter for JS and other used languages
       syntax              ; tasing you for every semicolon you forget
       (spell +aspell +everywhere)   ; tasing you for misspelling mispelling
       ;; grammar             ; tasing grammar mistake every you make

       :tools
       docker
       lookup              ; navigate your code and its documentation
       lsp                 ; M-x vscode
       ;; TODO learn magit
       magit               ; a git porcelain for Emacs
       make                ; run make from tasks Emacs
       tree-sitter
       (eval +overlay)     ; run code, run (also, repls)
       ;; ansible
       ;; debugger          ; FIXME stepping through code, to help you add bugs
       ;; direnv
       ;; editorconfig      ; let someone else argue about tabs vs spaces
       ;; ein               ; tame Jupyter notebooks with emacs
       ;; gist              ; interacting with github gists
       ;; pass              ; password manager for nerds
       ;; pdf               ; pdf enhancements
       ;; prodigy           ; FIXME managing external services & code builders
       ;; rgb               ; creating color strings
       ;; taskrunner        ; taskrunner for all your projects
       ;; terraform         ; infrastructure as code
       ;; tmux              ; an API for interacting with tmux
       ;; upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       emacs-lisp                    ; drown in parentheses
       json                          ; At least it ain't XML
       markdown                      ; writing docs for people to ignore
       sh                            ; she sells {ba,z,fi}sh shells on the C xor
       yaml                          ; JSON, but readable
       (haskell +dante)              ; a language that's lazier than I am
       (javascript +lsp tree-sitter) ; all(hope(abandon(ye(who(enter(here))))))
       (org +roam2 +dragndrop)       ; organize your plain life in plain text
       (web +lsp tree-sitter)        ; the tubes
       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       ;;cc                ; C > C++ == 1
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;;data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;dhall
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       ;;(java +meghanada)   ; the poster child for carpal tunnel syndrome
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean              ; for folks with too much to prove
       ;;ledger            ; be audit you can be
       ;;lua               ; one-based indices? one-based indices
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       ;;python            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;;zig               ; C, but simpler

       :email
       ;;(mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       ;;everywhere        ; *leave* Emacs!? You must be joking
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader

       :config
       ;;literate
       (default +bindings +smartparens))
