;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :completion
       (corfu +icons)         ; new code completion backend
       (vertico +icons)       ; the search engine of the future

       :ui
       doom                   ; what makes DOOM look the way it does
       doom-quit              ; DOOM quit-message prompts when you quit Emacs
       doom-dashboard         ; a nifty splash screen for Emacs
       zen                    ; distraction-free coding or writing
       ophints                ; highlight the region an operation acts on
       hl-todo                ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       unicode                ; extended unicode support for various languages
       modeline               ; snazzy, Atom-inspired modeline, plus API
       (treemacs +lsp)        ; a project drawer, like neotree but cooler
       nav-flash              ; blink cursor line after big motions
       workspaces             ; tab emulation, persistence & separate workspaces
       window-select          ; visually switch windows
       indent-guides          ; highlighted indent columns
       (emoji +unicode)       ; 🙂
       (popup +defaults)      ; tame sudden yet inevitable temporary windows
       (vc-gutter +pretty)    ; vcs diff in the fringe
       ;; tabs                   ; a tab bar for Emacs
       ;; (ligatures +extra)     ; ligatures and symbols to make your code pretty again
       ;; vi-tilde-fringe        ; fringe tildes to mark beyond EOB (end of buffer)

       :editor
       (evil +everywhere)  ; come to the dark side, we have cookies
       file-templates      ; auto-snippets for empty files
       multiple-cursors    ; editing in many places at once
       fold                ; (nigh) universal code folding
       (format +onsave)    ; automated prettiness
       snippets            ; my elves. They type so I don't have to
       ;; rotate-text         ; cycle region at point between text candidates (inversion: true -> false etc)
       ;; word-wrap           ; soft wrapping with language-aware indent
       ;; lispy               ; vim for lisp, for people who don't like vim
       ;; parinfer            ; turn lisp into python, sort of

       :emacs
       vc                       ; version-control and Emacs, sitting in a tree
       undo                     ; persistent, smarter undo for your inevitable mistakes
       electric                 ; smarter, keyword-based electric-indent
       (ibuffer +icons)         ; interactive buffer management
       (dired +dirvish +icons)  ; making dired pretty [functional]

       :term
       ;; vterm  ; the best terminal emulation in Emacs
       ;; eshell ; the elisp shell that works everywhere
       ;; shell  ; simple shell REPL for Emacs
       ;; term   ; basic terminal emulator for Emacs

       :checkers
       (syntax +icons)              ; tasing you for every semicolon you forget
       (spell +aspell +everywhere)  ; tasing you for misspelling mispelling
       ;; grammar                      ; tasing grammar mistake every you make

       :tools
       (docker +lsp)
       lsp                 ; M-x vscode
       lookup              ; navigate your code and its documentation
       magit               ; a git porcelain for Emacs
       make                ; run make from tasks Emacs
       tree-sitter
       (eval +overlay)      ; run code, run (also, repls)
       editorconfig         ; let someone else argue about tabs vs spaces

       ;; NOTE: Try to use 'debugger'
       ;; debugger         ; FIXME stepping through code, to help you add bugs
       ;;
       ;; direnv
       ;; gist             ; interacting with github gists
       ;; pdf              ; pdf enhancements
       ;; prodigy          ; FIXME managing external services & code builders
       ;; rgb              ; creating color strings
       ;; taskrunner       ; taskrunner for all your projects
       ;; terraform        ; infrastructure as code
       ;; upload           ; map local to remote projects via ssh/ftp

       :lang
       (web +lsp +tree-sitter)        ; the tubes
       (javascript +lsp +tree-sitter) ; all(hope(abandon(ye(who(enter(here))))))
       (json +lsp +tree-sitter)       ; At least it ain't XML
       (org +roam +dragndrop)         ; organize your plain life in plain text
       (yaml +lsp)                    ; JSON, but readable
       (sh +lsp)                      ; she sells {ba,z,fi}sh shells on the C xor
       haskell                        ; a language that's lazier than I am
       (lua +lsp)                     ; one-based indices? one-based indices
       emacs-lisp                     ; drown in parentheses
       markdown                       ; writing docs for people to ignore

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

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience
       ;;
       :email
       ;;(mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       calendar
       ;; everywhere       ; *leave* Emacs!? You must be joking
       ;;emms
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader

       :input
       ;;layout            ; using Doom with non-qwerty keyboard layouts.

       :config
       literate
       (default +bindings +smartparens))
