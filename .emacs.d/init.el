(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa"))


(defvar macosx-p (string-match "darwin" (symbol-name system-type)))
(defvar windows-p (string-match "windows-nt" (symbol-name system-type)))
(defvar linux-p (string-match "gnu/linux" (symbol-name system-type)))



(defun setup-helm ()
  (use-package helm
    :config
    (helm-mode 1)
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.

    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t)
    )
  )

(defun system-setup ()
  (when macosx-p
    (setq mac-option-modifier 'meta)
    (setq default-input-method "MacOSX")
    (add-to-list
     'default-frame-alist
                                        ;'(font . "-apple-Bitstream_Vera_Sans_Mono-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"))
     '(font . "-*-Hack-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"))
    )
  (set-frame-height (selected-frame) 68)
  (set-frame-width (selected-frame) 91)

  )


(defun global-settings ()
  (setq ring-bell-function 'ignore)
  (setq-default indent-tabs-mode nil)
  (setq tab-width 2)
  (line-number-mode 1)
  (setq frame-title-format "%b - emacs")
  (column-number-mode 1)
  (setq scroll-step 1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-font-lock-mode t)
  (which-function-mode)
  (mouse-avoidance-mode 'cat-and-mouse)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq tab-always-indent 'complete)
  )


(defun mode-hooks ()

  (defun my-default-mode-hook ()
    )

  (defun my-cc-mode-hook ()
    (my-default-mode-hook)
    (setq c-default-style "k&r"
          c-basic-offset 2)
    (setq show-trailing-whitespace t)
    (c-toggle-hungry-state)
    (c-toggle-auto-state -1))

  (defun my-js-mode-hook ()
    (my-default-mode-hook)
    (c-toggle-hungry-state)
    (c-toggle-auto-state -1)
    )


  (defun my-sh-mode-hook ()
    (my-default-mode-hook)
    (setq
     sh-basic-offset 2
     sh-indentation 2)
    )


  (defun my-ruby-mode-hook ()
    (my-default-mode-hook)
    (setq
     indent-tabs-mode nil)
    (c-toggle-hungry-state)
    )

  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-style-padding 1)
    (setq web-mode-script-padding 1)
    (setq web-mode-block-padding 0)
    (setq web-mode-comment-style 2)
    (hl-tags-mode 1)
    )

  (add-hook 'emacs-lisp-mode-hook 'my-default-mode-hook)
  (add-hook 'lisp-mode-hook 'my-default-mode-hook)
  (add-hook 'c-mode-hook 'my-cc-mode-hook)
  (add-hook 'cc-mode-hook 'my-cc-mode-hook)
  (add-hook 'c++-mode-hook 'my-cc-mode-hook)
  (add-hook 'makefile-mode-hook 'my-default-mode-hook)
  (add-hook 'mutt-mode-hook 'my-default-mode-hook)
  (add-hook 'perl-mode-hook 'my-default-mode-hook)
  (add-hook 'cperl-mode-hook 'my-default-mode-hook)
  (add-hook 'c++-c-mode-hook 'my-cc-mode-hook)
  (add-hook 'java-mode-hook 'my-cc-mode-hook)
  (add-hook 'tex-mode-hook 'my-default-mode-hook)
  (add-hook 'latex-mode-hook 'my-default-mode-hook)
  (add-hook 'dired-mode-hook 'my-default-mode-hook)
  (add-hook 'compilation-mode-hook 'my-default-mode-hook)
  (add-hook 'nxml-my 'mode-default-mode-hook)

  (add-hook 'lua-mode-hook 'my-default-mode-hook)
  (add-hook 'text-mode-hook 'my-default-mode-hook)
  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

  (add-hook 'rails-modee-hook 'my-default-mode-hook)
  (add-hook 'yaml-mode-hook 'my-js-mode-hook)
  (add-hook 'sh-mode-hook 'my-sh-mode-hook)
  (add-hook 'objc-mode-hook 'my-default-mode-hook)
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  (add-hook 'compilation-finish-functions 'my-shorten-filenames-in-compilation)

  (defun maybe-goldy-offset ()
    (if (string-match "goldy" buffer-file-name)
        (setq c-basic-offset 4)))
  (add-hook 'c-mode-hook 'maybe-goldy-offset)
  )


(defun mode-mapping ()
  (add-to-mode-list "\\.text$" 'text-mode)
  (add-to-mode-list "\\.tex$" 'tex-mode)
  (add-to-mode-list "\\.txt$" 'text-mode)
  (add-to-mode-list "\\.doc$" 'text-mode)
  (add-to-mode-list "\\.sh$" 'sh-mode)
  (add-to-mode-list "\\.awk$" 'awk-mode)
  (add-to-mode-list "\\.perl$" 'perl-mode)
  (add-to-mode-list "\\.plx$" 'perl-mode)
  (add-to-mode-list "\\.pl$" 'perl-mode)
  (add-to-mode-list "\\.c$" 'c++-mode)
  (add-to-mode-list "\\.m$" 'objc-mode)
  (add-to-mode-list "\\.cc$" 'c++-mode)
  (add-to-mode-list "\\.c$" 'c++-mode)
  (add-to-mode-list "\\.h$" 'c++-mode)
  (add-to-mode-list "\\.cpp$" 'c++-mode)
  (add-to-mode-list "\\.cxx$" 'c++-mode)
  (add-to-mode-list "\\.java$" 'java-mode)
  (add-to-mode-list "\\.jj$" 'text-mode)
  (add-to-mode-list "\\.tcl$" 'tcl-mode)
  (add-to-mode-list "\\.sh$" 'shell-script-mode)
  (add-to-mode-list "\\.lua$" 'lua-mode)
  (add-to-mode-list "\\.ws$" 'lua-mode)
  (add-to-mode-list "\\.mpc$" 'lua-mode)  ; does good indentation job
  (add-to-mode-list "\\.mpw$" 'lua-mode)  ; does good indentation job
  (add-to-mode-list "\\.php$" 'php-mode)
  (add-to-mode-list "\\.css$" 'css-mode)
  (add-to-mode-list "\\.m$" 'objc-mode)
  (add-to-mode-list "\\.mm$" 'objc-mode)
  (add-to-mode-list "\\.go$" 'go-mode)
  (add-to-mode-list "\\.rb$" 'ruby-mode)
  (add-to-mode-list "\\.ru$" 'ruby-mode)
  (add-to-mode-list "rakefile$" 'ruby-mode)
  (add-to-mode-list "capfile$" 'ruby-mode)
  (add-to-mode-list "\\.html?\\'" 'web-mode)
  (add-to-mode-list "\\.yml$" 'yaml-mode)
  (add-to-mode-list "\\.yaml$" 'yaml-mode)
  (add-to-mode-list "\\.md$" 'markdown-mode)

  )

(defun setup-igrep ()
  (use-package igrep
    :commands (igrep igrep-find dired-do-igrep dired-do-igrep-find)

    :config
    (setq igrep-options "-i -s")
    (setq igrep-expression-quote-char ?')
    (setq igrep-parenthesis-escape-char ?\\)
    (setq igrep-find-use-xargs nil))
  )

(defun set-keys ()
  ;; override Macbook pro annoying tilde placement
  (global-set-key "§" "~")
  (global-unset-key "\C-j")
  (global-set-key "\C-j" 'goto-line)
  (global-set-key "\C-x\C-q" 'my-toggle-read-only)
  (global-set-key [(control return)] 'calculator)
  (global-set-key (kbd "C-|") 'vi-paren-match)

  (global-set-key "\C-cb" 'push-mark-command)
  (global-set-key "\C-cj" 'jump-to-char)
  (global-set-key "\C-cz" 'zap-to-char-and-add)
  (fset 'indent-buffer "\C-xh\C-[\C-\\")
  (global-set-key "\C-ch" 'indent-buffer)
  (global-set-key "\C-ck" 'kill-line-from-start)
  (fset 'duplicate-from-line-start [?\C-a ?\C-k ?\C-y return ?\C-y])
  (global-set-key "\C-cw" 'duplicate-from-line-start)
  (fset 'copy-this-line [?\C-a ?\C-  down ?\M-w])

  (global-set-key "\C-cy" 'copy-this-line)
  (global-set-key "\e0" 'delete-window)
  (global-set-key "\C-cr" 'revert-buffer-dont-ask)
  (global-set-key "\C-cc" 'goto-last-change)
  (global-set-key "\C-cf" 'find-file-at-point)
  (global-set-key (kbd "C-<tab>") 'comint-replace-by-expanded-filename)

  (global-set-key (kbd "C-M-<down>") 'forward-list)
  (global-set-key (kbd "C-M-<up>") 'backward-list)
  (global-set-key (kbd "M-<return>") 'hippie-expand)
  (global-set-key (kbd "C-M-<right>") 'jump-to-white)
  (global-set-key (kbd "C-<right>") 'forward-word)
  (global-set-key (kbd "C-<left>") 'backward-word)

  (global-set-key "\C-co" 'other-window)
  (global-set-key "\C-cm" 'buffer-menu)
  (global-set-key "\C-cp" 'split-window)

  (global-set-key "\C-cg" 'igrep)
  (global-set-key "\C-cG" 'igrep-find)
  (global-set-key "\C-c>" 'open-init-el)

                                        ;(global-set-key (kbd "C-c M-w") 'append-to-register)
  (global-set-key "\C-c\C-c" 'compile)
  (global-set-key "\C-cC" 'recompile)

  (global-set-key (kbd "C-c C-<tab>") 'sourcepair-load)

  (global-set-key "\C-cD" 'delete-trailing-whitespace)

  (global-set-key (kbd "M-r") 'sacha/search-word-backward)
  (global-set-key (kbd "M-s") 'sacha/search-word-forward)

  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-`") 'switch-to-previous-buffer)
  (global-set-key (kbd "C-x C-g") 'ack)
  )

(defun setup-scrat ()
  (use-package scrat
    :config
    (defun jot () (interactive)
           (scratch "*jot*" 'text-mode))
    (defun jotter () (interactive)
           (scratch "*jotter*" 'text-mode))
    (defun joty () (interactive)
           (scratch "*joty*" 'text-mode))
    )
 )

(defun setup-postack ()
  (use-package postack
    :bind
    ("C-c (" . postack-push)
    ("C-c )" . postack-pop)
    )
  )

(defun setup-tramp ()
  (use-package tramp
    :config
    (setq tramp-default-method "ssh")
    )
  )

(defun setup-misc-modes ()
  (use-package goto-last-change)
  (use-package magit)
  (use-package nginx-mode)
  (use-package multiple-cursors)
  (use-package sourcepair)
  (use-package hl-tags-mode)
  (use-package whole-line-or-region-kill-region)
  )

(defun setup-ocaml ()
  ;;;; from https://github.com/diml/utopa
  ;; Setup environment variables using opam
  (when macosx-p
    (setq opam-where "/usr/local/bin/opam"))
  (when linux-p
    (setq opam-where "/usr/bin/opam"))

  (dolist (var (car (read-from-string (shell-command-to-string
                                       (concat opam-where " config env --sexp")))))
    (setenv (car var) (cadr var)))

  ;; Update the emacs path
  (setq exec-path (append (parse-colon-path (getenv "PATH"))
                          (list exec-directory)))

  ;; Update the emacs load path
  (add-to-list 'load-path (expand-file-name "../../share/emacs/site-lisp"
                                            (getenv "OCAML_TOPLEVEL_PATH")))

  ;; Automatically load utop.el
  (autoload 'utop "utop" "Toplevel for OCaml" t)

                                        ; typical error - File "graph.ml", line 46, characters 38-41:

                                        ;  (add-to-list 'compilation-error-regexp-alist 'corebuild)
                                        ;  (add-to-list 'compilation-error-regexp-alist-alist
                                        ;               '(corebuild "File \"\\(.+?\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)"

                                        ;                       1 2 3))
  ;; merlin
  (autoload 'merlin-mode "merlin" "Merlin mode" t)
  (setq merlin-command "/Users/shmul/.opam/4.01.0/bin/ocamlmerlin")
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (add-hook 'caml-mode-hook 'merlin-mode)
  (setq merlin-use-auto-complete-mode 'easy)
  ;; Use opam switch to lookup ocamlmerlin binary
                                        ;(setq merlin-command 'opam)
  )

(defun setup-theme ()
  (use-package color-theme
    :config
    (color-theme-initialize)
  )
)

(defun setup-ido ()
  (use-package ido
    :config
    (ido-mode 1)
    (setq
     read-buffer-function 'ido-read-buffer
     ido-enable-flex-matching t)
                                        ; from emacs wiki
    (setq ido-execute-command-cache nil))
  )

(defun setup-hippie ()
  (use-package hippie-exp
    :config
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-all-abbrevs
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-expand-list
            try-expand-line
            try-complete-file-name-partially
            try-complete-file-name))
    )
  )

(defun setup-auto-complete ()
  (use-package auto-complete
    :config
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
    (ac-config-default)
    )
  )

(defun setup-swoop ()
  (require 'swoop)
  (global-set-key (kbd "C-o")   'swoop)
  (global-set-key (kbd "C-M-o") 'swoop-multi)
  (global-set-key (kbd "M-o")   'swoop-pcre-regexp)
  (global-set-key (kbd "C-S-o") 'swoop-back-to-last-position)

  ;; t:   Show swoop lines within the current window
  ;; nil: Show swoop lines in another window
  (setq swoop-window-split-current-window: nil)
  ;; Determine the split direction 'split-window-horizontally or 'split-window-vertically
  (setq swoop-window-split-direction: 'split-window-vertically)

  ;; Change whole buffer's font size (t or nil)
  (setq swoop-font-size-change: t)
  ;; Font size (e.g. 0.8, 1.0, 1.5, 80, 135)
  (setq swoop-font-size: 0.9)
  ;;
  ;; Enable around target lines magnifier (t or nil)
  (setq swoop-use-target-magnifier: t)
  ;; Magnify area from target line
  (setq swoop-use-target-magnifier-around: 10)
  ;; Font size for magnify area (e.g. 0.8, 1.0, 1.5, 80, 135)
  (setq swoop-use-target-magnifier-size: 1.2)
  )

(defun setup-expand-region ()
  (use-package expand-region
    :bind
    ("C-=" . er/expand-region)
    )
  )

(defun setup-smex ()
  (use-package smex
    :config
    (smex-initialize)
    :bind
    ("M-x" . smex)
    ("M-X" . smex-major-mode-commands)
  ;; This is your old M-x.
    ( "C-c C-c M-x" . execute-extended-command)
    )
  )

(defun setup-swiper ()
  (use-package swiper
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    :bind
    ("\C-S" . swiper)
    ("\C-R"  . swiper)
    ("C-c C-r" . ivy-resume)
    ([f6] . ivy-resume)
    )
  )

(defun setup-corral  ()
  (global-set-key (kbd "M-9") 'corral-parentheses-backward)
  (global-set-key (kbd "M-0") 'corral-parentheses-forward)
  (global-set-key (kbd "M-[") 'corral-brackets-backward)
  (global-set-key (kbd "M-]") 'corral-brackets-forward)
  (global-set-key (kbd "M-{") 'corral-braces-backward)
  (global-set-key (kbd "M-}") 'corral-braces-forward)
  (global-set-key (kbd "M-\"") 'corral-double-quotes-backward)
  (setq corral-preserve-point t)
  )

(defun setup-anzu ()
  (use-package anzu
    :config
    (global-anzu-mode 1)
    :bind
    ("M-%" . anzu-query-replace)
    ("C-M-%" . anzu-query-replace-regexp)
    )
  )

(defun setup-notes-taking ()
  (when (require 'deft nil 'noerror)
    (setq
     deft-extensions '("org" "txt")
     deft-directory "~/Documents/deft/")
    (global-set-key (kbd "<f9>") 'deft))
  )

(defun setup-smartwin ()
  (require 'smartwin)
  (smartwin-mode 1)
  )

(defun setup-fuzzy ()
  (use-package fuzzy
    :config
    (turn-on-fuzzy-isearch)
    )
  )

(defun setup-git-gutter ()
  (use-package git-gutter

                                        ;(global-git-gutter-mode t)
    :bind
    ("C-x M-g" . git-gutter:toggle)
    ("C-x v =" . git-gutter:popup-hunk)
    ;; Jump to next/previous hunk
    ("C-x p" . git-gutter:previous-hunk)
    ("C-x n" . git-gutter:next-hunk)
    ;; Stage current hunk
    ("C-x v s" . git-gutter:stage-hunk)
    ;; Revert current hunk
    ("C-x v r" . git-gutter:revert-hunk)
    )
  )

(defun setup-docker ()
  ;; from here http://www.emacswiki.org/emacs/TrampAndDocker
;; Open files in Docker containers like so: /docker:drunk_bardeen:/etc/passwd
  (push
   (cons
    "docker"
    '((tramp-login-program "docker")
      (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
      (tramp-remote-shell "/bin/sh")
      (tramp-remote-shell-args ("-i") ("-c"))))
   tramp-methods)

  (defadvice tramp-completion-handle-file-name-all-completions
      (around dotemacs-completion-docker activate)
    "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
    (if (equal (ad-get-arg 1) "/docker:")
        (let* ((dockernames-raw (shell-command-to-string "docker ps | perl -we 'use strict; $_ = <>; m/^(.*)NAMES/ or die; my $offset = length($1); while(<>) {substr($_, 0, $offset, q()); chomp; for(split m/\\W+/) {print qq($_:\n)} }'"))
               (dockernames (cl-remove-if-not
                             #'(lambda (dockerline) (string-match ":$" dockerline))
                             (split-string dockernames-raw "\n"))))
          (setq ad-return-value dockernames))
      ad-do-it))

  )


(defun setup-go ()
  (use-package go-mode
    :config
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    ;(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
    :bind
    ("M-." . godef-jump)
    )

  (use-package golint
              )
  ;; (use-package go-autocomplete
  ;;   )
  )

(defun setup-auto-indent ()

  ;; from https://www.emacswiki.org/emacs/AutoIndentation
  (dolist (command '(yank yank-pop))
    (eval `(defadvice ,command (after indent-region activate)
             (and (not current-prefix-arg)
                  (member major-mode '(emacs-lisp-mode lisp-mode
                                                       clojure-mode    scheme-mode
                                                       haskell-mode    ruby-mode
                                                       rspec-mode      python-mode
                                                       c-mode          c++-mode
                                                       objc-mode       latex-mode
                                                       go-mode         web-mode
                                                       js-mode         plain-tex-mode
                                                       ))
                  (let ((mark-even-if-inactive transient-mark-mode))
                    (indent-region (region-beginning) (region-end) nil))))))

  (defadvice yank (around html-yank-indent)
    "Indents after yanking."
    (let ((point-before (point)))
      ad-do-it
      (when (eq major-mode 'html-mode) ;; check what mode we're in
        (indent-region point-before (point)))))
  (ad-activate 'yank)

  )

(defun setup-flycheck ()
  (use-package global-flycheck-mode
    )
  )

(defun setup-hilight ()
  (use-package highlight-symbol
    :bind
    (("C-<f3>" . highlight-symbol)
     ("<f3>" . highlight-symbol-next)
     ("S-<f3>" . highlight-symbol-prev)
     ("M-<f3>" . highlight-symbol-query-replace))
    )
  (use-package hilight-sexp
    :config
    (add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)
    (add-hook 'go-mode-hook 'highlight-sexp-mode)
    )
  )

(defun setup-avy ()
  (use-package avy
    :ensure t
    :bind (("C-M-s" . avy-goto-word-1)
    ))
  )

(defun my-after-init-hook ()
  (load custom-file)

  ;; do things after package initialization
  ;(setup-smex)
  (setup-igrep)
  (setup-scrat)
  (setup-postack)
  (setup-tramp)
  ;(setup-ocaml)
  (setup-ido)
  (setup-hippie)
  ;(setup-auto-complete)
  ;(setup-swoop)
  (setup-expand-region)
  (setup-misc-modes)
  ;(setup-swiper)
  (setup-corral)
  (setup-anzu)
  (setup-notes-taking)
  ;(setup-smartwin)
  (setup-fuzzy)
  (setup-git-gutter)
  (setup-docker)
  (setup-go)
  (setup-theme)
  (setup-auto-indent)
  (setup-hilight)
  (setup-avy)
  ;;(setup-helm)
  (set-keys)
  (mode-hooks)
  (mode-mapping)
  )

(add-hook 'after-init-hook 'my-after-init-hook)

(system-setup)
(global-settings)
(load "shmul-funcs")

(setq custom-file "~/.emacs.d/custom.el")
