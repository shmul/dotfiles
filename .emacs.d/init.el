(setenv "USER"  "shmul")
(setenv "LONGNAME"  "shmulik")
(setq user-full-name "Shmulik Regev")



(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa"))


(defvar macosx-p (string-match "darwin" (symbol-name system-type)))
(defvar windows-p (string-match "windows-nt" (symbol-name system-type)))
(defvar linux-p (string-match "gnu/linux" (symbol-name system-type)))



(defun setup-helm ()

  (require 'helm-config)
  (helm-mode 1)
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

     (when (executable-find "curl")
       (setq helm-google-suggest-use-curl-p t))

     (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
           helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
           helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
           helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
           helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
           helm-ff-file-name-history-use-recentf t)


     )
(defun system-setup ()
  (when macosx-p
    (setq mac-option-modifier 'meta)
    (setq default-input-method "MacOSX")
    (add-to-list
     'default-frame-alist
     '(font . "-apple-Bitstream_Vera_Sans_Mono-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"))
    )
  (set-frame-height (selected-frame) 68)
  (set-frame-width (selected-frame) 91)

  )


(defun global-settings ()
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
)


(defun mode-hooks ()

  (defun my-default-mode-hook ()
  )

  (defun my-cc-mode-hook ()
    (my-default-mode-hook)
    (if (use-curl-c-style-p)
        (progn
          (c-add-style "curl" curl-c-style t)
          (c-set-style "curl"))
      (c-set-style "gnu"))
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

  )

(defun setup-igrep ()
  (autoload (function igrep) "igrep"
    "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
  (autoload (function igrep-find) "igrep"
    "*Run `grep` via `find`..." t)
  (autoload (function dired-do-igrep) "igrep"
    "*Run `grep` on the marked (or next prefix ARG) files." t)
  (autoload (function dired-do-igrep-find) "igrep"
    "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
  (setq igrep-options "-i -s")
  (when windows-p
    (setq igrep-program "c:\\cygwin\\bin\\grep.exe")
    (setq igrep-find-program "c:\\cygwin\\bin\\find.exe")
    )
  (when macosx-p
    (setq igrep-find-use-xargs nil))
  (setq igrep-expression-quote-char ?')
  (setq igrep-parenthesis-escape-char ?\\)

  )

(defun set-keys ()
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
  (global-set-key "\C-cC" 'recompile)

  (global-set-key (kbd "C-c C-<tab>") 'sourcepair-load)

  (global-set-key "\C-cD" 'delete-trailing-whitespace)

  (global-set-key (kbd "M-r") 'sacha/search-word-backward)
  (global-set-key (kbd "M-s") 'sacha/search-word-forward)

  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

  )

(defun setup-scrat ()
  (autoload 'scratch "scrat" nil t)
  (defun jot () (interactive)
         (scratch "*jot*" 'text-mode))
  (defun jotter () (interactive)
         (scratch "*jotter*" 'text-mode))
  (defun joty () (interactive)
         (scratch "*joty*" 'text-mode))
  )

(defun setup-postack ()
  (load-library "postack")
  (global-set-key "\C-c(" 'postack-push)
  (global-set-key "\C-c)" 'postack-pop)
  )

(defun setup-tramp ()
    (require 'tramp)
    (setq tramp-default-method "ssh")
    )

(defun setup-misc-modes ()
  (autoload 'goto-last-change "goto-last-change"
    "Set point to the position of the last change." t)
  ;(require 'hippie-exp)
  (require 'magit)
  (require 'nginx-mode nil 'noerror)
  (require 'multiple-cursors)

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
  )

(defun set-theme ()
  (require 'color-theme)
  (color-theme-initialize)
  )

(defun setup-ido ()
  (ido-mode 1)
  (setq
   read-buffer-function 'ido-read-buffer
   ido-enable-flex-matching t)

                                        ; from emacs wiki
  (setq ido-execute-command-cache nil)
  )

(defun setup-hippie ()
  (require 'hippie-exp)

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

(defun my-after-init-hook ()
  (load custom-file)

  ;; do things after package initialization
  ;(setup-helm)
  (setup-igrep)
  (setup-scrat)
  (setup-postack)
  (setup-tramp)
  (setup-ocaml)
  (setup-ido)
  (setup-hippie)
  (setup-swoop)
  (setup-misc-modes)
  (set-theme)
  (set-keys)
  (mode-hooks)
  (mode-mapping)
  )

(add-hook 'after-init-hook 'my-after-init-hook)

(system-setup)
(global-settings)
(load "shmul-funcs")

(setq custom-file "~/.emacs.d/custom.el")
