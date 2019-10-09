;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/melpa"))
(setq-default abbrev-mode t) ; should be set early
(package-initialize)

(server-start)
;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar macosx-p (string-match "darwin" (symbol-name system-type)))
(defvar windows-p (string-match "windows-nt" (symbol-name system-type)))
(defvar linux-p (string-match "gnu/linux" (symbol-name system-type)))

(setq use-package-verbose t)
;(setq debug-on-error t)

;; based on a recommendation from https://github.com/lewang/flx
(setq gc-cons-threshold 20000000)

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
                                        ;(set-frame-height (selected-frame) 68)
                                        ;(set-frame-width (selected-frame) 91)

  )


(defun global-settings ()
  (setq ring-bell-function 'ignore)
  (setq-default indent-tabs-mode nil)
  (setq tab-width 4)
  (line-number-mode 1)
  (setq frame-title-format "%b - emacs")
  (column-number-mode 1)
  (setq scroll-step 1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-font-lock-mode t)
  (which-function-mode)
  (mouse-avoidance-mode 'cat-and-mouse)
  (add-hook 'before-save-hook 'delete-trailing-whitespace t)
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq tab-always-indent 'complete)
  )


(defun mode-hooks ()

  (defun my-default-mode-hook ()
    )

  (defun my-default-prog-mode-hook ()
    (my-default-mode-hook)
    ;; seems to slow us down - (idle-highlight)
    )

  (defun my-cc-mode-hook ()
    (my-default-prog-mode-hook)
    (setq c-default-style "k&r"
          c-basic-offset 4)
    ;;(setq show-trailing-whitespace t)
    (c-toggle-hungry-state)
    (c-toggle-auto-state -1)
                                        ;(superword-mode 1)
    )

  (defun my-sh-mode-hook ()
    (my-default-mode-hook)
    (setq
     sh-basic-offset 4
     sh-indentation 4)
    )


  (defun my-ruby-mode-hook ()
    (my-default-prog-mode-hook)
    (c-toggle-hungry-state)
    )

  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (my-default-prog-mode-hook)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-style-padding 1)
    (setq web-mode-script-padding 1)
    (setq web-mode-block-padding 0)
    (setq web-mode-comment-style 2)
                                        ;(set-face-attribute 'web-mode-current-element-highlight-face t :background "LightYellow2")
    (set-face-attribute 'web-mode-current-column-highlight-face t :background "LightYellow2")
                                        ;(hl-tags-mode 1)
    (rainbow-mode 1)
    )

  (add-hook 'emacs-lisp-mode-hook 'my-default-mode-hook)
  (add-hook 'lisp-mode-hook 'my-default-mode-hook)

  (add-hook 'c-mode-hook 'my-cc-mode-hook)
                                        ;(modify-syntax-entry ?_ "w" c-mode-syntax-table)

  (add-hook 'c++-mode-hook 'my-cc-mode-hook)
                                        ;(modify-syntax-entry ?_ "w" c++-mode-syntax-table)

  (add-hook 'makefile-mode-hook 'my-default-mode-hook)
  (add-hook 'mutt-mode-hook 'my-default-mode-hook)
  ;; (add-hook 'perl-mode-hook 'my-default-mode-hook)
  ;; (add-hook 'cperl-mode-hook 'my-default-mode-hook)
  (add-hook 'java-mode-hook 'my-cc-mode-hook)
  (add-hook 'tex-mode-hook 'my-default-mode-hook)
  (add-hook 'latex-mode-hook 'my-default-mode-hook)
  (add-hook 'dired-mode-hook 'my-default-mode-hook)
  (add-hook 'compilation-mode-hook 'my-default-mode-hook)
  (add-hook 'nxml-my 'mode-default-mode-hook)

  (add-hook 'lua-mode-hook 'my-default-prog-mode-hook)
  (add-hook 'text-mode-hook 'my-default-mode-hook)
  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

  (add-hook 'sh-mode-hook 'my-sh-mode-hook)
  ;; (add-hook 'objc-mode-hook 'my-default-mode-hook)
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  ;(add-hook 'compilation-finish-functions 'my-shorten-filenames-in-compilation)

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
  (add-to-mode-list "\\.php$" 'php-mode)
  (add-to-mode-list "\\.css$" 'css-mode)
  (add-to-mode-list "\\.m$" 'objc-mode)
  (add-to-mode-list "\\.mm$" 'objc-mode)
  (add-to-mode-list "go.mod" 'go-mode)
  (add-to-mode-list "\\.go$" 'go-mode)
  (add-to-mode-list "^go\\.mod$" 'go-mode)
  (add-to-mode-list "\\.rb$" 'ruby-mode)
  (add-to-mode-list "\\.ru$" 'ruby-mode)
  (add-to-mode-list "rakefile$" 'ruby-mode)
  (add-to-mode-list "capfile$" 'ruby-mode)
  (add-to-mode-list "\\.html?\\'" 'web-mode)
  (add-to-mode-list "\\.yml$" 'yaml-mode)
  (add-to-mode-list "\\.yaml$" 'yaml-mode)
  (add-to-mode-list "\\.md$" 'markdown-mode)
  (add-to-mode-list "\\.js$" 'js2-mode)

  )

(defun setup-igrep ()
  (use-package igrep
    :ensure t
    :commands (igrep igrep-find dired-do-igrep dired-do-igrep-find)

    :config
    (setq igrep-options "-i -s")
    (setq igrep-expression-quote-char ?')
    (setq igrep-parenthesis-escape-char ?\\)
    (setq igrep-find-use-xargs nil)
    :bind
    ("C-c g" . igrep)
    ("C-c G" . igrep-find))
  )

(defun set-keys ()
  ;; override Macbook pro annoying backtick/tilde placement
  (global-set-key "§" "`")
  (global-set-key "±" "~")
  (global-set-key "\C-c§" 'next-error)
  (global-set-key (kbd "C-x k") 'kill-this-buffer)

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
  (global-set-key "\M-W" 'copy-word)

  (global-set-key "\e0" 'delete-window)
  (global-set-key "\C-cr" 'revert-buffer-dont-ask)
  (global-set-key "\C-cc" 'goto-last-change)
  (global-set-key "\C-cf" 'find-file-at-point)
  (global-set-key (kbd "C-<tab>") 'comint-replace-by-expanded-filename)

  ;;(global-set-key (kbd "C-M-<down>") 'forward-list)
  ;;(global-set-key (kbd "C-M-<up>") 'backward-list)
  (global-set-key (kbd "C-M-<right>") 'jump-to-white)
  (global-set-key (kbd "C-<right>") 'forward-word)
  (global-set-key (kbd "C-<left>") 'backward-word)

  (global-set-key "\C-co" 'other-window)
  (global-set-key "\C-cm" 'buffer-menu)

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

  (global-set-key (kbd "C-`") 'switch-to-previous-buffer)
  (global-set-key (kbd "C-c C-g") 'goto-address-at-point)
  (global-set-key (kbd "C-x m") 'magit-status)

  (global-set-key (kbd "C-+") 'align)
  (global-set-key (kbd "C-c C-o") 'org-open-at-point)

  (global-set-key (kbd "C-M-c") 'capitalize-first)

  (global-set-key (kbd "M-<left>")  'windmove-left)
  (global-set-key (kbd "M-<right>") 'windmove-right)
  (global-set-key (kbd "M-<up>")    'windmove-up)
  (global-set-key (kbd "M-<down>")  'windmove-down)
  (global-set-key (kbd "M-#") 'chris2-toggle-case)
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
    :ensure t
    :config
    (setq tramp-default-method "ssh")
    )
  )

(defun setup-web-mode ()
  (use-package web-mode
    :ensure t
    )
  (use-package web-beautify :disabled
    :config
    (setq web-beautify-js-program "/opt/local/bin/js-beautify")
    (setq web-beautify-html-program "/opt/local/bin/html-beautify")
    )
  (use-package js-auto-beautify :disabled
    )
  (use-package js2-mode
    :ensure t
    :config
    ;;(add-hook 'js2-mode-hook 'js-auto-beautify-mode)
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (setq js2-strict-trailing-comma-warning nil)
    ;; (defun my-js-mode-hook()
    ;;   (add-hook 'before-save-hook 'web-beautify-mode t))
    ;; (add-hook 'js2-mode-hook 'my-js-mode-hook)
    )


  (use-package xref-js2
    :ensure t
    :config
    ;; (setq semantic-symref-filepattern-alist
    ;;       (append semantic-symref-filepattern-alist
    ;;               '((js2-mode "*.js" ))))
    (add-hook 'js2-mode-hook
              (lambda ()
                (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
              )
    )
  )

(defun setup-mode-line ()
  (use-package smart-mode-line
    :ensure t
    :config
    (sml/setup)
    (sml/apply-theme 'automatic)

    )
  )

(defun setup-gnu-global ()
  (use-package ggtags
    :ensure t
    :init
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode)
                  (ggtags-mode 1))))
    (set (make-local-variable 'eldoc-documentation-function)
         'ggtags-eldoc-function)
    (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
    (setq-local hippie-expand-try-functions-list
                (cons 'ggtags-try-complete-tag hippie-expand-try-functions-list))
    (setq
     ggtags-highlight-tag nil
     ggtags-enable-navigation-keys nil)
    :bind
    (:map ggtags-mode-map
          ("C-c l s" . ggtags-find-other-symbol)
          ("C-c l h" . ggtags-view-tag-history)
          ("C-c l r" . ggtags-find-reference)
          ("C-c l d" . ggtags-find-definition)
          ("C-c l f" . ggtags-find-file)
          ("C-c l c" . ggtags-create-tags)
          ("C-c l u" . ggtags-update-tags)
          ("M-." . ggtags-find-tag-dwim)
          ("M-," . pop-tag-mark)
          )
    ;; (:map ggtags-navigation-map
    ;;       ("C-M-}" . ggtags-navigation-next-file)
    ;;       ("C-M-{" . ggtags-navigation-previous-file)
    ;;       ("C-M-=" . ggtags-navigation-start-file)
    ;;       ("C-M->" . ggtags-navigation-last-error)
    ;;       ("C-M-<" . first-error))
    ;; (define-key ggtags-navigation-map "M-ss" nil)  ;; to prevent overriding our isearch
    )
  (use-package gxref :disabled
    :ensure t
    :config
    (add-to-list 'xref-backend-functions 'gxref-xref-backend)
    )

  )


(defun setup-c-coding ()
  (use-package c-eldoc :disabled
    :init
    ;;(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
    (add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode)
    )


  (use-package call-graph :disabled
    :config
    ;;(setq call-graph-initial-max-depth 3)
    )
  )

(defun setup-projectile ()
  (use-package projectile
    :ensure t
    :pin melpa-stable
    :config
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (projectile-mode +1)
    (defun maybe-projectile-find-file ()
      (interactive)
      (call-interactively
       (if (projectile-project-p)
           #'projectile-find-file
         #'ido-find-file)))
    :bind
    ;;("C-x C-f" . maybe-projectile-find-file)

    )
  )

(defun setup-function-args ()
  (use-package function-args
    :config
    (fa-config-default)
    (set-default 'semantic-case-fold t)
    )
  )

(defun setup-company ()
  ;; based on https://gist.githubusercontent.com/rswgnu/85ca5c69bb26551f3f27500855893dbe/raw/0ceb8e34f27c14914a1e9547f4c9dd6142315b27/rsw-company-config.el
  ;; Company function modified by RSW to prevent error when completing
  ;; in the minibuffer (uses standard Emacs completion instead since
  ;; company does not support minibuffer completion).
  (defun company-complete ()
    "Insert the common part of all candidates or the current selection.
The first time this is called, the common part is inserted, the second
time, or when the selection has been changed, the selected candidate is
inserted."
    (interactive)
    (if company-mode
        (when (company-manual-begin)
          (if (or company-selection-changed
                  (eq last-command 'company-complete-common))
              (call-interactively 'company-complete-selection)
            (call-interactively 'company-complete-common)
            (setq this-command 'company-complete-common)))
      (completion-at-point)))

  ;; New function by RSW
  (defun company-quit ()
    "Insert any selected completion and quit completing."
    (interactive)
    (when (and company-selection-changed company--manual-action
               (boundp 'company-tng--overlay) company-tng--overlay)
      (company--insert-candidate
       (nth company-selection company-candidates)))
    (company-cancel))

  ;; New function by RSW
  (defun company-select-first ()
    "Select first company completion candidate even if not visible."
    (interactive)
    (when (company-manual-begin)
      (company-set-selection 0)))

  ;; New function by RSW
  (defun company-select-last ()
    "Select last company completion candidate even if not visible."
    (interactive)
    (when (company-manual-begin)
      (company-set-selection (1- company-candidates-length))))

  ;; New function by RSW
  (defun company-select-first-visible ()
    "Select first visible company completion candidate."
    (interactive)
    (when (company-manual-begin)
      (company-set-selection 0)))

  ;; New function by RSW
  (defun company-select-last-visible ()
    "Select last visible company completion candidate."
    (interactive)
    (when (company-manual-begin)
      (company-set-selection (1- company-candidates-length))))

  (use-package company
    :ensure t
    :defer t
    :config
    (global-company-mode 1)
    (setq company-selection-wrap-around t
          company-idle-delay            nil
          company-minimum-prefix-length 2
          company-show-numbers          t
          company-tooltip-limit         20
          ompany-tooltip-align-annotations t
          company-dabbrev-downcase      nil
          company-dabbrev-ignore-case   nil
          company-backends '(company-dabbrev-code company-dabbrev company-clang
                                                  company-irony company-go company-keywords company-capf ))

    (company-tng-configure-default)
    ;; Use numbers 0-9 (in addition to M-<num>) to select company completion candidates
    (mapc (lambda (x) (define-key company-active-map (format "%d" x) 'company-complete-number))
          (number-sequence 0 9))

    :bind
    ("M-/" . company-complete)
    (:map company-active-map
          ("RET" . company-complete)
          ("C-n" . company-select-next)
          ("C-o" . company-other-backend)
          ("C-p" . company-select-previous)
          ("C-v" . company-next-page)
          ("M-v" . company-previous-page)
          ("C-<" . company-select-first)
          ("C->" . company-select-last)
          ("M-<" . company-select-first)
          ("M->" . company-select-last))
    (:map company-search-map
          ("RET" . company-complete)
          ("C-n" . company-select-next)
          ("C-o" . company-other-backend))
    )


  (use-package company-quickhelp          ; Documentation popups for Company
    :ensure t
    :defer t
    :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode)
    :bind
    ("C-c h" . company-quickhelp-manual-begin))

  (use-package company-web-html
    )

  (use-package company-statistics
    :config
    (company-statistics-mode)
    )
  )

(defun setup-misc-modes ()
  (use-package yaml-mode
    :ensure t
    )

  (use-package json-mode
    :ensure t
    )

  (use-package goto-last-change
    :ensure t
    )
  (use-package magit
    :ensure t
    )
  (use-package rainbow-mode
    :ensure t
    )
  (use-package nginx-mode
    :ensure t
    )
                                        ;(use-package multiple-cursors)
  (use-package sourcepair
    )
  (use-package hl-tags-mode)
                                        ;(use-package whole-line-or-region)
  (use-package grep-buffers)
  (use-package google-this
    :ensure t
    :config
    (google-this-mode 1)
    )
  (use-package protobuf-mode
    :ensure t
    :mode "\\.proto\\'"
    )

  (use-package lua-mode
    :bind
    ("C-c C-b" . lua-goto-matching-block)
    )

                                        ;(use-package modeline-git-branch)
  (use-package ibuffer
    :ensure t
    :bind
    ("C-x C-b" . ibuffer)
    )

  (use-package wgrep-ag
    :ensure t
    )
  (use-package iedit
    :ensure t
    )
  (show-paren-mode 1)

  (use-package fiplr
    :config
    (setq fiplr-ignored-globs '((directories (".git" ".svn"))
                                (files ("*.jpg" "*.png" "*.zip" "*.txt" "*.log" "*.orig" "*~"))))
    :bind
    ("C-x f" . fiplr-find-file)
    )

  (use-package imenu-list
    :ensure t
    :config
    (setq imenu-list-focus-after-activation t
          imenu-list-auto-resize t)
    :bind
    ("C-'" . imenu-list-smart-toggle)
    )

  (use-package which-key
    :ensure t
    :config
    (which-key-mode)
    )

  (use-package rg
    :ensure t
    :demand
    :config
    (rg-enable-default-bindings)
    (add-hook 'rg-mode-hook 'wgrep-ag-setup)
    :bind
    ("C-c g" . rg)
    )

  (use-package move-text
    :ensure t
    :bind
    ([C-S-up] . move-text-up)
    ([C-S-down] . move-text-down)
    )

  ;; (use-package idle-highlight
  ;;   )

  (use-package osx-dictionary :disabled )

  )

(defun setup-window-management()
                                        ;(desktop-save-mode 1)
  ;; +-------------+---------+
  ;; |             | compile |
  ;; |     edit    | search  |
  ;; |             | magit   |
  ;; +-------------+---------+
  ;; |     ?       |   ?     |
  ;; +-------------+---------+

  (use-package buffer-move
    :ensure t
    ;; no key bindings required (yet). Use:
    ;; (global-set-key (kbd "<C-S-up>")     'buf-move-up)
    ;; (global-set-key (kbd "<C-S-down>")   'buf-move-down)
    ;; (global-set-key (kbd "<C-S-left>")   'buf-move-left)
    ;; (global-set-key (kbd "<C-S-right>")  'buf-move-right)
    )

  (use-package rotate
    :bind
    ("M-^" . rotate-window)
    ("C-M-^" . rotate-layout)
    )

  (use-package golden-ratio
    :ensure t
    :init
    (golden-ratio-mode 1)
    :config
    (setq golden-ratio-auto-scale t)
    )

  (use-package zygospore :disabled
    :bind
    ("C-x 1" . zygospore-toggle-delete-other-windows)
    )

  (use-package zoom :disabled
    :config
    (zoom-mode t)
    ;; (defun size-callback ()
    ;;   (cond ((> (frame-pixel-width) 1024) '(90 . 0.75))
    ;;         (t                            '(0.618 . 0.618))))
    ;; (custom-set-variables
    ;;  '(zoom-size 'size-callback))
    :bind
    ("C--" . zoom)
    )

  ;; based on https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-shackle.el
  (use-package shackle :disabled
    :ensure t
    :config
    (progn
      (setq shackle-lighter "")
      (setq shackle-select-reused-windows nil) ; default nil
      (setq shackle-default-alignment 'below) ; default below
      (setq shackle-default-size 0.4) ; default 0.5

      (setq shackle-rules
            ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
            '((compilation-mode              :select nil                                               )
              ("*undo-tree*"                 :select t                          :size 0.25 :align right)
              ("*eshell*"                    :select t                          :other t               )
              ("*Shell Command Output*"      :select nil                                               )
              (occur-mode                    :select nil                                   :align t    )
              ("*Help*"                      :select t   :inhibit-window-quit t :other t               )
              ("*Completions*"                                                  :size 0.3  :align t    )
              ("*Messages*"                  :select nil :inhibit-window-quit t :other t               )
              ("\\*[Wo]*Man.*\\*"  :regexp t :select t   :inhibit-window-quit t :other t               )
              ("*Calendar*"                  :select t                          :size 0.3  :align below)
              ("*ag search*"                 :select t                          :size 0.3  :align below)
              ("*cscope*"        :select t                          :size 0.3  :align below)
              ;;("*ggtags-global*"        :select t                          :size 0.3  :align below)
              ("*info*"                      :select t   :inhibit-window-quit t                         :same t)
              (magit-status-mode             :select t   :inhibit-window-quit t                         :popup t)
              (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
              ))

      (shackle-mode 1))
    )



  (use-package window-purpose :disabled
    :ensure t
    :init
    (purpose-mode)
    :config
                                        ;(purpose-x-golden-ratio-setup)
    (purpose-x-code1-setup)
    :bind
    ("C-x 4" . purpose-x-code1-setup)
    ;; (add-to-list 'purpose-user-mode-purposes '(c-mode . c-file-purpose))
    ;; (add-to-list 'purpose-user-mode-purposes '(cscope-list-entry-mode . cscope-purpose))
    ;; (add-to-list 'purpose-user-mode-purposes '(compilation-mode . compilation-purpose))
    ;; ;; build it
    ;;(purpose-compile-user-configuration)
    )
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
  (load-theme 'doom-spacegrey) ;sanityinc-tomorrow-eighties,
  )

(defun setup-ido ()
  (use-package browse-url
    :ensure t
    ) ;; ffap source says to load browse-url prior to ffap
  (use-package ido-grid-mode
    :ensure t
    )
  (use-package ido
    :ensure t
    :config
    (ido-mode 1)
    (ido-everywhere)
    (ido-grid-mode 1)
    (setq
     read-buffer-function 'ido-read-buffer
     ido-enable-flex-matching t
     ido-use-filename-at-point 'guess
     )
                                        ; from emacs wiki
    (setq ido-execute-command-cache nil))

  (use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights.
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)
    )

  ;; from https://gist.github.com/nmurthy/3427972
  (add-hook 'ido-setup-hook 'ido-my-keys)

  (defun ido-completing-read-horiz (prompt choices &optional _predicate require-match
                                           initial-input hist def _inherit-input-method)
    "do ido-completing-read, but horizontally"
    (let ((old-decorations ido-decorations)
          (res ""))
      (setq ido-decorations '( "{" "}" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
      (setq res (ido-completing-read prompt choices _predicate require-match initial-input hist def _inherit-input-method))
      (setq ido-decorations old-decorations)
      (identity res)))

  (defun my-ido-search-directories ()
    "Search directory list to jump to one quickly"
    (interactive)
    (let* ((enable-recursive-minibuffers t)
           (dirlist (remove-if (lambda (x) (string= "" x)) (split-string ido-current-directory "/")))
           (chosen (ido-completing-read-horiz (concat "Choose dir (" (mapconcat 'identity dirlist ",") "): ") dirlist))
           (chosen-dir-start-idx (string-match chosen ido-current-directory))
           (chosen-dir-end-idx (match-end 0))
           (chosen-dir (substring ido-current-directory 0 chosen-dir-end-idx)))
      (when chosen-dir
        (ido-set-current-directory chosen-dir)
        (setq ido-exit 'refresh)
        (exit-minibuffer))))

  (defun ido-my-keys ()
    "Add my keybindings for ido."
    (define-key ido-completion-map "\C-t" 'projectile-find-file)
    ;;(define-key ido-completion-map "\C-t" 'my-ido-search-directories)
    )
  )

(defun setup-hippie ()
  (use-package hippie-exp
    :ensure t
    :config
    (setq hippie-expand-try-functions-list
          '(;;try-expand-dabbrev
            ;;try-expand-all-abbrevs
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-expand-list
            try-expand-line
            try-complete-file-name-partially
            try-complete-file-name
            ))
    :bind
    ("M-<return>" . hippie-expand)
    )
  )

(defun setup-yasnippets ()
  (use-package yasnippet
    :ensure t
    :bind
    (:map yas-minor-mode-map
          ("TAB" . nil)
          ("<tab>" . nil)
          ("C-<tab>" . yas-expand)
          )
    :config
    (yas-reload-all)
    )
  (use-package yasnippet-snippets
    :ensure t
    )
  )

(defun setup-auto-complete ()
  (use-package auto-complete
    :ensure t
    :commands auto-complete-mode
    :init
    (auto-complete-mode t)

    :bind (("C-n" . ac-next)
           ("C-p" . ac-previous)
           (:map ac-completing-map
                 ("\e" . ac-stop)))
    :config
    (use-package auto-complete-config)
    (ac-set-trigger-key "TAB")

    (setq ac-delay 0.8
          ac-use-menu-map t
          ac-menu-height 50
          ac-use-quick-help nil
          ac-comphist-file  "~/.emacs.d/ac-comphist.dat"
          ac-ignore-case nil
          ac-dwim t
          ac-fuzzy-enable nil
          ac-auto-start nil
          shmul/ac-sources-c '(ac-source-dabbrev ac-source-gtags ac-source-words-in-same-mode-buffers )
          shmul/ac-sources-go '(ac-source-dabbrev ac-source-go ac-source-words-in-same-mode-buffers )
          )
    (ac-config-default)
    (setq ac-modes '(js2-mode
                     c-mode
                     cc-mode
                     c++-mode
                     go-mode
                     web-mode
                     ocaml-mode
                                        ;tuareg-mode
                                        ;haskell-mode
                     python-mode
                     ruby-mode
                     lua-mode
                     javascript-mode
                     js-mode
                     css-mode
                     makefile-mode
                     sh-mode
                                        ;xml-mode
                     emacs-lisp-mode
                                        ;lisp-mode
                                        ;lisp-interaction-mode
                                        ;java-mode
                                        ;scheme-mode
                     protobuf-mode
                     ))
    (add-hook 'c-mode-common-hook (lambda ()
                                    (setq ac-sources shmul/ac-sources-c)))
    ;;(global-auto-complete-mode t)
    ;; ;; instead of calling (ac-config-default) here are the functions I want
    ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
    ;; (add-hook 'css-mode-hook 'ac-css-mode-setup)
    ;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)
    )

  (use-package ac-dabbrev
    )

  (use-package ac-c-headers :disabled)

  (use-package go-autocomplete
    :ensure t
    :config
    (add-hook 'go-mode-common-hook (lambda ()
                                     (setq ac-sources shmul/ac-sources-go)))
    (add-hook 'go-mode-common-hook
              (lambda () (set (make-local-variable 'compile-command) (format "make -f %s" (get-closest-pathname)))))
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
    :ensure t
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
    ("C-%" . anzu-query-replace-at-cursor)
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
    :ensure t
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
    (exec-path-from-shell-copy-env "GOPATH")
    (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
    (setq gofmt-command "gofmt") ;; when goimports isn't so slow, use it again
    (defun my-go-mode-hook()
      (add-hook 'before-save-hook 'gofmt-before-save t)
                                        ; Customize compile command to run go build
      (go-guru-hl-identifier-mode)
      (yas-minor-mode)
      )

    (add-hook 'go-mode-hook 'my-go-mode-hook)
    ;; work around for flycheck error https://gitmemory.com/issue/flycheck/flycheck/1523/469402280
    (let ((govet (flycheck-checker-get 'go-vet 'command)))
      (when (equal (cadr govet) "tool")
        (setf (cdr govet) (cddr govet))))

    ;;(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
    :bind
    (:map go-mode-map
          ("M-." . godef-jump)
          ("C-M-." . pop-tag-mark))
    )

  (use-package golint :disabled
    )

  (use-package go-guru :disabled
    )

  (use-package go-eldoc
    :ensure t
    :config
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    )


  ;; see also - go get github.com/juntaki/gogtags for gnu global with golang
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

(defun setup-dump-jump ()
  (use-package dumb-jump
    :ensure t
    :config
    (setq dumb-jump-force-searcher 'rg)
    :bind (("M-g o" . dumb-jump-go-other-window)
           ("M-g j" . dumb-jump-go)
           ("M-g i" . dumb-jump-go-prompt)
           ("M-g x" . dumb-jump-go-prefer-external)
           ("M-g z" . dumb-jump-go-prefer-external-other-window)
           ("M-." . dumb-jump-go)
           )

    )
  )

(defun setup-cscope ()
  (use-package xcscope
    :ensure t
    :config
    (cscope-setup)
    :bind (:map cscope-minor-mode-keymap
                ("M-." . cscope-find-global-definition-no-prompting))
    )
  )

(defun setup-undo-tree ()
  (use-package undo-tree
    :ensure t
    :config
    (global-undo-tree-mode)
    )
  )


(defun setup-smart-compile ()
                                        ; from https://emacs.stackexchange.com/a/8137
  (use-package ansi-color
    :config
    (defun my/ansi-colorize-buffer ()
      (let ((buffer-read-only nil))
        (ansi-color-apply-on-region (point-min) (point-max))))
    (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)
    )

  (use-package smart-compile
    :config
    ;; using http://stackoverflow.com/a/40789413
    (setq compilation-last-buffer nil)
    (defun compile-again (ARG)
      "Run the same compile as the last time.

With a prefix argument or no last time, this acts like M-x compile,
and you can reconfigure the compile args."
      (interactive "p")
      ;; the following two lines create bug: split a new window every time
      ;; (if (not (get-buffer-window "*compilation*"))
      ;;      (split-window-below))
      (if (and (eq ARG 1) compilation-last-buffer)
          (recompile)
        (call-interactively 'smart-compile)))
    ;; create a new small frame to show the compilation info
    ;; will be auto closed if no error
    ;; (setq special-display-buffer-names
    ;;       `(("*compilation*" . ((name . "*compilation*")
    ;;                             ,@default-frame-alist
    ;;                             (left . (- 1))
    ;;                             (top . 0)))))
    (setq compilation-finish-functions
          (lambda (buf str)
            (if (null (string-match ".*exited abnormally.*" str))
                ;;no errors, make the compilation window go away in a few seconds
                (progn
                  (run-at-time
                   "1 sec" nil 'delete-windows-on
                   (get-buffer-create "*compilation*"))
                  (message "No Compilation Errors!")))))

    :bind (("C-x C-m" . compile-again))
    )

  ;; using https://stackoverflow.com/a/28268829
  ;;     (setq compilation-last-buffer nil)
  ;;     (defun compile-again (ARG)
  ;;       "Run the same compile as the last time.

  ;; If there is no last time, or there is a prefix argument, this acts like M-x compile."
  ;;       (interactive "p")
  ;;       (if (and (eq ARG 1)
  ;;                compilation-last-buffer)
  ;;           (progn
  ;;             (set-buffer compilation-last-buffer)
  ;;             (revert-buffer t t))
  ;;         (progn
  ;;           (call-interactively 'smart-compile)
  ;;           (setq cur (selected-window))
  ;;           (setq w (get-buffer-window "*compilation*"))
  ;;           (select-window w)
  ;;           (setq h (window-height w))
  ;;           (shrink-window (- h 10))
  ;;           (select-window cur))))

  ;;     (defun my-compilation-hook ()
  ;;       "Make sure that the compile window is splitting vertically."
  ;;       (progn
  ;;         (if (not (get-buffer-window "*compilation*"))
  ;;             (progn
  ;;               (split-window-vertically)))))
  ;;     (add-hook 'compilation-mode-hook 'my-compilation-hook)
  ;;     (defun compilation-exit-autoclose (STATUS code msg)
  ;;       "Close the compilation window if there was no error at all."
  ;;       ;; If M-x compile exists with a 0
  ;;       (when (and (eq STATUS 'exit) (zerop code))
  ;;         ;; then bury the *compilation* buffer, so that C-x b doesn't go there
  ;;         (bury-buffer)
  ;;         ;; and delete the *compilation* window
  ;;         (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;;       ;; Always return the anticipated result of compilation-exit-message-function
  ;;       (cons msg code))
  ;;     (setq compilation-exit-message-function 'compilation-exit-autoclose)
  ;;     (defvar all-overlays ())
  ;;     (defun delete-this-overlay(overlay is-after begin end &optional len)
  ;;       (delete-overlay overlay)
  ;;       )
  ;;     (defun highlight-current-line ()
  ;;       "Highlight current line."
  ;;       (interactive)
  ;;       (setq current-point (point))
  ;;       (beginning-of-line)
  ;;       (setq beg (point))
  ;;       (forward-line 1)
  ;;       (setq end (point))
  ;;       ;; Create and place the overlay
  ;;       (setq error-line-overlay (make-overlay 1 1))

  ;;       ;; Append to list of all overlays
  ;;       (setq all-overlays (cons error-line-overlay all-overlays))

  ;;       (overlay-put error-line-overlay
  ;;                    'face '(background-color . "red"))
  ;;       (overlay-put error-line-overlay
  ;;                    'modification-hooks (list 'delete-this-overlay))
  ;;       (move-overlay error-line-overlay beg end)
  ;;       (goto-char current-point))

  ;;     (defun delete-all-overlays ()
  ;;       "Delete all overlays"
  ;;       (while all-overlays
  ;;         (delete-overlay (car all-overlays))
  ;;         (setq all-overlays (cdr all-overlays))))
  ;;     (defun highlight-error-lines(compilation-buffer process-result)
  ;;       (interactive)
  ;;       (delete-all-overlays)
  ;;       (condition-case nil
  ;;           (while t
  ;;             (next-error)
  ;;             (highlight-current-line))
  ;;         (error nil)))
  ;;     (setq compilation-finish-functions 'highlight-error-lines)

  )

(defun setup-markdown ()
  (use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :config
    (setq markdown-command "multimarkdown")
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    )
  )

(defun setup-flycheck ()
  (use-package flycheck
    :ensure t
    :init
    ;;(global-flycheck-mode)
    (add-hook 'go-mode-hook 'flycheck-mode)
    )
  (use-package add-node-modules-path
    :ensure t
    :init
    (add-hook 'js-mode-hook 'add-node-modules-path)
    )

  (use-package flycheck-color-mode-line
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
    )
  )

(defun setup-irony ()
  (use-package irony
    :ensure t
    :init
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)

    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    )

  (use-package irony-eldoc
    :config
    (add-hook 'irony-mode-hook #'irony-eldoc)
    )
  )


(defun my-after-init-hook ()
  (exec-path-from-shell-initialize)
  (load custom-file)

  ;; do things after package initialization
                                        ;(setup-smex)
  ;(setup-igrep)
  (setup-scrat)
  (setup-postack)
  (setup-tramp)
                                        ;(setup-ocaml)
  (setup-ido)
  (setup-hippie)
  (setup-auto-complete)
  (setup-yasnippets)
                                        ;(setup-swoop)
  (setup-expand-region)
  (setup-misc-modes)
                                        ;(setup-swiper)
                                        ;(setup-corral)
                                        ;(setup-anzu)
                                        ;(setup-notes-taking)
                                        ;(setup-smartwin)
  (setup-fuzzy)
                                        ;(setup-git-gutter)
                                        ;(setup-docker)
  (setup-gnu-global)
  (setup-go)
  (setup-theme)
  (setup-auto-indent)
                                        ;(setup-hilight)
  (setup-avy)
  (setup-dump-jump)
                                        ;(setup-cscope)
  (setup-undo-tree)
  (setup-smart-compile)
  (setup-markdown)
  (setup-web-mode)
  (setup-mode-line)
                                        ;(setup-helm)
                                        ;(setup-function-args)
                                        ;(setup-company)
  (setup-flycheck)
  (setup-window-management)
                                        ;(setup-irony)
  (setup-c-coding)
  (setup-projectile)
  (set-keys)
  (mode-hooks)
  (mode-mapping)
  )

(add-hook 'after-init-hook 'my-after-init-hook)

(system-setup)
(global-settings)
(load "shmul-funcs")

(setq custom-file "~/.emacs.d/custom.el")
