;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

(require 'package)
(setq
 inhibit-startup-screen t
 package-enable-at-startup nil
 )
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             ;'("melpa-stable" . "https://stable.melpa.org/packages/")
             )
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa"))

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
(use-package exec-path-from-shell
  :ensure t)
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
    (tool-bar-mode -1)
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
  (setq frame-title-format "%b")
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
  (add-to-mode-list "\\.py$" 'python-mode)
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
  ;;(add-to-mode-list "\\.j2$" 'jinja2-mode)
  (add-to-mode-list "\\.vue$" 'vue-mode)
  (add-to-mode-list "drafts.md" 'auto-save-visited-mode)

  )

(defun setup-igrep ()
  (use-package igrep
    :defer t
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

  (global-set-key "\C-c>" 'open-init-el)

                                        ;(global-set-key (kbd "C-c M-w") 'append-to-register)
  ;;(global-set-key "\C-c\C-c" 'compile)
  ;;(global-set-key "\C-cC" 'recompile)

  (global-set-key (kbd "C-c C-<tab>") 'sourcepair-load)

  (global-set-key "\C-cD" 'delete-trailing-whitespace)

  (global-set-key (kbd "M-r") 'sacha/search-word-backward)
  (global-set-key (kbd "M-s") 'sacha/search-word-forward)


  (global-set-key (kbd "C-`") 'switch-to-previous-buffer)
  (global-set-key (kbd "C-c C-g") 'goto-address-at-point)
  (global-set-key (kbd "C-x m") 'magit-status)
  (global-set-key (kbd "C-x M") 'magit-file-dispatch)

  (global-set-key (kbd "C-+") 'align)
  (global-set-key (kbd "C-c C-o") 'org-open-at-point)

  (global-set-key (kbd "C-M-c") 'capitalize-first)

  (global-set-key (kbd "M-<left>")  'windmove-left)
  (global-set-key (kbd "M-<right>") 'windmove-right)
  (global-set-key (kbd "M-<up>")    'windmove-up)
  (global-set-key (kbd "M-<down>")  'windmove-down)
  (global-set-key (kbd "M-#") 'chris2-toggle-case)

                                        ;(global-set-key (kbd "C-a") 'shmul/move-beginning-of-line-or-indentation)
                                        ;(global-set-key (kbd "C-e") 'shmul/move-end-of-line-or-indentation)
  )

(defun setup-abbrev ()
  :ensure t
  :delight
  :defer t
  )

(defun setup-delight ()
  (use-package simple-modeline
:disabled
    :ensure t
    :hook (after-init . simple-modeline-mode)
    )

  (use-package delight
    :ensure t
    :config
    (advice-add 'c-update-modeline :override #'ignore)
    )
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
    :defer t
    :ensure t
    :config
    (setq tramp-default-method "ssh")
    )
  )

(defun setup-web-mode ()
  (use-package web-mode
    :defer t
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
    :defer t
    :config
    ;;(add-hook 'js2-mode-hook 'js-auto-beautify-mode)
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (setq js2-strict-trailing-comma-warning nil
          js-indent-level 2
          )
    ;; (defun my-js-mode-hook()
    ;;   (add-hook 'before-save-hook 'web-beautify-mode t))
    ;; (add-hook 'js2-mode-hook 'my-js-mode-hook)
    )


  (use-package xref-js2
    :ensure t
    :defer t
    :config
    ;; (setq semantic-symref-filepattern-alist
    ;;       (append semantic-symref-filepattern-alist
    ;;               '((js2-mode "*.js" ))))
    (add-hook 'js2-mode-hook
              (lambda ()
                (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
              )
    )

  (use-package typescript-mode
    :ensure t
    :defer t
    :config
    (setq typescript-indent-level 2)
    )

  (use-package vue-mode
    :ensure t
    :defer t
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

  (require 'clang-format) ;; no clang-format package yed
  (add-hook 'c-mode-common-hook (lambda ()
                                  (add-hook (make-local-variable 'before-save-hook)
                                            'clang-format-buffer)))
  )

(defun setup-projectile ()
  (use-package projectile
    :ensure t
    :delight
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

(defun setup-jira ()
  (use-package org-jira
    :ensure t
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

  (use-package company
    :ensure t
    :delight
    :hook (after-init . simple-modeline-mode)
    :config
    (global-company-mode 1)
    (setq company-selection-wrap-around t
          company-idle-delay            0.5
          company-minimum-prefix-length 2
          company-show-numbers          t
          company-tooltip-limit         20
          company-tooltip-align-annotations t
          company-dabbrev-downcase      nil
          company-dabbrev-ignore-case   nil
          company-semantic-insert-arguments t
          )
    (company-tng-configure-default)
    ;; Use numbers 0-9 (in addition to M-<num>) to select company completion candidates
    (mapc (lambda (x) (define-key company-active-map (format "%d" x) 'company-complete-tooltip-row))
          (number-sequence 0 9))
    ;; trigger completion on tab
    (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)
    :custom
    ;; Don't use company in the following modes
    (company-global-modes '(not shell-mode))

    :bind
    (:map company-active-map
          ("RET" . company-complete)
          ("TAB" . company-complete)
          ("C-n" . company-select-next)
          ("C-o" . company-other-backend)
          ("C-p" . company-select-previous)
          ("C-v" . company-next-page)
          ("M-v" . company-previous-page)
          ("C-<" . company-select-first)
          ("C->" . company-select-last)
          ("M-<" . company-select-first)
          ("M->" . company-select-last)
          )
    (:map company-search-map
          ("RET" . company-complete)
          ("TAB" . company-complete)
          ("C-n" . company-select-next)
          ("C-o" . company-other-backend)
                                        ;("M-/" . company-complete)
          )
    )


  (use-package company-quickhelp          ; Documentation popups for Company
    :ensure t
    :defer t
    :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode)
    :bind
    ("C-c h" . company-quickhelp-manual-begin)
    )

  (use-package company-restclient
    :ensure t
    :after restclient
    :defer t
    )

  )

(defun setup-fancy-dabbrev ()

  (use-package fancy-dabbrev
    :ensure t
    :defer t
    :commands (fancy-dabbrev-mode)
    :config
    (setq
     fancy-dabbrev-preview-delay 0.1
     fancy-dabbrev-preview-context 'before-non-word
     fancy-dabbrev-expansion-on-preview-only t
     fancy-dabbrev-indent-command 'tab-to-tab-stop
     company-frontends '(company-preview-frontend)
     )
    )
  )

(defun setup-misc-modes ()
  (show-paren-mode 1)
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
    :defer t
    :config
    (setq
     magit-section-initial-visibility-alist
     '((stashes . hide) (untracked . hide) (unpushed . hide)))
    )

  (use-package rainbow-mode
    :ensure t
    )
  (use-package nginx-mode
    :ensure t
    )

  (use-package multiple-cursors
    :ensure t
    :bind
    ("C-S-c C-S-c" . mc/edit-lines)
    ("C->" . mc/mark-next-like-this)
    ("C-<" . mc/mark-previous-like-this)
    ("C-c C-<" . 'mc/mark-all-like-this)
    )

  (use-package sourcepair
    )
  (use-package hl-tags-mode)
                                        ;(use-package whole-line-or-region)
  (use-package grep-buffers)
  (use-package google-this
    :ensure t
    :delight
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

  (use-package fiplr :disabled
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
    :delight
    :config
    (which-key-mode)
    )

  (use-package ripgrep
    :ensure t
    :demand
    :config
    (rg-enable-default-bindings)
    (add-hook 'rg-mode-hook 'wgrep-ag-setup)
    :bind
    ("C-c g" . rg)
    )


  (use-package deadgrep
    :ensure t
    :bind
    ("C-c d" . deadgrep)
    ("C-c D" . deadgrep)
    )

  (use-package jinja2-mode
    :disabled
    :ensure
    )

  (use-package move-text
    :ensure t
    :bind
    ([C-S-up] . move-text-up)
    ([C-S-down] . move-text-down)
    )

  ;; (use-package idle-highlight
  ;;   )

  (use-package vc-msg
    :ensure t
    :bind
    ("C-c v" . vc-msg-show)

    :config
    (eval-after-load 'vc-msg-git
      '(progn
         ;; show code of commit
         (setq vc-msg-git-show-commit-function 'magit-show-commit)
         ;; open file of certain revision
         (push '("m"
                 "[m]agit-find-file"
                 (lambda ()
                   (let* ((info vc-msg-previous-commit-info)
                          (git-dir (locate-dominating-file default-directory ".git")))
                     (magit-find-file (plist-get info :id )
                                      (concat git-dir (plist-get info :filename))))))
               vc-msg-git-extra)))
    )

  (use-package fzf
    :ensure t
    :defer t
    )

  (use-package color-identifiers-mode :disabled
    :ensure t
    :hook (after-init . global-color-identifiers-mode)
    )

  (use-package whole-line-or-region
    :ensure t
    :defer t
    )

  (use-package hcl-mode
    :ensure t
    :defer t
    )

  (use-package discover :disabled
    :ensure t
    :defer t
    :init
    (global-discover-mode 1)
    )

  (use-package restclient
    :ensure t
    :defer t
    )

  (use-package smartscan
    :ensure t
    :defer t
    :init
    (smartscan-mode 1)
    )

  (use-package all-the-icons
    :ensure t
    :defer t
    )

  (use-package neotree
    :ensure t
    :defer t
    :config
    (setq
     neo-theme (if (display-graphic-p) 'icons 'arrow))
    )

  (use-package wsd-mode
    :ensure t
    :defer t
    :init
    (add-hook 'wsd-mode-hook #'company-mode)
    )
  ;; from https://tech.toryanderson.com/2021/09/24/replacing-beacon.el-with-hl-line-flash/
  (use-package hl-line+
    :hook
    (window-scroll-functions . hl-line-flash)
    (focus-in . hl-line-flash)
    (post-command . hl-line-flash)

    :custom
    (global-hl-line-mode nil)
    (hl-line-flash-show-period 0.5)
    (hl-line-inhibit-highlighting-for-modes '(dired-mode))
    (hl-line-overlay-priority -100) ;; sadly, seems not observed by diredfl
    )

  (use-package hl-block-mode
    :ensure t
    :commands hl-block-mode
    :config
    (setq hl-block-bracket nil)    ;; Match all brackets.
    (setq hl-block-single-level t) ;; Only one pair of brackets.
    (setq hl-block-style 'color-tint)
    :hook ((prog-mode) . hl-block-mode)
    )

  (use-package sql-indent
    :ensure t
    :hook
    (sql-mode . sqlind-minor-mode)
    )

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
    :disabled
    :ensure t
    :delight
    :init
    (golden-ratio-mode 1)
    :config
    (setq golden-ratio-auto-scale t)
    )

  (use-package zygospore :disabled
    :bind
    ("C-x 1" . zygospore-toggle-delete-other-windows)
    )

  (use-package zoom
    :ensure t
    :config
    (zoom-mode t)
    (custom-set-variables
     '(zoom-size '(0.618 . 0.618))
     '(zoom-ignored-major-modes '(dired-mode markdown-mode))
     '(zoom-ignored-buffer-names '("zoom.el" "init.el"))
     '(zoom-ignored-buffer-name-regexps '("^*calc"))
     '(zoom-ignore-predicates '((lambda () (< (count-lines (point-min) (point-max)) 10))))
     )
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
  (use-package doom-themes :disabled
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    (load-theme 'doom-city-lights t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Enable custom neotree theme (all-the-icons must be installed!)
    (doom-themes-neotree-config)
    ;; or for treemacs users
    (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
    (doom-themes-treemacs-config)

    (use-package doom-modeline :disabled
      :ensure t
      :init (doom-modeline-mode 1)
      :config
      (setq
       doom-modeline-height 25
       doom-modeline-buffer-encoding nil
       find-file-visit-truename t
       doom-modeline-buffer-file-name-style 'buffer-name
       doom-modeline-icon nil
       )
      )
    ;; also run M-x all-the-icons-install-fonts

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

  (use-package darktooth-theme  :disabled
    :ensure t
    :config
    (load-theme 'darktooth t)
    )

  (use-package base16-theme
    :ensure t
    :config
    (load-theme 'base16-eighties t)
    )
  )


(defun setup-python ()
  (use-package elpy :disabled
    :ensure t
    :delight
    :defer t
    :init
    (elpy-enable)
    :config
    (setq
     elpy-rpc-virtualenv-path 'current)
    (define-key elpy-mode-map (kbd "M-<left>") nil)
    (define-key elpy-mode-map (kbd "M-<right>") nil)
    (define-key elpy-mode-map (kbd "M-<down>") nil)
    (define-key elpy-mode-map (kbd "M-<up>") nil)
    )

  (use-package yapfify :disabled
    :ensure t
    :delight
    :config
    (add-hook 'python-mode-hook 'yapf-mode)
    )
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
    :delight yas-minor-mode
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
    :delight
    )
  )

(defun setup-zettlekasten ()
  (use-package deft
    :ensure t
    :config
    (setq
     deft-default-extension "md"
     deft-extensions '("md" "txt" "org")
     deft-directory "~/Documents/notes"
     deft-use-filename-as-title t
     )
    )

  (use-package zetteldeft
    :ensure t
    :after deft
    :config
    ;;(zetteldeft-set-classic-keybindings)
    (setq
     zetteldeft-title-prefix "# "
     )
    )
  )

(defun setup-auto-complete ()
  (use-package auto-complete
    :ensure t
    :delight
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
                     rust-mode
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
    :delight
    )

  (use-package ac-c-headers :disabled)

  (use-package go-autocomplete
    :ensure t
    :disabled
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
  (use-package ivy
    :ensure t
    :delight
    )

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

(defun setup-selectrum ()
  (use-package prescient
    :ensure t
    )
  (use-package selectrum-prescient
    :ensure t
    )

  (use-package selectrum
    :ensure t
    :config
    (selectrum-mode +1)
    (selectrum-prescient-mode +1)
    (prescient-persist-mode +1)
    )

  ;; (use-package marginalia
  ;;   :disabled
  ;;   :ensure t
  ;;   :bind (:map minibuffer-local-map
  ;;               ("C-M-a" . marginalia-cycle)
  ;;               ;; When using the Embark package, you can bind `marginalia-cycle' as an Embark action!
  ;;               ;;:map embark-general-map
  ;;               ;;     ("A" . marginalia-cycle)
  ;;               )

  ;;   ;; The :init configuration is always executed (Not lazy!)
  ;;   :init

  ;;   ;; Must be in the :init section of use-package such that the mode gets
  ;;   ;; enabled right away. Note that this forces loading the package.
  ;;   (marginalia-mode)

  ;;   ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  ;;   (advice-add #'marginalia-cycle :after
  ;;               (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
  ;;   )
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

(defun setup-eglot ()
  (use-package eldoc-box
    :ensure t
    :delight
    )

  (use-package eglot
    :ensure t
    :delight
    :defer t
    :config
    (add-to-list 'eglot-stay-out-of 'company)
                                        ;(setq eglot-ignored-server-capabilites (quote (:workspaceSymbolProvider)))
    (add-hook 'go-mode-hook 'eglot-ensure)
    :hook (eldoc-box-hover-mode . eldoc-box-hover-mode)
    :bind
    (:map eglot-mode-map
          ("C-c C-r" . eglot-rename)
          ("C-c C-a" . eglot-code-actions)
          ("C-c C-t" . eglot-help-at-point)
          )
    )

  (use-package flymake-diagnostic-at-point
    :after flymake
    :ensure t
    :config
    (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
    )

  )


(defun setup-go ()
  (use-package go-mode
    :delight
    :defer t

    :config
    (exec-path-from-shell-copy-env "GOPATH")
    (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
    (setq gofmt-command "gofmt") ;; when goimports isn't so slow, use it again
    (defun my-go-mode-hook()
      (add-hook 'before-save-hook 'gofmt-before-save t)
                                        ; Customize compile command to run go build
                                        ;(go-guru-hl-identifier-mode)
      (yas-minor-mode)
      )

    (add-hook 'go-mode-hook 'my-go-mode-hook)
                                        ;(add-hook 'go-mode-hook 'lsp)
                                        ;(add-hook 'go-mode-hook 'flycheck-mode)
    ;; from https://github.com/joaotavora/eglot/issues/574
    (defun eglot-organize-imports ()
      "Offer to execute the source.organizeImports code action."
      (interactive)
      (unless (eglot--server-capable :codeActionProvider)
        (eglot--error "Server can't execute code actions!"))
      (let* ((server (eglot--current-server-or-lose))
             (actions (jsonrpc-request
                       server
                       :textDocument/codeAction
                       (list :textDocument (eglot--TextDocumentIdentifier))))
             (action (cl-find-if
                      (jsonrpc-lambda (&key kind &allow-other-keys)
                        (string-equal kind "source.organizeImports" ))
                      actions)))
        (when action
          (eglot--dcase action
            (((Command) command arguments)
             (eglot-execute-command server (intern command) arguments))
            (((CodeAction) edit command)
             (when edit (eglot--apply-workspace-edit edit))
             (when command
               (eglot--dbind ((Command) command arguments) command
                 (eglot-execute-command server (intern command) arguments))))))))

    (defun eglot-organize-imports-on-save ()
      (defun eglot-organize-imports-nosignal ()
        "Run eglot-organize-imports, but demote errors to messages."
        ;; Demote errors to work around
        ;; https://github.com/joaotavora/eglot/issues/411#issuecomment-749305401
        ;; so that we do not prevent subsequent save hooks from running
        ;; if we encounter a spurious error.
        (with-demoted-errors "Error: %s" (eglot-organize-imports)))
      (add-hook 'before-save-hook #'eglot-organize-imports-on-save))

    (add-hook 'go-mode-hook #'eglot-organize-imports-on-save)

    ;; ;; work around for flycheck error https://gitmemory.com/issue/flycheck/flycheck/1523/469402280
    ;; (let ((govet (flycheck-checker-get 'go-vet 'command)))
    ;;   (when (equal (cadr govet) "tool")
    ;;     (setf (cdr govet) (cddr govet))))

    ;;(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
    ;; :bind
    ;; (:map go-mode-map
    ;;       ("M-." . godef-jump)
    ;;       ("C-M-." . pop-tag-mark))
    )

  (use-package golint :disabled
    )

  (use-package go-guru :disabled
    )

  (use-package eldoc
    :delight)

  (use-package go-eldoc
    :ensure t
    :delight
    :defer t
    :config
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    )

  (use-package flycheck-golangci-lint
    :disabled
    :ensure t
    :delight
    :hook (go-mode . flycheck-golangci-lint-setup)
    )

  ;; see also - go get github.com/juntaki/gogtags for gnu global with golang

  (use-package gotest
    :ensure t
    :defer t
    )

  (use-package go-dlv
    :ensure t
    :defer t
    )

  (use-package go-playground
    :ensure t
    :defer t
    )

  (use-package company-go          ; Documentation popups for Company
    :ensure t
    :defer t
    :config
    (setq company-tooltip-limit 20                      ; bigger popup window
          company-idle-delay .3                         ; decrease delay before autocompletion popup shows
          company-echo-delay 0                          ; remove annoying blinking
          company-begin-commands '(self-insert-command) ; start autocompletion only after typin
	      )
    )
  )

(defun setup-rust ()
  (use-package rustic
    :ensure t
    :delight
    :defer t
    :init
    (setq rustic-lsp-server 'rust-analyzer)

    :config
    (setq
     lsp-rust-analyzer-server-command '("/usr/local/bin/rust-analyzer")
     rustic-format-on-save t)
    (add-hook 'rustic-mode-hook 'lsp)
    )
  )

;; https://ladicle.com/post/config/
;; https://framagit.org/citreu/emacs.d/blob/master/etc/init-prog-edit.el
;; https://github.com/CSRaghunandan/.emacs.d/blob/master/setup-files/setup-lsp.el
;; https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
;; https://github.com/emacs-lsp/lsp-ui

(defun setup-lsp ()
  (use-package lsp-mode
    :ensure t
    :delight
    :commands (lsp lsp-deferred)
    :hook (
           (lsp-after-open . lsp-enable-imenu)
           ((go-mode c-mode c++-mode) . lsp-deferred)
           )
    :bind
    (:map lsp-mode-map
          ("C-c C-r"   . lsp-rename)
          ("C-c C-t" . lsp-describe-thing-at-point)
          )
    :config
    (setq
     lsp-prefer-flymake nil
     lsp-document-sync-method 'incremental ;; none, full, incremental, or nil
     lsp-response-timeout 10
     lsp-eldoc-render-all t
     lsp-clients-go-server-args '("--cache-style=always" "--diagnostics-style=onsave" "--format-style=goimports")

     ;; from http://blog.binchen.org/posts/how-to-speed-up-lsp-mode.html

     lsp-log-io nil
     lsp-enable-folding nil
     lsp-diagnostic-package :none
     lsp-enable-snippet nil
     lsp-enable-completion-at-point t
     lsp-enable-links nil
     lsp-enable-file-watchers nil
     lsp-restart 'auto-restart
     lsp-completion-provider :capf

     ;; from https://emacs-lsp.github.io/lsp-mode/page/performance/
     read-process-output-max (* 1024 1024) ;; 1mb
     )
    (defvar lsp-on-touch-time 0)
    (defadvice lsp-on-change (around lsp-on-change-hack activate)
      ;; don't run `lsp-on-change' too frequently
      (when (> (- (float-time (current-time))
                  lsp-on-touch-time) 30) ;; 30 seconds
        (setq lsp-on-touch-time (float-time (current-time)))
        ad-do-it))
    )

  (use-package lsp-ui
    :ensure t

    :custom
    (lsp-ui-doc-enable nil)
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature nil)
    (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width 60)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit t)
    (lsp-ui-doc-border "violet")
    ;; lsp-ui-flycheck
    (lsp-ui-flycheck-enable nil)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable nil)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics nil)
    (lsp-ui-sideline-show-code-actions t)
    (lsp-ui-sideline-code-actions-prefix "")
    (lsp-ui-sideline-delay 2)
    (lsp-ui-sideline-update-mode 'line)
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable t)
    (lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-peek-height 20)
    (lsp-ui-peek-list-width 50)
    (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always

    :preface
    (defun ladicle/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
          (progn
            (lsp-ui-doc-mode -1)
            (lsp-ui-doc--hide-frame))
        (lsp-ui-doc-mode 1)))
    :bind
    (:map lsp-mode-map
          ("C-c m"   . lsp-ui-imenu)
          ("C-c s"   . lsp-ui-sideline-mode)
          ("C-c i"   . lsp-ui-peek-find-implementation)
          ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
          ([remap xref-find-references] . lsp-ui-peek-find-references))

    :hook
    (lsp-mode . lsp-ui-mode)
    )

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

(defun setup-dumb-jump ()
  (use-package dumb-jump
    :ensure t
    :config
    (setq dumb-jump-force-searcher 'rg)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

    :bind (
           ("M-." . dumb-jump-go)
           ;; ("M-g o" . dumb-jump-go-other-window)
           ;; ("M-g j" . dumb-jump-go)
           ;; ("M-g i" . dumb-jump-go-prompt)
           ;; ("M-g x" . dumb-jump-go-prefer-external)
           ;; ("M-g z" . dumb-jump-go-prefer-external-other-window)
           ;; ("M-g l" . dumb-jump-quick-look)
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
    :delight
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

    :bind (("C-c C-c" . compile-again))
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
    :defer t
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

(defun setup-containers ()
  (use-package terraform-mode
    :ensure t
    :init
    (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
    )

  (use-package company-terraform
    :ensure t
    )

  (use-package terraform-doc
    :ensure t
    )

  (use-package dockerfile-mode
    :ensure t
    )

  (use-package k8s-mode
    :ensure t
    :hook
    (k8s-mode . yas-minor-mode)
  )

  (use-package kubel
    :ensure t
    )
  )



(defun my-after-init-hook ()
  (exec-path-from-shell-initialize)
  (load custom-file)
  (setup-delight)
  (setup-abbrev)
  ;; do things after package initialization

                                        ;(setup-smex)
                                        ;(setup-igrep)
  (setup-scrat)
  (setup-postack)
                                        ;(setup-tramp)
                                        ;(setup-ocaml)
  (setup-ido)

                                        ;(setup-auto-complete)
                                        ;(setup-swoop)
  (setup-expand-region)
  (setup-misc-modes)
                                        ;(setup-swiper)

                                        ;(setup-corral)
                                        ;(setup-anzu)
                                        ;(setup-notes-taking)
                                        ;(setup-smartwin)
  (setup-fuzzy)
  (setup-selectrum)
                                        ;(setup-git-gutter)
                                        ;(setup-docker)
                                        ;(setup-gnu-global)
                                        ;(setup-lsp)
  (setup-eglot)
  (setup-go)
  ;(setup-rust)
  (setup-python)
  (setup-theme)
  (setup-auto-indent)
                                        ;(setup-hilight)
  (setup-avy)
  (setup-dumb-jump)
                                        ;(setup-cscope)
  (setup-undo-tree)
  (setup-smart-compile)
  (setup-markdown)
  (setup-web-mode)
  (setup-mode-line)
                                        ;(setup-helm)
                                        ;(setup-function-args)
  (setup-hippie)
  (setup-company)
  (setup-fancy-dabbrev)
                                        ;(setup-flycheck)
  (setup-window-management)
                                        ;(setup-irony)
  (setup-c-coding)
  (setup-projectile)
  (setup-yasnippets)
  (setup-containers)
                                        ;(setup-zettlekasten)
  (set-keys)
  (mode-hooks)
  (mode-mapping)


  )

(add-hook 'after-init-hook 'my-after-init-hook)

(system-setup)
(global-settings)
(load "shmul-funcs")

(setq custom-file "~/.emacs.d/custom.el")
(put 'downcase-region 'disabled nil)
