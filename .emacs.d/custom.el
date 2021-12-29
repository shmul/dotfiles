(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-begin-commands
   '(self-insert-command org-self-insert-command orgtbl-self-insert-command c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash))
 '(eglot-extend-to-xref t)
 '(markdown-command "multimarkdown")
 '(markdown-hide-urls t)
 '(markdown-link-space-sub-char "-")
 '(markdown-wiki-link-search-subdirectories t)
 '(package-selected-packages
   '(pandoc sql-indent hl-block-mode go-imports dockerfile-mode wsd-mode ripgrep rg all-the-icons neotree kubel k8s-mode terraform-doc company-terraform terraform-mode yasnippet-snippets yasnippet projectile zoom buffer-move fancy-dabbrev company-restclient company-quickhelp smart-mode-line vue-mode typescript-mode xref-js2 js2-mode web-mode markdown-mode undo-tree dumb-jump avy base16-theme company-go go-playground go-dlv gotest go-eldoc flymake-diagnostic-at-point eglot eldoc-box selectrum-prescient prescient fuzzy smartscan restclient hcl-mode whole-line-or-region fzf vc-msg move-text deadgrep which-key imenu-list iedit wgrep-ag protobuf-mode google-this multiple-cursors nginx-mode rainbow-mode magit goto-last-change json-mode yaml-mode expand-region flx-ido ido-grid-mode delight exec-path-from-shell use-package))
 '(tab-width 2)
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp)))
 '(zoom-ignore-predicates
   '((lambda nil
       (<
        (count-lines
         (point-min)
         (point-max))
        10))))
 '(zoom-ignored-buffer-name-regexps '("^*calc"))
 '(zoom-ignored-buffer-names '("zoom.el" "init.el"))
 '(zoom-ignored-major-modes '(dired-mode markdown-mode))
 '(zoom-size '(0.618 . 0.618)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
