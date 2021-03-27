(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-mode-text "")
 '(calendar-intermonth-text
   '(propertize
     (format "%2d"
             (car
              (calendar-iso-from-absolute
               (calendar-absolute-from-gregorian
                (list month day year)))))
     'font-lock-face 'font-lock-function-name-face))
 '(company-backends
   '(company-semantic company-capf
                      (company-dabbrev-code company-go company-keywords)
                      company-dabbrev))
 '(company-begin-commands '(self-insert-command))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-minimum-prefix-length 2)
 '(company-require-match nil)
 '(company-selection-wrap-around t)
 '(company-semantic-insert-arguments t)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 20)
 '(company-transformers nil)
 '(custom-safe-themes
   '("93268bf5365f22c685550a3cbb8c687a1211e827edc76ce7be3c4bd764054bad" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "cc71cf67745d023dd2e81f69172888e5e9298a80a2684cbf6d340973dd0e9b75" "25c06a000382b6239999582dfa2b81cc0649f3897b394a75ad5a670329600b45" "ae65ccecdcc9eb29ec29172e1bfb6cadbe68108e1c0334f3ae52414097c501d2" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "45caeeb594422c20499f96b51c73f9bc04d666983d0a1d16f5b9f51a9ec874fa" "7510c9c175f736d50b3c6589380c460b66b86762a17b728f13b18013188d2385" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default))
 '(dabbrev-case-replace 'case-replace)
 '(eldoc-echo-area-use-multiline-p nil)
 '(flycheck-checkers
   '(ada-gnat asciidoctor asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint d-dmd dockerfile-hadolint elixir-dogma emacs-lisp emacs-lisp-checkdoc erlang-rebar3 erlang eruby-erubis fortran-gfortran go-gofmt go-vet go-build go-test go-errcheck go-unconvert go-megacheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-jscs javascript-standard json-jsonlint json-python-json less less-stylelint llvm-llc lua-luacheck lua perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile r-lintr racket rpm-rpmlint markdown-mdl nix rst-sphinx rst ruby-rubocop ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tex-chktex tex-lacheck texinfo typescript-tslint verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby))
 '(gc-cons-threshold 100000000)
 '(ggtags-extra-args nil)
 '(ggtags-use-sqlite3 t)
 '(global-company-mode t)
 '(go-eldoc-gocode "/Users/shmul/go/bin/gocode")
 '(go-guess-gopath-functions '(go-godep-gopath go-wgo-gopath go-plain-gopath))
 '(go-guru-scope "hub/")
 '(godef-command "/Users/shmul/go/bin/godef")
 '(gxref-gtags-label nil)
 '(ido-ignore-buffers '("\\`*Eglot" "\\` "))
 '(json-reformat:indent-width 2)
 '(lsp-ui-doc-border "violet" t)
 '(lsp-ui-doc-enable nil t nil "Customized with use-package lsp-ui")
 '(lsp-ui-doc-header t t)
 '(lsp-ui-doc-include-signature nil t)
 '(lsp-ui-doc-max-height 30 t)
 '(lsp-ui-doc-max-width 60 t)
 '(lsp-ui-doc-position 'top t)
 '(lsp-ui-doc-use-childframe t t)
 '(lsp-ui-doc-use-webkit t t)
 '(lsp-ui-flycheck-enable nil t)
 '(lsp-ui-imenu-enable t t)
 '(lsp-ui-imenu-kind-position 'top t)
 '(lsp-ui-peek-enable t t)
 '(lsp-ui-peek-fontify 'on-demand t)
 '(lsp-ui-peek-list-width 50 t)
 '(lsp-ui-peek-peek-height 20 t)
 '(lsp-ui-sideline-code-actions-prefix "" t)
 '(lsp-ui-sideline-delay 2 t)
 '(lsp-ui-sideline-enable nil t)
 '(lsp-ui-sideline-ignore-duplicate t t)
 '(lsp-ui-sideline-show-code-actions t t)
 '(lsp-ui-sideline-show-diagnostics nil t)
 '(lsp-ui-sideline-show-hover t t)
 '(lsp-ui-sideline-show-symbol t t)
 '(lsp-ui-sideline-update-mode 'line t)
 '(max-mini-window-height 10)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(smartscan company-restclient restclient discover go-dlv marginalia zoom flymake-diagnostic-at-point eldoc-box zerodark-theme selectrum-prescient prescient selectrum company-go eglot company-statistics company-web-html company-quickhelp swiper hcl-mode rustic whole-line-or-region hole-line-or-region simple-modeline frog-jump-buffer go-playground gotest python-mode color-identifiers-mode org-jira zetteldeft deft base16-theme fzf prettier-js typescript-mode yapfify delight elpy doom-modeline darktooth-theme clues-theme tangotango-theme moe-theme vue-mode jinja2-mode vc-msg lsp-ui lsp-mode deadgrep multiple-cursors wgrep-ag json-mode projectile 0blayout scala-mode flycheck-golangci-lint dump-jump dumb-jump doom-themes flycheck-color-mode-line add-node-modules-path flycheck agtags fireplace gxref smart-mode-line yasnippet-snippets ac-js2 expand-region expand-region- go-eldoc fuzzy ac-dabbrev call-graph ggtags rotate xref-js2 js2-mode web-mode move-text imenu-list iedit protobuf-mode google-this grep-buffers hl-tags-mode sourcepair nginx-mode rainbow-mode magit goto-last-change yaml-mode autobookmarks yasnippet auto-complete-config flx-ido postack scrat rg which-key golden-ratio buffer-move markdown-mode undo-tree avy color-theme-sanityinc-tomorrow go-guru go-autocomplete auto-complete ido-grid-mode ac-c-headers exec-path-from-shell use-package))
 '(rm-blacklist '("golden" "yas" "google" "hl-p" "unto-tree" "ac"))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(zoom-ignore-predicates
   '((lambda nil
       (>
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
