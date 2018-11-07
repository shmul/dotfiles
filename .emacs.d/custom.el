(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-fuzzy-limit 4)
 '(ac-ispell-requires 4)
 '(ack-command "/opt/local/bin/ack ")
 '(ag-executable "/opt/local/bin/ag")
 '(ag-group-matches nil)
 '(ag-highlight-search nil)
 '(ag-ignore-list (quote ("*.map" "*min.css" "*min.js")))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(c-basic-offset 4)
 '(call-graph-filters (quote ("grep -E \"\\.(cpp|cc|c):\"")))
 '(call-graph-path-to-global "/opt/local/bin/")
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-search-path (quote ("~/workspace/hub.git/src/" nil)))
 '(cscope-program "/opt/local/bin/cscope")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-blue)))
 '(custom-safe-themes
   (quote
    ("e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "f11e219c9d043cbd5f4b2e01713c2c24a948a98bed48828dc670bd64ae771aa1" "edea0b35681cb05d1cffe47f7eae912aa8a930fa330f8c4aeb032118a5d0aabf" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "c90fd1c669f260120d32ddd20168343f5c717ca69e95d2f805e42e88430c340e" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215" "b9a06c75084a7744b8a38cb48bc987de10d68f0317697ccbd894b2d0aca06d2b" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "fa80190f587f2fab395b878d78d4db0aab0fac9844e1345d55f2f7558eff221f" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "b34636117b62837b3c0c149260dfebe12c5dad3d1177a758bb41c4b15259ed7e" "9e147cee63e1a2a6b16021e0645bc66c633c42b849e78b8e295df4b7fe55c56a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" "19ba41b6dc0b5dd34e1b8628ad7ae47deb19f968fe8c31853d64ea8c4df252b8" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "708df3cbb25425ccbf077a6e6f014dc3588faba968c90b74097d11177b711ad1" "dcf229d4673483cb7b38505360824fa56a0d7b52f54edbcdca98cf5059fa1662" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "c3e567dedaa800e869d879c4df8478237d6ea31fd04464086fd674c864fe4d71" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" default)))
 '(dabbrev-case-fold-search nil)
 '(deft-auto-save-interval 4.0)
 '(desktop-path (quote ("." "~/.emacs.d")))
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-minor-mode-string " El")
 '(eldoc-print-after-edit nil)
 '(exec-path-from-shell-arguments (quote ("-l" "-i")))
 '(fci-rule-color "#515151")
 '(font-lock-verbose nil)
 '(fringe-mode (quote (0)) nil (fringe))
 '(ggtags-enable-navigation-keys nil)
 '(ggtags-highlight-tag nil)
 '(ggtags-oversize-limit 2621440)
 '(go-command "/usr/local/go/bin/go")
 '(go-eldoc-gocode "/Users/shmul/dev/gw/bin/gocode")
 '(go-test-gb-command "/Users/shmul/dev/gw/bin/gb")
 '(google-this-modeline-indicator "")
 '(graphviz-dot-dot-program "/opt/local/bin/dot")
 '(ido-use-url-at-point t)
 '(inhibit-startup-screen t)
 '(ispell-program-name "/opt/local/bin/ispell")
 '(jit-lock-debug-mode t)
 '(js-indent-level 2)
 '(kill-ring-max 100)
 '(line-move-visual nil)
 '(linum-format " %5i ")
 '(lua-indent-level 2)
 '(magit-git-executable "/opt/local/bin/git")
 '(markdown-command "/opt/local/bin/multimarkdown")
 '(menu-bar-mode t)
 '(modeline-git-branch-mode t)
 '(nginx-indent-level 2)
 '(ns-alternate-modifier (quote meta))
 '(nxml-slash-auto-complete-flag t)
 '(org-startup-truncated nil)
 '(package-archives
   (quote
    (("ELPA" . "http://tromey.com/elpa/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(package-selected-packages
   (quote
    (htmlize clues-theme zeno-theme yasnippet-snippets autobookmarks ido-grid-mode deadgrep material-theme osx-dictionary idle-highlight move-text imenu-list flx flx-ido go-projectile projectile color-theme-sanityinc-tomorrow color-theme flatland-theme gxref xref-js2 afternoon-theme moe-theme rg wsd-mode call-graph ggtags golden-ratio ac-c-headers buffer-move gotest rotate go-guru go-autocomplete ac-capf ac-clang ac-js2 c-eldoc ac-dabbrev golint rainbow-mode go-eldoc js2-mode web-beautify markdown-mode markdown-preview-mode auto-complete which-key exec-path-from-shell go-mode iedit goto-last-change undo-tree lua-mode whole-line-or-region web-mode vagrant-tramp vagrant use-package sublime-themes nginx-mode multi-web-mode magit igrep google-this fuzzy expand-region avy ag ack)))
 '(projectile-mode t nil (projectile))
 '(ripgrep-executable "/opt/local/bin/rg")
 '(safe-local-variable-values
   (quote
    ((encoding . binary)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby"))))
 '(scroll-margin 3)
 '(sml/replacer-regexp-list
   (quote
    (("^~/org/" ":Org:")
     ("^~/\\.emacs\\.d/elpa/" ":ELPA:")
     ("^~/\\.emacs\\.d/" ":ED:")
     ("^/sudo:.*:" ":SU:")
     ("^~/Documents/" ":Doc:")
     ("^~/Dropbox/" ":DB:")
     ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
     ("^~/workspace/catod.git/" ":catod:")
     ("^~/workspace/hub.git/" ":hub:")
     ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:"))))
 '(sourcepair-header-extensions
   (quote
    (".h" ".H" ".hpp" ".HPP" ".Hpp" ".hh" ".HH" ".hxx" ".HXX" ".mli")))
 '(sourcepair-header-path (quote (".")))
 '(sourcepair-source-extensions
   (quote
    (".cpp" ".CPP" ".Cpp" ".cxx" ".CXX" ".cc" ".CC" ".c" ".C" ".c++" ".C++" ".ml")))
 '(sourcepair-source-path (quote (".")))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tramp-default-method "sshx" nil (tramp))
 '(undo-tree-mode-lighter " ")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil t)
 '(vc-handled-backends nil)
 '(web-mode-enable-current-column-highlight t)
 '(web-mode-enable-current-element-highlight nil)
 '(whole-line-or-region-mode t)
 '(yas-global-mode t)
 '(yas-snippet-dirs
   (quote
    ("/Users/shmul/.emacs.d/snippets" yasnippet-snippets-dir)))
 '(zoom-size (quote size-callback)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(iedit-occurrence ((t (:inherit ag-match-face)))))
