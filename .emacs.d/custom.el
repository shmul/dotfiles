(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-command "/opt/local/bin/ack ")
 '(ag-executable "/opt/local/bin/ag")
 '(ag-group-matches nil)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#373b41"))
 '(c-basic-offset 4)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-search-path (quote ("." nil)))
 '(cscope-program "/opt/local/bin/cscope")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-blue)))
 '(custom-safe-themes
   (quote
    ("0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "fa80190f587f2fab395b878d78d4db0aab0fac9844e1345d55f2f7558eff221f" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "b34636117b62837b3c0c149260dfebe12c5dad3d1177a758bb41c4b15259ed7e" "9e147cee63e1a2a6b16021e0645bc66c633c42b849e78b8e295df4b7fe55c56a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" "19ba41b6dc0b5dd34e1b8628ad7ae47deb19f968fe8c31853d64ea8c4df252b8" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "708df3cbb25425ccbf077a6e6f014dc3588faba968c90b74097d11177b711ad1" "dcf229d4673483cb7b38505360824fa56a0d7b52f54edbcdca98cf5059fa1662" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "c3e567dedaa800e869d879c4df8478237d6ea31fd04464086fd674c864fe4d71" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" default)))
 '(deft-auto-save-interval 4.0)
 '(desktop-path (quote ("." "~/.emacs.d")))
 '(exec-path
   (quote
    ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin" "/opt/local/bin")))
 '(exec-path-from-shell-arguments (quote ("-l" "-i")))
 '(fci-rule-color "#373b41")
 '(font-lock-verbose nil)
 '(fringe-mode (quote (0)) nil (fringe))
 '(google-this-modeline-indicator "")
 '(graphviz-dot-dot-program "/opt/local/bin/dot")
 '(inhibit-startup-screen t)
 '(ispell-program-name "/opt/local/bin/ispell")
 '(jit-lock-debug-mode t)
 '(js-indent-level 2)
 '(line-move-visual nil)
 '(linum-format " %5i ")
 '(lua-indent-level 2)
 '(magit-git-executable "/opt/local/bin/git")
 '(markdown-command "/opt/local/bin/multimarkdown" t)
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
    (ivy markdown-mode markdown-preview-mode auto-complete which-key exec-path-from-shell go-mode iedit goto-last-change origami undo-tree melancholy-theme lua-mode whole-line-or-region xcscope web-mode vdiff vagrant-tramp vagrant use-package underwater-theme sublime-themes subatomic-theme reykjavik-theme noctilux-theme nginx-mode multiple-cursors multi-web-mode metalheart-theme magit igrep helm google-this git-gutter fuzzy expand-region corral color-theme-sanityinc-tomorrow color-theme avy anzu ag ack)))
 '(safe-local-variable-values
   (quote
    ((encoding . binary)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby"))))
 '(scroll-margin 3)
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
 '(tramp-default-method "sshx")
 '(undo-tree-mode-lighter " ")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil t)
 '(vc-handled-backends nil)
 '(web-mode-enable-current-column-highlight t)
 '(web-mode-enable-current-element-highlight nil)
 '(whole-line-or-region-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-current-column-highlight-face ((t (:background "orange3")))))
