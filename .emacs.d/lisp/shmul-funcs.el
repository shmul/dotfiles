;; a collection of function either written by me (the small trivial ones)
;; or collected from the net (the really useful ones)


(defun add-to-mode-list (suffix mode)
  (add-to-list 'auto-mode-alist (cons suffix  mode)))
;  (setq auto-mode-alist (cons (cons suffix  mode) auto-mode-alist)))

(defun jump-to-char (c)
  "moves the cursor to the next occurrence of the given char"
  (interactive "cJump to char:")
  (search-forward (string c))
  (forward-char -1)
  )

(defun jump-to-white ()
  "moves the cursor to the next occurrence of white char"
  (interactive)
  (search-forward-regexp " ")
  (forward-char -1)
  )


(defun zap-to-char-and-add (c)
  "moves the cursor to the next occurrence of the given char"
  (interactive "cJump to char:")
  (zap-to-char 1 c)
  (insert-char c 1)
  )

(defun revert-buffer-dont-ask ()
  "reverts the buffer without asking for conformation"
  (interactive)
  (revert-buffer t 1))

(defun open-init-el ()
  "opens the init el file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun kill-line-from-start ()
  "kills the current line from its start"
  (interactive)
  (beginning-of-line)
  (kill-line)
  (backward-delete-char 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUFFER SWITCHING FIX
;;
;; This changes the behaviour of the switch-to-buffer completion functions so
;; that the current buffer is NOT in the completion list.
;;
;; i.e. say you're working in "temp.c", and you want to visit "temp.h"; so you
;; type "C-xb", then "t<TAB>" which then presents you with a completion list of
;; temp.c and temp.h, so you then must type "h<RET>".  This is annoying since
;; why would you want to switch back to the buffer you're in?!?
;; Using this fix would remove "temp.c" from the completion lits so that when
;; you had typed "t<TAB>" the name would be completed as "temp.h" as desired.
;;
;; Steve Dodd
;; March 1998
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun s-minibuffer-complete ()
  "A shell around minibuffer-complete which removes the name of the
current buffer from the buffer completion list.  The default behaviour
doesn't make sense since there is no reason to ask to switch to the buffer
you are already in!"
  (interactive)
  (if s-remove-first-completion
  (progn (setq s-remove-first-completion nil)
	 (if (consp minibuffer-completion-table)
	     (setq  minibuffer-completion-table
		    (cdr minibuffer-completion-table)) ()))
  ())
  (minibuffer-complete))

(defun s-minibuffer-complete-word ()
  "A shell around minibuffer-complete-word which removes the name of the
current buffer from the buffer completion list.  The default behaviour
doesn't make sense since there is no reason to ask to switch to the buffer
you are already in!"
  (interactive)
  (if s-remove-first-completion
  (progn (setq s-remove-first-completion nil)
	 (if (consp minibuffer-completion-table)
	     (setq  minibuffer-completion-table
		    (cdr minibuffer-completion-table)) ()))
  ())
  (minibuffer-complete-word)
  )

(defun s-minibuffer-complete-and-exit ()
  "A shell around minibuffer-complete-and-exit which removes the name of
the current buffer from the buffer completion list.  The default behaviour
doesn't make sense since there is no reason to ask to switch to the buffer
you are already in!"
  (interactive)
  (if s-remove-first-completion
  (progn (setq s-remove-first-completion nil)
	 (if (consp minibuffer-completion-table)
	     (setq  minibuffer-completion-table
		    (cdr minibuffer-completion-table)) ()))
  ())
  (minibuffer-complete-and-exit))


(defun s-switch-to-buffer ()
  "A shell around switch-to-buffer which removes the name of the current
buffer from the buffer completion list.  The default behaviour doesn't
make sense since there is no reason to ask to switch to the buffer you are
already in!"
  (interactive)
  (setq s-remove-first-completion 't)
  (ido-switch-buffer))

(setq s-remove-first-completion 'nil)

(define-key minibuffer-local-completion-map "\040" 's-minibuffer-complete-word)
(define-key minibuffer-local-completion-map "\t" 's-minibuffer-complete)
(define-key minibuffer-local-must-match-map [return]
  's-minibuffer-complete-and-exit)
(global-set-key "\C-xb" 's-switch-to-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF BUFFER SWITCHING FIX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun vi-paren-match ()
  "(ARG) Go to the matching parenthesis."
  (interactive "")
  (let (limit)
  (cond ((looking-at "[\(\[{]")
	 (forward-sexp 1)
	 (backward-char))
	((looking-at "[])}]")
	 (forward-char)
	 (backward-sexp 1))
	(t (save-excursion
	     (end-of-line)
	     (setq limit (point)))
	   (skip-chars-forward "^[](){}" limit)
	   (if (looking-at "[])}\(\[{]")
	       (vi-paren-match)
	     (progn
	       (save-excursion
		 (beginning-of-line)
		 (setq limit (point)))
	       (end-of-line)
	       (skip-chars-backward "^[](){}" limit)
	       (backward-char 1)
	       (if (looking-at "[])}\(\[{]")
		   (vi-paren-match)
		 (error ""))))))))

(defvar highlight-word-overlays nil "")
(defun highlight-word (WORD)
  ""
  (interactive "MEnter word to highlight:")
  ;; remove all overlays in the buffer
  (while (not (null highlight-word-overlays))
  (delete-overlay (car highlight-word-overlays))
  (setq highlight-word-overlays (cdr highlight-word-overlays)))
  (if (> (length WORD) 0)
  (save-excursion
    (goto-char (point-min))
    (while (and (< (point) (point-max))
		(search-forward WORD nil t))
      (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
	(overlay-put overlay 'face 'highlight)
	(setq highlight-word-overlays
	      (cons overlay highlight-word-overlays)))))))


;; align assignments
(defun find-assignment ()
  (if (re-search-forward
       "[^<>=!]=\\|\\+=\\|-=\\|\\*=\\|/=\\|&=\\||=\\|\\^=\\|<<=\\|>>="
       (save-excursion (end-of-line) (point)) t)
  (progn
    (goto-char (match-beginning 0))
    (if (looking-at ".==")
	nil
      (if (looking-at
	   "\\+=\\|-=\\|\\*=\\|/=\\|&=\\||=\\|\\^=\\|<<=\\|>>=")
	  (set-mark (match-end 0))
	(forward-char 1)
	(set-mark (1+ (point))))
      (delete-horizontal-space)
      t))
  nil)
  )


(defun align-equals (start end)
  "make the first assignment operator on each line line up vertically"
  (interactive "*r")
  (save-excursion
  (let ((indent 0))
    (narrow-to-region start end)
    (beginning-of-buffer)
    (while (not (eobp))
      (if (find-assignment)
	  (progn
	    (exchange-point-and-mark)
	    (setq indent (max indent (current-column)))
	    (delete-horizontal-space)
	    (insert " ")))
      (forward-line 1))
    (beginning-of-buffer)
    (while (not (eobp))
      (if (find-assignment)
	  (indent-to-column (1+ (- indent  (- (mark) (point))))))
      (forward-line 1)))
  (deactivate-mark)
  (widen))
  )

;; toggle-read-only with backup of original copy
(defun my-toggle-read-only ()
  "Change read-only status of current buffer, possibly making backup."
  (interactive)
  (let ((orig (concat (buffer-file-name) ".orig")))
  (if buffer-read-only
      (if (y-or-n-p "Save original copy? ")
	  (progn
	    ;; let rename-file throw an error if .orig file exists
	    (rename-file (buffer-file-name) orig)
	    (set-file-modes orig 292)
	    ;; save contents under original name
	    (toggle-read-only)
	    (set-buffer-modified-p t)
	    (basic-save-buffer))
	(toggle-read-only))
    ;; else discard changes and revert to original
    (if (and (file-exists-p orig)
	     (yes-or-no-p (format "Discard changes to %s? " (buffer-name))))
	(progn
	  (set-file-modes (buffer-file-name) 511)
	  (delete-file (buffer-file-name))
	  (rename-file orig (buffer-file-name))
	  (set-file-modes (buffer-file-name) 292)
	  ;; revert-buffer should set buffer-read-only
	  (revert-buffer t t))
      (toggle-read-only)))))


(defun my-trace (msg)
  "just prints the message to the mini buffer"
  (print msg))


(defun put-file-in-register (register path)
  (set-register register `(file . ,path)))


(defun sum-column (start end)
    "Adds numbers in a rectangle"
    (interactive "r")
    (copy-rectangle-to-register 9 start end)
    (set-buffer (get-buffer-create "*calc-sum*"))
    (erase-buffer)
    (insert-register 9)
    (let ((sum 0))
      (while (re-search-forward "[0-9]*\\.?[0-9]+" nil t)
        (setq sum (+ sum (string-to-number (match-string 0)))))
      (message "Sum: %f" sum)))
;;;; from www.cabochon.com/~stevey/blog-rants/my-dot-emacs-file.html
;;
;;  Never understood why Emacs doesn't have this function.
;;
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn
	  (rename-file name new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil))))))

;;
;;  Never understood why Emacs doesn't have this function, either.
;;
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	  (if (string-match dir "\\(?:/\\|\\\\)$")
	      (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (progn
	(copy-file filename newname 1)
	(delete-file filename)
	(set-visited-file-name newname)
	(set-buffer-modified-p nil)
	t))))

(defun use-curl-c-style-p ()
  "Set curl indentation style when visiting curl related files"
  (interactive)
  (let ((filename (buffer-file-name)))
	(if (string-match "\\(thirdparty/c\\-ares\\)\\|\\(thirdparty/curl\\)" filename)
		(progn
		  (message "curl style" filename)
		  t)
	)))

;; (defun kill-matching-buffers ()
;;   "Kill all buffers whos name matches the input"
;;   (interactive)

;;   (let ((count 0) (regexp (read-string (format "kill buffers matching: "))))
;; 	(dolist(buffer (buffer-list))
;; 	  (when (string-match regexp (buffer-name buffer))
;; 		(kill-buffer buffer)
;; 		(setq count (1+ count))
;; 		(kill-buffer buffer)))
;; 	(message "Killed %i buffer(s)." count))
;; )


(defun act-on-buffers (pred action)
  (let ((count 0))
    (dolist(buffer (buffer-list))
      (when (funcall pred buffer)
        (setq count (1+ count))
        (funcall action buffer)))
    count)
)

(defun kill-matching-buffers-by (accessor)
 "Kill buffers whose name matches the input"
  (let* (
		 (regexp (read-string (format "kill buffers matching: ")))
		 (match #'(lambda (buffer) (string-match regexp (funcall accessor buffer))))
		 (kill #'(lambda (buffer) (kill-buffer buffer)))
		 (count (act-on-buffers match kill))
		)

    (message "%d buffer(s) killed" count))
  )

(defun kill-matching-buffers-by-name ()
   (interactive)
   (kill-matching-buffers-by 'buffer-name)
)


(defun kill-matching-buffers-by-filename ()
   (interactive)
   (kill-matching-buffers-by #'(lambda (b) (or (buffer-file-name b) "")))
)

; from http://20y.hu/20080603/unique-lines-in-an-emacs-buffer.html
(defun uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))

; adopted from http://stackoverflow.com/questions/1631807/decoding-html-entities-in-emacs-elisp

(defun insert-encode-entities-string (str)
  (mapconcat
   (lambda (char)
	 (if (or (> char 127)
			 (= char 39))
		 (format "&#%d;" char)
	   (format "%c" char)))
   (string-to-list str)
   ""))

(defun to-html-entities (beg end)
  "replace chars > 127 to their decimal html entities represntation"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
	  (let ((translated (insert-encode-entities-string (filter-buffer-substring beg end))))
		;(message "%s" translated)
		(kill-region beg end)
		(insert-for-yank translated)
	  ))))

;;; from http://emacswiki.org/emacs/AsciiTable
(defun ascii-table ()
    "Display basic ASCII table (0 thru 128)."
    (interactive)
    (setq buffer-read-only nil)        ;; Not need to edit the content, just read mode (added)
    (local-set-key "q" 'bury-buffer)   ;; Nice to have the option to bury the buffer (added)
    (switch-to-buffer "*ASCII*")
    (erase-buffer)
    (save-excursion (let ((i -1))
      (insert "ASCII characters 0 thru 127.\n\n")
      (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
      (while (< i 31)
        (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                        (setq i (+ 1  i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)))
        (setq i (- i 96))))))

; from http://stackoverflow.com/questions/611831/how-to-url-decode-a-string-in-emacs-lisp
(defun func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

(defun date (&optional insert)
  "Display the current date and time.
  With a prefix arg, INSERT it into the buffer."
  (interactive "P")
  (funcall (if insert 'insert 'message)
           (format-time-string "%a, %d %b %Y %T %Z" (current-time))))

; from http://trey-jackson.blogspot.co.il/2009/06/emacs-tip-30-igrep.html
(defun my-shorten-filenames-in-compilation (buffer &optional stat)
  "remove the absolute filenames if they match the default-directory"
  (interactive "b")
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let ((buffer-read-only nil)
          (base-dir (if (re-search-forward "find \\([^ ]+\\) " nil t)
                        (if (string-match "/$" (match-string 1))
                            (match-string 1)
                          (concat (match-string 1) "/"))
                      default-directory)))
      (setq default-directory base-dir)
      (while (re-search-forward (concat "^" default-directory) nil t)
        (replace-match "")))))

(defun query-replace-in-open-buffers (arg1 arg2)
  "query-replace in open files"
  (interactive "sQuery Replace in open Buffers: \nsquery with: ")
  (mapcar
   (lambda (x)
     (find-file x)
     (save-excursion
       (beginning-of-buffer)
       (query-replace arg1 arg2)))
   (delq
    nil
    (mapcar
     (lambda (x)
       (buffer-file-name x))
     (buffer-list)))))

; from http://sachachua.com/wp/2008/07/27/emacs-keyboard-shortcuts-for-navigating-code/
(defun sacha/search-word-backward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (goto-char
     (if (re-search-backward (concat "\\_<" (current-word) "\\_>") nil t)
		 (match-beginning 0)
       cur))))

(defun sacha/search-word-forward ()
  "Find the next occurrance of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (goto-char
     (if (re-search-forward (concat "\\_<" (current-word) "\\_>") nil t)
		 (match-beginning 0)
       cur))))

; from http://www.emacswiki.org/emacs/FindingNonAsciiCharacters
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(defun find-next-unsafe-char (&optional coding-system)
  "Find the next character in the buffer that cannot be encoded by
coding-system. If coding-system is unspecified, default to the coding
system that would be used to save this buffer. With prefix argument,
prompt the user for a coding system."
  (interactive "Zcoding-system: ")
  (if (stringp coding-system) (setq coding-system (intern coding-system)))
  (if coding-system nil
    (setq coding-system
          (or save-buffer-coding-system buffer-file-coding-system)))
  (let ((found nil) (char nil) (csets nil) (safe nil))
    (setq safe (coding-system-get coding-system 'safe-chars))
    ;; some systems merely specify the charsets as ones they can encode:
    (setq csets (coding-system-get coding-system 'safe-charsets))
    (save-excursion
      ;;(message "zoom to <")
      (let ((end  (point-max))
            (here (point    ))
            (char  nil))
        (while (and (< here end) (not found))
          (setq char (char-after here))
          (if (or (eq safe t)
                  (< char ?\177)
                  (and safe  (aref safe char))
                  (and csets (memq (char-charset char) csets)))
              nil ;; safe char, noop
            (setq found (cons here char)))
          (setq here (1+ here))) ))
    (and found (goto-char (1+ (car found))))
    found))
(put 'upcase-region 'disabled nil)

; ;; from http://emacswiki.org/emacs/IndentingC
 (defun c-reformat-buffer()
   (interactive)
   (save-buffer)
   (setq sh-indent-command (concat
                            "indent "
                            "-bad -brf -brs -nfbs -ce -i2 -npsl -nut -cli1 -di1 "
                            " "
                           buffer-file-name
                           )
        )
  (mark-whole-buffer)
  (universal-argument)
  (shell-command-on-region
   (point-min)
   (point-max)
   sh-indent-command
   (buffer-name)
   )
  (save-buffer)
  )

;; http://www.emacswiki.org/emacs/SwitchingBuffers#toc5
(defun switch-to-previous-buffer ()
  "Switch to most recent buffer. Repeated calls toggle back and forth between the most recent two buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


;; http://paste.lisp.org/display/304865
(defmacro th/define-context-key (keymap key dispatch)
  "Define KEY in KEYMAP to execute according to DISPATCH.

DISPATCH is a form that is evaluated and should return the
command to be executed.

If DISPATCH returns nil, then the command normally bound to KEY
will be executed.

Example:

  (th/define-context-key hs-minor-mode-map
     (kbd \"<C-tab>\")
     (cond
      ((not (hs-already-hidden-p))
       'hs-hide-block)
      ((hs-already-hidden-p)
       'hs-show-block)))

This will make <C-tab> show a hidden block.  If the block is
shown, then it'll be hidden."
  `(define-key ,keymap ,key
     `(menu-item "context-key" ignore
		 :filter ,(lambda (&optional ignored)
			    ,dispatch))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Some usages                                                         ;;
;;                                                                        ;;
;; (th/define-context-key outline-minor-mode-map                          ;;
;; 		       (kbd "TAB")                                                  ;;
;; 		       (when (th/outline-context-p)                                 ;;
;; 			 'org-cycle))                                                     ;;
;;                                                                        ;;
;; ;; Jump out of a TeX macro when pressing TAB twice.                    ;;
;; (th/define-context-key TeX-mode-map (kbd "TAB")                        ;;
;; 	 (when (and (= 1 (length (this-command-keys-vector)))                 ;;
;; 		    (equal last-command-event (elt (this-command-keys-vector) 0))   ;;
;; 		    (TeX-current-macro))                                            ;;
;; 	   #'th/TeX-goto-macro-end)))                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; from https://www.emacswiki.org/emacs/CopyWithoutSelection#toc4
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )

(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )


;; from https://stackoverflow.com/a/18814469
(defun copy-buffer-file-name-as-kill (choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (f) Full, (d) Directory, (n) Name")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

;; indents a script element
(fset 'shmul/indent-script-element
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("</scrip\234" 0 "%d")) arg)))

;; from https://stackoverflow.com/a/22381730
(defun chris2-toggle-case ()
  (interactive)
  (let ((char (following-char)))
    (if (eq char (upcase char))
        (insert-char (downcase char) 1 t)
      (insert-char (upcase char) 1 t)))
  (delete-char 1 nil)
  (backward-char))



;; from https://www.emacswiki.org/emacs/CompileCommand#toc5
(require 'cl) ; If you don't have it already

(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (expand-file-name file
		              (loop
			           for d = default-directory then (expand-file-name ".." d)
			           if (file-exists-p (expand-file-name file d))
			           return d
			           if (equal d root)
			           return nil))))


;; from https://emacs.stackexchange.com/a/46517
(defun copy-diff-region ()
  "Copy diff region without + or - markers."
  (interactive)
  (deactivate-mark)
  (let ((text (buffer-substring-no-properties
               (region-beginning) (region-end))))
    (kill-new (replace-regexp-in-string "^[\\+\\-]" "" text))))

;; https://www.jayconrod.com/posts/67/emacs-run-gitblame-on-the-current-line
(defun git-blame-line ()
  "Runs `git blame` on the current line and
   adds the commit id to the kill ring"
  (interactive)
  (let* ((line-number (save-excursion
                        (goto-char (point-at-bol))
                        (+ 1 (count-lines 1 (point)))))
         (line-arg (format "%d,%d" line-number line-number))
         (commit-buf (generate-new-buffer "*git-blame-line-commit*")))
    (call-process "git" nil commit-buf nil
                  "blame" (buffer-file-name) "-L" line-arg)
    (let* ((commit-id (with-current-buffer commit-buf
                        (buffer-substring 1 9)))
           (log-buf (generate-new-buffer "*git-blame-line-log*")))
      (kill-new commit-id)
      (call-process "git" nil log-buf nil
                    "log" "-1" "--pretty=%h   %an   %s" commit-id)
      (with-current-buffer log-buf
        (message "Line %d: %s" line-number (buffer-string)))
      (kill-buffer log-buf))
    (kill-buffer commit-buf)))


;; from https://www.reddit.com/r/emacs/comments/l51ocx/what_is_the_most_useful_part_of_your_emacs_config/gkrz1di?utm_source=share&utm_medium=web2x&context=3
;;; C-a move-beginning-of-line-or-indentation
(defun shmul/at-or-before-indentation-p ()
  (save-excursion
    (let ((old-point (point)))
      (back-to-indentation)
      (<= old-point (point)))))
(defun shmul/move-beginning-of-line-or-indentation () (interactive)
       "If at the begining of line go to previous line.
 If at the indention go to begining of line.
 Go to indention otherwise."
       (cond ((bolp) (forward-line -1))
             ((shmul/at-or-before-indentation-p) (move-beginning-of-line nil))
             (t (back-to-indentation))))

;;; C-e move-end-of-line-or-indentation
(defun shmul/at-or-after-indentation-p ()
  (save-excursion
    (let ((old-point (point)))
      (back-to-indentation)
      (>= old-point (point)))))
(defun shmul/move-end-of-line-or-indentation () (interactive)
       "If at end of line go to next line.
If at indentation go to end of line.
Go to indentation otherwise"
       (cond ((eolp) (forward-line 1))
             ((shmul/at-or-after-indentation-p) (move-end-of-line nil))
             (t (back-to-indentation))))

;; https://zck.org/emacs-move-file
(defun move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (message "old file is %s and new file is %s"
             old-location
             new-location)
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))
