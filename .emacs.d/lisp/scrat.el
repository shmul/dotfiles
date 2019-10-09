;;; scrat.el --- access *scratch* like you do *shell*

;; Copyright (C) 2004 Joe Corneli <jcorneli@math.utexas.edu>

;; Time-stamp: <jac -- Mon Nov  8 01:40:19 CST 2004>

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file provides a function `scratch' that is similar to `shell',
;; `info', `grep', and other such functions.  By this I mean M-x
;; scratch takes you to the *scratch* buffer if one exists, otherwise
;; it generates a new *scratch* buffer.  User-defined customizations
;; along the following lines allow you to choose a new name for the
;; scratch buffer, or to use a different major mode, or to add minor
;; modes:
;;
;; (defun itch () (interactive)
;;   (scratch "*itch*"))
;;
;; (defun notes () (interactive)
;;   (scratch "*notes*" 'text-mode))
;;
;; (defun music () (interactive)
;;   (scratch "*music*" 'fundamental-mode
;;                      'musical-letters-mode
;;                      'auto-fill-mode))
;;
;; The function `back-from-scratch' is also provided, to take you
;; back from your most recent scratch buffer to the previous buffer
;; (restoring the window configuration).
;;
;; For those who are interested primarily in the typical *scratch*
;; buffer the documentation of `lisp-interaction-mode' should be
;; required reading.

;;; Notes:

;; There is not any explicit type checking in `scratch', so if you
;; suspect that someone might pass something other than modes in for
;; `initmode' and/or `minormodes', well, you had better hope that they
;; know what they are doing.  We could add tests similar to the
;; `with-temp-buffer' test we already use to detect whether these
;; things that are being passed in are actually functions that set up
;; modes, but that seems like overkill at this point.

;; The `back-from-scratch' function should perhaps access a
;; stack-like data structure so that you can call `scratch' multiple
;; times with different options in succession and still just use this
;; one function (iteratively) to get back to where you started.

;; Also, the *scratch* buffer (or whatever buffer is accessed) should
;; _not_ be added to the buffer history ring (at least, this should
;; be the default).

;; And it would be cool to prompt saving when the *scratch* buffer is
;; killed.  (This was just being talked about on help-gnu-emacs.)

;;; History:

;; Earlier this year, I encouraged the Emacs developers to make
;; `info' more like `shell', and they did, which I appreciated.  This
;; mode is inspired by this new more consistent interface.

;;; Code:

(defvar pre-scratch-buffer nil
"Name of active buffer before switching to the scratch buffer.")

(defvar pre-scratch-config nil
"Window configuration before switching to the scratch buffer.")

(defun scratch (&optional buffername initmode &rest minormodes)
  "Access an existing scratch buffer or create one anew if none exists.
This is similar to the function `shell' for accessing or creating
an interactive shell.

Optional argument BUFFERNAME specifies the scratch buffer name;
the default buffer name is *scratch*.

Optional argument INITMODE specifies the major mode in which
scratch buffer should be initialized; the default mode is
`initial-major-mode'.

Any remaining arguments are MINORMODES to apply in the scratch
buffer."
  (interactive)
  (pre-scratch-setup)
  (let* ((name (if buffername
                   buffername
                 "*scratch*"))
         (live (buffer-live-p (get-buffer name)))
         (buffer (get-buffer-create name)))
    (pop-to-buffer buffer)
    ;; switch to `lisp-interaction-mode' (or whatever the initial
    ;; major mode is) if the buffer is in `fundamental-mode' (or
    ;; whatever the default major mode is)...
    (if (equal mode-name (with-temp-buffer (funcall major-mode) ;; shmul changed this from default-major-mode
                                           mode-name))
        ;; but only if the buffer has not been fiddled with
        (unless live
          (funcall (if initmode
                       initmode
                     initial-major-mode))
          ;; also take the opportunity to set up any minor modes
          ;; that were requested
          (mapcar 'funcall minormodes)))
    buffer))

(defun pre-scratch-setup ()
  (setq pre-scratch-buffer (buffer-name (current-buffer)))
  (setq pre-scratch-config (current-window-configuration)))

(defun back-from-scratch ()
  "Used after running `scratch' to restore the window
configuration to the state it was in beforehand."
  (interactive)
  (if (not (get-buffer pre-scratch-buffer))
      (message "Buffer %s no longer exists!" pre-scratch-buffer)
    (set-window-configuration pre-scratch-config)))

;;; Example Configuration:

;; (Comment this out if you don't like it!)

(defun messages () (interactive)
  (scratch "*Messages*" 'fundamental-mode)
  (goto-char (point-max))
  (recenter))

;; better than `shell' because you can jump back quickly.
(defun shel () (interactive)
  (if (buffer-live-p (get-buffer "*shell*"))
      (scratch "*shell*")
    (pre-scratch-setup)
    (shell)))

(defun posting () (interactive)
  (scratch "*Posting*" 'text-mode 'auto-fill-mode)
  (write-file "~/.posting-spool")
  (delete-other-windows)
  (set-fill-column 142))

(global-set-key "\C-cs" (make-sparse-keymap))
(global-set-key "\C-csm" 'messages)
(global-set-key "\C-css" 'scratch)
(global-set-key "\C-csp" 'posting)
(global-set-key "\C-csb" 'back-from-scratch)

(provide 'scrat)
;;; scrat.el ends here
