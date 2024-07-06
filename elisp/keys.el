(require 'ace-window)

(defun nonk/comment-whole-line ()
  (interactive)
  (comment-line 1))

(defun nonk/find-file-sudo (filename)
  (interactive "Fsudo find file: ")
  (find-file (concat "/sudo::" filename)))

(bind-keys ("C-x M-f" . nonk/find-file-sudo))

(bind-keys ("M-*" . replace-regexp)
	   ("C-;" . nonk/comment-whole-line))

(bind-keys ("M-o" . ace-window)
	   ("C-x \"" . split-window-below)
	   ("C-x %" . split-window-right)
	   ("C-x M-o" . window-swap-states))

(bind-keys ("C-c m" . man-follow))
