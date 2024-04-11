(require 'ace-window)

(defun nonk/comment-whole-line ()
  (interactive)
  (comment-line 1))

(bind-keys ("M-*" . replace-regexp)
 	   ("C-;" . nonk/comment-whole-line))

(bind-keys ("M-o" . ace-window)
	   ("C-x \"" . split-window-below)
	   ("C-x %" . split-window-right)
	   ("C-x M-o" . window-swap-states))

(bind-keys ("M-n" . scroll-up-line)
	   ("M-p" . scroll-down-line))

(bind-keys ("C-c m" . man-follow))
