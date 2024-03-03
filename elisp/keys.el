(require 'ace-window)

(bind-keys ("M-o" . ace-window)
	   ("C-x \"" . split-window-below)
	   ("C-x %" . split-window-right)
	   ("C-x M-o" . window-swap-states))

(bind-keys ("M-n" . scroll-up-line)
	   ("M-p" . scroll-down-line))

(bind-keys ("C-c m" . man-follow))
