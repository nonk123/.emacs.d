(require 'ace-window)

(defun nonk/comment-whole-line ()
  "Comment the entire current line."
  (interactive)
  (comment-line 1))

(defun nonk/find-file-sudo (filename)
  "Find FILENAME prepending the sudo protocol to it."
  (interactive "Fsudo find file: ")
  (find-file (concat "/sudo::" filename)))

(defun nonk/read-minor-mode (&optional enabled-only)
  "Read a minor-mode from the minibuffer.

If ENABLED-ONLY is not nil, display only minor-modes enabled in the current buffer."
  (interactive "i")
  (let ((x (completing-read
	    "Minor mode: "
	    (mapcar #'car minor-mode-alist)
	    (and enabled-only #'symbol-value))))
    (and x (intern x))))

(defun nonk/disable-mode-globally (mode)
  "Disable minor MODE in all buffers."
  (interactive (list (nonk/read-minor-mode)))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (funcall mode -1))))

(bind-keys ("C-x M-f" . nonk/find-file-sudo))

(bind-keys ("M-*" . replace-regexp)
	   ("C-;" . nonk/comment-whole-line))

(bind-keys ("M-o" . ace-window)
	   ("C-x \"" . split-window-below)
	   ("C-x %" . split-window-right)
	   ("C-x M-o" . window-swap-states))

(bind-keys ("C-c m" . man-follow))
