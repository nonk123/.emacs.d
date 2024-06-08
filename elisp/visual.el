(require 'treemacs)
(require 'all-the-icons)
(require 'treemacs-all-the-icons)

(defvar nonk/theme-set nil)

(defun nonk/window-disable-fringes (&optional window)
  (interactive)
  (set-window-fringes (or window (selected-window)) 0 0 0 t))

(defun nonk/apply-theming (&optional force)
  (interactive "P")
  (when (and (or force (not nonk/theme-set)))
    ;; Another dumb Windows vs Linux difference...
    (let ((font-name (if nonk/windows?
			 "LiterationMono Nerd Font Mono"
		       "LiterationMono Nerd Font")))
      (set-face-font 'default (concat font-name ":spacing=100:pixelsize=12")))
    (treemacs-load-theme "all-the-icons")
    (setq doom-themes-enable-bold t)
    (setq doom-themes-enable-italic t)
    (load-theme 'doom-dracula t nil)
    (doom-themes-treemacs-config)
    (doom-themes-org-config)
    (setq nonk/theme-set t)))

(defun nonk/minibuffer-disable-fringes (&optional frame)
  (interactive)
  (nonk/window-disable-fringes (minibuffer-window frame)))

(add-hook 'emacs-startup-hook #'nonk/minibuffer-disable-fringes)
(add-hook 'after-make-frame-functions #'nonk/minibuffer-disable-fringes)

(defun nonk/apply-server-theming (&optional frame)
  (when (display-graphic-p frame)
    (nonk/apply-theming t)))

;; Ensure the correct theme is set even in server mode.
(add-hook 'server-after-make-frame-hook #'nonk/apply-server-theming)
(add-hook 'after-make-frame-functions #'nonk/apply-server-theming)
