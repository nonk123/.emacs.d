(require 'treemacs)
(require 'all-the-icons)
(require 'treemacs-all-the-icons)
(require 'elcord)
(require 'centaur-tabs)

(defvar nonk/theme-set nil)

(setq elcord--editor-name "GNU/Emacs")
(setq elcord-idle-message "Jacking it, perhaps?")
(setq elcord-display-elapsed nil)

(setq centaur-tabs-style "box")
(setq centaur-tabs-height 32)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-icon-type 'all-the-icons)
(setq centaur-tabs-gray-out-icons 'buffer)
(setq centaur-tabs-set-bar 'under)
(setq x-underline-at-descent-line t)
(setq centaur-tabs-set-close-button nil)
(setq centaur-tabs-set-modified-marker t)

(centaur-tabs-mode 1)
(add-hook 'dired-mode-hook #'centaur-tabs-local-mode)
(add-hook 'magit-mode-hook #'centaur-tabs-local-mode)

(bind-keys ("M-n" . centaur-tabs-forward)
	   ("M-p" . centaur-tabs-backward))

(defun nonk/window-disable-fringes (&optional window)
  (interactive)
  (set-window-fringes (or window (selected-window)) 0 0 0 t))

(defun nonk/minibuffer-disable-fringes (&optional frame)
  (interactive)
  (nonk/window-disable-fringes (minibuffer-window frame)))

(defun nonk/apply-theming (&optional force)
  (interactive "P")
  (when (and (or force (not nonk/theme-set)))
    (let ((font-name "Monaspace Neon"))
      (set-face-font 'default (concat font-name "-9")))
    (setq doom-themes-enable-bold t)
    (setq doom-themes-enable-italic t)
    (load-theme 'doom-dark+ t nil)
    (doom-themes-treemacs-config)
    (doom-themes-org-config)
    (nonk/minibuffer-disable-fringes)
    (setq nonk/theme-set t)))

(add-hook 'emacs-startup-hook #'nonk/minibuffer-disable-fringes)

(defun nonk/apply-server-theming (&optional frame)
  (when (display-graphic-p frame)
    (nonk/apply-theming t)
    (elcord-mode 1)))

;; Ensure the correct theme is set even in server mode.
(add-hook 'server-after-make-frame-hook #'nonk/apply-server-theming)
(add-hook 'after-make-frame-functions #'nonk/apply-server-theming)
