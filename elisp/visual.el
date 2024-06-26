(require 'treemacs)
(require 'all-the-icons)
(require 'treemacs-all-the-icons)
(require 'elcord)

(defvar nonk/theme-set nil)

(setq elcord--editor-name "GNU/Emacs")
(setq elcord-idle-message "Jacking it, perhaps?")
(setq elcord-display-elapsed nil)

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
