(require 'treemacs)
(require 'all-the-icons)
(require 'treemacs-all-the-icons)
(require 'elcord)
(require 'nerd-icons)
(require 'nerd-icons-completion)
(require 'git-gutter)

(defvar nonk/theme-set nil)

(defvar nonk/hide-tabs-list
  '("COMMIT_EDITMSG"))

(global-git-gutter-mode 1)

(setq elcord--editor-name "GNU/Emacs")
(setq elcord-idle-message "Jacking it, perhaps?")
(setq elcord-display-elapsed nil)

(nerd-icons-completion-mode 1)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

(defun nonk/window-disable-fringes (&optional window)
  (interactive)
  (set-window-fringes (or window (selected-window)) 0 0 0 t))

(defun nonk/minibuffer-disable-fringes (&optional frame)
  (interactive)
  (nonk/window-disable-fringes (minibuffer-window frame)))

(defun nonk/apply-theming (&optional force)
  (interactive "P")
  (when (or force (not nonk/theme-set))
    (let ((font-name "Monaspace Neon"))
      (set-face-font 'default (concat font-name "-9")))
    (nonk/minibuffer-disable-fringes)
    (load-theme 'doom-dark+ t nil)
    (doom-themes-treemacs-config)
    (doom-themes-org-config)
    (setq nonk/theme-set t)))

(add-hook 'emacs-startup-hook #'nonk/minibuffer-disable-fringes)

(defun nonk/apply-server-theming (&optional frame)
  (when (display-graphic-p frame)
    (nonk/apply-theming t)
    (elcord-mode 1)))

;; Ensure the correct theme is set even in server mode.
(add-hook 'server-after-make-frame-hook #'nonk/apply-server-theming)
(add-hook 'after-make-frame-functions #'nonk/apply-server-theming)
