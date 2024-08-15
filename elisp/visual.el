(require 'treemacs)
(require 'all-the-icons)
(require 'treemacs-all-the-icons)
(require 'elcord)
(require 'centaur-tabs)
(require 'doom-modeline)
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

(setq centaur-tabs-style "box")
(setq centaur-tabs-height 32)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-icon-type 'nerd-icons)
(setq centaur-tabs-gray-out-icons 'buffer)
(setq centaur-tabs-set-bar 'under)
(setq centaur-tabs-set-close-button nil)
(setq centaur-tabs-show-new-tab-button nil)
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-adjust-buffer-order 'left)
(setq centaur-tabs-cycle-scope 'tabs)

(setq doom-modeline-height 22)
(setq doom-modeline-bar-width 4)
(setq doom-modeline-window-width-limit 320)

(nerd-icons-completion-mode 1)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

(defvar nonk/centaur-tabs-project-timer (run-at-time 5 3 #'nonk/centaur-tabs-project-only))
(add-hook 'window-buffer-change-functions #'nonk/centaur-tabs-project-only--wbcf 100)

(defun nonk/centaur-tabs-project-only--wbcf (frame)
  "Run `nonk/centaur-tabs-project-only' in all of FRAME's windows."
  (when centaur-tabs-mode
    (dolist (window (window-list frame 'never nil))
      (nonk/centaur-tabs-project-only (window-buffer window)))))

(defun nonk/centaur-tabs-project-only (&optional buffer)
  "Disable `centaur-tabs' for BUFFER unless it's bound to a project."
  (interactive "i")
  (setq buffer (or buffer (window-buffer)))
  (with-current-buffer buffer
    (centaur-tabs-local-mode
     (if (and (not (member (buffer-name buffer) nonk/hide-tabs-list))
              (buffer-file-name buffer)
              (project-current nil))
         -1 1))))

(bind-keys :map centaur-tabs-mode-map
           ("M-]" . centaur-tabs-forward)
           ("M-[" . centaur-tabs-backward))

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
    (doom-modeline-mode 1)
    (centaur-tabs-mode 1)
    (centaur-tabs-enable-buffer-reordering)
    (setq nonk/theme-set t)))

(add-hook 'emacs-startup-hook #'nonk/minibuffer-disable-fringes)

(defun nonk/apply-server-theming (&optional frame)
  (when (display-graphic-p frame)
    (nonk/apply-theming t)
    (elcord-mode 1)))

;; Ensure the correct theme is set even in server mode.
(add-hook 'server-after-make-frame-hook #'nonk/apply-server-theming)
(add-hook 'after-make-frame-functions #'nonk/apply-server-theming)
