(require 'projectile)
(require 'magit)

(setq projectile-auto-discover t)
(setq projectile-file-exists-remote-cache-expire nil)
(setq projectile-indexing-method (if nonk/windows? 'hybrid 'alien))
(setq projectile-project-search-path `((,(concat nonk/home "Sources") . 1)))

(bind-key "C-c p" projectile-command-map)

(setq vc-follow-symlinks t)

(defun nonk/magit-here ()
  (interactive)
  (if-let ((root (projectile-project-root)))
      (magit-status root)
    (user-error "You're not inside a project yet")))

(bind-keys ("C-c g" . nonk/magit-here))
