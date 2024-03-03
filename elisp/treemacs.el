(require 'treemacs)
(require 'projectile)
(require 'treemacs-icons-dired)
(require 'treemacs-projectile)
(require 'treemacs-magit)

(setq treemacs-is-never-other-window t)
(add-hook 'treemacs-mode-hook #'nonk/window-disable-fringes)

(defun nonk/treemacs-toggle-sidebar (&optional arg)
  "Toggle the treemacs sidebar.

Toggle if ARG is nil; enable if ARG is a positive number; disable otherwise."
  (interactive)
  (let* ((was-visible (eq (treemacs-current-visibility) 'visible))
	 (did-exist (or was-visible (eq (treemacs-current-visibility) 'exists))))
    (cond
     ((not arg)
      (if did-exist
	  (with-current-buffer (treemacs-get-local-buffer)
  	    (kill-buffer))
	(treemacs--init (projectile-project-root)
 			(projectile-project-name))
	(nonk/treemacs-toggle-focus -1)))
     ((and arg (> arg 0) (not was-visible))
      (nonk/treemacs-toggle-sidebar))
     ((and arg (not did-exist))
      (nonk/treemacs-toggle-sidebar)))))

(defun nonk/treemacs-toggle-focus (&optional arg)
  "Toggle the treemacs sidebar focus.

Toggle if ARG is nil; focus if ARG is a positive number; unfocus otherwise."
  (interactive)
  (let ((was-visible (eq (treemacs-current-visibility) 'visible)))
    (cond
     ((not arg)
      (if (not was-visible)
	  (progn
	    (nonk/treemacs-toggle-sidebar 1)
	    (treemacs--select-visible-window))
	(if (treemacs-is-treemacs-window-selected?)
	    (select-window (get-mru-window))
	  (treemacs--select-visible-window))))
     ((and arg (> arg 0) (not was-visible))
      (nonk/treemacs-toggle-focus))
     (was-visible
      (nonk/treemacs-toggle-focus)))))

(bind-keys ("C-c t" . nonk/treemacs-toggle-focus)
           ("C-c C-t" . nonk/treemacs-toggle-sidebar))

(treemacs-icons-dired-mode 1)
(treemacs-follow-mode -1)
(treemacs-git-mode 'deferred)
