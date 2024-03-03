(require 'flycheck)

(setq eldoc-documentation-strategy #'eldoc-documentation-compose)

(bind-keys :map flycheck-mode-map
	   ("C-c C-n" . flycheck-next-error)
	   ("C-c C-p" . flycheck-previous-error))
