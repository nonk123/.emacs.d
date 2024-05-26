(defun nonk/diminish-things ()
  (require 'diminish)
  (diminish 'eldoc-mode)
  (diminish 'yas-minor-mode)
  (diminish 'projectile-mode)
  (diminish 'abbrev-mode)
  (diminish 'company-mode)
  (diminish 'aggressive-indent-mode)
  (diminish 'wakatime-mode)
  (diminish 'editorconfig-mode))

(add-hook 'after-init-hook #'nonk/diminish-things)

(unless (file-exists-p custom-file)
  (with-temp-buffer
    (write-file custom-file)))

(load-file custom-file)
