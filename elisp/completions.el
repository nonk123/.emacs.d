(require 'company)
(require 'yasnippet)
(require 'consult)
(require 'embark)
(require 'cape)

(setq completion-styles '(partial-completion basic orderless))
(setq completion-category-defaults nil)

(setq ispell-alternate-dictionary "/usr/share/dict/words")

(setq company-backends nil)
(setq company-frontends nil)

(setq-default completion-at-point-functions (list #'cape-file #'cape-keyword))

(defun nonk/wrap-elisp-capf (orig)
  (cape-wrap-nonexclusive orig))
(advice-add #'elisp-completion-at-point :around #'nonk/wrap-elisp-capf)

(setq yas-fallback-behavior 'return-nil)
(setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))

(bind-keys :map yas-minor-mode-map ("<tab>" . nil) ("TAB" . nil))

(defun nonk/try-yas-then-cap ()
  (interactive)
  (unless (yas-expand)
    (completion-at-point)))

(bind-keys ("C-'" . nonk/try-yas-then-cap)
	   :map company-mode-map
	   ("C-'" . nonk/try-yas-then-cap))

(bind-keys :map company-active-map
	   ("C-p" . nil)
	   ("C-n" . nil))

(setq completion-in-region-function #'consult-completion-in-region)

(setq prefix-help-command #'embark-prefix-help-command)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

(bind-keys ("C-z" . consult-line)
	   ("C-c i" . consult-imenu-multi)
	   ("C-y" . consult-yank-from-kill-ring)
	   ("M-y" . yank)
	   ([remap switch-to-buffer] . consult-buffer)
	   ("M-." . xref-find-definitions))
