(defconst nonk/left-fringe 18)

(require 'diminish)
(require 'wakatime-mode)
(require 'editorconfig)
(require 'marginalia)
(require 'vertico)
(require 'savehist)
(require 'yasnippet)
(require 'flycheck)
(require 'company)
(require 'projectile)
(require 'web-mode)
(require 'eldoc-box)

(add-to-list 'auto-mode-alist '("LICENSE\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml\\(?:\\..+\\)?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml\\.cs\\'" . csharp-mode))

(diminish 'smerge-mode)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(fringe-mode (cons nonk/left-fringe 0))
(setq auto-hscroll-mode 'curent-line)

(global-eldoc-mode 1)

(defun nonk/eldoc-box--disable-line-numbers (orig-fun buffer)
  (let* ((frame (funcall orig-fun buffer))
	 (window (frame-root-window frame))
	 (buffer (window-buffer window)))
    (with-current-buffer buffer
      (display-line-numbers-mode -1))
    frame))
(advice-add #'eldoc-box--get-frame :around #'nonk/eldoc-box--disable-line-numbers)
(global-display-line-numbers-mode 1)

(global-hl-line-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(electric-pair-mode 1)
(electric-indent-mode 1)

(marginalia-mode 1)
(vertico-mode 1)
(savehist-mode 1)

(yas-global-mode 1)
(global-flycheck-mode 1)
(projectile-mode 1)
(global-company-mode 1)

(global-wakatime-mode 1)
(editorconfig-mode 1)
