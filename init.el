;; -*- lexical-binding: t; -*-

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'diminish)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(delete-selection-mode 1)

(electric-indent-mode 1)
(electric-pair-mode 1)

(use-package editorconfig
  :diminish
  :init (editorconfig-mode 1))

(use-package magit
  :bind ("C-x g" . magit))

(use-package projectile
  :diminish
  :init (projectile-mode 1)
  :custom
  (projectile-project-search-path '(("~/Sources" . 1)))
  (projectile-auto-cleanup-known-projects t)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package savehist
  :init (savehist-mode 1))

(use-package vertico
  :custom (vertico-cycle t)
  :init (vertico-mode 1))

(use-package consult
  :custom (completion-in-region-function #'consult-completion-in-region)
  :bind ("M-y" . consult-yank-pop))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package orderless
  :custom (completion-styles '(orderless basic)))

(use-package flycheck
  :init (global-flycheck-mode 1)
  :bind (:map flycheck-mode-map
	      ("C-c C-n" . flycheck-next-error)
	      ("C-c C-p" . flycheck-previous-error)))

(use-package lsp-mode
  :custom (lsp-keymap-prefix "C-c l")
  :hook ((c-mode c++-mode) . lsp))

(use-package lsp-ui
  :diminish
  :init (lsp-ui-mode 1)
  :custom (lsp-headerline--enable-breadcrumb nil))

(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook (lsp-mode . indent-bars-mode)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-starting-column 0)
  (indent-bars-prefer-character t)
  (indent-bars-no-stipple-char ?|)
  (indent-bars-treesit-ignore-blank-lines-types '("module")))

(use-package eldoc
  :diminish
  :init (global-eldoc-mode 1))

(use-package emacs
  :diminish abbrev-mode auto-revert-mode
  :bind ("C-'" . completion-at-point)
  :custom
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))
