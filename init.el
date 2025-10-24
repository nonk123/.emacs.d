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
(global-display-line-numbers-mode 1)
(column-number-mode 1)

(delete-selection-mode 1)
(setq kill-whole-line t)

;; Thanks <https://justine.lol/sectorlisp2>.
(unless standard-display-table
  (setq standard-display-table (make-display-table)))
(aset standard-display-table #x2028 [?↵]) ;; LINE SEPARATOR
(aset standard-display-table #x2029 [?¶]) ;; PARAGRAPH SEPARATOR
(aset standard-display-table #x202A [?⟫]) ;; LEFT-TO-RIGHT EMBEDDING
(aset standard-display-table #x202B [?⟪]) ;; RIGHT-TO-LEFT EMBEDDING
(aset standard-display-table #x202D [?❯]) ;; LEFT-TO-RIGHT OVERRIDE
(aset standard-display-table #x202E [?❮]) ;; RIGHT-TO-LEFT OVERRIDE
(aset standard-display-table #x2066 [?⟩]) ;; LEFT-TO-RIGHT ISOLATE
(aset standard-display-table #x2067 [?⟨]) ;; RIGHT-TO-LEFT ISOLATE
(aset standard-display-table #x2068 [?⧽]) ;; FIRST STRONG ISOLATE
(aset standard-display-table #x202C [?⇮]) ;; POP DIRECTIONAL FORMATTING
(aset standard-display-table #x2069 [?⇯]) ;; POP DIRECTIONAL ISOLATE
(aset standard-display-table 9 [?» ?  ?  ?  ?  ?  ?  ? ]) ;; tabs...

(use-package smartparens
  :diminish
  :bind
  ([remap forward-sexp] . sp-forward-sexp)
  ([remap backward-sexp] . sp-backward-sexp)
  ([remap transpose-sexp] . sp-transpose-sexp)
  :init
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1))

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
  :hook ((c-mode c++-mode rust-mode) . lsp))

(use-package lsp-ui
  :diminish
  :init (lsp-ui-mode 1)
  :custom (lsp-headerline-breadcrumb-enable nil))

(use-package indent-bars
  :disabled
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook (lsp-mode . indent-bars-mode)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-starting-column 0)
  (indent-bars-prefer-character t)
  (indent-bars-no-stipple-char ?|)
  (indent-bars-treesit-ignore-blank-lines-types '("module")))

(use-package dtrt-indent
  :diminish
  :init (dtrt-indent-global-mode 1))

(use-package ef-themes
  :custom (modus-themes-italic-constructs t)
  :init (load-theme 'ef-autumn t))

(use-package rust-mode)

(use-package eldoc :diminish
  :init (global-eldoc-mode 1))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package wdired
  :custom (wdired-allow-to-change-permissions t))

(use-package emacs
  :hook (after-save . nonk/format-on-save)
  :bind ("C-'" . completion-at-point)
  :custom
  (user-full-name "Sergey Sudakov")
  (user-mail-address "me@nonk.dev")
  (auto-save-interval 0)
  (make-backup-files nil)
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(defun nonk/format-on-save ()
  (interactive)
  (when lsp-mode
    (ignore-error lsp-capability-not-supported
      (lsp-format-buffer))))

(use-package gnus
  :preface (setq self-hosted-email-addrs '("me@nonk.dev"))
  :custom
  (gnus-select-method '(nnnil nil))
  (gnus-secondary-select-methods
   (mapcar (lambda (addr)
             `(nnimap "mx.q7x.ru"
                      (nnimap-user ,addr)
                      (nnimap-server-port "imaps")
                      (nnimap-stream tls)
                      (nnir-search-engine imap)
                      (nnmail-expiry-wait 'immediate)))
           self-hosted-email-addrs)))
