(require 'diminish)
(require 'aggressive-indent)
(require 'lsp-mode)
(require 'lsp-ui)
(require 'web-mode)
(require 'ccls)
(require 'eldoc-box)
(require 'ts-fold)
(require 'indent-bars)
(require 'indent-bars-ts)
(require 'yasnippet)
(require 'yasnippet-snippets)

(yasnippet-snippets-initialize)

(add-to-list 'lsp-language-id-configuration '(makefile-gmake-mode . "makefile") t)

(setq eldoc-box-lighter "")

(setq indent-bars-treesit-support t)
(setq indent-bars-starting-column 0)
(setq indent-bars-prefer-character t)
(setq indent-bars-no-stipple-char ?|)
(setq indent-bars-treesit-ignore-blank-lines-types '("module"))

(defun nonk/format-buffer--mode-specific ()
  (cond
   ((and lsp-mode (or (lsp-feature? "textDocument/formatting")
		      (lsp-feature? "textDocument/rangeFormatting")))
    (lsp-format-buffer))
   (t
    (condition-case nil
	(format-all-buffer)
      (error nil)))))

(defun nonk/format-buffer ()
  (interactive)
  (nonk/format-buffer--mode-specific)
  (whitespace-cleanup-region (point-min) (point-max)))

(defun nonk/start-coding ()
  (interactive)
  (eldoc-box-hover-at-point-mode 1)
  (when (-any-p #'derived-mode-p nonk/aggressive-indent-modes)
    (aggressive-indent-mode 1))
  (indent-bars-mode 1)
  (editorconfig-apply)
  (unless (apply #'derived-mode-p nonk/ignore-autoformat-modes)
    (add-hook 'before-save-hook #'nonk/format-buffer 99 t))
  (let ((ptr nonk/mode-extras) (stop nil))
    (while (and ptr (not stop))
      (pcase-let ((`(,mode ,hook ,fn) (car ptr)))
	(when (derived-mode-p mode)
	  (add-hook hook fn 99 t)
	  (setq stop t)))
      (setq ptr (cdr ptr))))
  (when-let* ((buf-file (buffer-file-name))
	      (buf-file (file-name-nondirectory buf-file)))
    (when (or (-any-p #'derived-mode-p '(rust-ts-mode rust-mode))
	      (string-equal buf-file "Cargo.toml"))
      (cargo-minor-mode 1)))
  (unless (-any-p #'derived-mode-p nonk/ignore-lsp-modes)
    (lsp nil)
    (lsp-ui-mode 1))
  (diminish 'auto-revert-mode))

(defun nonk/disable-auto-format ()
  (interactive)
  (remove-hook 'before-save-hook #'nonk/format-buffer t))

(add-to-list 'lsp-language-id-configuration '("\\.cshtml\\.cs\\'" . "csharp"))

(setq web-mode-engines-alist '(("razor" . "\\.cshtml\\(?:\\..+\\)?\\'")))

(setq lsp-headerline-breadcrumb-enable nil
      lsp-ui-peek-enable nil
      lsp-ui-sideline-enable nil
      lsp-signature-auto-activate '(:on-trigger-char :after-completion))

(bind-key "C-c l" lsp-command-map)

(dolist (mode '(prog-mode markdown-mode cmake-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
	    #'nonk/start-coding))
