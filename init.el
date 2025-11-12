;;; init.el --- nonk's GNU/Emacs config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst nonk/windose? (and (string-match-p "AppData\\\\Roaming" (getenv "HOME")) t)
  "Evaluates to t if this GNU/Emacs is running under Windose.")

(defconst nonk/home
  (if nonk/windose? (concat "C:/Users/" (user-login-name) "/") (expand-file-name "~/"))
  "Should be used instead of `~' in filenames for portability reasons.

In GNU/Emacs for Windows, `~' expands to the user's `AppData/Roaming'
directory.  I think it makes a lot more sense for it to expand to just
the user's directory, but overwriting the `HOME' environment variable to
do that breaks a lot of external packages.")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-region (point-min) (point-max) custom-file)))
(load-file custom-file)

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(load "bootstrap-straight")

;; Optional chezmoi fluff:
(load "chezmoi-defs" t)

(defvaralias 'coding-hook 'nonk/coding-hook
  "Shorter, more general name for `nonk/coding-hook'.")
(defvar nonk/coding-hook nil
  "Functions to be run whenever I'm coding.")

(defvar nonk/coding-modes
  '(c-mode c++-mode rust-mode yaml-mode yaml-ts-mode))
(dolist (mode nonk/coding-modes)
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda () (run-hooks 'coding-hook))))

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

(use-package smartparens
  :diminish
  :bind (([remap forward-sexp] . sp-forward-sexp)
         ([remap backward-sexp] . sp-backward-sexp)
         ([remap transpose-sexp] . sp-transpose-sexp))
  :custom
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1))

(use-package editorconfig
  :diminish
  :custom (editorconfig-mode 1))

(use-package magit
  :bind ("C-x g" . magit))

(use-package forge)

(use-package diff-hl
  :after magit
  :custom
  (diff-hl-show-staged-changes nil)
  (global-diff-hl-mode 1)
  :functions diff-hl--update-safe
  :hook ((magit-post-commit magit-post-stage magit-post-unstage) . nonk/diff-hl-update-everywhere)
  :bind (("C-c M-n" . diff-hl-next-hunk)
         ("C-c M-p" . diff-hl-previous-hunk))
  :init
  (defun nonk/diff-hl-update-everywhere ()
    (interactive)
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf) (buffer-file-name buf))
        (with-current-buffer buf (diff-hl--update-safe))))))

(use-package projectile
  :diminish
  :custom
  (projectile-project-search-path
   `((,(expand-file-name "Sources" nonk/home) . 1) ("~" . 1)))
  (projectile-current-project-on-switch 'keep)
  (projectile-enable-caching t)
  (projectile-indexing-method 'alien)
  (projectile-auto-cleanup-known-projects t)
  (projectile-auto-discover nil)
  (projectile-require-project-root t)
  (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map))

(defvar nonk/vscode-setting-alist
  '((format-on-save "editor.formatOnSave" t))
  "List of VSCode settings recognized by `nonk/vscode-setting'.")

(defun nonk/vscode-setting (symbol)
  "Return the value of SYMBOL setting defined in `nonk/vscode-setting-alist'."
  (require 'project)
  (if-let* ((definition (alist-get symbol nonk/vscode-setting-alist))
            (project (project-current))
            (root (project-root project))
            (settings-file (expand-file-name ".vscode/settings.json" root))
            ((file-exists-p settings-file))
            (json (json-read-file settings-file)))
      (cdr (assoc-string (car definition) json))
    (cdr definition)))

(use-package savehist
  :custom (savehist-mode 1))

(use-package vertico
  :custom
  (vertico-cycle t)
  (vertico-mode 1))

(use-package consult
  :bind ("M-y" . consult-yank-pop))

(use-package marginalia
  :custom (marginalia-mode 1))

(use-package orderless
  :custom (completion-styles '(orderless basic partial-completion)))

(use-package flycheck
  :defines flycheck-mode-map
  :custom (global-flycheck-mode 1)
  :bind (:map flycheck-mode-map
              ("C-c C-n" . flycheck-next-error)
              ("C-c C-p" . flycheck-previous-error)))

(use-package ag)

(use-package yasnippet
  :diminish yas-minor-mode
  :custom (yas-global-mode 1))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (global-corfu-mode 1)
  (corfu-echo-mode 1))

(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :none)
  (lsp-eldoc-render-all t)
  :hook (coding . lsp)
  :functions lsp-format-buffer)

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
  :custom (dtrt-indent-global-mode 1))

(use-package ef-themes
  :custom (modus-themes-italic-constructs t)
  :init (load-theme 'ef-autumn t))

(use-package polymode
  :functions pm-around-advice polymode-inhibit-in-indirect-buffers
  :config (pm-around-advice #'lsp #'polymode-inhibit-in-indirect-buffers))

(use-package poly-markdown
  :mode ("\\.md" . poly-markdown-mode))

(use-package dockerfile-mode)
(use-package rust-mode)
(use-package cmake-mode)
(use-package yaml-mode)

(use-package eldoc-box
  :diminish eldoc-box-hover-mode
  :commands eldoc-box-hover-mode
  :init (defun nonk/enable-eldoc-box () (interactive) (eldoc-box-hover-mode 1))
  :hook ((prog-mode coding) . nonk/enable-eldoc-box))

(use-package eldoc
  :diminish
  :custom (global-eldoc-mode 1))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package wdired
  :custom (wdired-allow-to-change-permissions t))

(defun nonk/format-on-save ()
  "Format the just saved file using the running language server."
  (interactive)
  (when (nonk/vscode-setting 'format-on-save)
    (require 'lsp-mode)
    (cond
     ((bound-and-true-p lsp-mode)
      (ignore-error lsp-capability-not-supported
        (lsp-format-buffer)))
     (t
      (whitespace-cleanup-region (point-min) (point-max))))))

(use-package emacs
  :diminish abbrev-mode
  :hook (after-save . nonk/format-on-save)
  :bind (("C-'" . completion-at-point)
         ("M-n" . scroll-up-line)
         ("M-p" . scroll-down-line))
  :custom
  (user-full-name "Sergey Sudakov")
  (user-mail-address "me@nonk.dev")
  (auto-save-interval 0)
  (make-backup-files nil)
  (window-sides-vertical t)
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (completion-auto-help nil)
  (use-short-answers t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (text-mode-ispell-word-completion nil))

(use-package gnus
  :custom
  (gnus-select-method '(nnnil nil))
  (gnus-secondary-select-methods
   (mapcar (lambda (addr)
             `(nnimap "mx.q7x.ru"
                      (nnimap-user ,addr)
                      (nnimap-server-port imaps)
                      (nnimap-stream tls)
                      (nnir-search-engine imap)
                      (nnmail-expiry-wait immediate)))
           (bound-and-true-p self-hosted-email-addrs))))

(use-package dape
  :custom
  (dape-breakpoint-global-mode 1)
  (dape-cwd-function #'projectile-project-root)
  (dape-buffer-window-arrangement 'right))

(use-package cmake-integration
  :straight (cmake-integration :type git :host github :repo "darcamo/cmake-integration")
  :custom
  (cmake-integration-generator "Ninja")
  (cmake-integration-use-separated-compilation-buffer-for-each-target t)
  (cmake-integration-debug-launcher-function 'dape)
  :init
  ;; Thanks <https://github.com/darcamo/cmake-integration#example-keybindings>.
  (defvar cmake-project-mode-map (make-sparse-keymap))
  (defun nonk/is-cmake-project? ()
    "Determine if the current directory is inside of a CMake project."
    (interactive)
    (if-let* ((project (project-current))
              (project-root (project-root project))
              (cmakelists-path (expand-file-name "CMakeLists.txt" project-root)))
        (file-exists-p cmakelists-path)))
  (defun cmake-project-mode-turn-on-in-cmake-projects ()
    "Turn on `cmake-project-mode' in buffers belonging to a CMake project."
    (cmake-project-mode (if (nonk/is-cmake-project?) 1 -1)))
  (define-minor-mode cmake-project-mode
    "A mode enabled for buffers belonging to a CMake project."
    :keymap cmake-project-mode-map)
  (define-globalized-minor-mode cmake-detect-project-mode cmake-project-mode cmake-project-mode-turn-on-in-cmake-projects)
  (cmake-detect-project-mode 1)
  (bind-keys :map cmake-project-mode-map
             ([f3] . cmake-integration-select-current-target)
             ([f4] . cmake-integration-transient)
             ([f5] . cmake-integration-run-last-target)
             ([f6] . cmake-integration-debug-last-target)
             ([f9] . cmake-integration-save-and-compile-last-target)
             ([f10] . cmake-integration-cmake-reconfigure)))

;;; init.el ends here
