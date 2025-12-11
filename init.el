;;; init.el --- nonk's GNU/Emacs config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst nonk/windose? (and (string-match-p "AppData\\\\Roaming" (getenv "HOME")) t)
  "Evaluates to t if this GNU/Emacs is running under Windose.")

(defconst nonk/home
  (if nonk/windose? (concat "C:/Users/" (user-login-name) "/") (expand-file-name "~/"))
  "Should be used instead of `~' in code for portability reasons.

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

(declare-function straight-use-package "bootstrap-straight")
(straight-use-package 'use-package)

;; Optional chezmoi fluff:
(load "chezmoi-defs" t)

(defvaralias 'coding-hook 'nonk/coding-hook
  "Shorter, more general name for `nonk/coding-hook'.")
(defvar nonk/coding-hook nil
  "Functions to be run whenever I'm coding.")

(defvar nonk/coding-modes
  '(c-mode c++-mode cmake-ts-mode rust-ts-mode yaml-ts-mode))
(dolist (mode nonk/coding-modes)
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda () (run-hooks 'coding-hook))))

(setq disabled-command-function nil)
(setq kill-whole-line t)
(delete-selection-mode 1)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)

(electric-indent-mode 1)
(electric-pair-mode -1)

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
  :defines smartparens-mode-map
  :bind (:map smartparens-mode-map
              ([remap forward-sexp] . sp-forward-sexp)
              ([remap backward-sexp] . sp-backward-sexp)
              ([remap transpose-sexp] . sp-transpose-sexp))
  :custom
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1))

(use-package emacs
  :custom
  (enable-local-variables :all)
  (enable-local-eval t))

(use-package editorconfig
  :diminish
  :custom (editorconfig-mode 1))

(use-package magit
  :bind ("C-x g" . magit))

(use-package forge)

(defun nonk/diff-hl-update-everywhere ()
  "Update `diff-hl' gutter in all buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (when (buffer-file-name buf)
      (with-current-buffer buf (diff-hl--update-safe)))))

(use-package diff-hl
  :after magit
  :custom
  (diff-hl-show-staged-changes nil)
  (global-diff-hl-mode 1)
  :functions diff-hl--update-safe
  :hook ((git-commit-post-finish-hook magit-post-stage magit-post-unstage) . nonk/diff-hl-update-everywhere)
  :bind (("C-c M-n" . diff-hl-next-hunk)
         ("C-c M-p" . diff-hl-previous-hunk)))

(use-package project)

(use-package projectile
  :diminish
  :custom
  (projectile-project-search-path
   `((,(expand-file-name "Sources" nonk/home) . 1)
     ("~" . 1))) ; `~` intentional: this finds `.emacs.d` automatically on Windows
  (projectile-ignored-project-function #'nonk/ignore-project?)
  (projectile-current-project-on-switch 'keep)
  (projectile-enable-caching t)
  (projectile-indexing-method 'alien)
  (projectile-auto-cleanup-known-projects t)
  (projectile-auto-discover nil)
  (projectile-require-project-root t)
  (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :preface
  (defun nonk/ignore-project? (root)
    "Ignore project ROOTs inside straight.el repos and CMake deps."
    (let ((root (expand-file-name root)))
      (or (string-prefix-p (expand-file-name "straight" user-emacs-directory) root)
          (string-match-p "^.+?/build/_deps/" root)))))

(defvar nonk/vscode-setting-alist
  '(("editor.formatOnSave" t)
    ("cmake.debugConfig" nil))
  "List of VSCode settings recognized by `nonk/vscode-setting'.")

(defvar nonk/vscode-language-modes
  '((c-mode "c")
    (web-mode "html" "jinja-html"))
  "List of Emacs major-modes mapping to a list of possible VSCode language names.")

(defun nonk/vscode-setting--get (definition alist)
  "Fetch and parse a setting's value from ALIST by its DEFINITION."
  (declare (indent 1))
  (cdr-safe (assoc-string (car definition) alist)))

(defun nonk/vscode-setting (setting)
  "Return the value of SETTING defined in `nonk/vscode-setting-alist'."
  (when-let* ((definition (assoc-string setting nonk/vscode-setting-alist))
              (project (project-current))
              (root (project-root project))
              (settings-file (expand-file-name ".vscode/settings.json" root))
              ((file-exists-p settings-file))
              (json (json-read-file settings-file)))
    (or (let ((langs (alist-get major-mode nonk/vscode-language-modes))
              result)
          (while (and langs (null result))
            (setq result
                  (nonk/vscode-setting--get definition
                    (cdr-safe (assoc-string (concat "[" (car langs) "]") json))))
            (setq langs (cdr langs)))
          result)
        (nonk/vscode-setting--get definition json)
        (cdr definition))))

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
  :custom
  (yas-snippet-dirs
   (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-global-mode 1))

(use-package emacs
  :mode ("LICENSE\\'" . text-mode))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (global-corfu-mode 1)
  (corfu-echo-mode 1))

(defvar nonk/neocmakelsp-path
  (if nonk/windose? (expand-file-name "bundled/neocmakelsp.exe" user-emacs-directory) "neocmakelsp"))

(use-package lsp-mode
  :demand t
  :functions lsp-format-buffer lsp-register-client make-lsp-client lsp-stdio-connection lsp-activate-on
  :defines lsp-mode-map lsp-language-id-configuration
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :none)
  (lsp-enable-suggest-server-download nil)
  (lsp-eldoc-render-all t)
  (lsp-clangd-binary-path "clangd") ; assuming `PATH` is correct
  (lsp-clients-clangd-args '("--header-insertion=never"))
  :hook ((coding poly-markdown-mode) . lsp)
  :bind (:map lsp-mode-map
              ([f2] . lsp-rename))
  :config
  (add-to-list 'lsp-language-id-configuration '(cmake-ts-mode . "cmake"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection (list nonk/neocmakelsp-path "--stdio"))
                    :activation-fn (lsp-activate-on "cmake")
                    :language-id "cmake"
                    :server-id 'neocmakelsp)))

(use-package indent-bars
  :disabled
  :straight (:type git :host github :repo "jdtsmith/indent-bars")
  :hook lsp-mode
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-starting-column 0)
  (indent-bars-prefer-character t)
  (indent-bars-no-stipple-char ?|)
  (indent-bars-treesit-ignore-blank-lines-types '("module")))

(use-package dtrt-indent
  :diminish
  :custom
  (setq dtrt-indent-run-after-smie t)
  (dtrt-indent-global-mode 1))

(use-package aggressive-indent
  :diminish
  :hook emacs-lisp-mode)

(use-package emojify
  :custom
  (use-default-font-for-symbols nil)
  (emojify-display-style 'unicode)
  (emojify-emoji-styles '(unicode github))
  (global-emojify-mode 1)
  :config
  (defvar nonk/emoji-font "Segoe UI Emoji")
  (when (member nonk/emoji-font (font-family-list))
    (set-fontset-font t 'emoji nonk/emoji-font)))

(use-package ef-themes
  :disabled
  :custom (modus-themes-italic-constructs t)
  :init (load-theme 'ef-autumn t))

(use-package color-theme-sanityinc-tomorrow
  :init (load-theme 'sanityinc-tomorrow-night t))

(use-package polymode
  :functions pm-around-advice polymode-inhibit-in-indirect-buffers
  :config
  (dolist (fun #'(lsp flycheck-mode))
    (pm-around-advice fun #'polymode-inhibit-in-indirect-buffers)))

(use-package poly-markdown
  :diminish poly-gfm-mode poly-markdown-mode
  :mode ("\\.md\\'" . poly-markdown-mode))

;; BOOKMARK: major-modes.

(use-package dockerfile-mode)

(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'" "\\.clang.+\\'")

(use-package cmake-ts-mode
  :mode "CMakeLists\\.txt\\'" "\\.cmake\\'")

(use-package web-mode
  :mode "\\.html\\.j2\\'")

(use-package rust-ts-mode
  :mode ("\\.rs\\'" . rust-ts-mode))

(use-package glsl-mode
  :mode ("\\.\\(f|v\\)sh\\'" . glsl-mode))

(defun nonk/enable-eldoc-box ()
  "Force `eldoc-box-hover-mode' on."
  (interactive)
  (eldoc-box-hover-mode 1))

(use-package eldoc-box
  :diminish eldoc-box-hover-mode
  :commands eldoc-box-hover-mode
  :hook ((prog-mode coding) . nonk/enable-eldoc-box))

(use-package eldoc
  :diminish
  :custom (global-eldoc-mode 1))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package wdired
  :custom (wdired-allow-to-change-permissions t))

(use-package smerge-mode
  :diminish)

(defun nonk/format-on-save--real ()
  "Do the actual formatting."
  (cond
   ((bound-and-true-p lsp-mode)
    (ignore-error lsp-capability-not-supported
      (lsp-format-buffer)))
   (t
    (message "Couldn't find a suitable formatter"))))

(defun nonk/format-on-save ()
  "Format the just saved file using the running language server."
  (interactive)
  (when-let ((value (nonk/vscode-setting "editor.formatOnSave")))
    (if (eq value t)
        (nonk/format-on-save--real)
      (message "Formatting inhibited by VSCode settings.json"))))

(use-package emacs
  :diminish abbrev-mode
  :hook (before-save . nonk/format-on-save)
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
  (open-paren-in-column-0-is-defun-start nil)
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
                      (nnimap-server-port 993)
                      (nnimap-stream tls)
                      (nnir-search-engine imap)
                      (nnmail-expiry-wait immediate)))
           (bound-and-true-p nonk/self-hosted-email-addrs))))

(use-package sanity
  :straight (:type git :host github :repo "nonk123/sanity-emacs")
  :custom (sanity-mode 1))

(use-package wakatime-mode
  :diminish
  :custom (global-wakatime-mode 1))

(use-package dape
  :custom
  (dape-breakpoint-global-mode 1)
  (dape-cwd-function #'projectile-project-root)
  (dape-buffer-window-arrangement 'right))

(use-package cmake-integration
  :straight (:type git :host github :repo "nonk123/cmake-integration")
  :custom
  (cmake-integration-generator "Ninja")
  (cmake-integration-use-separated-compilation-buffer-for-each-target t)
  (cmake-integration-debug-launcher-function 'dape)
  (cmake-integration-program-launcher-function 'compilation)
  :bind (:map cmake-project-mode-map
              ([f3] . cmake-integration-select-current-target)
              ([f4] . cmake-integration-transient)
              ([f5] . cmake-integration-run-last-target)
              ([f6] . cmake-integration-debug-last-target)
              ([f9] . cmake-integration-save-and-compile-last-target)
              ([f10] . cmake-integration-cmake-reconfigure))
  :preface
  (defvar cmake-project-mode-map (make-sparse-keymap)))

(defvar cmake-integration-run-arguments)
(declare-function cmake-integration-run-last-target "cmake-integration-launch.el")

(defun nonk/cmake-integration-vscode-args ()
  "Fetch args from `settings.json' for `cmake-integration-run-last-target'."
  (setq cmake-integration-run-arguments "")
  (when-let* ((config (nonk/vscode-setting "cmake.debugConfig"))
              (args (cdr-safe (assoc-string "args" config))))
    (setq cmake-integration-run-arguments
          (seq-reduce (lambda (sum x) (if sum (concat sum " " x) x)) args nil))))
(advice-add #'cmake-integration-run-last-target :before #'nonk/cmake-integration-vscode-args)

;; Thanks <https://github.com/darcamo/cmake-integration#example-keybindings>.

(defun try-enable-cmake-project-mode ()
  "Enable `cmake-project-mode' in buffers belonging to a CMake project."
  (when-let* ((project (project-current))
              (project-root (project-root project))
              ((file-exists-p (expand-file-name "CMakeLists.txt" project-root))))
    (cmake-project-mode 1)))

(define-minor-mode cmake-project-mode
  "Auto-enabled for buffers detected as belonging to a CMake project."
  :group 'tools
  :group 'convenience
  :keymap cmake-project-mode-map)

(define-globalized-minor-mode detect-cmake-project-mode cmake-project-mode try-enable-cmake-project-mode
  :group 'tools
  :group 'convenience)

(detect-cmake-project-mode 1)

(defun nonk/suspend-frame ()
  "Call `suspend-frame' ONLY in terminal-mode frames."
  (interactive)
  (unless (display-graphic-p)
    (call-interactively #'suspend-frame)))

(bind-key [remap suspend-frame] #'nonk/suspend-frame)

;;; init.el ends here
