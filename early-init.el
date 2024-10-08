(setq inhibit-splash-screen t)
(setq doom-modeline-support-imenu t) ; weird shiz

;; Enforce `straight' usage.
(setq package-enable-at-startup nil)

;; Performance tweaks for `lsp-mode'.
(setenv "LSP_USE_PLISTS" "true")
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1 1024 1024))

;; Prevent `indent-bars-mode' initial-frame crashes.
(setq initial-major-mode #'fundamental-mode)
(setq initial-scratch-message "")
