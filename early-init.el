(setq inhibit-splash-screen t)

;; Enforce `straight' usage.
(setq package-enable-at-startup nil)

;; `lsp-mode' performance tweaks.
(setenv "LSP_USE_PLISTS" "true")

;; Prevent `indent-bars-mode' initial-frame crashes.
(setq initial-major-mode #'fundamental-mode)
(setq initial-scratch-message "")
