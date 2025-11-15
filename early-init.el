;;; early-init.el --- nonk's GNU/Emacs config: early init -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq inhibit-splash-screen t)

;; Enforce `straight' usage.
(setq package-enable-at-startup nil)

;; Performance tweaks for `lsp-mode'.
(setenv "LSP_USE_PLISTS" "true")
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1 1024 1024))

;; Prevent `indent-bars-mode' initial-frame crashes.
(setq initial-major-mode #'fundamental-mode)
(setq initial-scratch-message "")

;; Thanks <https://stackoverflow.com/a/79042584>.
(add-to-list 'default-frame-alist '(font . "Consolas-9"))

;;; early-init.el ends here
