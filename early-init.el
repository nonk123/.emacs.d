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
(defvar nonk/monospace-font "Consolas" "The default monospaced font to use.")
(add-to-list 'default-frame-alist (cons 'font (concat nonk/monospace-font "-9")))
(set-face-attribute 'fixed-pitch nil :family nonk/monospace-font)

;;; early-init.el ends here
