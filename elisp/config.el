(require 'dired)
(require 'wdired)

(defvar nonk/aggressive-indent-modes '(lisp-data-mode))
(defvar nonk/ignore-lsp-modes '(sh-mode ld-script-mode lisp-data-mode))
(defvar nonk/ignore-autoformat-modes '(html-mode))
(defvar nonk/mode-extras nil)

;; Misc. configuration that doesn't fit anywhere:

(setq find-file-visit-truename t)

(setq user-full-name "Sergey Sudakov")
(setq user-mail-address "me@nonk.dev")

(setq wdired-allow-to-change-permissions t)

(setq enable-recursive-minibuffers t)
(setq visible-bell nil)
(setq x-stretch-cursor t)

(setq kill-whole-line t)
(setq backward-delete-char-untabify-method 'hungry)

(setq sentence-end-double-space nil)

(setq inhibit-compacting-font-caches t)
