;; -*- lexical-binding: t; -*-

(defvar nonk/windows-p (and (string-match-p "AppData\\\\Roaming" (getenv "HOME")) t))

(defvar nonk/home
  (if nonk/windows-p (concat "C:/Users/" (user-login-name) "/") "~/")
  "Should be used instead of `~' in filenames for portability.")

(defconst nonk/module-order
  '("init" "straight" "config" "projects" "linting" "completions"
    "visual" "treemacs" "coding" "keys" "modes" "finalize")
  "The order in which init-file modules are loaded.  See `nonk/load-init'.")

(defun nonk/load-init (&optional arg)
  "Byte-compile and load the init-file in parts called modules.

If ARG is non-nil, recompile all modules."
  (interactive "P")
  (require 'bytecomp)
  (dolist (module nonk/module-order)
    (let* ((file (expand-file-name (concat "elisp/" module ".el") user-emacs-directory))
	   (compiled-file (byte-compile-dest-file file)))
      (when (or arg (not (file-exists-p compiled-file)))
	(byte-compile-file file))
      (load-file compiled-file))))

(nonk/load-init)
