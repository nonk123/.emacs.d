;; -*- lexical-binding: t; -*-

(defconst nonk/windows? (and (string-match-p "AppData\\\\Roaming" (getenv "HOME")) t))

(defconst nonk/home
  (if nonk/windows?
      (concat "C:/Users/" (user-login-name) "/")
    (expand-file-name "~/"))
  "Should be used instead of `~' in filenames for portability.")

(defconst nonk/module-order
  '("init" "straight" "config" "projects" "linting" "completions"
    "visual" "treemacs" "coding" "keys" "modes" "finalize")
  "The order in which init-file modules are loaded.  See `nonk/load-init'.")

(require 'bytecomp)
(require 'comp)

(setq native-comp-async-report-warnings-errors 'silent)

(defun nonk/load-init (&optional arg)
  "Byte-compile and load the init-file in parts called modules.

With a prefix argument ARG, recompile all modules."
  (interactive "P")
  (dolist (module nonk/module-order)
    (let* ((cur-file (expand-file-name (concat "elisp/" module ".el") user-emacs-directory))
	   (compiled-file (byte-compile-dest-file cur-file)))
      (when (or arg (not (file-readable-p compiled-file)))
	(byte-compile-file cur-file))
      (load-file compiled-file))))

(nonk/load-init t)
