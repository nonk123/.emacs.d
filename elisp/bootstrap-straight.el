;;; bootstrap-straight.el --- straight.el boilerplate -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(declare-function straight-use-package "bootstrap-straight")
(straight-use-package 'diminish)
(straight-use-package 'use-package)

(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)

;;; bootstrap-straight.el ends here
