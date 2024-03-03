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

(defconst nonk/package-list
  '(;; Theme.
    dracula-theme

    ;; Cool libraries.
    dash
    ag

    ;; Customization.
    diminish

    ;; Mandatory fluff.
    vertico
    marginalia
    embark
    consult
    embark-consult
    cape
    savehist
    orderless

    ;; LSP support.
    flycheck
    lsp-mode
    lsp-ui
    company
    projectile
    yasnippet
    ccls
    dap-mode

    ;; It's Magit!
    magit

    ;; Treemacs
    treemacs
    lsp-treemacs
    treemacs-projectile
    treemacs-magit

    ;; Fun stuff.
    treemacs-icons-dired
    all-the-icons
    treemacs-all-the-icons

    ;; Utilities.
    ace-window
    aggressive-indent-mode
    format-all
    editorconfig
    wakatime-mode

    ;; Various modes.
    web-mode
    rust-mode
    zig-mode
    jinja2-mode
    markdown-mode
    cmake-mode
    dockerfile-mode
    yaml-mode
    glsl-mode))

(dolist (package nonk/package-list)
  (straight-use-package package))

(require 'dash)
