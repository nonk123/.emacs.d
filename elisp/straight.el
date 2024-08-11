(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir) user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)

(defconst nonk/package-list
  '(;; Theme.
    doom-themes

    ;; Cool libraries.
    dash
    ag
    ripgrep
    rg
    polymode

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
    eldoc-box
    (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
    (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
    centaur-tabs
    doom-modeline

    ;; LSP support.
    flycheck
    lsp-mode
    lsp-ui
    company
    projectile
    yasnippet
    yasnippet-snippets
    ccls
    dap-mode

    ;; It's Magit!
    magit
    forge
    magit-todos
    git-gutter

    ;; Treemacs
    treemacs
    lsp-treemacs
    treemacs-projectile
    treemacs-magit

    ;; Fun stuff.
    treemacs-icons-dired
    all-the-icons
    nerd-icons
    nerd-icons-completion
    treemacs-all-the-icons
    elcord

    ;; Utilities.
    ace-window
    aggressive-indent-mode
    format-all
    editorconfig
    wakatime-mode
    cargo

    ;; Various modes.
    web-mode
    rust-mode
    go-mode
    zig-mode
    jinja2-mode
    markdown-mode
    cmake-mode
    dockerfile-mode
    groovy-mode ; only used for Jenkinsfiles
    yaml-mode
    glsl-mode
    sxhkdrc-mode
    poly-markdown))

(dolist (package nonk/package-list)
  (straight-use-package package))
