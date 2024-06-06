;;; Disable the builtin version as eglot wants 1.14 and Emacs 29.3 ships with 1.13
(neo/use-package eldoc
    :preface
    (unload-feature 'eldoc t)
    (setq custom-delayed-init-variables '())
    (defvar global-eldoc-mode nil)
    :config
    (global-eldoc-mode))

;;; Here too we hit a minimum version requirement
(use-package jsonrpc
;  :ensure t
  :ensure (jsonrpc :repo "emacs-straight/jsonrpc" :host github :files ("*.el"))
  :ensure (:type git :host github :repo "emacs-straight/jsonrpc" :files ("*.el") :tag "1.0.24")
;  :straight (:type git :host github :repo "emacs-straight/jsonrpc" :files ("*.el") :tag "1.0.24")
  )

(neo/use-package treesit
  :ensure nil
  :config
  (when (treesit-available-p)
    (add-hook 'c++-mode-hook #'turn-on-treesit)))


(neo/use-package eglot
  :hook ((c++-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider)))

(neo/use-package devdocs
  :ensure (devdocs :host github :repo "astoff/devdocs.el")
  :config
  ;; Add the programming languages you want to support
  (dolist (doc '("go" "rust" "cpp" "python"))
    (devdocs-install doc))

  ;; Optionally bind keys for easy access
  (global-set-key (kbd "C-c d o") 'devdocs-search)
  (global-set-key (kbd "C-c d b") 'devdocs-browser)
  (global-set-key (kbd "C-c d s") 'devdocs-lookup-symbol))

(provide 'neo-prog-general)
