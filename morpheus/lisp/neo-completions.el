(neo/use-package vertico
  :doc "UI component for minibuffer selection"
  :ensure
  (vertico
   :type git
   :host github
   :repo "minad/vertico"
   :files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  (setq vertico-resize t)
  (setq vertico-count 20) ; TODO: maybe a fraction of window height would be better
  )

(neo/use-package vertico-posframe
  :doc "Display vertico selections in a popup window"
  ;  :disabled ; not sure if I want it or not
  :after vertico
  :config (vertico-posframe-mode 1))

(neo/use-package vertico-directory
  :doc "Make navigating directories in vertico completions nicer by deleting entire components"
  :after vertico
  :ensure nil
  :bind
  (:map
   vertico-map
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word))
  ;; TODO: got this somewhere in the intertubes, but we don't use rfn-eshadow in Neo (yet?)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(neo/use-package orderless
  :doc "Allow completion on fragments"
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq
   completion-styles '(orderless basic)
   completion-category-defaults nil
   completion-category-overrides '((file (styles partial-completion)))))

(neo/use-package marginalia
  :doc "Annotate completions with additional information"
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind
  (("M-A" . marginalia-cycle)
   :map
   minibuffer-local-map
   ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left)
  (marginalia-align-offset 16)
  :init (marginalia-mode))

(defun neo/consult-outline ()
  (interactive)
  (if (eq major-mode 'org-mode)
      (consult-org-heading)
    (consult-outline)))

(neo/use-package consult
  :bind
  ("M-s G" . consult-git-grep)
  ("C-x b" . consult-buffer)
  ("M-g o" . neo/consult-outline))

;;   ;;    ("C-c h" . consult-history)
;;   ;;    ;; ("C-c m" . consult-mode-command)
;;   ;;    ;; ("C-c b" . consult-bookmark)
;;   ;;    ;; ("C-c k" . consult-kmacro)
;;   ;;    ;; ;; C-x bindings (ctl-x-map)
;;   ;;    ;; ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
;;   ;;    ;;
;;   ;;    ;; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
;;   ;;    ;; ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
;;   ;;    ;; ;; Custom M-# bindings for fast register access
;;   ;;    ;; ("M-#" . consult-register-load)
;;   ;;    ;; ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
;;   ;;    ;; ("C-M-#" . consult-register)
;;   ;;    ;; ;; Other custom bindings
;;   ;;    ;; ("M-y" . consult-yank-pop) ;; orig. yank-pop
;;   ;;    ;; ;; M-g bindings (goto-map)
;;   ;;    ;; ("M-g e" . consult-compile-error)
;;   ;;    ;; ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
;;   ;;    ;; ("M-g g" . consult-goto-line) ;; orig. goto-line
;;   ;;    ;; ("M-g M-g" . consult-goto-line) ;; orig. goto-line
;;   ;;    ;; ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
;;   ;;    ;; ("M-g m" . consult-mark)
;;   ;;    ;; ("M-g k" . consult-global-mark)
;;   ;;    ;; ("M-g i" . consult-imenu)
;;   ;;    ;; ("M-g I" . consult-imenu-multi)
;;   ;;    ;; ;; M-s bindings (search-map)
;;   ;;    ;; ("M-s f" . consult-find)
;;   ;;    ;; ("M-s L" . consult-locate)
;;   ;;    ;; ("M-s g" . consult-grep)
;;   ;;    ;; ("M-s G" . consult-git-grep)
;;   ;;    ;; ("M-s r" . consult-ripgrep)
;;   ;;    ;; ("M-s l" . consult-line)
;;   ;;    ;; ("M-s m" . consult-line-multi)
;;   ;;    ;; ("M-s k" . consult-keep-lines)
;;   ;;    ;; ("M-s u" . consult-focus-lines)
;;   ;;    ;; ;; Isearch integration
;;   ;; ;;   ("M-s e" . consult-isearch-history)
;;   ;;    :map
;;   ;;    isearch-mode-map
;;   ;;    ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
;;   ;;    ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
;;   ;;    ("M-s l" . consult-line)) ;; needed by consult-line to detect isearch

;;   (setq
;;    register-preview-delay 0
;;    register-preview-function #'consult-register-format)

;;   ;; Optionally tweak the register preview window.
;;   ;; This adds thin lines, sorting and hides the mode line of the window.
;;   (advice-add #'register-preview :override #'consult-register-window)

;;   ;; Configure other variables and modes in the :config section,
;;   ;; after lazily loading the package.
;;   :config

;;   ;; disable automatic preview by default,
;;   ;; selectively enable it for some prompts below.
;;   (setq consult-preview-key '("M-." "C-SPC"))

;;   ;; customize preview activation and delay while selecting candiates
;;   (consult-customize
;;    consult-theme
;;    :preview-key '("M-." "C-SPC" :debounce 0.2 any)

;;    ;; slightly delayed preview upon candidate selection
;;    ;; one usually wants quick feedback
;;    consult-buffer
;;    consult-ripgrep
;;    consult-git-grep
;;    consult-grep
;;    consult-bookmark
;;    consult-yank-pop
;;    :preview-key
;;    '("M-."
;;      "C-SPC"
;;      :debounce
;;      0.3
;;      "<up>"
;;      "<down>"
;;      "C-n"
;;      "C-p"
;;      :debounce
;;      0.6
;;      any))

;;   ;; hide magit buffer
;;   (add-to-list 'consult-buffer-filter "magit.*:.*")

;;   (setq consult-line-start-from-top nil)

;;   ;; Optionally configure the narrowing key.
;;   ;; Both < and C-+ work reasonably well.
;;   (setq consult-narrow-key "<") ;; (kbd "C-+")

;;   ;; Optionally make narrowing help available in the minibuffer.
;;   ;; You may want to use `embark-prefix-help-command' or which-key instead.
;;   ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

;;   ;; Make M-n as smart as ivy and helm equivalents
;;   (setq minibuffer-default-add-function
;;         'spacemacs/minibuffer-default-add-function)

;;   ;; Optionally configure a function which returns the project root directory.
;;   (setq consult-project-root-function
;;         (lambda ()
;;           (when-let (project
;;                      (project-current))
;;             (car (project-root project))))))

(neo/use-package corfu
  :ensure (corfu :host github :repo "minad/corfu" :files ("*.el"))
  :init
  (global-corfu-mode))

(use-package corfu-popupinfo
  :after corfu
  :ensure (corfu-popupinfo :host github :repo "minad/corfu" :files ("extensions/corfu-popupinfo.el"))
  :config
  (corfu-popupinfo-mode))

;; (neo/use-package corfu
;;   :doc "Show completions near the point"
;;   :ensure
;;   (corfu
;;    :host github
;;    :repo "minad/corfu"
;;    :files (:defaults "extensions/*"))
;;   :hook (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
;;   :custom
;;   ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
;;   ;; want to perform completion
;;   (tab-always-indent 'complete)
;;   (completion-cycle-threshold nil) ; Always show candidates in menu

;;   (corfu-auto nil)
;;   (corfu-auto-prefix 2)
;;   (corfu-auto-delay 0.25)

;;   (corfu-min-width 40)
;;   (corfu-max-width 120)
;;   (corfu-count 14)
;;   (corfu-scroll-margin 4)
;;   (corfu-cycle nil)

;;   ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
;;   ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
;;   ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
;;   ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
;;   ;; configuration already has pre-prepared). Necessary for manual corfu usage with
;;   ;; orderless, otherwise first component is ignored, unless `corfu-separator'
;;   ;; is inserted.
;;   (corfu-quit-at-boundary nil)
;;   (corfu-separator ?\s) ; Use space
;;   (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
;;   (corfu-preview-current 'insert) ; Preview first candidate. Insert on input if only one
;;   (corfu-preselect-first t) ; Preselect first candidate?

;;   ;; Other
;;   (lsp-completion-provider :none) ; Use corfu instead for lsp completions

;;   :config (global-corfu-mode)
;;   ;; (corfu-popupinfo-mode) TODO loading of extensions in :elpaca doesn't seem to work
;;   ;; Enable Corfu more generally for every minibuffer, as long as no other
;;   ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
;;   ;; completion UI. From
;;   ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
;;   (defun corfu-enable-always-in-minibuffer ()
;;     "Enable Corfu in the minibuffer if Vertico/Mct are not active."
;;     (unless (or
;;              (bound-and-true-p mct--active) ; Useful if I ever use MCT
;;              (bound-and-true-p vertico--input))
;;       (setq-local corfu-auto nil) ; Ensure auto completion is disabled
;;       (corfu-mode 1)))
;;   (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer
;;             1)

;;   ;; Setup lsp to use corfu for lsp completion
;;   (defun kb/corfu-setup-lsp ()
;;     "Use orderless completion style with lsp-capf instead of the
;; default lsp-passthrough."
;;     (setf (alist-get
;;            'styles
;;            (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless))))

;; TODO this should be possible from the corfu use-package
;; (elpaca
;;  nil
;;  (progn
;;    (add-to-list
;;     'load-path
;;     (expand-file-name "elpaca/builds/corfu/extensions"
;;                       user-emacs-directory))
;;    (require 'corfu-popupinfo)
;;    (setq corfu-popupinfo-delay '(1.0 . 0.3))

;;    (corfu-popupinfo-mode)
;;    (require 'corfu-history)
;;    (corfu-history-mode)))

(neo/use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind
  (("C-c p p" . completion-at-point) ;; capf
   ("C-c p t" . complete-tag) ;; etags
   ("C-c p d" . cape-dabbrev) ;; or dabbrev-completion
   ("C-c p h" . cape-history)
   ("C-c p f" . cape-file)
   ("C-c p k" . cape-keyword)
   ("C-c p s" . cape-symbol)
   ("C-c p a" . cape-abbrev)
   ("C-c p l" . cape-line)
   ("C-c p w" . cape-dict)
   ("C-c p \\" . cape-tex)
   ("C-c p _" . cape-tex)
   ("C-c p ^" . cape-tex)
   ("C-c p &" . cape-sgml)
   ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(provide 'neo-completions)
