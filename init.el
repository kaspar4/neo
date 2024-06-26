;;; neo --- My Emacs configuration, in its N-th reincarnation
;;; This is Neo -*- lexical-binding: t -*-

;;; Commentary:

;;; This is what I like to use. Most likely is not what you'd like to
;;; use and vscode might serve you better.

;;; Code:

;;; matching setting in early-init.el. This is here for bug-hunter
(setq-default no-littering-etc-directory
              (expand-file-name ".litter/config"
                                user-emacs-directory))
(setq-default no-littering-var-directory
              (expand-file-name ".litter/data" user-emacs-directory))

;;;-----------------------------------------------------------------------------------
;;; Neo utilities

(require 'map)

;;; TODO: elisp autocompletion on :custom should be for variables, not functions)

;;; ensure-system-package uses an async buffer (in a side window) which in turns
;;; causes the dashboard to be locked in there, a small window at the bottom of the
;;; frame that cannot even been grown.
;;; TODO: fix the mess for real
;;; for now, I just disable this, as I have everything installed.
(defvar neo/ignore-ensure-system-package t)

(defun neo/filter-package-args (args)
  (let ((ignore-list
         (append
          '(:doc)
          (if neo/ignore-ensure-system-package
              '(:ensure-system-package)
            '()))))
    (mapc (lambda (el) (setq args (map-delete args el))) ignore-list)
    args))

;;; TODO ideally we should add this after :disabled, but :chord is also added at the beginning
;;; so meh
;; (add-to-list 'use-package-keywords :var)

;; (defun use-package-normalize/:var (name-symbol keyword args)
;;   (message args)
;;   (use-package-only-one
;;    (symbol-name keyword) args
;;    (lambda (label arg)
;;      (cond
;;       ((stringp arg)
;;        arg)
;;       ((symbolp arg)
;;        (symbol-name arg))
;;       (t
;;        (use-package-error
;;         ":pin wants an archive name (a string)"))))))

;;; TODO: add a key 'var' and automatically split between :config and :custom
(defmacro neo/use-package (name &rest args)
  "Augment 'use-package' with Neo specific functionality.

:doc adds a documentation string, mainly for reminding me of what packages do."
  (declare (indent defun))
  (let ((args (neo/filter-package-args args)))
    `(use-package
      ,name
      ;     :elpaca nil
      ,@args)))

; TODO make this more general
(defun neo/maybe-install-fonts ()
  (let ((font-dir
         (concat
          (or (getenv "XDG_DATA_HOME")
              (expand-file-name "~/.local/share"))
          "/fonts/")))
    (unless (file-exists-p (concat font-dir "all-the-icons.ttf"))
      (all-the-icons-install-fonts t))))


;;;-----------------------------------------------------------------------------------
;;; Packages

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory
  (expand-file-name "elpaca/" no-littering-etc-directory))
(defvar elpaca-builds-directory
  (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory
  (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
  '(elpaca
    :repo "https://github.com/progfolio/elpaca.git"
    :ref nil
    :files (:defaults (:exclude "extensions"))
    :build (:not elpaca--activate-package)))
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list
   'load-path
   (if (file-exists-p build)
       build
     repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28)
      (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer
                  (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop
                   (call-process "git"
                                 nil
                                 buffer
                                 t
                                 "clone"
                                 (plist-get order :repo)
                                 repo)))
                 ((zerop
                   (call-process "git"
                                 nil
                                 buffer
                                 t
                                 "checkout"
                                 (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop
                   (call-process
                    emacs
                    nil
                    buffer
                    nil
                    "-Q"
                    "-L"
                    "."
                    "--batch"
                    "--eval"
                    "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
          (progn
            (message "%s" (buffer-string))
            (kill-buffer buffer))
          (error
           "%s"
           (with-current-buffer buffer
             (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues 90)
(elpaca `(,@elpaca-order))

(elpaca
 elpaca-use-package
 ;; Enable :elpaca use-package keyword.
 (elpaca-use-package-mode)
 ;; Assume :elpaca t unless otherwise specified.
 (setq elpaca-use-package-by-default t))

;;; TODO: figure out where duplicate packages get queued
;;; till then is very annoying to see things like:
;;; ⛔ Warning (emacs): Duplicate item queued: ace-window
;;; ⛔ Warning (emacs): Duplicate item queued: magit
;;; ⛔ Warning (emacs): Duplicate item queued: org-roam
(setq warning-minimum-level :error)

;; Block until current queue processed.
(elpaca-wait)

(neo/use-package system-packages
  :config
  (setq system-packages-package-manager 'apt)
  (setq system-packages-use-sudo t)
  (setq system-packages-noconfirm t)
  (setq async-shell-command-buffer 'rename-buffer))

(neo/use-package use-package-ensure-system-package
  :after system-packages)

(neo/use-package use-package-chords
  :config (key-chord-mode 1))

;;;-----------------------------------------------------------------------------------
;;; Save Areas

(neo/use-package no-littering
  :init
  ;; We define these in early-init.el so everything can be kept out of the way
  ;; in particular elpaca and eln-cache
  ;(setq no-littering-etc-directory (expand-file-name "litter/config" user-emacs-directory))
  ;(setq no-littering-var-directory (expand-file-name "litter/data" user-emacs-directory))
  (setq custom-file
        (expand-file-name "custom.el" no-littering-var-directory))
  :config
  (setq auto-save-file-name-transforms
        `((".*"
           ,(no-littering-expand-var-file-name "auto-save/")
           t))))


(neo/use-package delight)

;; Block until current queue processed.
(elpaca-wait)

;;;-----------------------------------------------------------------------------------
;;; Misc settings

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq message-log-max 10000) ; I cannot have t as my Emacs stays on forever
(setq kill-whole-line t)
(setq visible-bell 1)
(setq initial-scratch-message "")
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq use-dialog-box nil)
(setq ring-bell-function 'ignore)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq custom-safe-themes t) ; not sure, I'll probaby use very few themes no need to trust 'em all
(setq scroll-conservatively 10000) ; not sure abut this one
(setq scroll-preserve-screen-position t)
(put 'narrow-to-region 'disabled nil)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(toggle-menu-bar-mode-from-frame -1)
(tooltip-mode -1)
(blink-cursor-mode 0)
(set-fringe-mode '(10 . 0))
(fset 'yes-or-no-p 'y-or-n-p)
;; The following is mainly for avoiding the
;; 'Symbolic link to Git-controlled source file; follow link? (y or n)'
;; question every time I get to a package source via find-library or
;; find-function. The question could be avoided with
;; (setq vc-follow-symlinks t)
;; but since we don't use VC at all, I take the nuclear option.
(setq vc-handled-backends nil)

(prefer-coding-system 'utf-8)
(setq sentence-end-double-space nil)

;; we want to create a directory when visiting a file that doesn't exist.
;; this solution has the problem of leaving an empty directory around if
;; we then decide not to save the file. Maybe would be better o create
;; the directory on save
(defun neo/ensure-parent-directories ()
  (make-directory (file-name-directory buffer-file-name) t))
(add-hook
 'find-file-not-found-functions #'neo/ensure-parent-directories)

(defvar neo/backup-directory
  (expand-file-name "backups" no-littering-var-directory))

(if (not (file-exists-p neo/backup-directory))
    (make-directory neo/backup-directory t))
(setq backup-directory-alist `(("." . ,neo/backup-directory)))
(setq
 make-backup-files t ; backup of a file the first time it is saved.
 backup-by-copying t ; don't clobber symlinks
 version-control t ; version numbers for backup files
 delete-old-versions t ; delete excess backup files silently
 delete-by-moving-to-trash t
 kept-old-versions 6 ; oldest versions to keep when a new numbered backup is made (default: 2)
 kept-new-versions 9 ; newest versions to keep when a new numbered backup is made (default: 2)
 auto-save-default t ; auto-save every buffer that visits a file
 auto-save-timeout 20 ; number of seconds idle time before auto-save (default: 30)
 auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
 )

(neo/use-package key-chord
  :elpaca nil
  :config
  (key-chord-define-global "``" 'toggle-menu-bar-mode-from-frame)
  (key-chord-define-global ".." 'comment-or-uncomment-region)
  (key-chord-define-global ",," 'sort-lines)
  (key-chord-define-global "//" 'align-regexp))

(global-auto-revert-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Write protect

;;; This is special and works for me. Shouldn't impact anybody else. Unless you're me
(defvar neo/monorepo-admin "mvitale")
(defvar neo/monorepo-path
  (expand-file-name (format "~/Projects/uno")))

(defun neo/monorepo ()
  ;  "Check if we're in the uno monorepo."
  ; not sure what this is doing
  (if (and (equal (user-login-name) neo/monorepo-admin)
           (file-directory-p neo/monorepo-path))
      neo/monorepo-path))

(defvar neo/protect-neo-files t) ; set to nil when really intentional, but DNL

(defun neo/neo-file-p (file)
  "Check if FILE is part of neo, in that case t else nil."
  (and neo/protect-neo-files
       (string-prefix-p "/home/mvitale/neo/" file)))

(defun neo/maybe-write-protect ()
  (if (and (neo/monorepo) (neo/neo-file-p (buffer-file-name))) ;
      (progn
        (setq buffer-read-only t))))

(add-hook 'find-file-hook #'neo/maybe-write-protect)

(defadvice read-only-mode
    (around neo/disable-toggle-readonly activate)
  (if (neo/neo-file-p (buffer-file-name))
      (message
       (format
        "You probably want to edit this file inside the monorepo at %s"
        neo/monorepo-path))
    ad-do-it))

;;; We don't want to edit ~/neo/init.el directly, but when we do edit
;;; ~/Projects/uno/devex/editors/emacs/init.el we copy it to
;;; ~/neo/init.el for testing purposes, as pushing to github, creating
;;; a PR, merging, syncying to the public repo with copybara and
;;; pulling is annoying. We don't touch the repository in ~/neo so
;;; that it is easy to revert.
(defun neo/copy-init-on-save ()
  "Copy init.el to where Emacs does normally look for it after saving."
  (when
      (and
       buffer-file-name ; Ensure the buffer is associated with a file.
       (file-exists-p buffer-file-name)
       (string=
        buffer-file-name
        "/home/mvitale/Projects/uno/devex/editors/emacs/init.el"))
    (let
        ((target-directory (expand-file-name "~/neo/"))) ; Set your target directory.
      (let ((target-path
             (concat
              target-directory
              (file-name-nondirectory buffer-file-name))))
        (copy-file buffer-file-name target-path t))))) ; 't' to overwrite if it exists.

(add-hook 'after-save-hook #'neo/copy-init-on-save)


;;;-----------------------------------------------------------------------------------
;;; Restart Emacs

;;; This is for 28.2  compatibility
;; (if (boundp 'restart-emacs)
;;     (defun neo/restart-emacs ()
;;       (restart-emacs)))
;; (neo/use-package restart-emacs
;;   :if (not (boundp 'restart-emacs))
;;   :config
;;   (defun neo/emacs-args ()
;;     (with-temp-buffer
;;       (insert-file-contents "/proc/self/cmdline")
;;       (split-string (buffer-string) "\0" t)))
;;   (defun neo/restart-emacs ()
;;     (restart-emacs (cdr (neo/emacs-args)))))
;;; End of 28.2 compatibility

;;; Notes:
;;; gh pr merge --auto --squash 39 make things that pass checks be automatically merged

(defun neo/sync-neo ()
  (message "We try to sync Emacs before restarting"))

(defun neo/restart-emacs-or-exit (arg)
  (interactive "p")
  (if (>= arg 16)
      (neo/sync-neo))
  (if (>= arg 4)
      (restart-emacs)
    (save-buffers-kill-emacs)))

(global-set-key (kbd "C-x C-c") 'neo/restart-emacs-or-exit)

;; (unless (server-running-p)
;;   (server-start))

;;;-----------------------------------------------------------------------------------
;;; History

(require 'desktop)

(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

(setq desktop-save t)
(setq neo/desktop-path
      (expand-file-name "desktop" no-littering-var-directory))
(setq desktop-path `(,neo/desktop-path))
(setq desktop-dirname neo/desktop-path)
(setq desktop-restore-eager 5)
(setq desktop-load-locked-desktop 'check-pid)

(defun neo/desktop-restore ()
  (desktop-read)
  (desktop-save-mode +1)
  (savehist-mode))
(add-hook 'after-init-hook 'neo/desktop-restore 99)

(neo/use-package undo-tree
  :delight undo-tree-mode
  :chords (("uu" . undo-tree-visualize)) ; the only place this annoys me is when I have to type 'uuid'
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  :config
  (let ((undo-save-dir
         (expand-file-name "undo" no-littering-var-directory)))
    (setq undo-tree-history-directory-alist
          `(("." . ,undo-save-dir)))))

;;;-----------------------------------------------------------------------------------
;;; Help

(neo/use-package which-key
  :delight which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(neo/use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command))

;;;-----------------------------------------------------------------------------------
;;; Completions

(neo/use-package vertico
  :doc "UI component for minibuffer selection"
  :elpaca
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
  :elpaca nil
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
  :doc "Show completions near the point"
  :elpaca
  (corfu
   :host github
   :repo "minad/corfu"
   :files (:defaults "extensions/*"))
  :hook (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil) ; Always show candidates in menu

  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)

  (corfu-min-width 40)
  (corfu-max-width 120)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)

  ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
  ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
  ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
  ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
  ;; configuration already has pre-prepared). Necessary for manual corfu usage with
  ;; orderless, otherwise first component is ignored, unless `corfu-separator'
  ;; is inserted.
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s) ; Use space
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  (corfu-preview-current 'insert) ; Preview first candidate. Insert on input if only one
  (corfu-preselect-first t) ; Preselect first candidate?

  ;; Other
  (lsp-completion-provider :none) ; Use corfu instead for lsp completions

  :config (global-corfu-mode)
  ;; (corfu-popupinfo-mode) TODO loading of extensions in :elpaca doesn't seem to work
  ;; Enable Corfu more generally for every minibuffer, as long as no other
  ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
  ;; completion UI. From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or
             (bound-and-true-p mct--active) ; Useful if I ever use MCT
             (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil) ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer
            1)

  ;; Setup lsp to use corfu for lsp completion
  (defun kb/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the
default lsp-passthrough."
    (setf (alist-get
           'styles
           (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))))

;; TODO this should be possible from the corfu use-package
(elpaca
 nil
 (progn
   (add-to-list
    'load-path
    (expand-file-name "elpaca/builds/corfu/extensions"
                      user-emacs-directory))
   (require 'corfu-popupinfo)
   (setq corfu-popupinfo-delay '(1.0 . 0.3))

   (corfu-popupinfo-mode)
   (require 'corfu-history)
   (corfu-history-mode)))

(neo/use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil) ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  ;  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ;; Add hook to reset cache so the icon colors match my theme
;; ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
;; ;; the theme using my custom defined command for switching themes. If I don't
;; ;; do this, then the backgound color will remain the same, meaning it will not
;; ;; match the background color corresponding to the current theme. Important
;; ;; since I have a light theme and dark theme I switch between. This has no
;; ;; function unless you use something similar
;; (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

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

;;;-----------------------------------------------------------------------------------
;;; Snippets

(neo/use-package yasnippet
  :doc "Manage snippets. Use yas-describe-tables to see what's available"
  :config
  (yas-global-mode t)
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
  :hook
  (emacs-startup
   .
   (lambda ()
     (yas-load-directory
      (expand-file-name "snippets" user-emacs-directory)))))

(neo/use-package yasnippet-snippets
  :doc "A library of snippets")

(defun neo/expand-snippet-in-buffer (snippet major-mode buffer))

(defun neo/expand-snippet (snippet)
  (unwind-protect
      (let ((buffer (get-buffer-create "*temp-snippet*"))
            (snippet-body (yas-lookup-snippet snippet 'org-mode)))
        (set-buffer buffer)
        (erase-buffer)
        (yas-minor-mode)
        (yas-expand-snippet snippet-body)
        (org-mode))))

;;;-----------------------------------------------------------------------------------
;;; Movement

(neo/use-package ace-jump-mode
  :chords
  (("jj" . ace-jump-char-mode)
   ("jk" . ace-jump-word-mode)
   ("jl" . ace-jump-line-mode)))

(neo/use-package ace-window
  :bind ("C-x o" . ace-window)
  :chords (("''" . ace-window))
  :custom-face
  ;; foreground should be computed from current theme, preserved the same way across restarts and
  ;; restored.
  ;; font is https://www.1001fonts.com/download/font/faster-one.regular.ttf
  (aw-leading-char-face
   ((t
     (:inherit
      ace-jump-face-foreground
      :font "FasterOne"
      :height 3.0
      :foreground "dark gray")))))


;;;-----------------------------------------------------------------------------------
;;; UI

;;; Note: some appearence settings happen in early-init.el in order to minimize flashing/flickering on startup.

;;; TODO: for some reason golden-ratio doesn't work, probably conflictin with something
;;; ace-window?
;;; TODO: there's a workaround below, but if for any reason golden-ratio is not defined (error elsewhere + elpaca)
;;; then emacs becoems basically unusable
;; (neo/use-package golden-ratio
;;   :after ace-window
;;   :diminish golden-ratio-mode
;;   :config
;;   (golden-ratio-mode 1)
;;   (add-to-list 'golden-ratio-extra-commands 'aw--callback)
;;   :custom
;;   ;  (golden-ratio-auto-scale t)
;;   (golden-ratio-exclude-modes
;;    '("calendar-mode" "help-mode" "helpful-mode")))

;; ;;; The following are workarounds for ace-window.
;; ;;; golden-ratio is essentially unmaintained. If even these workarounds stop working
;; ;;; we'll do without golden-ratio
;; (defvar golden-ratio-selected-window (frame-selected-window)
;;   "Selected window.")

;; (defun golden-ratio-set-selected-window (&optional window)
;;   "Set selected window to WINDOW."
;;   (setq-default golden-ratio-selected-window
;;                 (or window (frame-selected-window))))

;; (defun golden-ratio-selected-window-p (&optional window)
;;   "Return t if WINDOW is selected window."
;;   (eq
;;    (or window (selected-window))
;;    (default-value 'golden-ratio-selected-window)))

;; (defun golden-ratio-maybe (&optional arg)
;;   "Run `golden-ratio' if `golden-ratio-selected-window-p' returns nil."
;;   (interactive "p")
;;   (unless (golden-ratio-selected-window-p)
;;     (golden-ratio-set-selected-window)
;;     (golden-ratio arg)))

;; (add-hook 'buffer-list-update-hook #'golden-ratio-maybe)
;; (add-hook 'focus-in-hook #'golden-ratio)
;; (add-hook 'focus-out-hook #'golden-ratio)

(neo/use-package all-the-icons
  :config (neo/maybe-install-fonts))

(neo/use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

;;; give a bit more breathing room around windows
(modify-all-frames-parameters
 '((right-divider-width . 40) (internal-border-width . 40)))

;; TODO this slows down terribly switching and saving buffers, or so it seems; wasn't the case w/ my previous config
(dolist (buffer-re
         '("\\*compilation\\*"
           "\\*system-packages\\*"
           "\\*Warnings\\*"
           "\\*Messages\\*"))
  (add-to-list
   'display-buffer-alist
   `(,buffer-re display-buffer-no-window (allow-no-window . t))))

(add-to-list
 'display-buffer-alist
 '("\\*info\\*"
   (display-buffer-in-side-window)
   (side . left)
   (slot . 0)
   (window-width . 80)
   (window-parameters (no-delete-other-windows . t))))

(add-to-list
 'display-buffer-alist
 '((lambda (buffer alist)
     (with-current-buffer buffer
       (eq major-mode 'DocView)))
   (display-buffer-in-side-window)
   (side . right)
   (slot . 0)
   (window-width . 80)
   (window-parameters (no-delete-other-windows . t))))

(add-to-list
 'display-buffer-alist
 '("\\*e?shell\\*"
   display-buffer-in-direction
   (direction . bottom)
   (window . root)
   (window-height . 0.3)))

(defvar neo/title "N E O   E M A C S")

(setq frame-title-format `(,(format "%s    [%%b]" neo/title)))
(setq icon-title-format frame-title-format)

(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'org-mode-hook #'hl-line-mode)

(if (version< emacs-version "29.0")
    (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0))

(neo/use-package popper
  :doc "Treat some buffers as 'popup' so that they stay out of the way"
  :bind
  (("C-`" . popper-toggle-latest)
   ("M-`" . popper-cycle)
   ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)) ; For echo area hints

(neo/use-package emojify
  :custom (emojify-emojis-dir (neo/litter-directory "emojis" emacs-version))
  :config
  (add-to-list 'emojify-inhibit-major-modes 'magit-status-mode)
  (if (display-graphic-p)
      (setq emojify-display-style 'image)
    (setq emojify-display-style 'unicode))
  (setq emojify-emoji-set "emojione-v2.2.6")
  ;               (setq emojify-emoji-set "openmoji-v13-0")
  :init (global-emojify-mode 1))

(neo/use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(neo/use-package rainbow-mode
  :config (rainbow-mode))

;;; TODO: not sure this still work
(neo/use-package centered-window
  :custom (cwm-centered-window-width 100))

(neo/use-package emacs
  :elpaca nil
  :hook
  (prog-mode
   .
   (lambda ()
     (setq glasses-separator "")
     (setq glasses-original-separator "")
     (setq glasses-face 'extra-bold)
     (which-function-mode)
     (subword-mode)
     (glasses-mode))))

(add-hook
 'prog-mode-hook
 (lambda ()
   (setq glasses-separator "")
   (setq glasses-face 'bold)
   (glasses-mode)))

(neo/use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq
     treemacs-collapse-dirs
     (if treemacs-python-executable
         3
       0)
     treemacs-deferred-git-apply-delay 0.5
     treemacs-directory-name-transformer #'identity
     treemacs-display-in-side-window t
     treemacs-eldoc-display 'simple
     treemacs-file-event-delay 2000
     treemacs-file-extension-regex treemacs-last-period-regex-value
     treemacs-file-follow-delay 0.2
     treemacs-file-name-transformer #'identity
     treemacs-follow-after-init t
     treemacs-expand-after-init t
     treemacs-find-workspace-method 'find-for-file-or-pick-first
     treemacs-git-command-pipe ""
     treemacs-goto-tag-strategy 'refetch-index
     treemacs-header-scroll-indicators '(nil . "^^^^^^")
     treemacs-hide-dot-git-directory t
     treemacs-indentation 2
     treemacs-indentation-string " "
     treemacs-is-never-other-window nil
     treemacs-max-git-entries 5000
     treemacs-missing-project-action 'ask
     treemacs-move-forward-on-expand nil
     treemacs-no-png-images nil
     treemacs-no-delete-other-windows t
     treemacs-project-follow-cleanup nil
     treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
     treemacs-position 'left
     treemacs-read-string-input 'from-child-frame
     treemacs-recenter-distance 0.1
     treemacs-recenter-after-file-follow nil
     treemacs-recenter-after-tag-follow nil
     treemacs-recenter-after-project-jump 'always
     treemacs-recenter-after-project-expand 'on-distance
     treemacs-litter-directories '("/node_modules" "/.venv" "/.cask")
     treemacs-project-follow-into-home nil
     treemacs-show-cursor nil
     treemacs-show-hidden-files t
     treemacs-silent-filewatch nil
     treemacs-silent-refresh nil
     treemacs-sorting 'alphabetic-asc
     treemacs-select-when-already-in-treemacs 'move-back
     treemacs-space-between-root-nodes t
     treemacs-tag-follow-cleanup t
     treemacs-tag-follow-delay 1.5
     treemacs-text-scale nil
     treemacs-user-mode-line-format nil
     treemacs-user-header-line-format nil
     treemacs-wide-toggle-width 70
     treemacs-width 35
     treemacs-width-increment 1
     treemacs-width-is-initially-locked t
     treemacs-workspace-switch-cleanup nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons
            (not (null (executable-find "git")))
            (not (null treemacs-python-executable)))
      (`(t . t) (treemacs-git-mode 'deferred))
      (`(t . _) (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :hook ((treemacs-mode . (lambda () (setq-local mode-line-format nil))))
  :bind
  (:map
   global-map
   ("M-0" . treemacs-select-window)
   ("C-x t 1" . treemacs-delete-other-windows)
   ("C-x t t" . treemacs)
   ("C-x t d" . treemacs-select-directory)
   ("C-x t B" . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag)))

;(use-package treemacs-projectile
;  :after (treemacs projectile)
;  :ensure t)

(neo/use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(neo/use-package treemacs-magit
  :after (treemacs magit))

(neo/use-package
  treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

(neo/use-package
  treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

(add-hook 'helpful-mode-hook (lambda () (set-window-margins nil 2 2)))
(add-hook 'Info-mode-hook (lambda () (set-window-margins nil 2 2)))

(neo/use-package hl-todo)


;;; Fonts


(defun neo/display-geometry ()
  (mapcar
   #'string-to-number
   (split-string (shell-command-to-string command) " " t "[ ]+")))

(defun neo/set-fonts ()
  ;;; height here is in 1/10 of a pt. 10pt is 3.528mm. Not much make sense here. My laptop has ~266dpi in both X and Y.
  (set-face-attribute 'default nil
                      :font "Ubuntu Mono"
                      :height 100
                      :weight 'regular)
  (set-face-attribute 'font-lock-comment-face nil
                      :font "Chivo Mono"
                      :height 1.0
                      :weight 'regular
                      :slant 'italic)
  (set-face-attribute 'variable-pitch nil :family "Linux Libertine") ; Ubuntu ans ET Book
  )

(add-hook 'after-init-hook #'neo/set-fonts 100)


;;; Themes

(neo/use-package blackboard-theme)
(neo/use-package doom-themes)
(neo/use-package dracula-theme)
(neo/use-package ef-themes)
(neo/use-package modus-themes
  :config
  (setq modus-themes-syntax
        (list 'yellow-comments 'green-strings 'alt-syntax))
  (setq modus-themes-org-blocks 'tinted-background))
(neo/use-package molokai-theme)
(neo/use-package twilight-theme)
(neo/use-package nordic-night-theme
  :elpaca (nordic-night :host sourcehut :repo "ashton314/nordic-night"))

;(elpaca-wait)

(defvar neo/current-theme 'doom-tomorrow-day
  "Theme applied")
(push 'neo/current-theme desktop-globals-to-save)

(defun neo/load-theme ()
  (interactive)
  (neo/load-theme-internal
   (completing-read
    "Load custom theme: "
    (mapcar 'symbol-name (custom-available-themes)))))

;;; need to do something for powerline and other modelines
(defun neo/load-theme-action (x)
  (condition-case nil
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme-internal (intern x) t))
    (error "Problem loading theme %s" x)))

(defun neo/update-x-defaults ()
  (with-temp-file "~/.Xdefaults"
    (let ((foreground (face-attribute 'default :foreground))
          (background (face-attribute 'default :background)))
      (insert "Emacs.fullscreen: maximized\n")
      (insert (format "Emacs.title: %s\n" neo/title))
      (insert (format "Emacs.background: %s\n" background))
      (insert (format "Emacs.foreground: %s\n" foreground))))
  (call-process-shell-command "xrdb -load ~/.Xdefaults" nil 0))

(defun neo/load-theme-internal (theme)
  (let ((theme
         (if (stringp theme)
             (intern theme)
           theme)))
    (message "Loading theme %s (disabling others)" theme)
    (mapc #'disable-theme custom-enabled-themes)
    (setq neo/current-theme theme)
    (load-theme theme t)
    (neo/set-divider-faces)
    (neo/update-x-defaults)
    ;; We should really change the foreground only when org-hide-leading-stars is t.
    ;; For me this is always true
    ))

;      (set-face-foreground 'org-superstar-leading (face-attribute 'default :background))
;      (set-face-foreground 'org-hide (face-attribute 'default :background))))

(defun neo/set-divider-faces ()
  ;; NOTE: the face names are for some reason displayed using the face itself.
  ;; so if you see nothing in the dolist below is because we set the foreground to match the
  ;; default background. This makes space between windows nicer and code here invisible
  ;; Toggle highlight-quoted-mode to see the actual names.
  ;; TODO: highlight-quoted-mode doesn't seem to be who does this.
  (dolist (face
           '(window-divider
             window-divider-first-pixel window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))

  (set-face-background 'fringe (face-attribute 'default :background)))

(add-hook
 'desktop-after-read-hook
 (lambda () (neo/load-theme-internal neo/current-theme)))

;;; Misc

(neo/use-package page-break-lines
  :delight
  :config (global-page-break-lines-mode))

(neo/use-package svg-tag-mode)

(neo/use-package diminish)

;;;-----------------------------------------------------------------------------------
;;; Help

(defun neo/help-mode-faces (background)
  "Buffer-local face remapping for help buffers."
  (face-remap-add-relative
   'default
   :background (face-attribute 'hl-line :background)))

(add-hook
 'Info-mode-hook (lambda () (neo/help-mode-faces "light steel blue")))
(add-hook 'help-mode-hook (lambda () (neo/help-mode-faces "thistle")))
(add-hook
 'helpful-mode-hook (lambda () (neo/help-mode-faces "thistle")))

;; not really from here`
(add-hook 'magit-mode-hook (lambda () (neo/help-mode-faces "azure1")))

;;;-----------------------------------------------------------------------------------
;;; Dev/General

(neo/use-package flycheck
  :init (global-flycheck-mode))

;;; Haven't managed to get elpaca-after-init-hook to work. Here we use the normal
;;; after init hook, which is also use by elpaca to execute its queues (and supposedly
;;; run elpaca-after-init-hook); because of this we use '100' to run this after
;;; elpacs does its things.
(add-hook 'after-init-hook
          (lambda ()
            (setq flycheck-highlighting-mode 'lines)
            (set-face-attribute 'flycheck-info nil
                                :background "#A0D0A0"
                                :underline nil)
            (set-face-attribute 'flycheck-error nil
                                :background "#FF9999"
                                :underline nil)
            (set-face-attribute 'flycheck-warning nil
                                :background "#FFFF99"
                                :underline nil))
          100)

(neo/use-package flycheck-clang-analyzer
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(neo/use-package flycheck-status-emoji)

(neo/use-package bug-reference-github
  :init
  (setq
   bug-reference-bug-regexp
   "\\(\\b\\(?:[Bb]ug ?#?\\|[Pp]atch ?#\\|[Ii]ssue ?#?\\|RFE ?#\\|PR [a-z+-]+/\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)\\)")
  :hook (prog-mode . bug-reference-github-set-url-format))

(neo/use-package eglot
  ; clangd-17 not installed through package manager
  :ensure-system-package (python3-pylsp clangd-15)
  :config
  (message "eglot config")
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd-17")))
  (add-to-list 'eglot-server-programs '(c++-ts-mode . ("clangd-17")))

  (setq-default eglot-workspace-configuration
                '((:pylsp
                   .
                   (:configurationSources
                    ["flake8"]
                    :plugins
                    (:pycodestyle
                     (:enabled nil)
                     :mccabe (:enabled nil)
                     :flake8 (:enabled t))))
                  ;		  (:gopls . (:verboseOutput t))
                  ))
  :custom (eglot-autoshutdown t)
  :bind
  (("C-c l c" . eglot-reconnect)
   ("C-c l d" . flymake-show-buffer-diagnostics)
   ("C-c l f f" . eglot-format)
   ("C-c l f b" . eglot-format-buffer)
   ("C-c l l" . eglot)
   ("C-c l r n" . eglot-rename)
   ("C-c l s" . eglot-shutdowmen))
  :hook
  ((python-mode . eglot-ensure)
   (haskell-mode . eglot-ensure)
   (go-mode . eglot-ensure)
   (go-ts-mode . eglot-ensure)
   (c++-ts-mode . eglot-ensure)
   (c++-mode . eglot-ensure)))

(neo/use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1)
  (message "Flycheck-eglot config. Why we show this"))

(defun eglot-organize-imports ()
  "Offer to execute the source.organizeImports code action."
  (interactive)
  (unless (eglot--server-capable :codeActionProvider)
    (eglot--error "Server can't execute code actions!"))
  (let* ((server (eglot--current-server-or-lose))
         (actions
          (jsonrpc-request
           server
           :textDocument/codeAction
           (list :textDocument (eglot--TextDocumentIdentifier))))
         (action
          (cl-find-if
           (jsonrpc-lambda
            (&key kind &allow-other-keys)
            (string-equal kind "source.organizeImports"))
           actions)))
    (when action
      (eglot--dcase
       action
       (((Command) command arguments)
        (eglot-execute-command server (intern command) arguments))
       (((CodeAction) edit command)
        (when edit
          (eglot--apply-workspace-edit edit))
        (when command
          (eglot--dbind
           ((Command) command arguments) command
           (eglot-execute-command
            server (intern command) arguments))))))))

;; (defun eglot-organize-imports-on-save ()
;;   (defun eglot-organize-imports-nosignal ()
;;     "Run eglot-organize-imports, but demote errors to messages."
;;     ;; Demote errors to work around
;;     ;; https://github.com/joaotavora/eglot/issues/411#issuecomment-749305401
;;     ;; so that we do not prevent subsequent save hooks from running
;;     ;; if we encounter a spurious error.
;;     (with-demoted-errors "Error: %s" (eglot-organize-imports)))
;;   (add-hook 'before-save-hook #'eglot-organize-imports-on-save))

;; (add-hook 'go-mode-hook #'eglot-organize-imports-on-save)

;;(defun neo/eglot-organize-imports () (interactive)
;;       (eglot-code-actions nil nil "source.organizeImports" t))
;;(add-hook 'before-save-hook 'neo/eglot-organize-imports nil t)
;;(add-hook 'before-save-hook 'eglot-format-buffer)

(defun neo/eglot-organize-imports-on-save ()
  (add-hook
   'before-save-hook (lambda () (message "Global before save hook")))
  (add-hook 'before-save-hook #'eglot-organize-imports-on-save)
  (add-hook 'before-save-hook 'eglot-format-buffer))

(add-hook 'go-mode-hook #'neo/eglot-organize-imports-on-save)

(defvar neo/treesit-grammar-dir
  (expand-file-name "tree-sitter/" no-littering-var-directory))
(make-directory neo/treesit-grammar-dir t)

(setq treesit-extra-load-path (list neo/treesit-grammar-dir))

;;; treesit for now hardcode outdir to nil which results in grammars to be placed in user-emacs-directory/tree-sitter
;;; we want grammar in .litter directories, hence this piece of advice.
(defun neo/treesit-add-outdir (orig-fun program &rest args)
  (apply orig-fun neo/treesit-grammar-dir args))
(advice-add
 'treesit--install-language-grammar-1
 :around #'neo/treesit-add-outdir)

(setq treesit-font-lock-level 4)

(neo/use-package treesit-auto
  :after treesit
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode))

(neo/use-package treesit
  :elpaca nil
  :ensure nil
  :config
  (setq treesit-extra-load-path
        (list (no-littering-expand-var-file-name "tree-sitter")))
  ;; (push '(css-mode . css-ts-mode) major-mode-remap-alist)
  ;; (push '(python-mode . python-ts-mode) major-mode-remap-alist)
  ;; (push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
  ;; (push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
  ;; (push '(typescript-mode . typescript-ts-mode) major-mode-remap-alist)
  ;; (push '(c-mode . c-ts-mode) major-mode-remap-alist)
  ;; (push '(go-mode . go-ts-mode) major-mode-remap-alist)
  ;; (push '(c++-mode . c++-ts-mode) major-mode-remap-alist)
  )

;;; TODO: Doesn't seem to do anything, but I don't have a C++ code base w/ comments.
(neo/use-package doc-show-inline
  :commands (doc-show-inline-mode)

  :config (define-key c++-mode-map (kbd "C-;") 'doc-show-inline-mode)

  :hook (c++-mode . doc-show-inline-mode))

(neo/use-package copilot
  :elpaca (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el")))

;;;-----------------------------------------------------------------------------------
;;; Dev/Project

(require 'project)

(defun neo/project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'neo/project-find-go-module)

(neo/use-package perspective)

;;;-----------------------------------------------------------------------------------
;;; Dev/documentation
(neo/use-package devdocs
  :hook
  ((python-mode
    .
    (lambda () (setq-local devdocs-current-docs '("python~3.10"))))
   (go-mode . (lambda () (setq-local devdocs-current-docs '("go"))))
   (c++-mode
    . (lambda () (setq-local devdocs-current-docs '("cpp")))))
  :bind (("C-h D" . #'devdocs-lookup)))

;;;-----------------------------------------------------------------------------------
;;; Dev/Version Control

(neo/use-package magit
  :config (setq magit-save-repository-buffers 'dontask)
  :custom
  (magit-list-refs-sortby "-creatordate") ; doesn't seem to have any effect
  (magit-refs-show-commit-count 'branch) ; may be too expsive
  (magit-completing-read-function 'magit-builtin-completing-read)
  (git-commit-summary-max-length 50)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

  :init
  ;; NOTE: order of functions in this hook is important, we make this clear with setq
  ;; instead of gambling with add-hook. There's magit-add-section-hook that might be better.
  ;; TODO: find the right place for magit-insert-branch-description (might also be
  ;; useful in magit-refs-sections-hook)
  (setq magit-status-sections-hook
        '(magit-insert-status-headers
          magit-insert-merge-log
          magit-insert-rebase-sequence
          magit-insert-am-sequence
          magit-insert-sequencer-sequence
          magit-insert-bisect-output
          magit-insert-bisect-rest
          magit-insert-bisect-log
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          magit-insert-stashes
          magit-insert-unpushed-to-pushremote
          magit-insert-unpushed-to-upstream-or-recent
          magit-insert-unpulled-from-pushremote
          magit-insert-unpulled-from-upstream
          magit-insert-modules
          magit-insert-local-branches))
  :chords (("`g" . magit-status))
  :bind
  ("<f12> s" . 'magit-status)
  ("<f12> g" . 'counsel-git-grep))


(neo/use-package forge
  :after magit
  :config
  (setq forge-topic-list-columns
        '(("#" 5 forge-topic-list-sort-by-number
           ;           (lambda (a b) (> (car a) (car b)))
           (:right-align t) number nil)
          ("Title" 40 t nil title nil)
          ("Milestone" 10 t nil milestone nil)
          ("State" 10 t nil state nil)
          ("Updated" 10 t nil updated nil)))
  (add-to-list
   'forge-alist
   '("kaspar4.github.com"
     "api.github.com"
     "github.com"
     forge-github-repository)))

;;; still problems with this dude, but since it only triggers when 'Fixes #' is inserted,
;;; for now we keep it around
(neo/use-package git-commit-insert-issue
  :hook (git-commit-mode . git-commit-insert-issue-mode))

(neo/use-package git-timemachine)

(neo/use-package git-gutter
  :hook
  ((markdown-mode . git-gutter-mode)
   (prog-mode . git-gutter-mode)
   (conf-mode . git-gutter-mode))
  :config
  (setq
   git-gutter:disabled-modes '(org-mode asm-mode image-mode)
   git-gutter:update-interval 1
   git-gutter:window-width 2
   git-gutter:ask-p nil))

(neo/use-package git-gutter-fringe
  :diminish git-gutter-mode
  :after git-gutter
  :demand fringe-helper
  :config
  ;; subtle diff indicators in the fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added
    [224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224
     224]
    nil nil 'center)
  (define-fringe-bitmap
    'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil
    nil
    'center))

;;;-----------------------------------------------------------------------------------
;;; Dev/Languages/Elisp

(neo/use-package bug-hunter)

(neo/use-package macrostep)

;;; Some buffers don't work well with hilight-defined.
;;; One case is the help-mode buffer produced by list-faces-display, so we
;;; skip those
(defun neo/maybe-highlight-defined ()
  (cond
   ((not (string= (buffer-name) "*Faces*"))
    (highlight-defined-mode))))

(neo/use-package highlight-defined
  :custom (highlight-defined-face-use-itself t)
  :hook
  (help-mode . neo/maybe-highlight-defined)
  (emacs-lisp-mode . highlight-defined-mode))

(neo/use-package highlight-quoted
  :doc "Package that, among other things, displays face names using the face itself TODO: doesn't seem true"
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(neo/use-package lispy)

(defun neo/eval-or-inspect-expression (arg)
  "Like `eval-expression', but also inspect when called with prefix ARG."
  (interactive "P")
  (pcase arg
    ('(4)
     (let ((current-prefix-arg nil))
       (call-interactively #'inspector-inspect-expression)))
    (_ (call-interactively #'eval-expression))))

(defun neo/eval-or-inspect-last-sexp (arg)
  "Like `eval-last-sexp', but also inspect when called with prefix ARG."
  (interactive "P")
  (pcase arg
    ('(4) (inspector-inspect-last-sexp))
    (_ (call-interactively #'eval-last-sexp))))

(neo/use-package inspector
  :config
  (progn
    (define-key
     global-map
     [remap eval-last-sexp]
     #'neo/eval-or-inspect-last-sexp)
    (define-key
     global-map
     [remap eval-expression]
     #'neo/eval-or-inspect-expression)))

(neo/use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

;;;-----------------------------------------------------------------------------------
;;; Dev/Languages/Rust

;;; TODO: doesn't seem we are setting the hook properly
(neo/use-package rust-mode
  :custom (rust-format-on-save t)
  :hook (rust-mode . (lambda () (message "Rust mode hook"))))
;; (rust-mode
;;  .
;;  (lambda ()
;;    (eglot-ensure)
;;    (add-hook 'before-save-hook #'eglot-format))))

;;;-----------------------------------------------------------------------------------
;;; Dev/Languages/C++

(neo/use-package clang-format)
(neo/use-package clang-format+
  :after clang-format
  :hook (c-mode-common . clang-format+-mode))

;;;-----------------------------------------------------------------------------------
;;; Dev/Languages/Bazel

(neo/use-package bazel)

;;;-----------------------------------------------------------------------------------
;;; Dev/Languages/Ansible

(neo/use-package ansible)
(neo/use-package ansible-doc)
(neo/use-package jinja2-mode)
(neo/use-package yaml-mode)

;;;-----------------------------------------------------------------------------------
;;; Dev/Languages/Terraform

(neo/use-package terraform-mode
  :custom (terraform-indent-level 4)
  :config
  (defun terraform-mode-init ()
    (outline-minor-mode 1))
  :hook (terraform-mode . terraform-mode-init))

;;;-----------------------------------------------------------------------------------
;;; Dev/Languages/Protobuf

(neo/use-package protobuf-mode)

;;;-----------------------------------------------------------------------------------
;;; Dev/Languages/Go

(defun neo/setup_go ()
  (eglot-ensure)
  (setq tab-width 4)
  (setq indent-tabs-mode 1)
  (add-hook 'before-save-hook 'gofmt-before-save))

(neo/use-package go-mode
  :ensure-system-package gopls
  :ensure-system-package golang-golang-x-tools
  :config
  (setq gofmt-command "goimports")
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  :hook ((go-mode . neo/setup_go)))

(setq-default eglot-workspace-configuration
              '((:gopls
                 .
                 ((staticcheck . t) (matcher . "CaseSensitive")))))


;;; requires go install  github.com/cweill/gotests/...@latest
(neo/use-package go-gen-test)


(neo/use-package go-add-tags)

;;; requires go install  github.com/cweill/gotests/...@latest
(neo/use-package go-tag)

;;; requires go install github.com/davidrjenni/reftools/cmd/fillstruct@latest~
(neo/use-package go-fill-struct)

;;; TODO: this cannot be the right way. If I had more than one
;;; project, which I don't, I would be doomed
(setenv "GOPACKAGESDRIVER"
        "/home/mvitale/Projects/uno/tools/gopackagesdriver.sh")

;;;-----------------------------------------------------------------------------------
;;; Dev/Languages/Haskell

(neo/use-package haskell-mode
  :config
  ;; so eglot can find haskell-language-server-wrapper
  (add-to-list 'exec-path "/home/user/.ghcup/bin")
  ;; so HLS can find ghc, cabal, and stack
  (setenv "PATH" (concat "/home/user/.ghcup/bin:" (getenv "PATH")))
  ;; start eglot session when in a haskell-mode-buffer
  (add-hook 'haskell-mode-hook 'eglot-ensure))

;;;-----------------------------------------------------------------------------------
;;; Dev/Languages/Latex

(defvar neo/latex-nofill-env
  '("equation"
    "equation*"
    "align"
    "align*"
    "tabular"
    "tabular*"
    "tabu"
    "tabu*"
    "tikzpicture"))

(defun neo/autofill ()
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill
                (not (string= current-environment "document")))
      (setq
       level (1+ level)
       current-environment (LaTeX-current-environment level)
       do-auto-fill (not (member current-environment neo/latex-nofill-env))))
    (when do-auto-fill
      (do-auto-fill))))

(defun neo/auto-fill-mode ()
  (interactive)
  (auto-fill-mode)
  (setq auto-fill-function 'neo/autofill))

;; Oh boy, this is something. Thanks god we could piggy back on the straight mirror. And still it was not obvious
(neo/use-package tex
  :elpaca (tex :host github :repo "emacs-straight/auctex")
  :ensure auctex
  :hook ((LaTeX-mode . #'latex-preview-pane-mode))
  :config
  (add-to-list
   'TeX-command-list
   '("LaTeX Shell Escape"
     "%`%l -shell-escape %(mode)%' %t"
     TeX-run-TeX
     nil
     t)))

;; :hook
;; ((LaTeX-mode . #'neo/auto-fill-mode)
;;  (LaTeX-mode . #'LaTeX-math-mode)
;;  (LaTeX-mode . #'TeX-source-correlate-mode)
;;  (LaTeX-mode . #'TeX-fold-mode)
;;  (LaTeX-mode . #'TeX-PDF-mode)
;;  (LaTeX-mode . #'latex-preview-pane-mode)
;;  (LaTeX-mode . #'turn-on-prettify-symbols-mode)
;;  (LaTeX-mode . #'turn-on-flyspell)
;;  (LaTeX-section . #'LaTeX-section-label)
;;  (LaTeX-mode . (function turn-on-reftex))))

(neo/use-package latex-preview-pane
  :config
  (setq pdf-latex-command "xelatex")
  (setq shell-escape-mode "-shell-escape")
  (setq
   preview-LaTeX-command
   '("%`%l -shell-escape \"\\nonstopmode\\nofiles\\PassOptionsToPackage{"
     ("," . preview-required-option-list)
     "}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined"
     preview-default-preamble
     "\\fi}\"%' \"\\detokenize{\" %(t-filename-only) \"}\""))
  (latex-preview-pane-enable))

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

;;;-----------------------------------------------------------------------------------
;;; Dev/Languages/Typescript

(neo/use-package typescript-mode)

(neo/use-package tide
  :after (flycheck)
  :hook
  ((typescript-ts-mode . tide-setup)
   (tsx-ts-mode . tide-setup)
   (typescript-ts-mode . tide-hl-identifier-mode)
   (before-save . tide-format-before-save)))

;;;-----------------------------------------------------------------------------------
;;; App/eat
;; (neo/use-package eat
;;   :init
;;   (defface eat-term-font-0 '((t :family "MesloLGM NF"))
;;     "Eat default face"
;;     :group 'neo-faces)
;;   :hook
;;   ((eat-mode
;;     .
;;     (lambda ()
;;       ;;      (set-face-attribute 'eat-term-font-0 nil :family "MesloLGM NF")
;;       ;; (face-remap-add-relative
;;       ;;  'eat-term-font-0
;;       ;;
;;       ;;  :foreground "#ffffff"
;;       ;;  :background "#000000")
;;       (face-remap-add-relative
;;        'default
;;        :family "MesloLGM NF"
;;        :foreground "#ffffff"
;;        :background "#000000")
;;       (face-remap-add-relative
;;        'fringe
;;        :foreground "#ffffff"
;;        :background "#000000")))))

(use-package
 eat
 :custom (eat-default-cursor-type '((hbar 1) nil nil))

 :custom-face (eat-term-font-0 ((t (:family "DejaVu Sans Mono"))))
 :config (setq eat-kill-buffer-on-exit t) (setq eat-enable-mouse t))

;; :custom
;; (eat-term-name "neo term")
;; (eat-term-inside-emacs "vterm")
;; :custom-face (eat-term-font-0 ((t (:family "DejaVu Sans Mono"))))
;; :config
;; (setq eat-kill-buffer-on-exit t)
;; (setq eat-enable-mouse t))
;(evil-set-initial-state 'eat-mode 'emacs))

;(eat-eshell-mode) (eat-eshell-visual-command-mode))

;;;-----------------------------------------------------------------------------------
;;; App/copilot

;;; TODO: investigate login
(neo/use-package copilot
  :elpaca (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)


;;;-----------------------------------------------------------------------------------
;;; App/Dashboard

(defun neo/setup-logo ()
  (let ((image
         (expand-file-name "assets/hacker.png" user-emacs-directory)))
    (if (file-readable-p image)
        (progn
          (setq dashboard-startup-banner image)
          (setq dashboard-banner-logo-title
                "W   E      A   R   E      L   E   G   I   O   N   S")
          (set-face-attribute 'dashboard-banner-logo-title nil
                              :font "Orbitron"
                              :height 200
                              :weight 'bold
                              :foreground "#196DB5"))
      (setq dashboard-startup-banner 'logo)
      (setq dashboard-banner-logo-title "Welcome to Emacs Neo"))))

(neo/use-package dashboard-hackernews
  :elpaca
  (dashboard-hackernews
   :host github
   :repo "hyakt/emacs-dashboard-hackernews")
  :config (require 'json))


;; TODO: no idea why this doesn't work. The hook seems invoked in the right buffer.
(defun neo/clear-cursor ()
  (with-current-buffer "*dashboard*"
    (setq cursor-type nil)))

(defun neo/dashboard-setup-startup-hook ()
  (add-hook 'window-setup-hook
            (lambda ()
              ;; 100 means `dashboard-resize-on-hook' will run last
              (add-hook
               'window-size-change-functions 'dashboard-resize-on-hook
               100)
              (dashboard-resize-on-hook))
            100)
  (add-hook 'after-init-hook
            (lambda ()
              ;; Display useful lists of items
              (dashboard-insert-startupify-lists))
            100)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (switch-to-buffer dashboard-buffer-name)
              (goto-char (point-min))
              (redisplay)
              (run-hooks 'dashboard-after-initialize-hook))
            100))

(defun neo/fortune ()
  (when (executable-find "fortune")
    (with-temp-buffer
      (shell-command "fortune" t)
      (concat (buffer-string) "\n"))))

(neo/use-package dashboard
  :after (dashboard-hackernews page-break-lines)
  :delight (dashboard-mode page-break-lines-mode)
  :init (setq dashboard-icon-type 'all-the-icons)
  ;  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  :config
  (setq dashboard-center-content t)
  (setq dashboard-set-init-info t)
  (setq dashboard-set-navigator t)
  (set-face-attribute 'dashboard-heading nil
                      :height 1.1
                      :weight 'bold)
  ; (dashboard-modify-heading-icons '((recents . "nf-oct-file_text")
  ;                                  (bookmarks . "nf-oct-book")))
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents . 5) (bookmarks . 5) (agenda . 5)))
  ;                        (hackernews .10)))
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        `( ;; line1
          ((,(all-the-icons-octicon
              "mark-github"
              :height 1.1
              :v-adjust 0.0)
            " U N O" "Browse homepage"
            (lambda (&rest _)
              (browse-url "https://github.com/kaspar4/uno")))
           (,(all-the-icons-octicon
              "mark-github"
              :height 1.1
              :v-adjust 0.0)
            " N E O" "Browse homepage"
            (lambda (&rest _)
              (browse-url "https://github.com/kaspar4/neo")))
           ("★"
            "Star"
            "Show stars"
            (lambda (&rest _) (show-stars))
            warning)
           ("?" "" "?/h" #'show-help nil "<" ">"))
          ;; line 2
          ((,(all-the-icons-faicon
              "linkedin"
              :height 1.1
              :v-adjust 0.0)
            "Linkedin" ""
            (lambda (&rest _)
              (browse-url "https://www.linkedin.com/feed/")))
           ("⚑"
            nil
            "Show flags"
            (lambda (&rest _) (message "flag"))
            error))))
  (setq dashboard-footer-messages (list (neo/fortune)))

  (neo/setup-logo)
  (setq initial-buffer-choice
        #'(lambda () (get-buffer-create "*dashboard*")))
  (neo/dashboard-setup-startup-hook)
  :hook ((dashboard-after-initialize . neo/clear-cursor)))

;;;-----------------------------------------------------------------------------------
;;; App/Citations
;;;
;;; not sure this should count as an 'appplication'

;; (neo/use-package citar
;;   :custom (citar-bibliography '("~/bib/references.bib"))
;;   :hook
;;   (LaTeX-mode . citar-capf-setup)
;;   (org-mode . citar-capf-setup))

;; (neo/use-package citar-org-roam
;;   :after (citar org-roam)
;;   :config (citar-org-roam-mode))

;;;-----------------------------------------------------------------------------------
;;; App/Org

;;; org-mdern is convenient, but
;;; TODO: we probably be better served by having our own open configuration
(neo/use-package org-modern
  :custom (org-startup-with-inline-images t)
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  (org-babel-after-execute . org-redisplay-inline-images)
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode))

(neo/use-package org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

;; (neo/use-package org-modern
;;   :config
;;   (setq org-startup-with-inline-images t)
;;   (add-to-list
;;    'ispell-skip-region-alist
;;    '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
;;   (add-to-list
;;    'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
;;   (add-to-list
;;    'ispell-skip-region-alist
;;    '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
;;   (setq
;;    org-capture-templates
;;    `(("p"
;;       "Protocol"
;;       entry
;;       (file+headline ,(concat org-directory "notes.org") "Inbox")
;;       "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
;;      ("L"
;;       "Protocol Link"
;;       entry
;;       (file+headline ,(concat org-directory "notes.org") "Inbox")
;;       "* %? [[%:link][%:description]] \nCaptured On: %U")))
;;   :hook
;;   (org-babel-after-execute . org-redisplay-inline-images)
;;   (org-mode . variable-pitch-mode)
;;   (org-mode . visual-line-mode))

;; ;;; org-capture
;; ;;
;; ;; in ~/.local/share/applications/org-protocol.desktop:
;; ;; [Desktop Entry]
;; ;; Name=org-protocol
;; ;; Exec=emacsclient %u
;; ;; Type=Application
;; ;; Terminal=false
;; ;; Categories=System;
;; ;; MimeType=x-scheme-handler/org-protocol;
;; ;;
;; ;; Run update-desktop-database ~/.local/share/applications/
;; ;;
;; ;; sudo mkdir -p /etc/opt/chrome/policies/managed/
;; ;; sudo tee /etc/opt/chrome/policies/managed/external_protocol_dialog.json >/dev/null <<'EOF'
;; ;; {
;; ;;   "ExternalProtocolDialogShowAlwaysOpenCheckbox": true
;; ;; }
;; ;; EOF
;; ;; sudo chmod 644 /etc/opt/chrome/policies/managed/external_protocol_dialog.json
;; ;;
;; ;; Install the chrome extension from https://chrome.google.com/webstore/detail/org-capture/kkkjlfejijcjgjllecmnejhogpbcigdc
;; ;;

;; TODO: this or the required server-start seems to make Emacs hang on normal things
;; (neo/use-package org-protocol
;;   :elpaca nil ; part of org
;;   )

;; (neo/use-package ob-mermaid
;;   :after org-modern
;;   :ensure-system-package
;;   (mermaid
;;    .
;;    "[ ! -d /usr/local/lib/node_modules/mermaid.cli ] && sudo npm install -g mermaid.cli")
;;   :ensure-system-package ditaa
;;   :config
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((mermaid . t) (ditaa . t) (shell . t) (dot . t) (scheme . t)))
;;   (setq org-confirm-babel-evaluate nil)
;;   (defun neo/org-confirm-babel-evaluate (lang body)
;;     (not (string= lang "mermaid")))
;;   ;; Fix for including SVGs
;;   (setq
;;    org-latex-pdf-process
;;    '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;      "bibtex %b"
;;      "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;      "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;;   (setq org-confirm-babel-evaluate 'neo/org-confirm-babel-evaluate)
;;   (setq
;;    org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"
;;    org-confirm-babel-evaluate nil
;;    ;; Edit settings
;;    org-auto-align-tags nil
;;    org-tags-column 0
;;    org-catch-invisible-edits 'show-and-error
;;    org-special-ctrl-a/e t
;;    org-insert-heading-respect-content t

;;    ;; Org styling, hide markup etc.
;;    org-hide-emphasis-markers t
;;    org-pretty-entities t
;;    org-ellipsis "…"

;;    ;; Agenda styling
;;    org-agenda-tags-column 0
;;    org-agenda-block-separator ?─
;;    org-agenda-time-grid
;;    '((daily today require-timed)
;;      (800 1000 1200 1400 1600 1800 2000)
;;      " ┄┄┄┄┄ "
;;      "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
;;    org-agenda-current-time-string "⭠ now ─────────────────────────────────────────────────")
;;   (global-org-modern-mode))

;; (neo/use-package org-tempo
;;   :elpaca nil ; part of org
;;   :after org
;;   :config
;;   (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
;;   (add-to-list 'org-structure-template-alist '("b" . "src bash"))
;;   (add-to-list
;;    'org-structure-template-alist '("el" . "src emacs-lisp"))
;;   (add-to-list 'org-structure-template-alist '("py" . "src python"))
;;   (add-to-list 'org-structure-template-alist '("go" . "src go"))
;;   (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
;;   (add-to-list 'org-structure-template-alist '("json" . "src json")))


;; (neo/use-package org-tidy
;;   :config (add-hook 'org-mode-hook #'org-tidy-mode))

;; ;;; TODO: once we setup agenda and todo list, use this to show agenda items on the side
;; (neo/use-package org-sidebar)

;; ;;; TODO: this is only an inspiration; I'd like to roll my own that integrates
;; ;;; runbook and incident reports
;; (neo/use-package org-runbook)

;; (neo/use-package org-appear
;;   :hook (org-mode . org-appear-mode))

;; (neo/use-package org-roam
;;   :elpaca
;;   (:host
;;    github
;;    :repo "org-roam/org-roam"
;;    :files (:defaults "extensions/*"))
;;   :custom (org-roam-directory (file-truename "~/org-roam/"))
;;   :bind
;;   (("C-c n f" . #'org-roam-node-find)
;;    ("C-c n g" . #'org-roam-graph)
;;    ("C-c n t" . #'org-roam-tag-add)
;;    ("C-c n o" . #'org-id-get-create)
;;    ("C-c n a" . #'org-roam-alias-add)
;;    ("C-c n i" . #'org-roam-node-insert)
;;    ("C-c n c" . #'org-roam-capture)
;;    ("C-c n :" . #'org-roam-buffer-toggle)
;;    ("C-c n j" . #'org-roam-dailies-capture-today))
;;   :config
;;   ;; (setq org-roam-dailies-directory "daily/")

;;   ;; (setq org-roam-dailies-capture-templates
;;   ;;       '(("d" "default" entry
;;   ;;          "* %?"

;;   ;;      :target ((format "message" format-args)ile+head "%<%Y-%m-%d>.org"
;;   ;;               "#+title: %<%Y-%m-%d>\n"))))

;;   (cl-defmethod org-roam-node-type ((node org-roam-node))
;;     "Return the TYPE of NODE."
;;     (condition-case nil
;;         (file-name-nondirectory
;;          (directory-file-name
;;           (file-name-directory
;;            (file-relative-name (org-roam-node-file node)
;;                                org-roam-directory))))
;;       (error "")))
;;   ;; If you're using a vertical completion framework, you might want a more informative completion interface
;;   (setq org-roam-node-display-template
;;         (concat
;;          "${type:15} ${title:*} "
;;          (propertize "${tags:10}" 'face 'org-tag)))
;;   (org-roam-db-autosync-mode)
;;   (setq
;;    org-roam-capture-templates
;;    '(("m"
;;       "main"
;;       plain
;;       "%?"
;;       :if-new (file+head "main/${slug}.org" "#+title: ${title}\n")
;;       :immediate-finish t
;;       :unnarrowed t)
;;      ("r"
;;       "reference"
;;       plain
;;       "%?"
;;       :if-new (file+head "reference/${title}.org" "#+title: ${title}\n")
;;       :immediate-finish t
;;       :unnarrowed t)
;;      ("a" "article" plain "%?"
;;       :if-new
;;       (file+head
;;        "articles/${title}.org"
;;        "#+title: ${title}\n#+filetags: :article:\n")
;;       :immediate-finish t
;;       :unnarrowed t)))
;;   ;; If using org-roam-protocol
;;   (require 'org-roam-protocol))

;;;-----------------------------------------------------------------------------------
;;; App/Presentations

;; TODO: figure out how to make this more discoverable and usable.
;; Also, make it work with org-mode capture
(neo/use-package screenshot
  :elpaca (screenshot :host github :repo "tecosaur/screenshot"))

(neo/use-package gif-screencast)

;;;-----------------------------------------------------------------------------------
;;; App/Shell

(neo/use-package vterm
  :ensure-system-package cmake
  :ensure-system-package libtool-bin
  :custom
  (vterm-max-scrollback 10000)
  (vterm-shell "zsh")
  :config
  (defun turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))
  :hook (vterm-mode . turn-off-chrome))

(neo/use-package vterm-toggle
  :custom
  (vterm-toggle-fullscreen-p t)
  (vterm-toggle-scope 'project)
  :bind
  (("s-t" . #'vterm-toggle)
   :map
   vterm-mode-map
   ("s-t" . #'vterm-toggle)))


(neo/use-package eshell-vterm
  :after eshell
  :config
  (eshell-vterm-mode)
  (defalias 'eshell/v 'eshell-exec-visual))

(neo/use-package eshell-syntax-highlighting
  :after eshell-mode
  :config (eshell-syntax-highlighting-global-mode +1))

;;; TODO: this often makes a mess by splitting buffers. Probably better to roll my own.
(neo/use-package eshell-toggle
  :custom (eshell-toggle-size-fraction 3)
  ;  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
  ;; This would be nice, but the Linux mint WM steals it
  :bind ("s-`" . eshell-toggle))

(neo/use-package eshell-fringe-status
  ;; for some (possibly elpaca) reasons, setting the hook in use-package doesn't work
  :hook (eshell-mode-hook . (lambda () (eshell-fringe-status-mode 1))))
(add-hook 'eshell-mode-hook 'eshell-fringe-status-mode)


;;;-----------------------------------------------------------------------------------
;;; App/Kubernetes

(neo/use-package kubernetes
  :commands (kubernetes-overview))

(neo/use-package kubernetes-tramp)

;;;-----------------------------------------------------------------------------------
;;; App/Bluetooth

(neo/use-package bluetooth)

;;;-----------------------------------------------------------------------------------
;;; App/ChatGPT

;;; get a token from https://platform.openai.com/account/api-keys
(defun neo/get-chatgpt-token ()
  (let ((credentials (auth-source-search :host "chatgpt")))
    (funcall (plist-get (car credentials) :secret))))

(use-package
 chatgpt-shell
 :custom ((chatgpt-shell-openai-key (neo/get-chatgpt-token))))

;;;-----------------------------------------------------------------------------------
;;; Fun

(neo/use-package xkcd)
(neo/use-package tetris
  :elpaca nil)
(neo/use-package autotetris-mode)
(neo/use-package selectric-mode)
(neo/use-package soccer
  :config (setq soccer-time-local-time-utc-offset "-04")
  :bind
  (("C-c s f" . soccer-fixtures-next)
   ("C-c s r" . soccer-results-last)
   ("C-c s t" . soccer-table)))


;;;-----------------------------------------------------------------------------------
;;; App

;;; TODO: maybe we should allow this to exit from all magit related modes
;;; (if (string-prefix-p "magit-" (symbol-name major-mode))
;;;   (message "You are in a Magit-related mode.")
;;; (message "You are not in a Magit-related mode."))
(defun neo/magit-toggle ()
  (interactive)
  (if (eq major-mode 'magit-status-mode)
      (magit-mode-bury-buffer)
    (magit-status)))

(global-set-key (kbd "s-g") 'neo/magit-toggle)

;;; TODO: these toggle things should look if anywhere a buffer in that mode is
;;; displayed, so that we can close from another window.
(defun neo/calendar-toggle ()
  (interactive)
  (if (eq major-mode 'calendar-mode)
      (calendar-exit)
    (calendar)))
(global-set-key (kbd "s-c") 'neo/calendar-toggle)

(defun neo/eshell-toggle ()
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (eshell/exit)
    (eshell)))
(global-set-key (kbd "s-s") 'eshell-toggle)

;;; TODO: should save windows and then go full frame
(global-set-key (kbd "s-d") 'dashboard-open)

(global-set-key (kbd "s-p") 'elpaca-manager)

;;;-----------------------------------------------------------------------------------
;;; TODO
