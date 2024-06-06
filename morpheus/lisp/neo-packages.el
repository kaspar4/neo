;;;-----------------------------------------------------------------------------------
;;; Packages

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory
  (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory
  (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory
  (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
  '(elpaca
    :repo "https://github.com/progfolio/elpaca.git"
    :ref nil
    :depth 1
    :files (:defaults "elpaca-test.el" (:exclude "extensions"))
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
                   (apply #'call-process
                          `("git" nil ,buffer t "clone" ,@
                            (when-let ((depth
                                        (plist-get order :depth)))
                              (list
                               (format "--depth=%d" depth)
                               "--no-single-branch"))
                            ,(plist-get order :repo) ,repo))))
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
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca
 elpaca-use-package
 ;; Enable :elpaca use-package keyword.
 (elpaca-use-package-mode)
 ;; Assume :elpaca t unless otherwise specified.
 (setq elpaca-use-package-by-default t))

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

;; TODO: not working anymore, maybe write our own
;; (neo/use-package bind-chord)
;; (neo/use-package use-package-chords
;;   :config (key-chord-mode 1))

(provide 'neo-packages)
