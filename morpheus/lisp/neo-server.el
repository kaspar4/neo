;;;-----------------------------------------------------------------------------------
;;; Server setup
(neo/use-package server
  :ensure nil  ;; Indicate that this is a built-in package
  :config
  (unless (server-running-p)
    (server-start)))

;;;-----------------------------------------------------------------------------------
;;; Emacs Restart

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


(provide 'neo-server)
