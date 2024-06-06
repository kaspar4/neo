
;;; We don't want to edit files under ~/neo/ directly, but when we do edit
;;; ~/Projects/uno/devex/editors/emacs/init.el we copy it to
;;; ~/neo/init.el for testing purposes, as pushing to github, creating
;;; a PR, merging, syncying to the public repo with copybara and
;;; pulling is annoying. We don't touch the repository in ~/neo so
;;; that it is easy to revert.
(defun neo/copy-init-on-save ()
  (let ((src
         (expand-file-name
          "/home/mvitale/Projects/uno/devex/editors/emacs"))
        (dst (expand-file-name "/home/mvitale/neo"))
        (file (buffer-file-name)))
    (when (and file (string-prefix-p src file))
      (let ((relative-path (file-relative-name file src))
            (destination-path
             (expand-file-name (file-relative-name file src) dst)))
        (make-directory (file-name-directory destination-path) t)
        (copy-file file destination-path t)
        (message "Copied %s to %s" file destination-path)))))

(add-hook 'after-save-hook #'neo/copy-init-on-save)


(neo/use-package bug-hunter)
(defun neo/bug-hunter-init-file-wrapper ()
  "Wrapper around `bug-hunter-init-file` to ensure `load-path` is correct."
  (interactive)
  ;; Ensure `load-path` is set correctly
  (let ((load-path (append (list (expand-file-name "lisp" user-emacs-directory))
				 load-path)))
    ;; Call the original `bug-hunter-init-file`
    (bug-hunter-init-file)))

(global-set-key (kbd "C-c b") 'neo/bug-hunter-init-file-wrapper)

(provide 'neo-prog-elisp)
