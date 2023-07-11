;;; early-init.el --- Neo Emacs -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Code in this file is executed before init.el, UI initialization or
;; package manager setup.  We do here things that needs to be done this early.

;;; Code:
(setq package-enable-at-startup nil)
(advice-add 'package--ensure-init-file :override 'ignore)

;; set/inhibit UI config to cure startup flickering
(setq frame-inhibit-implied-resize t)
(setq tool-bar-mode nil
      menu-bar-mode nil)
(set-scroll-bar-mode nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)


(setq no-littering-etc-directory (expand-file-name ".litter/config" user-emacs-directory))
(setq no-littering-var-directory (expand-file-name ".litter/data" user-emacs-directory))

;; (when (fboundp 'startup-redirect-eln-cache)
;;   (startup-redirect-eln-cache
;;    (convert-standard-filename
;;     (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; Set eln-cache dir
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" no-littering-var-directory)))

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil))
