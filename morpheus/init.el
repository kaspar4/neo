;;; This is Morpheus

(add-to-list
 'load-path (expand-file-name "lisp" user-emacs-directory))


(message "This is Morpheus")

(require 'neo-packages)
(require 'neo-server)

(require 'neo-completions)

(require 'neo-prog-general)
(require 'neo-prog-elisp)
(require 'neo-prog-c++)
