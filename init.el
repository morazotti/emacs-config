(setq custom-file (file-name-concat user-emacs-directory "emacs-custom.el"))

(add-to-list 'load-path (expand-file-name "modules/core"  user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/org"  user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/tools"  user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/langs"  user-emacs-directory))

(require 'core-config)
(require 'org-config)
(require 'tools-config)
(require 'langs-config)

;; (require 'wip-functions)
