(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(add-to-list 'load-path (expand-file-name "modules/core"  user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/org"  user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/tools"  user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/langs"  user-emacs-directory))

(require 'core-config)
(require 'org-config)
(require 'tools-config)
(require 'langs-config)

;; (require 'wip-functions)
