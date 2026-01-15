(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(dolist (module '("core" "org" "tools" "langs"))
  (add-to-list 'load-path
	       (expand-file-name
		(format "modules/%s" module)
		user-emacs-directory)))

(require 'core-config)
(require 'org-config)
(require 'tools-config)
(require 'langs-config)

;; (require 'wip-functions)
