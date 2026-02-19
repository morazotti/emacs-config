(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(dolist (module '("core" "org" "langs"))
  (add-to-list 'load-path
	       (expand-file-name
		(format "modules/%s" module)
		user-emacs-directory)))

(require 'core-packages-config)
(require 'core-variables-config)
(require 'org-core-config)
(require 'org-babel-config)
(require 'org-link-config)
(require 'org-ox-config)
(require 'langs-latex-config)
