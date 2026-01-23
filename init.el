(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

(let ((file-name-handler-alist-original file-name-handler-alist)
      (default-directory user-emacs-directory))
  (setq file-name-handler-alist nil)

  (when (file-exists-p custom-file) (load custom-file))

  (dolist (module '("core" "org" "tools" "langs" "ai"))
    (add-to-list 'load-path
                 (expand-file-name (format "modules/%s" module)
                                   user-emacs-directory))
    (require (intern (format "%s-config" module))))
  (setq file-name-handler-alist file-name-handler-alist-original))

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 16 1024 1024))))
