(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

(let ((file-name-handler-alist-original file-name-handler-alist))
  (setq file-name-handler-alist nil)

  (when (file-exists-p custom-file) (load custom-file))

  (dolist (module '("core" "org" "tools" "langs"))
    (add-to-list 'load-path
                 (expand-file-name
                  (format "modules/%s" module)
                  user-emacs-directory)))

  ;; if we ever need to M-x straight-freeze-versions, we need to uncomment the following line before
  ;; (load (expand-file-name "modules/core/core-packages-config.el" user-emacs-directory))

  (require 'core-config)
  (require 'org-config)
  (require 'tools-config)
  (require 'langs-config)

  ;; (require 'wip-functions)

  (setq file-name-handler-alist file-name-handler-alist-original))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))
