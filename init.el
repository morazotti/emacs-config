(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

(let ((file-name-handler-alist-original file-name-handler-alist)
      ;; Ensure load-path/module loading is stable even if init.el is loaded from a
      ;; non-standard location.
      (default-directory user-emacs-directory))
  (setq file-name-handler-alist nil)

  (when (file-exists-p custom-file) (load custom-file))

  ;; Add each module dir (modules/<name>/) to `load-path`.
  (dolist (module '("core" "org" "tools" "langs" "ai"))
    (add-to-list 'load-path
                 (expand-file-name (format "modules/%s" module)
                                   user-emacs-directory)))

  ;; if we ever need to M-x straight-freeze-versions, we need to uncomment the following line before
  ;; (load (expand-file-name "modules/core/core-packages-config.el" user-emacs-directory))

  (require 'core-config)
  (require 'org-config)
  (require 'tools-config)
  (require 'langs-config)
  (require 'ai-config)

  ;; (require 'wip-functions)

  (setq file-name-handler-alist file-name-handler-alist-original))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))
