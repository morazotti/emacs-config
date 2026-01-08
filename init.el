;; primeira linha -> excluir
;; (setq user-emacs-directory "~/.config/newmacs")
(setq custom-file (file-name-concat user-emacs-directory "emacs-custom.el"))

;; excluir assim que estiver pronto
;; (load-file (file-name-concat user-emacs-directory "early-init.el"))

(add-to-list 'load-path (expand-file-name "modules/core"  user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/org"  user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/tools"  user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/langs"  user-emacs-directory))

(require 'core-config)
(require 'org-config)
(require 'tools-config)
(require 'langs-config)

;; (load-file (file-name-concat user-emacs-directory "modules" "wip-functions.el"))
