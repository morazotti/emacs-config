; -*-  lexical-binding: t -*-
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

(when (file-exists-p custom-file) (load custom-file))
(dolist (module '("core"
		  "org"
		  "tools"
		  "langs"
		  "ai"))
  (add-to-list 'load-path (file-name-concat user-emacs-directory "modules" module))
  (require (intern (format "%s-config" module))))

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 16 1024 1024))))

;; (dolist (hook-fn kill-emacs-hook)
;;   (advice-add hook-fn :before
;;               (lambda (&rest _)
;;                 (message "Executando %s..." hook-fn))))

(defun my/profile-kill-emacs-hook ()
  "Mede o tempo de execução de cada função no kill-emacs-hook."
  (let ((hooks (remq 'my/profile-kill-emacs-hook kill-emacs-hook)))
    ;; Limpa o hook original para que o C-level do Emacs não rode tudo de novo
    (setq kill-emacs-hook nil)
    (dolist (fn hooks)
      (let ((start (current-time)))
        (message "Quitting: Running %s..." fn)
        (condition-case err
            (funcall fn)
          (error (message "Error at %s: %s" fn err)))
        (message "Quitting: %s ended in %.3f seconds."
                 fn (float-time (time-since start)))))))

;; O depth -100 garante que nossa função seja a primeira a rodar no desligamento
(add-hook 'kill-emacs-hook #'my/profile-kill-emacs-hook -100)
