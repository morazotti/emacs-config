(defconst home (expand-file-name "~"))
(setq-default abbrev-mode t)
(setq my/library-directory (file-name-concat home "Dropbox" "my_library")
      my/bibliography-file (file-name-concat my/library-directory "referencias.bib")
      my/pdf-library (file-name-concat my/library-directory  "pdfs")
      warning-minimum-level :emergency
      org-roam-directory (file-name-concat home "Documents" "roam")
      ispell-dictionary "pt_BR"
      fill-column 72

      ;; backups
      backup-directory-alist `(("." "~/.cache/emacs/backups"))
      version-control t
      kept-old-versions 2
      kept-new-versions 10
      delete-old-versions t

      ;; autosaves
      auto-save-file-name-transforms `((".*" "~/.cache/emacs/autosaves/" t))
      auto-save-default t

      ;; undo history
      undo-limit        800000
      undo-strong-limit 12000000
      undo-outer-limit  120000000
      undo-auto-save-history t
      undo-directory-alist `(("." . "~/.cache/emacs/undo")))

(setenv "SSH_AUTH_SOCK" (format "%s/ssh-agent.socket" (getenv "XDG_RUNTIME_DIR")))
(put 'narrow-to-region 'disabled nil)
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(defalias 'yes-or-no-p 'y-or-n-p)
(global-subword-mode 1)

(provide 'core-variables-config)
