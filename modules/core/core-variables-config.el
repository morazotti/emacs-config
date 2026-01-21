(defconst home (expand-file-name "~"))
(setq-default abbrev-mode t)
(setq my/library-directory (file-name-concat home "Dropbox" "my_library/")
      my/bibliography-file (file-name-concat my/library-directory "referencias.bib")
      my/pdf-library (file-name-concat my/library-directory  "pdfs")
      warning-minimum-level :emergency
      org-roam-directory (file-name-concat home "Documents" "roam")
      ispell-dictionary "pt_BR"
      fill-column 72

      ;; authinfo
      auth-sources '("~/.authinfo.gpg" "~/.authinfo")

      make-backup-files t
      backup-by-copying t              ; evita problemas com links/permissões
      version-control t
      delete-old-versions t
      kept-old-versions 2
      kept-new-versions 10
      backup-directory-alist `(("." . ,(expand-file-name "~/.cache/emacs/backups/")))
      backup-inhibited nil

      ;; autosaves
      auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.cache/emacs/autosaves/") t))
      auto-save-default t

      ;; undo history
      undo-limit        800000
      undo-strong-limit 12000000
      undo-auto-save-history t
      undo-directory-alist `(("." . "~/.cache/emacs/undo/")))

(setenv "SSH_AUTH_SOCK" (format "%s/ssh-agent.socket" (getenv "XDG_RUNTIME_DIR")))
(put 'narrow-to-region 'disabled nil)
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(defalias 'yes-or-no-p 'y-or-n-p)
(global-subword-mode 1)

(provide 'core-variables-config)
