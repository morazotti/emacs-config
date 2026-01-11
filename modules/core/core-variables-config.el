(defconst home (expand-file-name "~"))
(setq-default abbrev-mode t)
(setq my/bibliography-file (file-name-concat home "Dropbox" "referencias.bib")
      my/pdf-library (file-name-concat home "Documents" "my_lib" "pdfs")
      warning-minimum-level :emergency
      org-roam-directory (file-name-concat home "Documents" "roam")
      ispell-dictionary "pt_BR"
      make-backup-files nil
      auto-save-default nil
      fill-column 72)
(put 'narrow-to-region 'disabled nil)
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(defalias 'yes-or-no-p 'y-or-n-p)
(global-subword-mode 1)

(provide 'core-variables-config)
