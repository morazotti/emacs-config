;; i'm used to using ~org-ref~, but I'm going to migrate to ~citar~. 
;; the old config is going to be here

;; (use-package org-ref
;;   :hook (org-mode . (lambda () (require 'org-ref)))
;;   ;; :init (require 'org-ref-ivy)
;;   ;; :config (require 'org-ref-ivy)
;;   ;; (load-file "~/.config/emacs/straight/build/org-ref/org-ref-ivy.el") ;; talvez nao precise
;;   :config 
;;   (setq org-file-apps '((auto-mode . emacs)
;;                         (directory . emacs)
;;                         (\.mm\' . default)
;;                         (\.x?html?\' . default)
;;                         (\.pdf\' . emacs)))

;;   ;; :config (add-to-list 'org-latex-classes
;;   ;;                      '("uspsc"
;;   ;;                        "\\documentclass{USPSC-classe/USPSC}"
;;   ;;                        ("\\chapter{%s}" . "\\chapter*{%s}")
;;   ;;                        ("\\section{%s}" . "\\section*{%s}")
;;   ;;                        ("\\subsection{%s}" . "\\subsection*{%s}")
;;   ;;                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;   ;;                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
;;   :bind (("C-c C-]" . org-ref-insert-link)
;;          ("C-c )" . org-ref-insert-ref-link)))

(use-package citar
  :custom
  (citar-bibliography (list my/bibliography-file))
  (citar-notes-paths (list org-roam-directory))
  (citar-library-paths (list my/pdf-library)))

(use-package bibtex-completion
  :custom
  (bibtex-completion-bibliography my/bibliography-file)
  (bibtex-completion-notes-path org-roam-directory)
  (bibtex-completion-library-path my/pdf-library))

(provide 'org-ref-config)
