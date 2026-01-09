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
;;
;; (use-package org-roam-bibtex
;;     :ensure t
;;     :after org-roam
;;     :load-path org-roam-directory
;;     :config (require 'org-ref)
;;     :init (org-roam-bibtex-mode))

(use-package citar
  :custom
  (citar-bibliography (list my/bibliography-file))
  (citar-notes-paths (list org-roam-directory))
  (citar-library-paths (list my/pdf-library)))

;; not sure if this should be on
(use-package citar-org-roam
  :after (citar org-roam)
  :custom (citar-org-roam-capture-template-key "r"))
(citar-org-roam-mode)

(use-package bibtex-completion
  :custom
  (bibtex-completion-bibliography my/bibliography-file)
  (bibtex-completion-notes-path org-roam-directory)
  (bibtex-completion-library-path my/pdf-library))

;; useful to look for papers, never really used it
(use-package biblio
  :demand t)

;; bibliography tool
(use-package ebib
  :after biblio
  :custom ((ebib-default-directory my/pdf-library)
	   (ebib-bib-search-dirs (file-name-concat home "Dropbox"))
	   (ebib-preload-bib-files `(,my/bibliography-file))
	   (ebib-reading-list-file (file-name-concat ebib-default-directory "reading-list.org")))
  :config
  (require 'ebib-biblio)
  (define-key ebib-index-mode-map (kbd "B") #'ebib-biblio-import-doi)
  (define-key biblio-selection-mode-map (kbd "e") #'ebib-biblio-selection-import)

)

(provide 'tools-references-config)
