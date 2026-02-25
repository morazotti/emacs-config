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
  (citar-library-paths (list my/pdf-library))
  :hook ((LaTeX-mode . citar-capf-setup)
	 (org-mode . citar-capf-setup)))

;; not sure if this should be on
(use-package citar-org-roam
  :after (citar org-roam)
  :custom (citar-org-roam-capture-template-key "r"))

(use-package citar-embark
  :after (citar embark)
  :no-require)

(citar-org-roam-mode)
(citar-embark-mode)

(use-package bibtex-completion
  :custom
  (bibtex-completion-bibliography my/bibliography-file)
  (bibtex-completion-notes-path org-roam-directory)
  (bibtex-completion-library-path my/pdf-library))

;; useful to look for papers, never really used it
(use-package biblio
  :demand t)

(use-package biblio-openlibrary
  :ensure (:host github :repo "fabcontigiani/biblio-openlibrary" :branch "master"))

(use-package biblio-gbooks
  :ensure (:host github :repo "jrasband/biblio-gbooks" :branch "main"))

(defun my/ebib-reading-list-add-org-cite ()
  "Add an Org-cite citation to the newly created Ebib reading-list item."
  (let ((key (ebib--get-key-at-point)))
    (with-current-buffer (find-file-noselect ebib-reading-list-file)
      ;; Ebib deixa point no item recém-criado neste buffer.
      ;; Então inserimos uma linha de citação logo após o heading/properties.
      (save-excursion
        (org-back-to-heading t)
        ;; pula drawer de PROPERTIES, se existir
        (forward-line 1)
        (when (looking-at-p ":PROPERTIES:")
          (re-search-forward "^:END:[ \t]*$" nil t)
          (forward-line 1))
        (insert (format "- [cite:@%s]\n" key)))
      (save-buffer))))

;; bibliography tool
(use-package ebib
  :after biblio

  :custom
  (ebib-default-directory my/library-directory)
  (ebib-bib-search-dirs (list my/library-directory))
  (ebib-preload-bib-files (list my/bibliography-file))
  (ebib-import-source-directory (file-name-concat home "Downloads"))
  (ebib-import-target-directory (file-name-concat ebib-default-directory "pdfs"))
  (ebib-reading-list-file (file-name-concat ebib-default-directory "reading-list.org"))

  :bind
  (:map ebib-index-mode-map ("B" . ebib-biblio-import-doi))
  (:map biblio-selection-mode-map ("e" . ebib-biblio-selection-import))

  :config
  (require 'ebib-biblio)
  (add-to-list 'org-agenda-files ebib-reading-list-file)

  :hook (ebib-reading-list-new-item . my/ebib-reading-list-add-org-cite))

(provide 'tools-references-config)
