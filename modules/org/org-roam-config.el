(use-package org-roam
    :ensure (:host github :repo "org-roam/org-roam" :branch "main" :files (:defaults "extensions/*"))
    :demand t
    :config (org-roam-db-autosync-mode)
    :init (setq org-roam-v2-ack t)

    :custom ((org-roam-dailies-directory (file-name-concat org-roam-directory "projeto-pessoal"))
	     (org-roam-graph-link-hidden-types ("files" "https" "ref" "fuzzy")))

    :bind (("C-c r f" . org-roam-node-find)
           ("C-c r c" . org-roam-capture)
           ("C-c r b" . org-roam-buffer-toggle)
           ("C-c r I" . org-roam-node-insert-immediate)
           :map org-mode-map
           ("C-c r i" . org-roam-node-insert)))

(use-package org-roam-ui
  :ensure (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :demand t
  :custom
  ((org-roam-ui-open-on-start nil)
   (org-roam-ui-sync-theme t)
   (org-roam-ui-follow t)
   (org-roam-ui-update-on-save t))
  :config
  (unless noninteractive
    (org-roam-ui-mode 1))
  (setq org-roam-ui-latex-macros
        '(("\\Tr" . "\\mathrm{Tr}")
          ("\\tr" . "\\mathrm{Tr}")
          ("\\dyad" . "\\ket{#1}\\bra{#2}")
          ("\\order" . "\\mathcal{O}({#1})")
          ("\\I" . "\\mathbb{I}")
          ("\\norm" . "\\parallel{#1}\\parallel")
          ("\\id" . "\\mathbb{I}")
          ("\\expval" . "\\langle{#1}\\rangle")
          ("\\dd" . "\\mathrm{d}")
          ("\\op" . "|{#1}\\rangle\\langle{#2}|")
          ("\\label" . "\\vphantom")
          ("\\dv" . "\\frac{\\mathrm{d}{#1}}{\\mathrm{d}{#2}}")
          ("\\olra" . "\\overleftrightarrow{#1}"))))

(defun my/citar-get-noter-path (key)
  "Procura por um pdf ou djvu associado à KEY e retorna a string de propriedade."
  (let* ((dir (or (bound-and-true-p ebib-import-target-directory)
		  my/pdf-library))
         (extensions '(".pdf" ".djvu")))
    (or (seq-some (lambda (ext)
                    (let ((f (expand-file-name (concat key ext) dir)))
                      (when (file-exists-p f)
                        (format ":NOTER_DOCUMENT: %s" f))))
                  extensions)
        "")))

(with-eval-after-load 'org-roam
  (add-to-list
   'org-roam-capture-templates
   '("r" "reference" plain
     "%?"
     :if-new
     (file+head
      "%<%Y%m%d%H%M%S>-${citar-citekey}.org"
      ":PROPERTIES:
:ROAM_ALIASES: ${citar-citekey}
%(my/citar-get-noter-path \"${citar-citekey}\")
:END:
#+title: ${citar-title}
") :unnarrowed t)))

(provide 'org-roam-config)
