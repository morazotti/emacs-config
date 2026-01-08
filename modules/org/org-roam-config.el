(use-package org-roam
    :straight (:host github :repo "org-roam/org-roam" :branch "main")
    :config (org-roam-db-autosync-mode)
    :init (setq org-roam-v2-ack t)

    :custom ((org-roam-directory (file-name-concat home "Documents" "roam"))
	     (org-roam-dailies-directory (file-name-concat org-roam-directory "projeto-pessoal"))
	     (org-roam-graph-link-hidden-types ("files" "https" "ref" "fuzzy")))

    :bind (("C-c r f" . org-roam-node-find)
           ("C-c r c" . org-roam-capture)
           ("C-c r b" . org-roam-buffer-toggle)
           ("C-c r I" . org-roam-node-insert-immediate)
           :map org-mode-map
           ("C-c r i" . org-roam-node-insert)))

(with-eval-after-load 'org-roam
  (add-to-list
   'org-roam-capture-templates
   '("r" "reference" plain
     "%?"
     :if-new
     (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                "#+title: ${title}\n#+roam_key: ${ref}\n")
     :unnarrowed t)))

(provide 'org-roam-config)
