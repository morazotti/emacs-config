(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(use-package org-modern
  :after org
  :straight (:host github :repo "minad/org-modern" :branch "main")
  :config (setq org-modern-table nil)
  :config (setq org-modern-label-border nil)
  :hook (org-mode . org-modern-mode))

(provide 'org-ui-config)
