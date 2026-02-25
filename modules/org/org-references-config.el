;; code thanks to Karthink
(defun my/next-reference-or-label (_arg)
    (interactive "p")
    (let* ((prop))
      (pcase-let
          ((`(,_ . ,ov)
           (get-char-property-and-overlay (point) 'TeX-fold-type)))
        (when ov (TeX-fold-hide-item ov)))
      (save-excursion
        (and (setq prop (text-property-search-forward
                         'face nil
                         (lambda (_ val)
                           (memq val '(font-lock-constant-face org-cite)))
                         t))))
      (if prop
          (progn (goto-char (prop-match-beginning prop))
                 (when (and (derived-mode-p 'org-mode) (org-invisible-p))
                   (org-fold-show-context 'link-search))
                 (when eldoc-mode (eldoc--invoke-strategy t))
                 (pcase-let
                     ((`(,_ . ,ov)
                      (get-char-property-and-overlay (point) 'TeX-fold-type)))
                   (when ov (TeX-fold-show-item ov))))
        (message "No more references/labels."))))

(defun my/previous-reference-or-label (_arg)
   (interactive "p")
   (let ((p))
     (save-excursion
       (and (text-property-search-backward
             'face nil
             (lambda (_ val)
               (memq val '(font-lock-constant-face org-cite
                           TeX-fold-folded-face)))
             t)
            (setq p (point))))
     (pcase-let
         ((`(,_ . ,ov)
           (get-char-property-and-overlay (point) 'TeX-fold-type)))
       (when ov (TeX-fold-hide-item ov)))
     (when p
       (goto-char p)
       (when (and (derived-mode-p 'org-mode) (org-invisible-p))
         (org-fold-show-context 'link-search))
       (when eldoc-mode (eldoc--invoke-strategy t)))
     (pcase-let
         ((`(,_ . ,ov)
           (get-char-property-and-overlay (point) 'TeX-fold-type)))
       (when ov (TeX-fold-show-item ov)))))

(defvar-keymap my/TeX-ref-map
  :repeat t
  "r" 'my/next-reference-or-label
  "R" 'my/previous-reference-or-label)

(use-package reftex-xref
  :init (require 'latex)
  :ensure (:host github :repo "karthink/reftex-xref" :branch "master")
  :after (latex org)
  :demand t
  :hook
  (org-mode . reftex-xref-activate)
  (org-mode . reftex-eldoc-activate))

(use-package org-noter
  :custom
  (org-noter-default-insertion-template
        (list (quote org-noter-insert-under-heading) 
              "Notes"))
  (org-noter-highlight-selected-text t)
  (org-noter-notes-insert-below-target t))

(provide 'org-references-config)
