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

(keymap-set org-mode-map "M-g r" 'my/next-reference-or-label)
(keymap-set org-mode-map "M-g R" 'my/previous-reference-or-label)
(defvar-keymap my/TeX-ref-map
  :repeat t
  "r" 'my/next-reference-or-label
  "R" 'my/previous-reference-or-label)

(add-hook 'org-mode-hook 'reftex-xref-activate)

(provide 'org-references-config.el)
