(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(use-package org-modern
  :after org
  :straight (:host github :repo "minad/org-modern" :branch "main")
  :custom ((org-modern-table nil)
	   (org-modern-label-border nil))
  :hook ((org-mode . org-modern-mode)
	 (org-mode . variable-pitch-mode)))

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode))

;; svg-tags: create buttons with svg
;; (use-package svg-lib)
;; (use-package svg-tag
;;   :ensure svg-lib
;;   :straight (:host github :repo "rougier/svg-tag-mode" :branch "main")
;;   :hook (org-mode . svg-tag-mode))
;; (setq svg-tag-tags
;;       '(("\\(:@[A-Za-z0-9]+\\)" . ((lambda (tag)
;;                                      (svg-tag-make (upcase tag) :beg 2))))
;;         ("\\(:@[A-Za-z0-9]+:\\)$" . ((lambda (tag)
;;                                        (svg-tag-make (upcase tag) :beg 2 :end -1))))
;;         ("\\(:@[A-Za-z]+:\\)" . ((lambda (tag) (svg-tag-make (upcase tag) :beg 2 :end -1))))
;;         ("\\(#.transclude: .*\\)" . ((lambda (tag) (svg-tag-make tag :beg 57 :end -2))
;;                                      (lambda () (interactive) (org-transclusion-add))))
;;         ("\\[#[A-Z]\\]" . ( (lambda (tag)
;;                               (svg-tag-make tag :face 'org-priority
;;                                             :beg 2 :end -1 :margin 1))))
;;         ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 1))))
;;         ("NEXT" . ((lambda (tag) (svg-tag-make "NEXT" :face 'org-todo :inverse t :margin 1))))
;;         ("BACKLOG" . ((lambda (tag) (svg-tag-make "BACKLOG" :face 'org-todo :inverse t :margin 1))))
;;         ("REVIEW" . ((lambda (tag) (svg-tag-make "REVIEW" :face 'org-todo :inverse t :margin 1))))
;;         ("IN-PROGRESS" . ((lambda (tag) (svg-tag-make "IN-PROGRESS" :face 'org-todo :inverse t :margin 1))))
;;         ("WAITING" . ((lambda (tag) (svg-tag-make "WAITING" :face 'org-todo :inverse t :margin 1))))
;;         ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 1))))
;;         ("ABANDONED" . ((lambda (tag) (svg-tag-make "ABANDONED" :face 'org-done :margin 1))))
;;         ("COMPLETE" . ((lambda (tag) (svg-tag-make "COMPLETE" :face 'org-done :margin 1))))
;;         ))

(provide 'org-ui-config)
