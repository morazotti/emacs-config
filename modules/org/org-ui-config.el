(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(defvar my/latex-colors-alist
  '(("red"     . "red")
    ("blue"    . "blue")
    ("green"   . "forest green")
    ("yellow"  . "yellow")
    ("magenta" . "magenta")
    ("cyan"    . "cyan")
    ("brown"   . "brown")
    ("blood"   . "#aa2233")
    ("orange"  . "orange"))
  "Alist mapping macro names to display colors in Emacs.")

(defun my/org-fontify-color-macros ()
  "Aplica cores reais às macros {{{cor(texto)}}} no buffer."
  (setq font-lock-extra-managed-props (append '(invisible display) font-lock-extra-managed-props))
  (let ((color-list-aux (list)))
    (dolist (color-entry my/latex-colors-alist)
      (let ((macro-name (car color-entry))
            (color-value (cdr color-entry)))
        (push `(,(format "{{{%s(\\(.*?\\))}}}" macro-name)
                (0 (progn
                     (put-text-property (match-beginning 0) (match-beginning 1)
                                        'invisible t)
                     (put-text-property (match-beginning 1) (match-end 1)
                                        'face '(:foreground ,color-value :weight bold))
                     (put-text-property (match-end 1) (match-end 0)
                                        'invisible t))))
              color-list-aux)))
    (font-lock-add-keywords nil color-list-aux 'append)))

(defun my/org--color-macro-header (backend)
  (pcase backend
    ('latex "#+MACRO: color \\textcolor{$1}{$2}\n")
    ('html  "#+MACRO: color @@html:<font color=\"$1\">$2</font>@@\n")
    (_ (error "Backend inválido: %S" backend))))

(defun my/org--color-macro-render (backend color &optional mapping)
  (pcase backend
    ('latex
     (if (and (stringp color) (string-prefix-p "#" color))
         (format "\\textcolor[HTML]{%s}{$1}" (substring color 1))
       (format "\\textcolor{%s}{$1}" color)))
    ('html
     (let* ((raw-color (or (and mapping (cdr (assoc-string color mapping t)))
                           color))
            (html-color
             (if (stringp raw-color)
                 (replace-regexp-in-string
                  "\\b\\([[:alpha:]]\\)"
                  (lambda (m) (upcase m))
                  (downcase raw-color)
                  nil nil 1)
               raw-color)))
       (format "@@html:<font color=\"%s\">$1</font>@@" html-color)))
    (_ (error "Backend inválido: %S" backend))))

(defun my/update-colors-org-file (backend)
  (interactive
   (list (intern (completing-read "Backend: " '("latex" "html") nil t nil nil "latex"))))
  (let* ((relative-path (pcase backend
                          ('latex "macros/latex-colors.org")
                          ('html  "macros/colors.org")
                          (_ (error "Backend inválido: %S" backend))))
         (file-path (expand-file-name relative-path user-emacs-directory))
         (mapping (pcase backend
                    ('html '(("forest green" . "LightGreen")))
                    (_ nil))))
    (with-temp-file file-path
      (insert (my/org--color-macro-header backend))
      (dolist (entry my/latex-colors-alist)
        (let* ((name (car entry))
               (color (cdr entry))
               (body (my/org--color-macro-render backend color mapping)))
          (insert (format "#+MACRO: %s %s\n" name body)))))))

(defun my/update-latex-colors-org-file ()
  (interactive)
  (my/update-colors-org-file 'latex))

(defun my/update-html-colors-org-file ()
  (interactive)
  (my/update-colors-org-file 'html))

(when (bound-and-true-p my/latex-colors-alist)
  (progn (my/update-latex-colors-org-file)
	 (my/update-html-colors-org-file)))

(add-hook 'org-mode-hook #'my/org-fontify-color-macros)

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
