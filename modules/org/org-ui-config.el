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
  "Aplica cores às macros {{{cor(texto)}}} e adiciona lógica de revelar ao cursor."
  ;; Adicionamos 'my/org-color-macro' à lista de propriedades gerenciadas para limpeza correta
  (setq font-lock-extra-managed-props (append '(invisible display my/org-color-macro) font-lock-extra-managed-props))
  (let ((color-list-aux (list)))
    (dolist (color-entry my/latex-colors-alist)
      (let ((macro-name (car color-entry))
            (color-value (cdr color-entry)))
        (push `(,(format "{{{%s(\\(.*?\\))}}}" macro-name)
                (0 (progn
                     ;; Marcamos a região inteira com uma propriedade única (o ponto de início)
                     ;; Isso permite detectar limites mesmo entre macros adjacentes
                     (put-text-property (match-beginning 0) (match-end 0)
                                        'my/org-color-macro (match-beginning 0))
                     ;; Aplica invisibilidade e cores
                     (put-text-property (match-beginning 0) (match-beginning 1)
                                        'invisible t)
                     (put-text-property (match-beginning 1) (match-end 1)
                                        'face '(:foreground ,color-value :weight bold))
                     (put-text-property (match-end 1) (match-end 0)
                                        'invisible t))))
              color-list-aux)))
    (font-lock-add-keywords nil color-list-aux 'append)))

;; Variável para rastrear a última região revelada
(defvar-local my/org-color-macro-last-region nil)

(defun my/org-color-macro-auto-reveal ()
  "Revela macros de cor quando o cursor está sobre elas."
  (when (eq major-mode 'org-mode)
    (let* ((point (point))
           ;; Verifica se estamos sobre um macro
           (prop (get-text-property point 'my/org-color-macro))
           ;; Encontra os limites do macro atual (start e end)
           (start (if prop (or (previous-single-property-change (1+ point) 'my/org-color-macro) (point-min))))
           (end (if prop (or (next-single-property-change point 'my/org-color-macro) (point-max)))))

      ;; 1. Se saímos de um macro ou mudamos de macro, esconder o anterior
      (when (and my/org-color-macro-last-region
                 (or (not prop)
                     (not (equal (cons start end) my/org-color-macro-last-region))))
        (let ((reg-start (car my/org-color-macro-last-region))
              (reg-end (cdr my/org-color-macro-last-region)))
          ;; font-lock-flush força o Emacs a reaplicar as regras (re-escondendo o macro)
          (when (< reg-start reg-end)
            (font-lock-flush reg-start reg-end)))
        (setq my/org-color-macro-last-region nil))

      ;; 2. Se estamos dentro de um macro, revelar (remover invisibilidade)
      (when (and prop start end (not (equal (cons start end) my/org-color-macro-last-region)))
        (with-silent-modifications
          (remove-text-properties start end '(invisible nil)))
        (setq my/org-color-macro-last-region (cons start end))))))

;; Adiciona o hook globalmente (ou apenas no hook do org-mode se preferir)
(add-hook 'post-command-hook #'my/org-color-macro-auto-reveal)

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

;; Restante das configurações (org-modern, mixed-pitch, etc)
(use-package org-modern
  :after org
  :straight (:host github :repo "minad/org-modern" :branch "main")
  :custom ((org-modern-table nil)
	   (org-modern-label-border nil))
  :hook ((org-mode . org-modern-mode)
	 (org-mode . variable-pitch-mode)))

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode))

;; enhance latex stuff in org
(font-lock-add-keywords
   'org-mode
   '(("\\(\\(?:\\\\\\(?:label\\|ref\\|eqref\\)\\)\\){\\(.+?\\)}"
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face))))

;; Faces
(defun org--TeX-fold-setup ()
  (TeX-fold-mode)
  (unless (get 'org--TeX-fold-setup 'face-attribute-set)
    (set-face-attribute 'TeX-fold-folded-face nil :foreground nil :inherit 'shadow)
    (put 'org--TeX-fold-setup 'face-attribute-set t))
  (TeX-fold-buffer))

(add-hook 'org-mode-hook 'org--TeX-fold-setup)

;; Custom folded display for labels and refs

(defun my/TeX-fold-ref (text)
  (let* ((m (string-match "^\\([^:]+:\\)\\(.*\\)" text))
         (cat (or (match-string 1 text) ""))
         (ref (or (match-string 2 text) text)))
    (when (> (length ref) 13)
        (setq ref (concat (substring ref 0 6) "..." (substring ref -6))))
    (concat "[" (propertize cat 'face 'shadow) ref "]")))

(defun my/TeX-fold-label (&rest texts)
  (cl-loop for text in texts
           for m = (string-match "^\\([^:]+:\\)\\(.*\\)" text)
           for cat = (or (match-string 1 text) "")
           for ref = (or (match-string 2 text) text)
           collect (concat "[" (propertize cat 'face 'shadow) ref "]") into labels
           finally return (mapconcat #'identity labels ",")))

(setq-default TeX-fold-macro-spec-list
              '((my/TeX-fold-label ("cite"))
                (my/TeX-fold-label ("label"))
                (my/TeX-fold-ref ("ref" "pageref" "eqref" "footref"))))

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
