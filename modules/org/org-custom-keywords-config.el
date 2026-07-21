(defvar my/org-custom-keywords-alist
  '(("REFERENCE_TITLE" . my/org-inject-reference-title-action)
    ("PAGEBREAK" . my/org-inject-page-break-action))
  "Lista de pares (keyword . funcao) para processar no export.")

(defun my/org-inject-reference-title-action (value)
  "Injeta a marreta do LaTeX para alterar o título da referência usando VALUE."
  (goto-char (point-min))
  (insert (format "#+LATEX_HEADER: \\AtBeginDocument{\\renewcommand{\\refname}{%s}}\n" value))
  (insert (format "#+LATEX_HEADER: \\AtBeginDocument{\\renewcommand{\\bibname}{%s}}\n" value)))

(defun my/org-inject-page-break-action (value)
  "Substitui a keyword pelo código LaTeX de pagebreak."
  (replace-match "")
  (insert "#+LATEX: \\newpage\n")
  (insert "#+LATEX: \\begin{align*}\n")
  (insert "#+LATEX: \\textcolor{white}{2+2}\n")
  (insert "#+LATEX: \\end{align*}\n")
  (insert "#+LATEX: \\thispagestyle{empty}"))

(defun my/org-inject-custom-keywords (backend)
  "Vasculha o arquivo atrás das keywords e executa a função associada."
  (dolist (pair my/org-custom-keywords-alist)
    (let ((keyword (car pair))
          (func (cdr pair)))
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t))
          (while (re-search-forward (format "^#\\+%s:[ \t]*\\(.*\\)$" keyword) nil t)
            (let ((value (match-string 1))
                  (m (set-marker (make-marker) (match-end 0))))
              (funcall func value)
              (goto-char m)
              (set-marker m nil))))))))

(with-eval-after-load 'org
  ;; Injeta nossas keywords customizadas na lista oficial do Org para autocompletar (e.g. via completion-at-point/corfu)
  (dolist (pair my/org-custom-keywords-alist)
    (let ((kw (concat (car pair) ":")))
      (unless (member kw org-options-keywords)
        ;; Usamos setq e append porque org-options-keywords é um defconst e add-to-list pode dar warning
        (setq org-options-keywords (append org-options-keywords (list kw)))))))

(add-hook 'org-export-before-processing-hook #'my/org-inject-custom-keywords)
(provide 'org-custom-keywords-config)
