(use-package cdlatex
      :hook (org-mode . org-cdlatex-mode)

      ;; comandos extras
      :config (add-to-list 'cdlatex-math-modify-alist '(115 "\\mathscr" nil t nil nil))
              (add-to-list 'cdlatex-math-modify-alist '(66 "\\mathbb" nil t nil nil))
              (add-to-list 'cdlatex-math-modify-alist '(107 "\\mathfrak" nil t nil nil))
              (add-to-list 'cdlatex-math-symbol-alist '(42 ("\\times" "\\otimes")))
              (add-to-list 'cdlatex-math-symbol-alist '(100 ("\\delta" "\\partial" "^{\\dag}")))
              (add-to-list 'cdlatex-math-symbol-alist '(46 ("\\cdot" "\\odot")))
              (add-to-list 'cdlatex-math-symbol-alist '(126 ("\\approx" "\\simeq" "\\propto")))
              (cdlatex-reset-mode))

(provide 'org-latex-config)
