(use-package cdlatex
      :hook (org-mode . org-cdlatex-mode)

      ;; comandos extras
      :config (add-to-list 'cdlatex-math-modify-alist '(?s "\\mathscr" nil t nil nil))
              (add-to-list 'cdlatex-math-modify-alist '(?B "\\mathbb" nil t nil nil))
              (add-to-list 'cdlatex-math-modify-alist '(?k "\\mathfrak" nil t nil nil))
              (add-to-list 'cdlatex-math-symbol-alist '(?E ("\\mathcal{E}" "\\exists")))
              (add-to-list 'cdlatex-math-symbol-alist '(?* ("\\times" "\\otimes")))
              (add-to-list 'cdlatex-math-symbol-alist '(?d ("\\delta" "\\partial" "^{\\dag}")))
	      (add-to-list 'cdlatex-math-symbol-alist '(?> ("\\to" "\\longrightarrow")))
              (add-to-list 'cdlatex-math-symbol-alist '(?. ("\\cdot" "\\odot" "\\max")))
              (add-to-list 'cdlatex-math-symbol-alist '(?~ ("\\approx" "\\simeq" "\\propto")))
              (cdlatex-reset-mode))

(provide 'org-latex-config)
