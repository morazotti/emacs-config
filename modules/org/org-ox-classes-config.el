(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("MyTest"  ; O nome que você vai usar no #+LATEX_CLASS
                 "\\documentclass{MyTest}" ; O header padrão
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(provide 'org-ox-classes-config)
