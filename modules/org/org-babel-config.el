(require 'org-ob-jupyter-config)

(defvar my/org-latex-babel-lang-list
  '(C jupyter shell latex fortran python ledger))

(setq org-babel-load-languages '())
(dolist (lang my/org-latex-babel-lang-list)
  (add-to-list 'org-babel-load-languages (cons lang t)))
(org-babel-do-load-languages
 'org-babel-load-languages
 org-babel-load-languages)

(provide 'org-babel-config)
