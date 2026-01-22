(resnippets-define
   "math-mode"
   '(:mode (LaTeX-mode org-mode)
     :condition (or (texmathp) (org-inside-LaTeX-fragment-p)))
   ("\\([a-zA-Z\\]+\\)hat" '("\\hat{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)bar" '("\\bar{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)til" '("\\tilde{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)vec" '("\\vec{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)<-" '("\\overleftarrow{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)cnc" '("\\cancel{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)dag" '(1 "^{\\dag}"))
   ("\\([a-zA-Z{\\]+\\)hh" '(1 "\\hbar"))
   ("\\([a-zA-Z{\\]+\\)ll" '(1 "\\ell"))
   ("\\([a-zA-Z\\]+\\)conj" '(1 "^{*}"))
   ("\\([a-zA-Z\\]+\\)trans" '(1 "^{T}"))
   ("\\([a-zA-Z\\]+\\)comp" '(1 "^{\perp}"))
   ("\\([a-zA-Z\\]+\\)inv" '(1 "^{-1}"))

   ("\\(.[a-zA-Z0-9\\\\{}]+\\)\\(_{[^]]+}\\)!"
    '(1 2 "\\!^{" (resnippets-cursor)))
   ("\\(.[a-zA-Z0-9\\\\{}]+\\)\\(\\^{[^]]+}\\)!"
    '(1 2 "\\!_{" (resnippets-cursor)))

   ("<\\([a-zA-Z0-9_^{}\\\\]+\\)|" '("\\bra{" 1 "}"))
   ("<\\([a-zA-Z0-9_^{}\\\\]+\\) ?|\\([a-zA-Z0-9_^{}\\\\]+\\) ?>"
    '("\\braket{" 1 "}{" 2 "}"))

   ("\\([\(]?\\)\\([ ]?\\)//" '(1 "\\frac{" (resnippets-cursor) "}{}"))
   ("\\([a-zA-Z0-9{}\\\\]+\\)/" '("\\frac{" 1 "}{" (resnippets-cursor "}")))
   ("\\([a-zA-Z}]\\)\\([0-9]\\)" '(1 "_" 2))
   ("_\\([0-9][0-9]\\)" '("_{" 1 (resnippets-cursor) "}"))

   ;; fun call
   ;; any elisp function ~fun~ with a numeric argument
   ;; can be called by ;fun=argument;
   (";\\([^=]+\\)=\\([\-0-9.]+\\);"
    '((number-to-string
       (funcall (intern (resnippets-group 1))
                (string-to-number (resnippets-group 2)))))))

(provide 'langs-resnippets-config)
