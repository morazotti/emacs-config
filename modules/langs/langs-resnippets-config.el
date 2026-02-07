(resnippets-define
   "math-mode"
   '(:mode (LaTeX-mode org-mode)
     :condition (or (texmathp) (org-inside-LaTeX-fragment-p))
     :priority 10)
   ("hat" '("\\hat{" (resnippets-cursor) "}") :priority 1)
   ("bar" '("\\bar{" (resnippets-cursor) "}") :priority 1)
   ("dot" '("\\dot{" (resnippets-cursor) "}") :priority 1)
   ("ddot" '("\\ddot{" (resnippets-cursor) "}") :priority 20)
   ("til" '("\\tilde{" (resnippets-cursor) "}") :priority 1)
   ("vec" '("\\vec{" (resnippets-cursor) "}") :priority 1)
   ("cnc" '("\\cancel{" (resnippets-cursor) "}") :priority 1)
   ("ck" '("\\check{" (resnippets-cursor) "}") :priority 1)
   ("\\([a-zA-Z\\]+\\)hat" '("\\hat{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)bar" '("\\bar{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)dot" '("\\dot{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)ddot" '("\\ddot{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)til" '("\\tilde{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)vec" '("\\vec{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)<-" '("\\overleftarrow{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)cnc" '("\\cancel{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)ck" '("\\check{" 1 "}"))
   ("\\([a-zA-Z\\]+\\)dag" '(1 "^{\\dag}"))
   ("\\([a-zA-Z{\\]+\\)hh" '(1 "\\hbar"))
   ("\\([0-9a-zA-Z(){\\]+\\)ll" '(1 "\\ell"))
   ("\\([a-zA-Z0-9 \\]+\\)conj" '(1 "^{*}"))
   ("\\([a-zA-Z\\]+\\)trans" '(1 "^{T}"))
   ("\\([a-zA-Z\\]+\\)comp" '(1 "^{\\perp}"))
   ("\\([a-zA-Z\\]+\\)perp" '(1 "^{\\perp}"))
   ("\\([a-zA-Z\\]+\\)inv" '(1 "^{-1}"))
   ((rx (or ",." ".,")) "\\to")

   ("\\(.[a-zA-Z0-9\\\\{}]+\\)\\(_{[^]]+}\\)!"
    '(1 2 "\\!^{" (resnippets-cursor)))
   ("\\(.[a-zA-Z0-9\\\\{}]+\\)\\(\\^{[^]]+}\\)!"
    '(1 2 "\\!_{" (resnippets-cursor)))

   ("\\(\\mqty.\\)\\([^])}]*\\);" '(1 2 "\\\\ " (resnippets-cursor)))

   ("^," "&")
   ("\\ket{bra" '("\ketbra{" (resnippets-cursor) "}{"))
   ("<\\([a-zA-Z0-9_^{}\\\\]+\\)|" '("\\bra{" 1 "}"))
   ("<\\([a-zA-Z0-9_^{}\\\\]+\\) ?|\\([a-zA-Z0-9_^{}\\\\]+\\) ?>"
    '("\\braket{" 1 "}{" 2 "}"))

   ("\\([\(]?\\)\\([ ]?\\)//" '(1 "\\frac{" (resnippets-cursor) "}{}"))
   ("\\([a-zA-Z0-9{}_\\^\\\\]+\\)/" '("\\frac{" 1 "}{" (resnippets-cursor) "}"))
   ("\\([a-zA-Z}]\\)\\([0-9]\\)" '(1 "_" 2))
   ("_\\([0-9][0-9]\\)" '("_{" 1 (resnippets-cursor) "}"))

   ;; fun call
   ;; any elisp function ~fun~ with a numeric argument
   ;; can be called by ;fun=argument;
   (";\\([^=]+\\)=\\([\-0-9.]+\\);"
    '((number-to-string
       (funcall (intern (resnippets-group 1))
                (string-to-number (resnippets-group 2)))))))

(resnippets-define
 "text-mode"
 '(:mode text-mode
	 :condition (not (or (texmathp) (org-inside-LaTeX-fragment-p)))
	 :match-case t)
 ("cao " '(1 "ção ") :priority 1)
 ("cao " "cão " :priority 2 :word-boundary t)
 ("coes " '(1 "ções "))
 ((rx (or "mv" "vm")) '("\\(" (resnippets-cursor) "\\)") :word-boundary t)
 ((rx (or "ts" "st")) '("\\[" (resnippets-cursor) "\\]") :word-boundary t)
 ("aa " "à " :word-boundary t)
 ("apos " "após " :word-boundary t)
 ("e\\([he]\\) " "é " :word-boundary t)
 ("estah" "está")
 ("quencia\\(s?\\) " '("quência" 1 " "))
 ("freq\\(s?\\) " '("frequência" 1 " "))
 ("msm " "mesmo ")
 ("tbm" "também")
 ("mt\\(s?\\) " '("muito" 1 " "))
 ("rapido " "rápido ")
 ("ruido " "ruído ")
 ("qt\\(s?\\) " '("qubit" 1 " "))
 ("qi " "qubit ")
 ("qii " "qubits ")
 ("port\\(i?\\)f\\([oó]\\)lio " "portfólio ")
 ("voce " "você ")
 ("vc " "você ")
 ("ali\\*" '((cdlatex-environment "align*")))
 ("rf" '((funcall (org-cite-insert 1))) :word-boundary t)
 
 (",," ","))


(provide 'langs-resnippets-config)
