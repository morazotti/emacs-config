;; auctex enhances usability in latex
(use-package auctex
  :init (require 'latex)
  :bind (:map LaTeX-mode-map
	      ("C-S-e" . latex-math-from-calc)
  	      :map org-mode-map
	      ("C-S-e" . latex-math-from-calc))

  :config
  (setq-default TeX-master nil)
  ;; Format math as a Latex string with Calc
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0) 
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad)))))))
  :custom ((TeX-source-correlate-mode t)
	   (TeX-source-correlate-method-active 'synctex)
	   (TeX-auto-save t)
	   (TeX-parse-self t)
	   (font-latex-fontify-script nil)))

(use-package latex-extra)

;; latex references manager -- not really using latex much tho
(use-package reftex
  :custom
  (reftex-plug-into-AUCTeX t)
  (prettify-symbols-unprettify-at-point 'right-edge)
  (reftex-default-bibliography (list my/bibliography-file))
  :hook ((TeX-mode . prettify-symbols-mode)
	 (LaTeX-mode . LaTeX-math-mode)
	 (TeX-mode . outline-minor-mode)
	 (TeX-mode . turn-on-reftex)))

(provide 'langs-latex-config)
