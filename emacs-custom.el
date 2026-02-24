(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-fold-command-prefix "\3o")
 '(bibtex-autokey-name-year-separator "_")
 '(bibtex-autokey-titleword-length 'infty)
 '(bibtex-autokey-titlewords 2)
 '(bibtex-autokey-year-length 4)
 '(bibtex-autokey-year-title-separator "_")
 '(ebib-file-associations '(("pdf") ("ps" . "gv")))
 '(ebib-keywords
   '("Quantum Computation" "Computer Science - Cryptography and Security"
     "Lie groups" "Lie algebras" "Physics - Optics"
     "Technical writing" "Technical English" "Style" "Rhetoric"
     "English language" "Business writing" "Business English"
     "Computer Science - Information Theory"
     "Mathematics - Mathematical Physics" "Attention Network"
     "Molecular evolution" "Computational models"
     "Quantitative Biology - Quantitative Methods"
     "Quantitative Biology - Populations and Evolution"
     "Quantitative Biology - Biomolecules" "81Q93"
     "Quantum Physics (quant-ph)"
     "Nonlinear Sciences - Cellular Automata and Lattice Gases"
     "Structural Biology"
     "Computer Science - Computer Vision and Pattern Recognition"
     "Geodesics (Mathematics)" "Riemannian}"
     "Physics - Atomic Physics"
     "Computer Science - Emerging Technologies"
     "Physics - Chemical Physics"
     "Condensed Matter - Disordered Systems and Neural Networks"
     "Quantum States Geometry" "Quantum Circuits"
     "Physics - Applied Physics" "Rafael"
     "Exterior differential systems" "Control theory" "Congresses"
     "Unread" "Biotechnology" "Mathematics - Statistics Theory"
     "Physics - Computational Physics"
     "FOS: Computer and information sciences"
     "FOS: Computer and information\12                  sciences"
     "Computational Physics\12                  (physics.comp-ph)"
     "Applied Physics\12                  (physics.app-ph)"
     "Machine Learning (cs.LG)" "Physics - Physics and Society"
     "Computer Science - Social and Information Networks"
     "FOS: Physical sciences"
     "FOS: Physical\12                  sciences"
     "Classical Physics (physics.class-ph)"
     "Statistical Mechanics (cond-mat.stat-mech)"
     "Astrophysics - Earth and Planetary Astrophysics"
     "Python (Computer program language)"
     "Neural networks (Computer science)" "Mathematical Physics"
     "Computer Science - Neural and Evolutionary Computing"
     "Computer Science - Computation and Language"
     "Condensed Matter - Mesoscale and Nanoscale Physics"
     "Open systems (Physics)" "Computer Science - Robotics"
     "Quantum theory" "Nonlinear Sciences - Chaotic Dynamics"
     "Condensed Matter - Statistical Mechanics"
     "Computer Science - Artificial Intelligence"
     "Mathematics - Differential Geometry" "58A32}" "58A20" "58A05"
     "53C35" "53B05" "{22E65"
     "Condensed Matter - Other Condensed Matter"
     "Condensed Matter - Strongly Correlated Electrons"
     "Condensed Matter - Quantum Gases" "Quantum computers"
     "Statistics - Machine Learning" "Metabolomics"
     "Quantum Complexity" "High Energy Physics - Theory"
     "Finsler spaces" "Differential}" "{Geometry" "Quantum Tomography"
     "Quantitative Biology - Genomics" "Synthetic Biology"
     "Computer Science - Machine Learning" "Biology" "Quantum Physics"
     "General Relativity and Quantum Cosmology"
     "Computer Science - Computational Complexity"))
 '(ebib-reading-list-template "* %M %T\12\12\12:PROPERTIES:\12%K\12:END:\12")
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-hide-macro-markers t)
 '(org-image-actual-width '(350))
 '(org-image-align 'center)
 '(org-latex-preview-appearance-options
   '(:foreground default :background default :scale 2.0 :html-foreground
		 "Black" :html-background "Transparent" :html-scale
		 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")) nil nil "Customized with use-package org")
 '(org-latex-src-block-backend 'minted nil nil "Customized with use-package org")
 '(org-modern-hide-stars " ")
 '(org-modern-star 'replace)
 '(org-pretty-entities-include-sub-superscripts nil)
 '(org-safe-remote-resources '("\\`\\[\\[file:early-init\\.el]]\\'"))
 '(org-startup-with-latex-preview t)
 '(org-use-sub-superscripts nil)
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
	   (lambda nil
	     (shell-command "pandoc README.org -o README.md"))
	   nil t)))
 '(xah-fly-keys t)
 '(xah-fly-use-control-key nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(italic ((t (:slant italic))))
 '(jinx-misspelled ((t (:underline (:color "red" :style wave :position nil)))))
 '(org-document-title ((t (:weight bold :height 1.0))))
 '(org-drawer ((t (:inherit fixed-pitch :foreground "Blue1"))))
 '(org-modern-date-active ((t (:inherit (default fixed-pitch) :background "gray90" :foreground "black" :height 0.8 :family "Ligamonacop"))))
 '(org-modern-date-inactive ((t (:inherit (default fixed-pitch) :background "gray90" :foreground "gray30" :height 0.8))))
 '(org-modern-time-active ((t (0.8 (default fixed-pitch) :inherit :height :background "gray35" :foreground "white" :weight semibold))))
 '(org-modern-time-inactive ((t (:inherit (default fixed-pitch) :background "gray50" :foreground "gray95" :height 0.8))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit fixed-pitch :foreground "#fe640b"))))
 '(visual-shorthands-face ((t (:inherit font-lock-keyword-face :extend nil :background "#e0e0e0")))))
